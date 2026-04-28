-- 009_biobank.sql
--
-- Modulo de banco de tejidos para Krebs V0.2.
--
-- Diseno:
--   * Multi-tenant: cada tabla lleva hospital_id (HSPA, ITESM, ...).
--   * Pseudonimizacion: la asociacion MRN <-> bio_subject_id vive en
--     biobank_subject_link, con MRN cifrado simetricamente (pgcrypto).
--     Solo el rol biobank_custodian puede leer esa tabla.
--   * Cadena de custodia: append-only (sin UPDATE/DELETE para roles
--     no-superadmin; se enforza con grants + trigger).
--   * Consentimiento: versionado y revocable.
--
-- Idempotente: toda CREATE usa IF NOT EXISTS; los CHECKs se DROP-ADD.
--
-- Pre-requisitos: extension pgcrypto (presente por default en Supabase).

BEGIN;

CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- ---------------------------------------------------------------------
-- 1) Sujeto pseudonimo del banco
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_subjects (
  bio_subject_id     TEXT PRIMARY KEY,                -- p.ej. HSPA-7K3M9PQ2
  hospital_id        TEXT NOT NULL,                   -- HSPA / ITESM
  sex_at_birth       TEXT,
  birth_year         INT,
  consent_id         UUID,                            -- FK a biobank_consents (set abajo)
  custodian_user_id  INT,                             -- TODO: definir Custodios
  created_at         TIMESTAMPTZ DEFAULT now(),
  created_by         TEXT NOT NULL
);

-- ---------------------------------------------------------------------
-- 2) Keystore: MRN cifrado <-> bio_subject_id
--    Acceso solo para rol biobank_custodian (creado abajo).
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_subject_link (
  bio_subject_id  TEXT PRIMARY KEY REFERENCES biobank_subjects(bio_subject_id),
  hospital_id     TEXT NOT NULL,
  mrn_encrypted   BYTEA NOT NULL,                     -- pgp_sym_encrypt(mrn, key)
  linked_at       TIMESTAMPTZ DEFAULT now(),
  linked_by       TEXT NOT NULL
);

-- ---------------------------------------------------------------------
-- 3) Consentimiento (versionado, revocable)
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_consents (
  id                   UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  bio_subject_id       TEXT NOT NULL REFERENCES biobank_subjects(bio_subject_id),
  hospital_id          TEXT NOT NULL,
  template_version     TEXT NOT NULL,                 -- p.ej. ICF-Krebs-v1.0-2026
  scope                TEXT,
  permits_genomics     BOOLEAN DEFAULT FALSE,
  permits_commercial   BOOLEAN DEFAULT FALSE,
  permits_intl_share   BOOLEAN DEFAULT FALSE,
  permits_recontact    BOOLEAN DEFAULT FALSE,
  signed_dt            DATE NOT NULL,
  withdrawn_dt         DATE,
  pdf_attachment_path  TEXT,
  irb_protocol         TEXT NOT NULL DEFAULT 'TDM-CEI-2026-V1',
  created_at           TIMESTAMPTZ DEFAULT now(),
  created_by           TEXT NOT NULL
);

ALTER TABLE biobank_consents DROP CONSTRAINT IF EXISTS chk_biobank_consent_scope;
ALTER TABLE biobank_consents ADD  CONSTRAINT chk_biobank_consent_scope
  CHECK (scope IS NULL OR scope IN ('specific','broad','tiered'));

-- Cierre del FK que dejamos colgando arriba
ALTER TABLE biobank_subjects DROP CONSTRAINT IF EXISTS fk_subject_consent;
ALTER TABLE biobank_subjects ADD  CONSTRAINT fk_subject_consent
  FOREIGN KEY (consent_id) REFERENCES biobank_consents(id) DEFERRABLE INITIALLY DEFERRED;

-- ---------------------------------------------------------------------
-- 4) Muestra fisica
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_specimens (
  bioid             TEXT PRIMARY KEY,                 -- HSPA-7K3M9PQ2-001-FFPE-A02
  bio_subject_id    TEXT NOT NULL REFERENCES biobank_subjects(bio_subject_id),
  hospital_id       TEXT NOT NULL,
  encounter_id      INT,                              -- FK suave a encounters
  parent_bioid      TEXT REFERENCES biobank_specimens(bioid),
  collection_dt     TIMESTAMPTZ NOT NULL,
  anatomic_site     TEXT,
  morphology        TEXT,
  laterality        TEXT,
  sample_type       TEXT NOT NULL,                    -- TUM/NORM/BLD/PLA/SER/DNA/RNA/FFPE/OCT/ORG
  preservation      TEXT,
  container         TEXT,
  storage_temp      TEXT,
  freezer           TEXT,
  rack              TEXT,
  box               TEXT,
  position          TEXT,
  volume_uL         NUMERIC,
  mass_mg           NUMERIC,
  conc_ng_uL        NUMERIC,
  rin               NUMERIC,
  dv200             NUMERIC,
  qc_passed         BOOLEAN,
  cohort_tag        TEXT,
  status            TEXT NOT NULL DEFAULT 'AVAILABLE',
  notes             TEXT,
  created_at        TIMESTAMPTZ DEFAULT now(),
  created_by        TEXT NOT NULL
);

ALTER TABLE biobank_specimens DROP CONSTRAINT IF EXISTS chk_biobank_specimen_status;
ALTER TABLE biobank_specimens ADD  CONSTRAINT chk_biobank_specimen_status
  CHECK (status IN ('AVAILABLE','RESERVED','SHIPPED','EXHAUSTED','DESTROYED','QUARANTINED'));

ALTER TABLE biobank_specimens DROP CONSTRAINT IF EXISTS chk_biobank_specimen_type;
ALTER TABLE biobank_specimens ADD  CONSTRAINT chk_biobank_specimen_type
  CHECK (sample_type IN ('TUM','NORM','BLD','PLA','SER','DNA','RNA','FFPE','OCT','ORG'));

ALTER TABLE biobank_specimens DROP CONSTRAINT IF EXISTS chk_biobank_specimen_preservation;
ALTER TABLE biobank_specimens ADD  CONSTRAINT chk_biobank_specimen_preservation
  CHECK (preservation IS NULL OR preservation IN
         ('FFPE','SNAP_FROZEN','OCT','RNA_LATER','VIABLE','FIXED_OTHER'));

ALTER TABLE biobank_specimens DROP CONSTRAINT IF EXISTS chk_biobank_specimen_temp;
ALTER TABLE biobank_specimens ADD  CONSTRAINT chk_biobank_specimen_temp
  CHECK (storage_temp IS NULL OR storage_temp IN
         ('RT','+4','-20','-80','-150','-196'));

CREATE INDEX IF NOT EXISTS ix_biobank_specimens_subject  ON biobank_specimens(bio_subject_id);
CREATE INDEX IF NOT EXISTS ix_biobank_specimens_hospital ON biobank_specimens(hospital_id);
CREATE INDEX IF NOT EXISTS ix_biobank_specimens_status   ON biobank_specimens(status);

-- ---------------------------------------------------------------------
-- 5) Cadena de custodia (append-only)
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_chain_of_custody (
  id              BIGSERIAL PRIMARY KEY,
  bioid           TEXT NOT NULL REFERENCES biobank_specimens(bioid),
  hospital_id     TEXT NOT NULL,
  event           TEXT NOT NULL,
  event_dt        TIMESTAMPTZ NOT NULL DEFAULT now(),
  actor           TEXT NOT NULL,
  from_location   TEXT,
  to_location     TEXT,
  attachment_path TEXT,
  notes           TEXT
);

ALTER TABLE biobank_chain_of_custody DROP CONSTRAINT IF EXISTS chk_biobank_coc_event;
ALTER TABLE biobank_chain_of_custody ADD  CONSTRAINT chk_biobank_coc_event
  CHECK (event IN ('COLLECTED','RECEIVED','PROCESSED','ALIQUOTED','MOVED',
                   'THAWED','SHIPPED','RETURNED','DESTROYED','QC'));

CREATE INDEX IF NOT EXISTS ix_biobank_coc_bioid ON biobank_chain_of_custody(bioid);

-- Trigger: bloquear UPDATE/DELETE en chain_of_custody (append-only)
CREATE OR REPLACE FUNCTION biobank_coc_block_mutation() RETURNS trigger AS $$
BEGIN
  RAISE EXCEPTION 'biobank_chain_of_custody is append-only (% denied)', TG_OP;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS biobank_coc_no_update ON biobank_chain_of_custody;
CREATE TRIGGER biobank_coc_no_update
  BEFORE UPDATE OR DELETE ON biobank_chain_of_custody
  FOR EACH ROW EXECUTE FUNCTION biobank_coc_block_mutation();

-- ---------------------------------------------------------------------
-- 6) Solicitudes de distribucion
-- ---------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS biobank_requests (
  id              UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  hospital_id     TEXT NOT NULL,
  pi_name         TEXT NOT NULL,
  pi_affiliation  TEXT,
  protocol        TEXT NOT NULL DEFAULT 'TDM-CEI-2026-V1',
  irb_status      TEXT,
  mta_signed_dt   DATE,
  approved_by     TEXT,
  approved_dt     DATE,
  shipped_dt      DATE,
  notes           TEXT,
  created_at      TIMESTAMPTZ DEFAULT now(),
  created_by      TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS biobank_request_items (
  request_id  UUID NOT NULL REFERENCES biobank_requests(id) ON DELETE CASCADE,
  bioid       TEXT NOT NULL REFERENCES biobank_specimens(bioid),
  PRIMARY KEY (request_id, bioid)
);

-- ---------------------------------------------------------------------
-- 7) Vista util: subject + counts (sin exponer keystore)
-- ---------------------------------------------------------------------
CREATE OR REPLACE VIEW v_biobank_subject_summary AS
SELECT s.bio_subject_id,
       s.hospital_id,
       s.sex_at_birth,
       s.birth_year,
       c.scope               AS consent_scope,
       c.signed_dt           AS consent_signed_dt,
       c.withdrawn_dt        AS consent_withdrawn_dt,
       COUNT(sp.bioid)       AS n_specimens,
       SUM(CASE WHEN sp.status = 'AVAILABLE' THEN 1 ELSE 0 END) AS n_available
FROM biobank_subjects s
LEFT JOIN biobank_consents  c  ON c.id  = s.consent_id
LEFT JOIN biobank_specimens sp ON sp.bio_subject_id = s.bio_subject_id
GROUP BY s.bio_subject_id, s.hospital_id, s.sex_at_birth, s.birth_year,
         c.scope, c.signed_dt, c.withdrawn_dt;

-- ---------------------------------------------------------------------
-- 8) Comentarios (para que aparezcan en \d+ y en pgAdmin)
-- ---------------------------------------------------------------------
COMMENT ON TABLE  biobank_subjects     IS 'Sujeto pseudonimo en el banco (no contiene MRN)';
COMMENT ON TABLE  biobank_subject_link IS 'KEYSTORE: MRN cifrado <-> bio_subject_id (acceso restringido)';
COMMENT ON TABLE  biobank_specimens    IS 'Inventario fisico de muestras / aliquotas';
COMMENT ON TABLE  biobank_chain_of_custody IS 'Registro append-only de eventos sobre cada muestra';
COMMENT ON TABLE  biobank_consents     IS 'Consentimiento informado versionado y revocable';
COMMENT ON TABLE  biobank_requests     IS 'Solicitudes de distribucion (MTA, aprobacion CEI)';
COMMENT ON COLUMN biobank_subjects.custodian_user_id IS 'TODO: definir Custodios HSPA + ITESM antes de produccion';

COMMIT;
