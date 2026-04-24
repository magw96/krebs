-- ============================================================================
-- Krebs V0.2 schema  --  PostgreSQL 14+
-- Multi-tenant oncology cancer registry with row-level security.
-- ============================================================================
--
-- One-shot bootstrap. Idempotent (CREATE ... IF NOT EXISTS) so it's safe to
-- rerun in dev. For production, prefer numbered migration files.
--
-- Run as the database owner:
--   psql "$KREBS_DB_URL" -f inst/sql/schema.sql
-- ============================================================================

CREATE EXTENSION IF NOT EXISTS pgcrypto;     -- gen_random_uuid
CREATE EXTENSION IF NOT EXISTS pg_trgm;      -- fuzzy name search
CREATE EXTENSION IF NOT EXISTS citext;       -- case-insensitive emails

-- ----------------------------------------------------------------------------
-- 1. HOSPITALS (tenants)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS hospitals (
    hospital_id   SMALLSERIAL PRIMARY KEY,
    code          TEXT UNIQUE NOT NULL,           -- 'INCAN', 'HGM', etc.
    name          TEXT NOT NULL,
    country       TEXT NOT NULL DEFAULT 'MX',
    contact_email CITEXT,
    is_active     BOOLEAN NOT NULL DEFAULT TRUE,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

-- ----------------------------------------------------------------------------
-- 2. USERS (auth + role)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS users (
    user_id       BIGSERIAL PRIMARY KEY,
    username      CITEXT UNIQUE NOT NULL,
    full_name     TEXT NOT NULL,
    email         CITEXT,
    pwd_hash      TEXT NOT NULL,                  -- bcrypt; managed by shinymanager
    role          TEXT NOT NULL CHECK (role IN
                  ('viewer','clinician','researcher','admin','super_admin')),
    hospital_id   SMALLINT REFERENCES hospitals(hospital_id),  -- NULL only for super_admin
    is_active     BOOLEAN NOT NULL DEFAULT TRUE,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    last_login_at TIMESTAMPTZ,
    CHECK ((role = 'super_admin') OR (hospital_id IS NOT NULL))
);
CREATE INDEX IF NOT EXISTS ix_users_hospital ON users(hospital_id);

-- ----------------------------------------------------------------------------
-- 3. PATIENT IDENTIFIERS  (PHI - separated from clinical data)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS patient_identifiers (
    hospital_id   SMALLINT NOT NULL REFERENCES hospitals(hospital_id),
    mrn           TEXT NOT NULL,                  -- expediente, numeric stored as text
    nombre        TEXT NOT NULL,
    fecha_nac     DATE NOT NULL,
    sexo          TEXT NOT NULL CHECK (sexo IN ('M','F','Otro')),
    estado_n      TEXT,
    municipio_n   TEXT,
    is_active     BOOLEAN NOT NULL DEFAULT TRUE,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by    BIGINT NOT NULL REFERENCES users(user_id),
    PRIMARY KEY (hospital_id, mrn),
    CONSTRAINT mrn_numeric CHECK (mrn ~ '^[0-9]+$')
);
-- Trigram index for fuzzy name search:
CREATE INDEX IF NOT EXISTS ix_pi_nombre_trgm
    ON patient_identifiers USING gin (lower(nombre) gin_trgm_ops);
-- Tenant filter speed-up:
CREATE INDEX IF NOT EXISTS ix_pi_hospital ON patient_identifiers(hospital_id);

-- ----------------------------------------------------------------------------
-- 4. ENCOUNTERS  (the longitudinal clinical record)
--    One row per clinical event. Recurrence/treatment/follow-up = new row.
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS encounters (
    encounter_id      UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    hospital_id       SMALLINT NOT NULL,
    mrn               TEXT NOT NULL,
    encounter_type    TEXT NOT NULL CHECK (encounter_type IN
                      ('initial_dx','recurrence','treatment','followup','death')),
    encounter_date    DATE NOT NULL,

    -- staging (nullable; only meaningful for initial_dx and recurrence)
    tnm_t             TEXT,
    tnm_n             TEXT,
    tnm_m             TEXT,
    tnm_t_basis       TEXT CHECK (tnm_t_basis IN ('clinico','patologico') OR tnm_t_basis IS NULL),
    tnm_n_basis       TEXT CHECK (tnm_n_basis IN ('clinico','patologico') OR tnm_n_basis IS NULL),
    icdo3_topo        TEXT,
    icdo3_morph       TEXT,
    oncotree          TEXT,
    primary_site      TEXT,
    bilateral         BOOLEAN,

    -- treatment
    chemo             BOOLEAN DEFAULT FALSE,
    chemo_intent      TEXT CHECK (chemo_intent IN ('neoadyuvante','adyuvante','paliativo') OR chemo_intent IS NULL),
    chemo_drugs       TEXT[],
    chemo_cycles      INT,
    chemo_response    TEXT CHECK (chemo_response IN ('completa','parcial','estable','progresion') OR chemo_response IS NULL),

    radio             BOOLEAN DEFAULT FALSE,
    radio_dose_gy     NUMERIC(6,2),

    surgery_cpt       TEXT[],
    surgery_date      DATE,
    discharge_date    DATE,
    los_days          INT GENERATED ALWAYS AS (
                        CASE WHEN discharge_date IS NOT NULL AND surgery_date IS NOT NULL
                             THEN (discharge_date - surgery_date)::INT END
                      ) STORED,
    complication      TEXT CHECK (complication IN ('ninguna','menor','mayor') OR complication IS NULL),

    -- vital status (only meaningful for followup / death rows)
    vital_status      TEXT CHECK (vital_status IN ('vivo','muerto','perdido') OR vital_status IS NULL),
    death_date        DATE,
    death_cause       TEXT,

    -- meta
    notes             TEXT,
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    created_by        BIGINT NOT NULL REFERENCES users(user_id),

    FOREIGN KEY (hospital_id, mrn) REFERENCES patient_identifiers(hospital_id, mrn)
);
CREATE INDEX IF NOT EXISTS ix_enc_hospital_mrn ON encounters(hospital_id, mrn);
CREATE INDEX IF NOT EXISTS ix_enc_date         ON encounters(encounter_date);
CREATE INDEX IF NOT EXISTS ix_enc_type         ON encounters(encounter_type);

-- ----------------------------------------------------------------------------
-- 5. COMORBIDITIES
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS comorbidities (
    id            BIGSERIAL PRIMARY KEY,
    hospital_id   SMALLINT NOT NULL,
    mrn           TEXT NOT NULL,
    icd11_code    TEXT NOT NULL,
    diagnosed_on  DATE,
    on_treatment  BOOLEAN DEFAULT FALSE,
    drug          TEXT,
    created_at    TIMESTAMPTZ NOT NULL DEFAULT now(),
    FOREIGN KEY (hospital_id, mrn) REFERENCES patient_identifiers(hospital_id, mrn)
);
CREATE INDEX IF NOT EXISTS ix_com_hospital_mrn ON comorbidities(hospital_id, mrn);

-- ----------------------------------------------------------------------------
-- 6. RISK FACTORS / LIFESTYLE  (often need to be updated, hence separate table)
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS lifestyle (
    id              BIGSERIAL PRIMARY KEY,
    hospital_id     SMALLINT NOT NULL,
    mrn             TEXT NOT NULL,
    recorded_on     DATE NOT NULL DEFAULT CURRENT_DATE,
    weight_kg       NUMERIC(5,2),
    height_cm       NUMERIC(5,2),
    bmi             NUMERIC(5,2) GENERATED ALWAYS AS (
                      CASE WHEN height_cm > 0 THEN weight_kg / ((height_cm/100)^2) END
                    ) STORED,
    smoking         TEXT CHECK (smoking IN ('No','1-10','10-20','20+') OR smoking IS NULL),
    alcohol         TEXT CHECK (alcohol IN ('No','1-6','7-13','14+') OR alcohol IS NULL),
    FOREIGN KEY (hospital_id, mrn) REFERENCES patient_identifiers(hospital_id, mrn)
);
CREATE INDEX IF NOT EXISTS ix_life_hospital_mrn ON lifestyle(hospital_id, mrn);

-- ----------------------------------------------------------------------------
-- 7. AUDIT LOG
-- ----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS audit_log (
    id            BIGSERIAL PRIMARY KEY,
    actor_user_id BIGINT REFERENCES users(user_id),
    actor_name    TEXT NOT NULL,        -- captured even if user is later deleted
    hospital_id   SMALLINT,
    action        TEXT NOT NULL,        -- INSERT / UPDATE / DELETE / EXPORT / LOGIN / LOGOUT
    target_table  TEXT,
    target_id     TEXT,
    diff          JSONB,                -- before/after for UPDATE
    at            TIMESTAMPTZ NOT NULL DEFAULT now(),
    ip            INET
);
CREATE INDEX IF NOT EXISTS ix_audit_at      ON audit_log(at DESC);
CREATE INDEX IF NOT EXISTS ix_audit_actor   ON audit_log(actor_user_id);
CREATE INDEX IF NOT EXISTS ix_audit_target  ON audit_log(target_table, target_id);

-- ----------------------------------------------------------------------------
-- 8. ROW-LEVEL SECURITY (multi-tenant isolation)
--    The app sets `app.current_hospital` and `app.is_super_admin` per session.
--    All queries are then automatically scoped to the user's hospital.
-- ----------------------------------------------------------------------------
ALTER TABLE patient_identifiers ENABLE ROW LEVEL SECURITY;
ALTER TABLE encounters          ENABLE ROW LEVEL SECURITY;
ALTER TABLE comorbidities       ENABLE ROW LEVEL SECURITY;
ALTER TABLE lifestyle           ENABLE ROW LEVEL SECURITY;

-- Helper: returns the current hospital_id from the session, or NULL if super_admin.
CREATE OR REPLACE FUNCTION current_hospital() RETURNS SMALLINT
LANGUAGE SQL STABLE AS $$
    SELECT NULLIF(current_setting('app.current_hospital', TRUE), '')::SMALLINT
$$;

CREATE OR REPLACE FUNCTION is_super_admin() RETURNS BOOLEAN
LANGUAGE SQL STABLE AS $$
    SELECT COALESCE(current_setting('app.is_super_admin', TRUE)::BOOLEAN, FALSE)
$$;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_policies WHERE tablename='patient_identifiers' AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON patient_identifiers
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_policies WHERE tablename='encounters' AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON encounters
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_policies WHERE tablename='comorbidities' AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON comorbidities
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
  IF NOT EXISTS (SELECT 1 FROM pg_policies WHERE tablename='lifestyle' AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON lifestyle
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
END $$;

-- ----------------------------------------------------------------------------
-- 9. CONVENIENCE VIEWS
-- ----------------------------------------------------------------------------

-- Latest encounter per patient (for dashboard widgets):
CREATE OR REPLACE VIEW v_patient_latest_encounter AS
SELECT DISTINCT ON (hospital_id, mrn)
       hospital_id, mrn, encounter_id, encounter_type, encounter_date,
       vital_status, death_date
FROM encounters
ORDER BY hospital_id, mrn, encounter_date DESC, created_at DESC;

-- One-row-per-patient summary (initial dx + latest status):
CREATE OR REPLACE VIEW v_patient_summary AS
SELECT pi.hospital_id, pi.mrn, pi.sexo, pi.fecha_nac,
       (CURRENT_DATE - pi.fecha_nac) / 365 AS edad_actual,
       dx.encounter_date  AS fecha_dx,
       dx.tnm_t, dx.tnm_n, dx.tnm_m,
       dx.oncotree        AS tipo_cancer,
       dx.primary_site,
       le.vital_status,
       le.death_date,
       (SELECT COUNT(*) FROM encounters e
          WHERE e.hospital_id = pi.hospital_id AND e.mrn = pi.mrn
            AND e.encounter_type = 'recurrence') AS n_recurrencias,
       (SELECT COUNT(*) FROM encounters e
          WHERE e.hospital_id = pi.hospital_id AND e.mrn = pi.mrn) AS n_encuentros
FROM patient_identifiers pi
LEFT JOIN LATERAL (
    SELECT * FROM encounters e
     WHERE e.hospital_id = pi.hospital_id AND e.mrn = pi.mrn
       AND e.encounter_type = 'initial_dx'
     ORDER BY encounter_date ASC LIMIT 1
) dx ON TRUE
LEFT JOIN v_patient_latest_encounter le
       ON le.hospital_id = pi.hospital_id AND le.mrn = pi.mrn;

-- De-identified clinical view (for researcher role):
CREATE OR REPLACE VIEW v_clinical_deidentified AS
SELECT e.encounter_id, e.hospital_id,
       md5(e.hospital_id::text || ':' || e.mrn) AS pseudo_id,
       e.encounter_type, e.encounter_date,
       e.tnm_t, e.tnm_n, e.tnm_m,
       e.icdo3_topo, e.icdo3_morph, e.oncotree, e.primary_site,
       e.chemo, e.chemo_intent, e.chemo_cycles, e.chemo_response,
       e.radio, e.radio_dose_gy,
       e.surgery_cpt, e.complication,
       e.vital_status, e.death_date, e.death_cause,
       e.los_days
FROM encounters e;

-- ============================================================================
-- DONE.
-- ============================================================================
