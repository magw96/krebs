-- =============================================================================
-- Krebs V0.2 - migration 002: extra oncology fields
-- =============================================================================
-- Apply once:  psql "$KREBS_DB_URL" -f inst/sql/002_extra_clinical_fields.sql
-- Idempotent: every ADD COLUMN uses IF NOT EXISTS, every CHECK is dropped first.
-- =============================================================================

-- --- patient_identifiers (PHI + sociodemographics) -------------------------
ALTER TABLE patient_identifiers
  ADD COLUMN IF NOT EXISTS insurance      TEXT,
  ADD COLUMN IF NOT EXISTS ocupacion      TEXT,
  ADD COLUMN IF NOT EXISTS escolaridad    TEXT,
  ADD COLUMN IF NOT EXISTS estado_civil   TEXT,
  ADD COLUMN IF NOT EXISTS telefono       TEXT,
  ADD COLUMN IF NOT EXISTS email          CITEXT,
  ADD COLUMN IF NOT EXISTS curp           TEXT;

-- --- lifestyle (risk factors) ---------------------------------------------
ALTER TABLE lifestyle
  ADD COLUMN IF NOT EXISTS physical_activity TEXT,   -- ninguna / leve / moderada / vigorosa
  ADD COLUMN IF NOT EXISTS drugs             TEXT;   -- ninguna / ocasional / frecuente

-- --- encounters (clinical depth) ------------------------------------------
ALTER TABLE encounters
  ADD COLUMN IF NOT EXISTS tumor_grade           TEXT,    -- G1..G4, GX
  ADD COLUMN IF NOT EXISTS ecog_ps               SMALLINT,-- 0..4
  ADD COLUMN IF NOT EXISTS dx_method             TEXT,    -- biopsia / citologia / imagen / clinico
  ADD COLUMN IF NOT EXISTS imaging_at_dx         TEXT[],  -- TC, RM, PET-CT, US, gammagrafia
  ADD COLUMN IF NOT EXISTS biomarkers            JSONB,   -- ER:+, PR:-, HER2:2+, KRAS:G12D ...
  ADD COLUMN IF NOT EXISTS tumor_markers         JSONB,   -- {"CEA":4.2, "CA125":35}
  ADD COLUMN IF NOT EXISTS family_history_cancer BOOLEAN,
  ADD COLUMN IF NOT EXISTS family_history_detail TEXT,
  ADD COLUMN IF NOT EXISTS prior_cancer          BOOLEAN,
  ADD COLUMN IF NOT EXISTS prior_cancer_site     TEXT,
  ADD COLUMN IF NOT EXISTS first_symptom_date    DATE,
  ADD COLUMN IF NOT EXISTS referral_source       TEXT,    -- urgencias / consulta / referido / tamizaje
  ADD COLUMN IF NOT EXISTS hpv                   TEXT,    -- positivo / negativo / desconocido
  ADD COLUMN IF NOT EXISTS surgery_intent        TEXT,    -- curativa / paliativa / diagnostica
  ADD COLUMN IF NOT EXISTS surgery_margin        TEXT,    -- R0 / R1 / R2
  ADD COLUMN IF NOT EXISTS lymph_nodes_examined  SMALLINT,
  ADD COLUMN IF NOT EXISTS lymph_nodes_positive  SMALLINT,
  ADD COLUMN IF NOT EXISTS hormonal_therapy      BOOLEAN DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS hormonal_drug         TEXT,
  ADD COLUMN IF NOT EXISTS targeted_therapy      BOOLEAN DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS targeted_drug         TEXT,
  ADD COLUMN IF NOT EXISTS immunotherapy         BOOLEAN DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS immuno_drug           TEXT;

-- --- CHECK constraints (drop-and-readd so re-apply works) -----------------
ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_tumor_grade;
ALTER TABLE encounters ADD  CONSTRAINT chk_tumor_grade
  CHECK (tumor_grade IS NULL OR tumor_grade IN ('G1','G2','G3','G4','GX'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_ecog_ps;
ALTER TABLE encounters ADD  CONSTRAINT chk_ecog_ps
  CHECK (ecog_ps IS NULL OR ecog_ps BETWEEN 0 AND 4);

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_dx_method;
ALTER TABLE encounters ADD  CONSTRAINT chk_dx_method
  CHECK (dx_method IS NULL OR dx_method IN ('biopsia','citologia','imagen','clinico','quirurgico'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_surgery_margin;
ALTER TABLE encounters ADD  CONSTRAINT chk_surgery_margin
  CHECK (surgery_margin IS NULL OR surgery_margin IN ('R0','R1','R2'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_surgery_intent;
ALTER TABLE encounters ADD  CONSTRAINT chk_surgery_intent
  CHECK (surgery_intent IS NULL OR surgery_intent IN ('curativa','paliativa','diagnostica'));

-- Refresh the de-identified view to surface the new columns.
DROP VIEW IF EXISTS v_clinical_deidentified;
CREATE VIEW v_clinical_deidentified AS
SELECT
  md5(hospital_id::text || ':' || mrn) AS pseudo_id,
  hospital_id, encounter_type, encounter_date,
  tnm_t, tnm_n, tnm_m, tnm_t_basis, tnm_n_basis,
  primary_site, oncotree, icdo3_morph, bilateral, tumor_grade, ecog_ps,
  dx_method, imaging_at_dx, biomarkers, tumor_markers,
  family_history_cancer, prior_cancer, first_symptom_date, referral_source,
  hpv,
  chemo, chemo_intent, chemo_drugs, chemo_cycles, chemo_response,
  radio, radio_dose_gy,
  hormonal_therapy, hormonal_drug,
  targeted_therapy, targeted_drug,
  immunotherapy, immuno_drug,
  surgery_cpt, surgery_intent, surgery_margin,
  lymph_nodes_examined, lymph_nodes_positive,
  surgery_date, discharge_date, los_days, complication,
  vital_status, death_date, death_cause
FROM encounters;
