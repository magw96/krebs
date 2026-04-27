-- 006_recurrence_metastasis.sql
--
-- Add structured fields to capture re-staging at the time of a
-- recurrence/progression encounter, so that ECOG, TNM and the actual
-- pattern of recurrence (local vs regional vs distant) are stored on the
-- recurrence row itself and DO NOT overwrite the initial_dx values.
--
-- All columns are nullable: previous rows remain valid.

ALTER TABLE encounters
  -- Pattern of recurrence ----------------------------------------------------
  ADD COLUMN IF NOT EXISTS recurrence_type         TEXT,
  ADD COLUMN IF NOT EXISTS recurrence_sites        TEXT[],   -- distant met sites
  ADD COLUMN IF NOT EXISTS recurrence_confirmation TEXT,     -- how was it confirmed
  ADD COLUMN IF NOT EXISTS biopsy_done             BOOLEAN,  -- new histology obtained?
  ADD COLUMN IF NOT EXISTS prior_treatment_response TEXT,    -- response to last line before recurrence
  ADD COLUMN IF NOT EXISTS time_to_recurrence_days INT;      -- days from initial_dx to this event

-- Constraint sets (drop-then-add so re-running the migration is safe).
ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_recurrence_type;
ALTER TABLE encounters ADD  CONSTRAINT chk_recurrence_type
  CHECK (recurrence_type IS NULL OR recurrence_type IN
         ('local','regional','distancia','mixta','desconocida'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_recurrence_confirmation;
ALTER TABLE encounters ADD  CONSTRAINT chk_recurrence_confirmation
  CHECK (recurrence_confirmation IS NULL OR recurrence_confirmation IN
         ('imagen','biopsia','citologia','clinico','marcador_serologico','quirurgico'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_prior_treatment_response;
ALTER TABLE encounters ADD  CONSTRAINT chk_prior_treatment_response
  CHECK (prior_treatment_response IS NULL OR prior_treatment_response IN
         ('completa','parcial','estable','progresion','no_evaluable'));

COMMENT ON COLUMN encounters.recurrence_type IS
  'Pattern of recurrence/progression captured at the recurrence encounter (local / regional / distancia / mixta / desconocida). NULL for non-recurrence rows.';
COMMENT ON COLUMN encounters.recurrence_sites IS
  'Distant metastasis sites recorded at the recurrence encounter (e.g. cerebro, hueso, pulmon, higado, peritoneo, ganglios distantes, suprarrenal, piel).';
COMMENT ON COLUMN encounters.recurrence_confirmation IS
  'How the recurrence was confirmed (imaging / biopsy / cytology / clinical / serum marker / surgical).';
COMMENT ON COLUMN encounters.biopsy_done IS
  'Whether a new biopsy was obtained at the recurrence (re-biopsy informs molecular re-profiling).';
COMMENT ON COLUMN encounters.prior_treatment_response IS
  'Best response to the most recent treatment line BEFORE this recurrence/progression.';
COMMENT ON COLUMN encounters.time_to_recurrence_days IS
  'Days from initial_dx encounter_date to this encounter_date. Computed by the app at insert time.';

CREATE INDEX IF NOT EXISTS ix_enc_rec_type
  ON encounters(hospital_id, mrn, recurrence_type)
  WHERE encounter_type = 'recurrence';
