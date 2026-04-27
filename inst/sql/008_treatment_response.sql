-- 008_treatment_response.sql
--
-- Replace the awkward `prior_treatment_response` field (which only made
-- sense for recurrence rows AND only described the line BEFORE the
-- recurrence) with a forward-looking pair: `treatment_response` is the
-- response to the treatment registered IN this same encounter, and
-- `treatment_response_method` records how it was evaluated (clinical,
-- imaging, or pathological in re-operated patients).
--
-- The old `prior_treatment_response` column is left in place so historical
-- recurrence rows do not lose data. New writes go to the new pair.
--
-- Idempotent: drop-then-add the CHECK constraints so re-running is safe.

BEGIN;

ALTER TABLE encounters
  ADD COLUMN IF NOT EXISTS treatment_response        TEXT,
  ADD COLUMN IF NOT EXISTS treatment_response_method TEXT;

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_treatment_response;
ALTER TABLE encounters ADD  CONSTRAINT chk_treatment_response
  CHECK (treatment_response IS NULL OR treatment_response IN
         ('completa','parcial','estable','progresion','no_evaluable'));

ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_treatment_response_method;
ALTER TABLE encounters ADD  CONSTRAINT chk_treatment_response_method
  CHECK (treatment_response_method IS NULL OR treatment_response_method IN
         ('clinica','imagen','patologica'));

COMMENT ON COLUMN encounters.treatment_response IS
  'Best response to the treatment registered in this same encounter (completa / parcial / estable / progresion / no_evaluable). Replaces chemo_response for the unified treatment block.';
COMMENT ON COLUMN encounters.treatment_response_method IS
  'How the response was evaluated: clinica (physical exam / symptoms), imagen (RECIST or other radiology), patologica (only in re-operated patients where tissue was re-examined).';

COMMIT;
