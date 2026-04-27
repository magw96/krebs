-- 007_complication_clavien_dindo.sql
--
-- Replace the legacy `complication` enum (ninguna/menor/mayor) with the
-- standardized Clavien-Dindo classification of surgical complications:
--   - ninguna  : no postoperative complication
--   - I        : any deviation from normal course w/o pharm/surgical intervention
--   - II       : requires pharm treatment (incl. blood transfusion, TPN)
--   - IIIa     : requires surgical/endoscopic/radiologic intervention, NO general anesthesia
--   - IIIb     : requires intervention UNDER general anesthesia
--   - IVa      : life-threatening; single-organ dysfunction (incl. dialysis)
--   - IVb      : life-threatening; multi-organ dysfunction
--   - V        : death of the patient
--
-- Existing rows: 'menor' -> 'II', 'mayor' -> 'IIIb' (best-fit mapping).
-- Re-running the migration is safe (drop-then-add constraint, idempotent UPDATE).

BEGIN;

-- 1. Drop the old constraint so the migration can rewrite values.
ALTER TABLE encounters DROP CONSTRAINT IF EXISTS encounters_complication_check;
ALTER TABLE encounters DROP CONSTRAINT IF EXISTS chk_complication_clavien;

-- 2. Best-effort backfill of legacy values.
UPDATE encounters SET complication = 'II'   WHERE complication = 'menor';
UPDATE encounters SET complication = 'IIIb' WHERE complication = 'mayor';

-- 3. New Clavien-Dindo CHECK constraint.
ALTER TABLE encounters ADD CONSTRAINT chk_complication_clavien
  CHECK (complication IS NULL OR complication IN
         ('ninguna','I','II','IIIa','IIIb','IVa','IVb','V'));

COMMENT ON COLUMN encounters.complication IS
  'Postoperative complication, Clavien-Dindo classification (ninguna / I / II / IIIa / IIIb / IVa / IVb / V).';

COMMIT;
