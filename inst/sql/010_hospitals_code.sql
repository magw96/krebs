-- 010_hospitals_code.sql
--
-- Anade un codigo corto a cada hospital (HSPA, ITESM, ...) para que el
-- modulo del biobanco pueda construir BIOIDs legibles a partir del
-- hospital_id (SMALLINT) del paciente.
--
-- Idempotente: ADD COLUMN IF NOT EXISTS, UPDATE solo donde code IS NULL,
-- DROP-ADD en CHECK / UNIQUE.

BEGIN;

ALTER TABLE hospitals ADD COLUMN IF NOT EXISTS code TEXT;

-- Backfill heuristico por nombre. Si tu hospital no aparece, agrega su
-- codigo manualmente despues de aplicar la migracion.
UPDATE hospitals SET code =
  CASE
    WHEN code IS NOT NULL AND code <> '' THEN code
    WHEN UPPER(name) LIKE '%ANGELES%'                   THEN 'HSPA'
    WHEN UPPER(name) LIKE '%TEC%'
      OR UPPER(name) LIKE '%MONTERREY%'
      OR UPPER(name) LIKE '%ITESM%'                     THEN 'ITESM'
    WHEN UPPER(name) LIKE '%INCAN%'                     THEN 'INCAN'
    WHEN UPPER(name) LIKE '%INER%'                      THEN 'INER'
    ELSE 'INST' || LPAD(hospital_id::TEXT, 2, '0')
  END
WHERE code IS NULL OR code = '';

-- Garantiza unicidad y formato
ALTER TABLE hospitals DROP CONSTRAINT IF EXISTS chk_hospitals_code_format;
ALTER TABLE hospitals ADD  CONSTRAINT chk_hospitals_code_format
  CHECK (code ~ '^[A-Z0-9]{2,8}$');

ALTER TABLE hospitals DROP CONSTRAINT IF EXISTS uq_hospitals_code;
ALTER TABLE hospitals ADD  CONSTRAINT uq_hospitals_code UNIQUE (code);

ALTER TABLE hospitals ALTER COLUMN code SET NOT NULL;

COMMENT ON COLUMN hospitals.code IS
  'Codigo corto en mayusculas (HSPA, ITESM, ...) usado como prefijo en los BIOIDs del biobanco. Maximo 8 chars, solo A-Z y 0-9.';

COMMIT;
