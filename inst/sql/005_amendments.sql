-- ============================================================================
-- Krebs - migration 005
--   patient_amendments: tamper-evident log of any post-hoc edit to a
--   patient identity field or a previously-saved encounter field. The
--   underlying row IS still updated (so reads stay simple), but every
--   change is mirrored here with old/new values and a clinician-supplied
--   `motivo`. This is what we show to auditors / IRB when asked
--   "who changed what, when, and why?".
--
-- Idempotent. Safe to re-run.
-- ============================================================================

CREATE TABLE IF NOT EXISTS patient_amendments (
    amendment_id   BIGSERIAL PRIMARY KEY,
    hospital_id    SMALLINT NOT NULL,
    mrn            TEXT     NOT NULL,
    target_table   TEXT     NOT NULL CHECK (target_table IN
                     ('patient_identifiers','encounters')),
    target_id      TEXT     NOT NULL,                  -- encounter_id or 'identity'
    field_name     TEXT     NOT NULL,
    old_value      TEXT,
    new_value      TEXT,
    motivo         TEXT     NOT NULL CHECK (length(trim(motivo)) >= 5),
    amended_by     BIGINT   NOT NULL REFERENCES users(user_id),
    amended_at     TIMESTAMPTZ NOT NULL DEFAULT now(),

    FOREIGN KEY (hospital_id, mrn)
      REFERENCES patient_identifiers(hospital_id, mrn)
      ON DELETE CASCADE
);

COMMENT ON TABLE  patient_amendments IS
  'Append-only log of post-hoc edits to identity or encounter fields.';
COMMENT ON COLUMN patient_amendments.motivo IS
  'Clinician-supplied justification (>=5 chars). Required at save time.';
COMMENT ON COLUMN patient_amendments.target_id IS
  'encounter_id (UUID as text) for encounter edits, or the literal ''identity'' for patient_identifiers edits.';

CREATE INDEX IF NOT EXISTS ix_amend_patient
  ON patient_amendments(hospital_id, mrn, amended_at DESC);
CREATE INDEX IF NOT EXISTS ix_amend_target
  ON patient_amendments(target_table, target_id, amended_at DESC);

-- ---- RLS ------------------------------------------------------------------
ALTER TABLE patient_amendments ENABLE ROW LEVEL SECURITY;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_policies
                  WHERE tablename='patient_amendments'
                    AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON patient_amendments
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
END $$;

-- ---- View: amendments joined with actor name (for the timeline) -----------
CREATE OR REPLACE VIEW v_patient_amendments AS
SELECT a.amendment_id, a.hospital_id, a.mrn,
       a.target_table, a.target_id, a.field_name,
       a.old_value, a.new_value, a.motivo,
       a.amended_at, a.amended_by,
       u.username  AS amended_by_username,
       u.full_name AS amended_by_full_name
  FROM patient_amendments a
  LEFT JOIN users u ON u.user_id = a.amended_by;

-- ============================================================================
-- DONE.
-- ============================================================================
