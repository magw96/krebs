-- ============================================================================
-- Krebs - migration 004
--   * encounters.line              : 1L, 2L, 3L treatment line tracking
--   * encounters.treatment_intent  : curative / palliative / adjuvant /
--                                    neoadjuvant / mantenimiento
--   * encounter_attachments        : PDF/PNG/JPG bytes inlined (10 MB cap
--                                    enforced application-side, plus a hard
--                                    DB CHECK as a backstop).
-- Idempotent. Safe to re-run.
-- ============================================================================

-- ---- encounters: line + intent ---------------------------------------------
ALTER TABLE encounters
  ADD COLUMN IF NOT EXISTS line             SMALLINT
    CHECK (line IS NULL OR (line >= 1 AND line <= 20)),
  ADD COLUMN IF NOT EXISTS treatment_intent TEXT
    CHECK (treatment_intent IN
      ('curativo','paliativo','adyuvante','neoadyuvante','mantenimiento')
      OR treatment_intent IS NULL);

COMMENT ON COLUMN encounters.line IS
  'Treatment line number for treatment encounters (1=1L, 2=2L, ...). NULL for non-treatment encounters.';
COMMENT ON COLUMN encounters.treatment_intent IS
  'Clinical intent of the treatment encounter (curativo/paliativo/adyuvante/neoadyuvante/mantenimiento).';

-- Helpful for "next line" auto-suggestion and PFS analyses.
CREATE INDEX IF NOT EXISTS ix_enc_line
  ON encounters(hospital_id, mrn, line)
  WHERE encounter_type = 'treatment';

-- ---- encounter_attachments -------------------------------------------------
CREATE TABLE IF NOT EXISTS encounter_attachments (
    attachment_id  BIGSERIAL PRIMARY KEY,
    encounter_id   UUID    NOT NULL REFERENCES encounters(encounter_id)
                                ON DELETE CASCADE,
    hospital_id    SMALLINT NOT NULL,            -- denormalised for RLS
    mrn            TEXT     NOT NULL,            -- denormalised for browse
    filename       TEXT     NOT NULL,
    mime           TEXT     NOT NULL CHECK (mime IN
                              ('application/pdf','image/png','image/jpeg')),
    size_bytes     INTEGER  NOT NULL CHECK (size_bytes > 0
                                            AND size_bytes <= 10485760),  -- 10 MB
    content        BYTEA    NOT NULL,
    uploaded_by    BIGINT   NOT NULL REFERENCES users(user_id),
    uploaded_at    TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS ix_att_encounter
  ON encounter_attachments(encounter_id);
CREATE INDEX IF NOT EXISTS ix_att_patient
  ON encounter_attachments(hospital_id, mrn, uploaded_at DESC);

ALTER TABLE encounter_attachments ENABLE ROW LEVEL SECURITY;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_policies
                  WHERE tablename='encounter_attachments'
                    AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON encounter_attachments
        USING (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
END $$;

-- ---- View: list attachments without dragging the bytea blob ----------------
-- Use this view from R to render the "files attached" list in the timeline;
-- pull `content` only when the user actually clicks Download.
CREATE OR REPLACE VIEW v_encounter_attachments_meta AS
SELECT attachment_id, encounter_id, hospital_id, mrn,
       filename, mime, size_bytes, uploaded_by, uploaded_at
  FROM encounter_attachments;

-- ============================================================================
-- DONE.
-- ============================================================================
