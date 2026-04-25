-- =============================================================================
-- Krebs V0.2 - migration 003: encounter drafts + per-user recent values
-- =============================================================================
-- Apply once:  psql "$KREBS_DB_URL" -f inst/sql/003_drafts_recents.sql
-- Idempotent:  CREATE TABLE IF NOT EXISTS, DO blocks for policies.
-- =============================================================================

-- ---------------------------------------------------------------------------
-- 1. ENCOUNTER DRAFTS
--    One row per (user, hospital, mrn, encounter_type). Stores a JSON snapshot
--    of the encounter form so a clinician can resume after browser crash /
--    logout. Cleared on successful submit.
--
--    A clinician with three patient tabs open gets three drafts -- one per
--    (mrn, encounter_type) pair. mrn = '' for anonymous "register new patient"
--    drafts (no MRN yet).
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS encounter_drafts (
    user_id        BIGINT       NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    hospital_id    SMALLINT     NOT NULL REFERENCES hospitals(hospital_id),
    mrn            TEXT         NOT NULL DEFAULT '',
    encounter_type TEXT         NOT NULL,
    payload        JSONB        NOT NULL,
    updated_at     TIMESTAMPTZ  NOT NULL DEFAULT now(),
    PRIMARY KEY (user_id, hospital_id, mrn, encounter_type)
);
CREATE INDEX IF NOT EXISTS ix_drafts_updated ON encounter_drafts (updated_at);
CREATE INDEX IF NOT EXISTS ix_drafts_user    ON encounter_drafts (user_id);

ALTER TABLE encounter_drafts ENABLE ROW LEVEL SECURITY;

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_policies
                 WHERE tablename='encounter_drafts'
                   AND policyname='tenant_isolation') THEN
    EXECUTE $p$
      CREATE POLICY tenant_isolation ON encounter_drafts
        USING      (is_super_admin() OR hospital_id = current_hospital())
        WITH CHECK (is_super_admin() OR hospital_id = current_hospital())
    $p$;
  END IF;
END $$;

-- Auto-cleanup helper: drop drafts older than 30 days. Call from a cron / pg_cron
-- job, or invoke manually:  SELECT prune_old_drafts();
CREATE OR REPLACE FUNCTION prune_old_drafts() RETURNS INTEGER
LANGUAGE plpgsql AS $$
DECLARE
  n INTEGER;
BEGIN
  DELETE FROM encounter_drafts WHERE updated_at < now() - INTERVAL '30 days';
  GET DIAGNOSTICS n = ROW_COUNT;
  RETURN n;
END $$;

-- ---------------------------------------------------------------------------
-- 2. USER RECENT VALUES
--    Per-user MRU cache so heavy pickers (oncotree, chemo_drugs, surgery_cpt,
--    icdo3_morph) can prepend a "Recientes" optgroup with the user's top picks.
--    UPSERT on every successful encounter submit.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS user_recent_values (
    user_id     BIGINT       NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    field_key   TEXT         NOT NULL,
    value       TEXT         NOT NULL,
    last_used   TIMESTAMPTZ  NOT NULL DEFAULT now(),
    use_count   INTEGER      NOT NULL DEFAULT 1,
    PRIMARY KEY (user_id, field_key, value)
);
CREATE INDEX IF NOT EXISTS ix_recent_user_field
    ON user_recent_values (user_id, field_key, last_used DESC);

-- This table is per-user, not per-tenant: a clinician's MRU follows them even
-- if they switch hospitals. RLS is therefore on user_id only -- enforced at
-- the application layer (db_recent_values always filters by user_id).
-- No tenant policy is added; we just disable RLS to keep selects fast.
ALTER TABLE user_recent_values DISABLE ROW LEVEL SECURITY;
