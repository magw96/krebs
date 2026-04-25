# UX plan: autosave drafts + recent-values cache

Status: design doc. No code shipped yet for these two features. Implements
the third bullet from the user's "design improvements" list. Catalog + pickers
(bullet 1) and sticky submit + keyboard shortcuts (bullet 2) are already in.

---

## 1. Autosave drafts

### Goal
Clinicians lose work when they tab away, log out, or the browser crashes
mid-encounter. Autosave should silently persist the in-progress encounter
form every few seconds and restore it on next visit, scoped per
`(user_id, patient_mrn, encounter_type)`.

### Storage
New table:

```sql
create table if not exists encounter_drafts (
  user_id      text         not null,
  hospital_id  text         not null,
  mrn          text,            -- nullable for "register new patient" drafts
  encounter_type text       not null,
  payload      jsonb        not null,      -- snapshot of all input$ values
  updated_at   timestamptz  default now(),
  primary key (user_id, hospital_id, coalesce(mrn,''), encounter_type)
);
create index on encounter_drafts (updated_at);
```

One row per draft. `payload` is the full `reactiveValuesToList(input)` minus
the ephemeral keys (`encounter_date`, button counters, `_open`, search
boxes). UPSERT semantics, so we never accumulate junk.

A small worker should DELETE drafts older than 30 days to keep the table tidy.

### Client/server flow

1. `mod_encounter_form_server` adds:
   ```r
   # Debounced autosave
   draft_payload <- shiny::reactive({
     vals <- shiny::reactiveValuesToList(input)
     vals[grepl("^(submit|reset_form|tabs_)", names(vals))] <- NULL
     vals
   }) |> shiny::debounce(2000)

   shiny::observeEvent(draft_payload(), {
     u <- user(); p <- patient()
     DBI::dbExecute(pool,
       "insert into encounter_drafts(user_id,hospital_id,mrn,encounter_type,payload)
        values($1,$2,$3,$4,$5::jsonb)
        on conflict (user_id,hospital_id,coalesce(mrn,''),encounter_type)
        do update set payload=excluded.payload, updated_at=now()",
       params = list(u$user_id, u$hospital_id, p$mrn %||% '',
                     input$encounter_type,
                     jsonlite::toJSON(draft_payload(), auto_unbox = TRUE)))
   }, ignoreInit = TRUE)
   ```
2. On successful submit, DELETE the matching draft row.
3. On module mount, SELECT the matching draft. If found, show a non-blocking
   `bs4Dash::callout(status = "info")`:
   > Hay un borrador guardado de este paciente (hace 14 min). [Restaurar] [Descartar]

   Restore = call `shiny::updateXxxInput()` for each key in `payload`.
   Discard = DELETE the row.

### Edge cases
- Don't autosave anonymous "register new patient" drafts until at least
  `mrn`, `name`, or `dob` is filled — otherwise we save empty rows on every
  tab visit.
- Cap `payload` at ~64 KB. If exceeded, log and skip (free-text notes
  shouldn't blow that up unless the user pastes a chart).
- Prevent restore-loops: store a `draft_dismissed` reactive after the user
  picks Restore or Descartar so the callout doesn't re-appear in the same
  session.

---

## 2. Recent values cache

### Goal
Most clinicians repeat the same handful of choices: their hospital's
preferred OncoTree codes, their go-to chemo regimens, the CPT codes they
actually do. Surface those at the top of the picker so 80 % of choices
become two clicks instead of typing.

### Storage
Lightweight key-value table, cheap to write:

```sql
create table if not exists user_recent_values (
  user_id     text       not null,
  field_key   text       not null,    -- e.g. 'oncotree', 'chemo_drugs', 'surgery_cpt'
  value       text       not null,
  last_used   timestamptz default now(),
  use_count   integer     default 1,
  primary key (user_id, field_key, value)
);
create index on user_recent_values (user_id, field_key, last_used desc);
```

UPSERT on every successful encounter submit:

```sql
insert into user_recent_values (user_id, field_key, value)
values ($1,$2,$3)
on conflict (user_id, field_key, value)
do update set last_used = now(), use_count = user_recent_values.use_count + 1;
```

Fields to track: `oncotree`, `primary_site`, `icdo3_morph`, `chemo_drugs`,
`surgery_cpt`, `hormonal_drug`, `targeted_drug`, `immuno_drug`, `dx_method`,
`referral_source`, plus all `cs_*` cancer-specific picks.

### Surfacing in the picker
shinyWidgets supports `optgroup` via choices like:

```r
choices = list(
  "Recientes" = c("HER2+ HR+ NSCLC EGFRm", ...),
  "Todos"     = full_list
)
```

Server-side, after fetching the user's top-N recent values for that field
(N = 8), prepend them as a "Recientes" optgroup. Live-search still hits the
full list because it indexes both groups.

### Smart defaults
Two cheap wins, no extra table needed:

1. **`encounter_date` defaults to today** — already implemented.
2. **`tnm_t/n/m_basis` default to "clinico"** — clinicians almost always
   stage clinically before path is back; they can flip to "patologico" when
   the report arrives.
3. **`ecog_ps` defaults to user's most recent value for this patient** if
   the encounter is a followup (recurrence/treatment/followup): chase the
   `encounters` table for the most recent non-null `ecog_ps` for `mrn` and
   `updateNumericInput`.
4. **`hospital_id` is implicit** — already pulled from the user session, never
   shown.

---

## Order of work

1. Create both tables in a new migration `inst/migrations/0003_drafts_recents.sql`.
2. Add `db_upsert_recent_values()` and call it inside the existing
   `db_insert_encounter()` post-success path.
3. Add the autosave observer to `mod_encounter_form_server` plus the
   restore callout UI.
4. Patch the four heaviest pickers (oncotree, chemo_drugs, surgery_cpt,
   icdo3_morph) to load with a "Recientes" optgroup. Leave the cancer-
   specific therapy pickers alone for now since they're already filtered
   by category and the lists are short.
5. Add a smoke test in `tests/testthat/test-drafts.R` that round-trips a
   draft.

Estimated effort: ~half a day for autosave, ~2 hrs for recents (one helper
function + four picker updates).
