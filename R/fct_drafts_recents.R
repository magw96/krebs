#' Encounter draft autosave + per-user recent-values cache.
#'
#' Both features land in the same file because they share the same lifecycle:
#' write on every meaningful change, read on form mount, delete on submit.
#'
#' Tables created in inst/sql/003_drafts_recents.sql.

# ---------------------------------------------------------------------------
# Encounter drafts
# ---------------------------------------------------------------------------

#' Upsert a draft snapshot for the current user x patient x encounter_type.
#' @param payload Named list of input values. Will be JSON-encoded.
db_save_draft <- function(pool, user, mrn, encounter_type, payload) {
  if (!length(payload)) return(invisible(NULL))
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                           force = TRUE)
  with_tenant(pool, user, function(con) {
    DBI::dbExecute(con,
      "INSERT INTO encounter_drafts
         (user_id, hospital_id, mrn, encounter_type, payload, updated_at)
       VALUES ($1,$2,$3,$4,$5::jsonb, now())
       ON CONFLICT (user_id, hospital_id, mrn, encounter_type)
       DO UPDATE SET payload = EXCLUDED.payload, updated_at = now()",
      params = list(as.integer(user$user_id),
                    as.integer(user$hospital_id),
                    as.character(mrn %||% ""),
                    as.character(encounter_type),
                    as.character(json)))
  })
  invisible(NULL)
}

#' Load the most recent draft (if any) for this user x patient x type.
#' Returns NULL when no draft exists.
db_load_draft <- function(pool, user, mrn, encounter_type) {
  with_tenant(pool, user, function(con) {
    df <- DBI::dbGetQuery(con,
      "SELECT payload, updated_at FROM encounter_drafts
        WHERE user_id = $1 AND hospital_id = $2
          AND mrn = $3 AND encounter_type = $4",
      params = list(as.integer(user$user_id),
                    as.integer(user$hospital_id),
                    as.character(mrn %||% ""),
                    as.character(encounter_type)))
    if (!nrow(df)) return(NULL)
    list(payload = jsonlite::fromJSON(df$payload[1], simplifyVector = FALSE),
         updated_at = df$updated_at[1])
  })
}

#' Drop a draft after a successful submit (or after the user dismisses it).
db_delete_draft <- function(pool, user, mrn, encounter_type) {
  with_tenant(pool, user, function(con) {
    DBI::dbExecute(con,
      "DELETE FROM encounter_drafts
        WHERE user_id = $1 AND hospital_id = $2
          AND mrn = $3 AND encounter_type = $4",
      params = list(as.integer(user$user_id),
                    as.integer(user$hospital_id),
                    as.character(mrn %||% ""),
                    as.character(encounter_type)))
  })
  invisible(NULL)
}

#' Format the elapsed time between now and a timestamp for the restore callout.
human_ago <- function(ts) {
  if (is.null(ts) || is.na(ts)) return("hace un momento")
  s <- as.numeric(difftime(Sys.time(), as.POSIXct(ts), units = "secs"))
  if (s < 60)        sprintf("hace %d s", as.integer(s))
  else if (s < 3600) sprintf("hace %d min", as.integer(s / 60))
  else if (s < 86400) sprintf("hace %d h",  as.integer(s / 3600))
  else                sprintf("hace %d d",  as.integer(s / 86400))
}

# ---------------------------------------------------------------------------
# Recent values
# ---------------------------------------------------------------------------

# Fields tracked across all encounter forms. Order is significant only for
# the lookup loop below. Multi-value inputs (chemo_drugs, surgery_cpt,
# imaging_at_dx) are exploded into one row per value.
.RECENT_FIELDS <- c(
  "oncotree", "primary_site", "icdo3_morph",
  "chemo_drugs", "surgery_cpt",
  "hormonal_drug", "targeted_drug", "immuno_drug",
  "dx_method", "referral_source"
)

#' Fetch the top-N recent values for one (user, field) pair.
#' @return character vector ordered most recent first.
db_recent_values <- function(pool, user, field_key, n = 8) {
  if (is.null(user) || is.null(user$user_id)) return(character(0))
  con <- pool::poolCheckout(pool); on.exit(pool::poolReturn(con))
  df <- DBI::dbGetQuery(con,
    "SELECT value FROM user_recent_values
      WHERE user_id = $1 AND field_key = $2
      ORDER BY last_used DESC LIMIT $3",
    params = list(as.integer(user$user_id),
                  as.character(field_key),
                  as.integer(n)))
  if (!nrow(df)) character(0) else as.character(df$value)
}

#' Upsert one (user, field, value) row, bumping last_used + use_count.
db_upsert_recent_value <- function(pool, user, field_key, value) {
  if (is.null(value) || !nzchar(value)) return(invisible(NULL))
  con <- pool::poolCheckout(pool); on.exit(pool::poolReturn(con))
  DBI::dbExecute(con,
    "INSERT INTO user_recent_values (user_id, field_key, value, last_used, use_count)
     VALUES ($1, $2, $3, now(), 1)
     ON CONFLICT (user_id, field_key, value)
     DO UPDATE SET last_used = now(),
                   use_count = user_recent_values.use_count + 1",
    params = list(as.integer(user$user_id),
                  as.character(field_key),
                  as.character(value)))
  invisible(NULL)
}

#' Walk a values-list (the same one passed to db_insert) and upsert every
#' tracked field. Multi-value entries (lists / vectors of length > 1) are
#' exploded into one row per element. Comma-joined strings (the new therapy
#' drug pickers' format) are split too.
db_track_recents <- function(pool, user, values) {
  for (key in .RECENT_FIELDS) {
    v <- values[[key]]
    if (is.null(v)) next
    if (length(v) == 1L && is.character(v) && grepl(",", v)) {
      v <- trimws(unlist(strsplit(as.character(v), ",", fixed = TRUE)))
    }
    for (one in v) {
      one <- trimws(as.character(one))
      if (is.na(one) || !nzchar(one)) next
      try(db_upsert_recent_value(pool, user, key, one), silent = TRUE)
    }
  }
  invisible(NULL)
}

#' Build a choices object for shinyWidgets::pickerInput / updatePickerInput
#' that puts the user's recent values at the top under a "Recientes" optgroup.
#' Falls back to a flat character vector if there are no recents.
choices_with_recents <- function(full, recents) {
  full <- as.character(full)
  recents <- intersect(as.character(recents), full)
  if (!length(recents)) return(full)
  rest <- setdiff(full, recents)
  list(Recientes = recents, Todos = rest)
}
