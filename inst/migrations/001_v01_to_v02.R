# =============================================================================
# Migration: Krebs V0.1 (S3 CSVs) -> V0.2 (PostgreSQL)
# =============================================================================
#
# One-time ETL. Reads every CSV from the V0.1 AWS bucket "krebs17", maps each
# row to a patient_identifiers + initial_dx encounter pair in the V0.2 schema,
# and inserts everything in a single transaction.
#
# REQUIREMENTS BEFORE RUNNING:
#   * BACK UP THE S3 BUCKET FIRST. Migrations are reversible only if you have
#     the originals. From your shell:
#         aws s3 sync s3://krebs17 ./krebs17_backup_$(date +%Y%m%d)
#   * Schema applied (psql "$KREBS_DB_URL" -f inst/sql/schema.sql).
#   * Default hospital row inserted (or supply HOSPITAL_CODE below).
#   * Env vars set in ~/.Renviron:
#       KREBS_DB_URL, AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_DEFAULT_REGION
#
# USAGE:
#   Rscript inst/migrations/001_v01_to_v02.R [HOSPITAL_CODE] [--dry-run]
#
# All legacy rows get an MRN of the form "LEGACY9NNNNNNN" (8-digit numeric, to
# satisfy the schema CHECK) so a clinician can later replace each with the
# real expediente via the admin UI. The legacy MRN is stored verbatim in
# `notes` so it can be audited.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(aws.s3); library(uuid)
  library(dplyr); library(lubridate); library(jsonlite)
})

args         <- commandArgs(trailingOnly = TRUE)
HOSPITAL_CODE <- if (length(args) >= 1 && !startsWith(args[1], "--")) args[1] else "DEFAULT"
DRY_RUN      <- "--dry-run" %in% args
LEGACY_BUCKET <- "krebs17"

stopifnot(nzchar(Sys.getenv("KREBS_DB_URL")))
stopifnot(nzchar(Sys.getenv("AWS_ACCESS_KEY_ID")))

con <- DBI::dbConnect(RPostgres::Postgres(), .connectionString = Sys.getenv("KREBS_DB_URL"))
on.exit(DBI::dbDisconnect(con), add = TRUE)

# ---- 1. Resolve hospital_id ----
hospital_id <- DBI::dbGetQuery(con,
  "SELECT hospital_id FROM hospitals WHERE code = $1",
  params = list(HOSPITAL_CODE))$hospital_id
stopifnot(length(hospital_id) == 1)
message("[migration] target hospital_id = ", hospital_id, " (", HOSPITAL_CODE, ")")

# ---- 2. Resolve a system user_id for `created_by` ----
sys_user <- DBI::dbGetQuery(con,
  "SELECT user_id FROM users WHERE username = 'admin' LIMIT 1")$user_id
if (length(sys_user) == 0)
  stop("No 'admin' user found. Bootstrap the app once before migrating.")
message("[migration] inserts will be attributed to user_id = ", sys_user)

# ---- 3. Pull every CSV from the legacy bucket ----
message("[migration] listing legacy bucket: s3://", LEGACY_BUCKET)
keys <- vapply(get_bucket(LEGACY_BUCKET), `[[`, character(1), "Key")
message("[migration] found ", length(keys), " files")
if (length(keys) == 0) { message("[migration] nothing to do."); quit(save = "no") }

read_one <- function(k) {
  obj <- aws.s3::get_object(k, bucket = LEGACY_BUCKET)
  utils::read.csv(text = readBin(obj, "character"),
                  stringsAsFactors = FALSE, colClasses = "character")
}
all_rows <- do.call(rbind, lapply(keys, function(k) {
  out <- tryCatch(read_one(k), error = function(e) NULL)
  if (is.null(out)) message("  skipped: ", k) else
    message("  loaded:  ", k, " (", nrow(out), " rows)")
  out
}))
message("[migration] total legacy rows: ", nrow(all_rows))

# ---- 4. Map each row ----
mk_legacy_mrn <- function(i) sprintf("9%07d", i)   # 8 digits, starts with 9

n_inserted <- 0L
n_skipped  <- 0L

DBI::dbBegin(con)
DBI::dbExecute(con, "SET LOCAL app.is_super_admin = 'true'")  # bypass RLS

tryCatch({
  for (i in seq_len(nrow(all_rows))) {
    r <- all_rows[i, , drop = FALSE]

    legacy_mrn <- mk_legacy_mrn(i)
    nombre     <- if (!is.null(r$nombre))   r$nombre   else "DESCONOCIDO"
    fecha_nac  <- tryCatch(as.Date(r$nacimiento), error = function(e) NA)
    if (is.na(fecha_nac)) fecha_nac <- as.Date("1900-01-01")
    sexo       <- switch(toupper(substr(r$sexo, 1, 1)),
                         M = "M", F = "F", "Otro")

    # --- patient_identifiers ---
    DBI::dbExecute(con,
      "INSERT INTO patient_identifiers
         (hospital_id, mrn, nombre, fecha_nac, sexo,
          estado_n, municipio_n, created_by)
       VALUES ($1,$2,$3,$4,$5,$6,$7,$8)
       ON CONFLICT DO NOTHING",
      params = list(hospital_id, legacy_mrn, nombre, fecha_nac, sexo,
                    r$estado_n %||% NA, r$municipio_n %||% NA, sys_user))

    # --- lifestyle ---
    DBI::dbExecute(con,
      "INSERT INTO lifestyle (hospital_id, mrn, weight_kg, height_cm, smoking, alcohol)
       VALUES ($1,$2,$3,$4,$5,$6)",
      params = list(hospital_id, legacy_mrn,
                    suppressWarnings(as.numeric(r$peso)),
                    suppressWarnings(as.numeric(r$estatura)),
                    r$tabaquismo %||% NA, r$alcohol %||% NA))

    # --- initial_dx encounter ---
    fdx <- tryCatch(as.Date(r$fecha_dx), error = function(e) NA)
    if (is.na(fdx)) fdx <- as.Date("1900-01-01")

    # parse comma-separated arrays from V0.1
    chemo_drugs_arr <- if (nzchar(r$meds %||% "")) {
      paste0("{", paste(trimws(strsplit(r$meds, ",")[[1]]), collapse = ","), "}")
    } else NA
    surg_arr <- if (nzchar(r$cirugia %||% "")) {
      paste0("{", paste(trimws(strsplit(r$cirugia, ",")[[1]]), collapse = ","), "}")
    } else NA

    DBI::dbExecute(con,
      "INSERT INTO encounters
        (hospital_id, mrn, encounter_type, encounter_date,
         tnm_t, tnm_n, tnm_m,
         primary_site, oncotree, icdo3_morph, bilateral,
         chemo, chemo_intent, chemo_drugs, chemo_cycles,
         radio, radio_dose_gy,
         surgery_cpt, surgery_date, discharge_date, complication,
         vital_status, death_date,
         notes, created_by)
       VALUES ($1,$2,'initial_dx',$3,
               $4,$5,$6,
               $7,$8,$9,$10,
               $11,$12,$13,$14,
               $15,$16,
               $17,$18,$19,$20,
               $21,$22,
               $23,$24)",
      params = list(
        hospital_id, legacy_mrn, fdx,
        nz(r$tumor), nz(r$nodes), nz(r$mets),
        nz(r$tipo_c1), nz(r$tipo_c2), nz(r$tipo_c3),
        identical(tolower(r$bilateralidad %||% ""), "si"),
        nzchar(r$tipo_quimio %||% "") && r$tipo_quimio != "NA",
        if (r$tipo_quimio %in% c("Neoadyuvante","Adyuvante"))
          tolower(r$tipo_quimio) else NA,
        chemo_drugs_arr,
        suppressWarnings(as.integer(r$quimio_dosis)),
        suppressWarnings(as.numeric(r$radio_dosis)) > 0 &&
          !is.na(suppressWarnings(as.numeric(r$radio_dosis))),
        suppressWarnings(as.numeric(r$radio_dosis)),
        surg_arr,
        tryCatch(as.Date(r$fecha_qx), error = function(e) NA),
        tryCatch(as.Date(r$fecha_alta), error = function(e) NA),
        if (identical(r$complicacion, "No")) "ninguna"
          else if (identical(r$tipo_comp, "Mayor")) "mayor"
          else if (identical(r$tipo_comp, "Menor")) "menor"
          else NA,
        if (identical(r$muerte, "Si") || identical(r$muerte, "Sí")) "muerto" else "vivo",
        tryCatch(as.Date(r$fecha_muerte), error = function(e) NA),
        sprintf("Migrado de V0.1 (id legacy: %s, key: %s)",
                r$id %||% "?", keys[ceiling(i / 1)])
        , sys_user
      ))

    # comorbidities (comma-separated string)
    if (nzchar(r$comorbilidades %||% "")) {
      for (cc in trimws(strsplit(r$comorbilidades, ",")[[1]])) {
        DBI::dbExecute(con,
          "INSERT INTO comorbidities (hospital_id, mrn, icd11_code) VALUES ($1,$2,$3)",
          params = list(hospital_id, legacy_mrn, cc))
      }
    }

    n_inserted <- n_inserted + 1L
    if (n_inserted %% 50 == 0) message("  inserted ", n_inserted, " rows...")
  }

  if (DRY_RUN) {
    message("[migration] DRY RUN -- rolling back")
    DBI::dbRollback(con)
  } else {
    DBI::dbExecute(con,
      "INSERT INTO audit_log (actor_user_id, actor_name, hospital_id, action,
                              target_table, target_id, diff)
       VALUES ($1,'migration_script',$2,'INSERT','encounters',$3,$4::jsonb)",
      params = list(sys_user, hospital_id,
                    paste0("legacy:", n_inserted, "rows"),
                    jsonlite::toJSON(list(source = "krebs17",
                                          n = n_inserted), auto_unbox = TRUE)))
    DBI::dbCommit(con)
    message("[migration] done. ", n_inserted, " encounters inserted.")
  }
},
error = function(e) {
  DBI::dbRollback(con)
  message("[migration] FAILED, rolled back: ", conditionMessage(e))
})

`%||%` <- function(a, b) if (!is.null(a) && nzchar(as.character(a))) a else b
nz     <- function(x) if (is.null(x) || !nzchar(as.character(x))) NA else x
