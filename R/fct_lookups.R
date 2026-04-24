#' Lookup data loaders. Cached at app startup; never re-fetched per session.
#'
#' These mirror what V0.1 did with aws.s3::get_object(), but each loader is
#' wrapped in memoise so the S3 round-trip happens at most once per app
#' lifetime. If you migrate the lookups into Postgres later (recommended),
#' replace the loaders with DB queries - the rest of the app is unaffected.

# memoised cache (lives for the app process lifetime)
.cache <- new.env(parent = emptyenv())

#' Generic fetch from the cptcode S3 bucket. Falls back to a local CSV if S3
#' is unreachable so the app still boots in air-gapped dev.
.s3_csv <- function(key, bucket = "cptcode") {
  if (!is.null(.cache[[key]])) return(.cache[[key]])
  out <- tryCatch({
    raw <- aws.s3::get_object(key, bucket = bucket)
    utils::read.csv(text = readBin(raw, "character"),
                    sep = ",", colClasses = "character")
  }, error = function(e) {
    local <- system.file("extdata", key, package = "krebs")
    if (nzchar(local)) utils::read.csv(local, colClasses = "character")
    else                data.frame(stringsAsFactors = FALSE)
  })
  .cache[[key]] <- out
  out
}

lookup_icd11   <- function() .s3_csv("icd_codes.csv")$icd_code
lookup_icdo3   <- function() .s3_csv("icdo3.csv")
lookup_cpt     <- function() {
  d <- .s3_csv("ctps.csv")
  d[!duplicated(d$PROCEDURE.DESCRIPTION), , drop = FALSE]
}
lookup_drugs   <- function() .s3_csv("fda_active_filtered.csv")

#' Anatomical sites (keep V0.1 behaviour: ICD-O-3 + 4 explicit extremity codes).
lookup_sites <- function() {
  base <- unique(lookup_icdo3()$Site.Description)
  extras <- c("RIGHT UPPER EXTREMITY","LEFT UPPER EXTREMITY",
              "RIGHT LOWER EXTREMITY","LEFT LOWER EXTREMITY")
  sort(c(base, extras))
}

lookup_oncotree <- function() {
  # Read the V0.1 tumorlist.csv if bundled, else empty.
  f <- system.file("extdata", "tumorlist.csv", package = "krebs")
  if (nzchar(f)) utils::read.csv(f)[, 2] else character(0)
}

lookup_states <- function() {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    levels(as.factor(mxmaps::df_mxmunicipio_2020$state_name))
  } else character(0)
}

lookup_municipios <- function() {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    levels(as.factor(mxmaps::df_mxmunicipio_2020$municipio_name))
  } else character(0)
}
