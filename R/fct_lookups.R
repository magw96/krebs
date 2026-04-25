#' Lookup data loaders. Cached at app startup; never re-fetched per session.
#'
#' Resolution order for a given CSV key:
#'   1. inst/extdata/<key>            (source checkout)
#'   2. extdata/<key> via system.file (installed package)
#'   3. AWS S3 bucket "cptcode"       (legacy V0.1 fallback)
#'   4. empty data.frame              (so the app still boots)

.cache <- new.env(parent = emptyenv())

.local_extdata <- function(key) {
  for (cand in c(file.path("inst", "extdata", key),
                 system.file("extdata", key, package = "krebs"))) {
    if (nzchar(cand) && file.exists(cand)) return(cand)
  }
  ""
}

#' Strip a UTF-8 BOM from a column header (read.csv leaves it as
#' "X.U.FEFF.<name>" which then fails an %in% names() check).
.strip_bom <- function(df) {
  if (length(names(df))) {
    names(df)[1] <- sub("^\ufeff",       "", names(df)[1])
    names(df)[1] <- sub("^X\\.U\\.FEFF\\.", "", names(df)[1])
  }
  df
}

.s3_csv <- function(key, bucket = "cptcode") {
  if (!is.null(.cache[[key]])) return(.cache[[key]])
  local <- .local_extdata(key)
  out <- if (nzchar(local)) {
    tryCatch(utils::read.csv(local, colClasses = "character",
                             fileEncoding = "UTF-8-BOM"),
             error = function(e) tryCatch(
               utils::read.csv(local, colClasses = "character"),
               error = function(e) data.frame(stringsAsFactors = FALSE)))
  } else {
    tryCatch({
      raw <- aws.s3::get_object(key, bucket = bucket)
      utils::read.csv(text = readBin(raw, "character"),
                      sep = ",", colClasses = "character")
    }, error = function(e) {
      message("[lookup] ", key, " unavailable (", conditionMessage(e), ")")
      data.frame(stringsAsFactors = FALSE)
    })
  }
  out <- .strip_bom(out)
  .cache[[key]] <- out
  out
}

lookup_icd11 <- function() {
  d <- .s3_csv("icd_codes.csv")
  if ("icd_code" %in% names(d)) d$icd_code else character(0)
}
lookup_icdo3 <- function() .s3_csv("icdo3.csv")
lookup_cpt <- function() {
  d <- .s3_csv("ctps.csv")
  if (!"PROCEDURE.DESCRIPTION" %in% names(d)) return(d)
  d[!duplicated(d$PROCEDURE.DESCRIPTION), , drop = FALSE]
}
lookup_drugs <- function() {
  d <- .s3_csv("fda_active_filtered.csv")
  # keep V0.1 shape: caller does sort(lookup_drugs()$x), so map first column.
  if (ncol(d) >= 1L && !"x" %in% names(d)) names(d)[1] <- "x"
  d
}

#' Anatomical sites: use the column header as-it-arrives so a typo or
#' space-vs-period in the source CSV won't silently empty the list.
lookup_sites <- function() {
  base <- character(0)
  d <- lookup_icdo3()
  if (length(d) && nrow(d) > 0L) {
    # try the canonical name first, then any column that looks site-ish
    cands <- intersect(c("Site.Description","Site Description"), names(d))
    if (!length(cands)) {
      cands <- grep("site", names(d), ignore.case = TRUE, value = TRUE)
    }
    if (length(cands)) base <- unique(d[[cands[1]]])
  }
  extras <- c("EXTREMIDAD SUPERIOR DERECHA","EXTREMIDAD SUPERIOR IZQUIERDA",
              "EXTREMIDAD INFERIOR DERECHA","EXTREMIDAD INFERIOR IZQUIERDA")
  sort(unique(c(base[nzchar(base)], extras)))
}

lookup_oncotree <- function() {
  f <- .local_extdata("tumorlist.csv")
  if (!nzchar(f)) return(character(0))
  x <- tryCatch(utils::read.csv(f, fileEncoding = "UTF-8-BOM"),
                error = function(e) tryCatch(utils::read.csv(f),
                                             error = function(e) NULL))
  if (is.null(x) || ncol(x) < 2) return(character(0))
  vals <- x[[2]]
  sort(unique(vals[nzchar(vals)]))
}

#' Tumours that may be bilateral (drives the "Tumor bilateral" checkbox UX).
lookup_bilateral <- function() {
  f <- .local_extdata("bilat.tumors.csv")
  if (!nzchar(f)) return(character(0))
  x <- tryCatch(utils::read.csv(f, fileEncoding = "UTF-8-BOM"),
                error = function(e) tryCatch(utils::read.csv(f),
                                             error = function(e) NULL))
  if (is.null(x) || ncol(x) < 1) return(character(0))
  sort(unique(x[[1]]))
}

#' Mexican states + municipios. Resolution order:
#'   1. mxmaps package (if installed)
#'   2. bundled inst/extdata/mx_municipios.csv
#'   3. hardcoded 32-state fallback so the form never has an empty dropdown
.mx_state_fallback <- function() {
  sort(c("Aguascalientes","Baja California","Baja California Sur","Campeche",
         "Chiapas","Chihuahua",
         "Ciudad de Mexico","Coahuila","Colima","Durango",
         "Estado de Mexico","Guanajuato","Guerrero","Hidalgo","Jalisco",
         "Michoacan","Morelos","Nayarit","Nuevo Leon","Oaxaca","Puebla",
         "Queretaro","Quintana Roo","San Luis Potosi","Sinaloa","Sonora",
         "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatan","Zacatecas"))
}

.mx_load_csv <- function() {
  f <- .local_extdata("mx_municipios.csv")
  if (!nzchar(f)) return(NULL)
  tryCatch(utils::read.csv(f, fileEncoding = "UTF-8", stringsAsFactors = FALSE),
           error = function(e) NULL)
}

lookup_states <- function() {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    return(levels(as.factor(mxmaps::df_mxmunicipio_2020$state_name)))
  }
  d <- .mx_load_csv()
  if (!is.null(d) && "state_name" %in% names(d)) {
    return(sort(unique(d$state_name)))
  }
  .mx_state_fallback()
}

#' Municipios filtered by state. Returns ALL municipios when state is NULL/"".
lookup_municipios <- function(state = NULL) {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    df <- mxmaps::df_mxmunicipio_2020
    if (!is.null(state) && nzchar(state)) df <- df[df$state_name == state, ]
    return(sort(unique(df$municipio_name)))
  }
  d <- .mx_load_csv()
  if (is.null(d) || !"municipio_name" %in% names(d)) return(character(0))
  if (!is.null(state) && nzchar(state) && "state_name" %in% names(d)) {
    d <- d[d$state_name == state, , drop = FALSE]
  }
  sort(unique(d$municipio_name))
}
