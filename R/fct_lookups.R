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

.s3_csv <- function(key, bucket = "cptcode") {
  if (!is.null(.cache[[key]])) return(.cache[[key]])
  local <- .local_extdata(key)
  out <- if (nzchar(local)) {
    tryCatch(utils::read.csv(local, colClasses = "character"),
             error = function(e) data.frame(stringsAsFactors = FALSE))
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

#' Anatomical sites (V0.1 behaviour: ICD-O-3 + 4 explicit extremity codes).
lookup_sites <- function() {
  base <- character(0)
  d <- lookup_icdo3()
  if ("Site.Description" %in% names(d)) base <- unique(d$Site.Description)
  extras <- c("RIGHT UPPER EXTREMITY","LEFT UPPER EXTREMITY",
              "RIGHT LOWER EXTREMITY","LEFT LOWER EXTREMITY")
  sort(unique(c(base, extras)))
}

lookup_oncotree <- function() {
  f <- .local_extdata("tumorlist.csv")
  if (!nzchar(f)) return(character(0))
  x <- tryCatch(utils::read.csv(f), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 2) return(character(0))
  vals <- x[[2]]
  sort(unique(vals[nzchar(vals)]))
}

#' Tumours that may be bilateral (drives the "Tumor bilateral" checkbox UX).
lookup_bilateral <- function() {
  f <- .local_extdata("bilat.tumors.csv")
  if (!nzchar(f)) return(character(0))
  x <- tryCatch(utils::read.csv(f), error = function(e) NULL)
  if (is.null(x) || ncol(x) < 1) return(character(0))
  sort(unique(x[[1]]))
}

lookup_states <- function() {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    levels(as.factor(mxmaps::df_mxmunicipio_2020$state_name))
  } else {
    # built-in fallback so the dropdown isn't empty when mxmaps isn't installed
    sort(c("Aguascalientes","Baja California","Baja California Sur","Campeche",
           "Chiapas","Chihuahua","Ciudad de México","Coahuila","Colima","Durango",
           "Estado de México","Guanajuato","Guerrero","Hidalgo","Jalisco",
           "Michoacán","Morelos","Nayarit","Nuevo León","Oaxaca","Puebla",
           "Querétaro","Quintana Roo","San Luis Potosí","Sinaloa","Sonora",
           "Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas"))
  }
}

lookup_municipios <- function() {
  if (requireNamespace("mxmaps", quietly = TRUE)) {
    levels(as.factor(mxmaps::df_mxmunicipio_2020$municipio_name))
  } else character(0)
}
