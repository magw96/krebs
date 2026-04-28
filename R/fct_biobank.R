#' Biobank helpers (Krebs V0.2)
#'
#' Funciones de bajo nivel para el modulo de banco de tejidos:
#'   * pseudonymize_mrn()    HMAC-SHA256(mrn || institution_salt) -> base32 8 chars
#'   * bioid_compose()       Construye el BIOID a partir de los componentes
#'   * bioid_next_aliquot()  Siguiente A01/A02/... para una muestra padre
#'   * label_pdf_path()      Genera un PDF imprimible con codigo Code128
#'
#' Variables de entorno requeridas (rotables):
#'   KREBS_BIOBANK_HMAC_SALT  Sal global para el HMAC.
#'   KREBS_BIOBANK_ENC_KEY    Clave simetrica AES (pgp_sym_encrypt) para el
#'                            keystore (biobank_subject_link.mrn_encrypted).

# ---- Pseudonimizacion ------------------------------------------------------

#' HMAC-SHA256 truncado y codificado en base32 (Crockford, sin chars ambiguos).
#' Determinista: el mismo MRN dentro de la misma institucion siempre produce
#' el mismo SUBJ. Sin la sal el resultado no se puede invertir.
#'
#' @param mrn         MRN del paciente (character).
#' @param hospital_id Codigo corto de la institucion (HSPA / ITESM / ...).
#' @param salt        Sal global; si NULL, se toma de KREBS_BIOBANK_HMAC_SALT.
#' @return Cadena de 8 caracteres en base32 (Crockford).
pseudonymize_mrn <- function(mrn, hospital_id, salt = NULL) {
  if (is.null(mrn) || !nzchar(mrn) || is.null(hospital_id) || !nzchar(hospital_id))
    stop("pseudonymize_mrn: mrn y hospital_id son requeridos")
  salt <- salt %||% Sys.getenv("KREBS_BIOBANK_HMAC_SALT", unset = "")
  if (!nzchar(salt))
    stop("KREBS_BIOBANK_HMAC_SALT no esta definida en el entorno")

  msg  <- paste0(toupper(hospital_id), "|", mrn)
  raw  <- digest::hmac(key = salt, object = msg, algo = "sha256",
                       serialize = FALSE, raw = TRUE)
  # Crockford base32 sin I L O U
  alphabet <- c(strsplit("0123456789ABCDEFGHJKMNPQRSTVWXYZ","")[[1]])
  bits <- as.integer(rawToBits(raw))
  out <- character(0)
  i <- 1L
  while (length(out) < 8 && i + 4L <= length(bits)) {
    chunk <- bits[i:(i+4L)]
    val <- sum(chunk * c(1,2,4,8,16))
    out <- c(out, alphabet[val + 1L])
    i <- i + 5L
  }
  paste(out, collapse = "")
}

# ---- BIOID -----------------------------------------------------------------

#' Compose a BIOID from its parts.
#'
#' Formato: {INST}-{SUBJ}-{COL}-{TYPE}-{ALIQ}
#'   INST   HSPA / ITESM / ...
#'   SUBJ   8 chars base32 (de pseudonymize_mrn)
#'   COL    n.de colecta para ese sujeto, 3 digitos cero-padded ("001")
#'   TYPE   TUM/NORM/BLD/PLA/SER/DNA/RNA/FFPE/OCT/ORG
#'   ALIQ   "A01", "A02", ...
bioid_compose <- function(inst, subj, col_n, sample_type, aliq_n = 1L) {
  sprintf("%s-%s-%03d-%s-A%02d",
          toupper(inst), toupper(subj),
          as.integer(col_n), toupper(sample_type), as.integer(aliq_n))
}

#' Cuantas colectas existen ya para un sujeto, por tipo de muestra.
#' Devuelve el siguiente entero a usar en `col_n`.
bioid_next_collection <- function(pool, bio_subject_id, sample_type) {
  q <- "SELECT COUNT(DISTINCT split_part(bioid,'-',3))
          FROM biobank_specimens
         WHERE bio_subject_id = $1
           AND sample_type    = $2"
  n <- DBI::dbGetQuery(pool, q,
                       params = list(bio_subject_id, toupper(sample_type)))[[1]]
  as.integer(n) + 1L
}

#' Cuantas aliquotas existen ya para una colecta dada.
bioid_next_aliquot <- function(pool, bio_subject_id, sample_type, col_n) {
  q <- "SELECT COUNT(*)
          FROM biobank_specimens
         WHERE bio_subject_id = $1
           AND sample_type    = $2
           AND split_part(bioid,'-',3) = lpad($3::text,3,'0')"
  n <- DBI::dbGetQuery(pool, q,
                       params = list(bio_subject_id, toupper(sample_type),
                                     as.integer(col_n)))[[1]]
  as.integer(n) + 1L
}

# ---- Subject upsert ---------------------------------------------------------

#' Resuelve el codigo corto (HSPA / ITESM) a partir del hospital_id
#' (SMALLINT) almacenado en patients/users. Devuelve "INSTxx" si el
#' hospital no tiene code asignado todavia.
resolve_hospital_code <- function(pool, hospital_id) {
  if (is.null(hospital_id) || is.na(hospital_id))
    return("INST00")
  hid <- suppressWarnings(as.integer(hospital_id))
  if (is.na(hid)) return(toupper(as.character(hospital_id)))
  res <- tryCatch(
    DBI::dbGetQuery(pool,
      "SELECT code FROM hospitals WHERE hospital_id = $1",
      params = list(hid)),
    error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0L || !nzchar(res$code[1]))
    return(sprintf("INST%02d", hid))
  toupper(res$code[1])
}

#' Crea (si no existe) el bio_subject_id para un MRN dado y guarda la
#' asociacion cifrada en biobank_subject_link. Devuelve el bio_subject_id.
biobank_subject_upsert <- function(pool, mrn, hospital_id, user_email) {
  inst <- if (is.numeric(hospital_id) ||
              !is.na(suppressWarnings(as.integer(hospital_id))))
            resolve_hospital_code(pool, hospital_id)
          else
            toupper(as.character(hospital_id))
  subj <- pseudonymize_mrn(mrn, inst)
  bio_subject_id <- paste0(inst, "-", subj)

  enc_key <- Sys.getenv("KREBS_BIOBANK_ENC_KEY", unset = "")
  if (!nzchar(enc_key))
    stop("KREBS_BIOBANK_ENC_KEY no esta definida en el entorno")

  # Insertar sujeto si no existe
  DBI::dbExecute(pool,
    "INSERT INTO biobank_subjects (bio_subject_id, hospital_id, created_by)
     VALUES ($1, $2, $3)
     ON CONFLICT (bio_subject_id) DO NOTHING",
    params = list(bio_subject_id, inst, user_email))

  # Insertar mapping cifrado si no existe
  DBI::dbExecute(pool,
    "INSERT INTO biobank_subject_link
       (bio_subject_id, hospital_id, mrn_encrypted, linked_by)
     VALUES ($1, $2, pgp_sym_encrypt($3, $4), $5)
     ON CONFLICT (bio_subject_id) DO NOTHING",
    params = list(bio_subject_id, inst, mrn, enc_key, user_email))

  bio_subject_id
}

# ---- Etiqueta imprimible --------------------------------------------------

#' Genera un PDF de una etiqueta con BIOID + Code128.
#' Devuelve la ruta al archivo (en tempdir).
biobank_label_pdf <- function(bioid, sample_type, collection_dt,
                              hospital_id, out_path = NULL) {
  if (is.null(out_path))
    out_path <- tempfile(pattern = paste0("label_", bioid, "_"),
                         fileext = ".pdf")

  # Lazy: si baRcodeR no esta disponible, devolvemos un PDF de texto plano
  has_bar <- requireNamespace("baRcodeR", quietly = TRUE)

  grDevices::pdf(out_path, width = 4, height = 2)
  on.exit(grDevices::dev.off(), add = TRUE)

  graphics::par(mar = c(0.2, 0.2, 0.2, 0.2))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))

  graphics::text(0.02, 0.92, bioid, adj = 0, cex = 1.0, font = 2)
  graphics::text(0.02, 0.75,
                 sprintf("%s | %s", toupper(hospital_id), toupper(sample_type)),
                 adj = 0, cex = 0.7)
  graphics::text(0.02, 0.62,
                 format(as.POSIXct(collection_dt), "%Y-%m-%d %H:%M"),
                 adj = 0, cex = 0.7)

  if (has_bar) {
    # Code128 al pie
    try(baRcodeR::code_128_make(bioid), silent = TRUE)
  } else {
    graphics::rect(0.02, 0.10, 0.98, 0.45, border = "black")
    graphics::text(0.5, 0.27, paste0("(barcode pendiente: instalar baRcodeR)  ", bioid),
                   cex = 0.55)
  }

  out_path
}

# Sample-type catalog -------------------------------------------------------
biobank_sample_types <- function() {
  c("Tejido tumoral"      = "TUM",
    "Tejido normal pareado" = "NORM",
    "Sangre total"        = "BLD",
    "Plasma"              = "PLA",
    "Suero"               = "SER",
    "ADN extraido"        = "DNA",
    "ARN extraido"        = "RNA",
    "FFPE (bloque)"       = "FFPE",
    "OCT (criopreservado)" = "OCT",
    "Organoide / linea"   = "ORG")
}
