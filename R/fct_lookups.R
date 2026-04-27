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

#' Curated oncology procedure list (replaces the partial CPT catalog).
#' Source: inst/extdata/oncology_procedures.csv -- columns: site, procedure, modality.
#' Returns a named list grouped by anatomic site for shinyWidgets::pickerInput
#' optgroup rendering. Falls back to the flat CPT list if the CSV is missing.
lookup_oncology_procedures <- function() {
  if (!is.null(.cache[["oncology_procedures"]]))
    return(.cache[["oncology_procedures"]])
  f <- .local_extdata("oncology_procedures.csv")
  if (!nzchar(f)) {
    cpt <- lookup_cpt()
    out <- if ("PROCEDURE.DESCRIPTION" %in% names(cpt))
      sort(cpt$PROCEDURE.DESCRIPTION) else character(0)
    .cache[["oncology_procedures"]] <- out
    return(out)
  }
  d <- tryCatch(utils::read.csv(f, fileEncoding = "UTF-8",
                                stringsAsFactors = FALSE),
                error = function(e) NULL)
  if (is.null(d) || !all(c("site","procedure") %in% names(d))) {
    .cache[["oncology_procedures"]] <- character(0)
    return(character(0))
  }
  out <- split(d$procedure, d$site)
  out <- lapply(out, function(x) sort(unique(x)))
  out <- out[order(names(out))]
  .cache[["oncology_procedures"]] <- out
  out
}
lookup_drugs <- function() {
  d <- .s3_csv("fda_active_filtered.csv")
  # keep V0.1 shape: caller does sort(lookup_drugs()$x), so map first column.
  if (ncol(d) >= 1L && !"x" %in% names(d)) names(d)[1] <- "x"
  d
}

#' Spanish translation of ICD-O-3 site descriptions.
#'
#' The raw icdo3.csv ships with English, ALL-CAPS site names. We translate
#' (and Title-Case) them on the way out so the dropdown is in Spanish for
#' the clinical user. Anything not in this map is kept verbatim with the
#' leading double-quote stripped (some rows have stray quotes from the CSV).
.SITE_ES <- c(
  # Keys MUST match icdo3.csv "Site Description" column verbatim (including
  # ", NOS" / ", (EXCL. ...)" suffixes), otherwise the lookup falls through
  # to the Title-Case fallback and the user sees raw English.
  "ACCESSORY SINUS, NOS"                 = "Senos paranasales accesorios",
  "ADRENAL GLANDS"                       = "Glandulas suprarrenales",
  "ANAL CANAL & ANUS"                    = "Canal anal y ano",
  "APPENDIX"                             = "Apendice",
  "BASE OF TONGUE"                       = "Base de la lengua",
  "BLOOD, BONE MARROW, & HEMATOPOIETIC SYS" = "Sangre, medula osea y sistema hematopoyetico",
  "BONES & JOINTS (EXCL SKULL AND FACE, MANDIBLE)" = "Huesos y articulaciones (excl. craneo, cara y mandibula)",
  "BONES OF SKULL AND FACE"              = "Huesos del craneo y cara",
  "BRAIN, & CRANIAL NERVES, & SPINAL CORD, (EXCL. VENTRICLE, CEREBELLUM)" = "Encefalo, nervios craneales y medula espinal (excl. ventriculo, cerebelo)",
  "BREAST"                               = "Mama",
  "CEREBELLUM"                           = "Cerebelo",
  "CERVIX UTERI"                         = "Cervix uterino",
  "CONNECTIVE & SOFT TISSUE"             = "Tejido conectivo y blando",
  "CORPUS UTERI"                         = "Cuerpo uterino",
  "CRANIOPHARYNGEAL DUCT"                = "Conducto craneofaringeo",
  "EPIDIDYMIS, SPERMATIC CORD, MALE GENITAL, NOS" = "Epididimo, cordon espermatico y genitales masculinos",
  "ESOPHAGUS"                            = "Esofago",
  "EYE, NOS"                             = "Ojo",
  "EYEBALL"                              = "Globo ocular",
  "FALLOPIAN TUBE"                       = "Trompa de Falopio",
  "GALLBLADDER & EXTRAHEPATIC BILE DUCTS"= "Vesicula y vias biliares extrahepaticas",
  "GUM, FLOOR OF MOUTH, & OTHER MOUTH"   = "Encia, piso de boca y otras areas orales",
  "HEART"                                = "Corazon",
  "HYPOPHARYNX"                          = "Hipofaringe",
  "ILL-DEFINED"                          = "Mal definido",
  "INTRAHEPATIC BILE DUCTS"              = "Vias biliares intrahepaticas",
  "KIDNEY"                               = "Rinon",
  "LARGE INTESTINE, (EXCL. APPENDIX)"    = "Intestino grueso (excl. apendice)",
  "LARYNX"                               = "Laringe",
  "LIP"                                  = "Labio",
  "LIVER"                                = "Higado",
  "LUNG & BRONCHUS"                      = "Pulmon y bronquios",
  "LYMPH NODES"                          = "Ganglios linfaticos",
  "MANDIBLE"                             = "Mandibula",
  "MEDIASTINUM"                          = "Mediastino",
  "MENINGES (CEREBRAL,SPINAL)"           = "Meninges (cerebrales y espinales)",
  "MIDDLE EAR"                           = "Oido medio",
  "NASAL CAVITY (INCLUDING NASAL CARTILAGE)" = "Cavidad nasal (incluye cartilago nasal)",
  "NASOPHARYNX (EXCL POSTERIOR WALL)"    = "Nasofaringe (excl. pared posterior)",
  "ORBIT & LACRIMAL GLAND, (EXCL. RETINA, EYE, NOS)" = "Orbita y glandula lagrimal (excl. retina y ojo)",
  "OROPHARNYX"                           = "Orofaringe",
  "OTHER ENDOCRINE GLANDS"               = "Otras glandulas endocrinas",
  "OTHER FEMALE GENITAL (EXCL FALLOPIAN TUBE)" = "Otros organos genitales femeninos (excl. trompa)",
  "OTHER NERVOUS SYSTEM"                 = "Otro sistema nervioso",
  "OTHER URINARY ORGANS"                 = "Otros organos urinarios",
  "OVARY"                                = "Ovario",
  "PANCREAS"                             = "Pancreas",
  "PARATHYROID GLAND"                    = "Glandula paratiroides",
  "PENIS"                                = "Pene",
  "PERIPHERAL NERVES"                    = "Nervios perifericos",
  "PHARYNX"                              = "Faringe",
  "PINEAL GLAND"                         = "Glandula pineal",
  "PITUITARY GLAND"                      = "Glandula hipofisis",
  "PLACENTA"                             = "Placenta",
  "PLEURA"                               = "Pleura",
  "POSTERIOR WALL OF NASOPHARYNX"        = "Pared posterior de nasofaringe",
  "PROSTATE GLAND"                       = "Prostata",
  "RECTUM"                               = "Recto",
  "RENAL PELVIS, URETER"                 = "Pelvis renal y ureter",
  "RESPIRATORY, NOS"                     = "Aparato respiratorio (otro)",
  "RETICULO-ENDOTHELIAL"                 = "Sistema reticuloendotelial",
  "RETINA"                               = "Retina",
  "RETROPERITONEUM & PERITONEUM"         = "Retroperitoneo y peritoneo",
  "SALIVARY GLAND"                       = "Glandula salival",
  "SCROTUM"                              = "Escroto",
  "SINUSES"                              = "Senos paranasales",
  "SKIN"                                 = "Piel",
  "SMALL INTESTINE"                      = "Intestino delgado",
  "SPLEEN"                               = "Bazo",
  "STOMACH"                              = "Estomago",
  "TESTIS"                               = "Testiculo",
  "THYMUS"                               = "Timo",
  "THYROID GLAND"                        = "Glandula tiroides",
  "TONGUE (EXCL BASE OF TONGUE)"         = "Lengua (excl. base de lengua)",
  "TRACHEA"                              = "Traquea",
  "UNKNOWN"                              = "Desconocido",
  "UNSPECIFIED DIGEST. ORGANS"           = "Organos digestivos no especificados",
  "URINARY BLADDER"                      = "Vejiga urinaria",
  "UTERUS, NOS"                          = "Utero",
  "VAGINA & LABIA"                       = "Vagina y labios",
  "VENTRICLE"                            = "Ventriculo cerebral",
  "VULVA, NOS"                           = "Vulva"
)

#' Anatomical sites in Spanish. Translates each ICD-O-3 site description via
#' .SITE_ES; falls back to a Title-Case version of the original if no
#' translation is registered (rather than dropping the row).
lookup_sites <- function() {
  base <- character(0)
  d <- lookup_icdo3()
  if (length(d) && nrow(d) > 0L) {
    cands <- intersect(c("Site.Description","Site Description"), names(d))
    if (!length(cands)) {
      cands <- grep("site", names(d), ignore.case = TRUE, value = TRUE)
    }
    if (length(cands)) base <- unique(d[[cands[1]]])
  }
  base <- gsub("^[\"\ufeff]+|[\"\ufeff]+$", "", base)
  base <- base[nzchar(base)]
  translated <- vapply(base, function(x) {
    # NB: `[[` on a named character vector errors when the key is missing,
    # so use single-bracket lookup which returns NA for unknown names.
    es <- unname(.SITE_ES[x])
    if (!is.na(es) && nzchar(es)) return(es)
    # fallback: Title Case the leftover English so it doesn't look raw
    paste0(substr(x,1,1), tolower(substr(x,2,nchar(x))))
  }, character(1), USE.NAMES = FALSE)
  extras <- c("Extremidad superior derecha","Extremidad superior izquierda",
              "Extremidad inferior derecha","Extremidad inferior izquierda",
              "Cabeza y cuello (otro)","Multiples sitios / metastasico")
  sort(unique(c(translated, extras)))
}

#' Spanish translation map for the OncoTree tumour catalogue.
#' DB still stores the English code (so analyses across hospitals stay
#' comparable); only the displayed label is in Spanish.
.ONCOTREE_ES <- c(
  "Adenocarcinoma In Situ"                 = "Adenocarcinoma in situ",
  "Adrenal Gland Cancer"                   = "Cancer de glandula suprarrenal",
  "Adrenocortical Adenoma"                 = "Adenoma adrenocortical",
  "Adrenocortical Carcinoma"               = "Carcinoma adrenocortical",
  "Ampullary Cancer"                       = "Cancer de ampula de Vater",
  "Ampullary Carcinoma"                    = "Carcinoma de ampula de Vater",
  "Anal Cancer"                            = "Cancer anal",
  "Angiomatoid Fibrous Histiocytoma"       = "Histiocitoma fibroso angiomatoide",
  "Appendiceal Cancer"                     = "Cancer apendicular",
  "B-Lymphoblastic Leukemia/Lymphoma"      = "Leucemia/linfoma linfoblastico B",
  "Biliary Tract Cancer"                   = "Cancer de vias biliares",
  "Bladder Cancer"                         = "Cancer de vejiga",
  "Bladder/Urinary Tract Cancer"           = "Cancer de vejiga / vias urinarias",
  "Blastic Plasmacytoid Dendritic Cell Neoplasm" = "Neoplasia blastica de celulas dendriticas plasmocitoides",
  "Blood Cancer"                           = "Neoplasia hematologica",
  "Bone Cancer"                            = "Cancer oseo",
  "Bowel Cancer"                           = "Cancer intestinal",
  "Breast Cancer"                          = "Cancer de mama",
  "Breast Sarcoma"                         = "Sarcoma mamario",
  "CNS Cancer"                             = "Cancer del SNC",
  "CNS/Brain Cancer"                       = "Cancer del SNC / encefalo",
  "Cancer of Unknown Primary"              = "Cancer de origen primario desconocido",
  "Cervical Cancer"                        = "Cancer cervicouterino",
  "Choroid Plexus Tumor"                   = "Tumor del plexo coroideo",
  "Clear Cell Sarcoma of Kidney"           = "Sarcoma de celulas claras del rinon",
  "Colorectal Cancer"                      = "Cancer colorrectal",
  "Desmoplastic/Nodular Medulloblastoma"   = "Meduloblastoma desmoplasico/nodular",
  "Embryonal Tumor"                        = "Tumor embrionario",
  "Endometrial Cancer"                     = "Cancer endometrial",
  "Esophageal/Stomach Cancer"              = "Cancer esofagico / gastrico",
  "Esophagogastric Cancer"                 = "Cancer esofagogastrico",
  "Eye Cancer"                             = "Cancer ocular",
  "Gastrointestinal Neuroendocrine Tumor"  = "Tumor neuroendocrino gastrointestinal",
  "Gastrointestinal Neuroendocrine Tumors of the Esophagus/Stomach" = "Tumor neuroendocrino esofago/estomago",
  "Gastrointestinal Stromal Tumor"         = "Tumor del estroma gastrointestinal (GIST)",
  "Germ Cell Tumor"                        = "Tumor de celulas germinales",
  "Gestational Trophoblastic Disease"      = "Enfermedad trofoblastica gestacional",
  "Glioma"                                 = "Glioma",
  "Head and Neck Cancer"                   = "Cancer de cabeza y cuello",
  "Hepatobiliary Cancer"                   = "Cancer hepatobiliar",
  "Histiocytosis"                          = "Histiocitosis",
  "Hodgkin Lymphoma"                       = "Linfoma de Hodgkin",
  "Infantile Fibrosarcoma"                 = "Fibrosarcoma infantil",
  "Kidney Cancer"                          = "Cancer renal",
  "Lacrimal Gland Tumor"                   = "Tumor de glandula lagrimal",
  "Large Cell/Anaplastic Medulloblastoma"  = "Meduloblastoma de celulas grandes / anaplasico",
  "Leukemia"                               = "Leucemia",
  "Liver Cancer"                           = "Cancer hepatico",
  "Lung Cancer"                            = "Cancer de pulmon",
  "Lymphatic Cancer"                       = "Cancer linfatico",
  "Malignant Glomus Tumor"                 = "Tumor glomico maligno",
  "Malignant Rhabdoid Tumor of the Liver"  = "Tumor rabdoide maligno hepatico",
  "Mastocytosis"                           = "Mastocitosis",
  "Mature B-Cell Neoplasms"                = "Neoplasias maduras de celulas B",
  "Mature T and NK Neoplasms"              = "Neoplasias maduras de celulas T/NK",
  "Medulloblastoma"                        = "Meduloblastoma",
  "Medulloblastoma with Extensive Nodularity" = "Meduloblastoma con nodularidad extensa",
  "Melanocytoma"                           = "Melanocitoma",
  "Melanoma"                               = "Melanoma",
  "Mesothelioma"                           = "Mesotelioma",
  "Miscellaneous Brain Tumor"              = "Tumor cerebral (otro)",
  "Miscellaneous Neuroepithelial Tumor"    = "Tumor neuroepitelial (otro)",
  "Myelodysplastic Syndromes"              = "Sindromes mielodisplasicos",
  "Myelodysplastic/Myeloproliferative Neoplasms" = "Neoplasias mielodisplasicas/mieloproliferativas",
  "Myeloid Neoplasms with Germ Line Predisposition" = "Neoplasias mieloides con predisposicion germinal",
  "Myeloproliferative Neoplasms"           = "Neoplasias mieloproliferativas",
  "Myofibromatosis"                        = "Miofibromatosis",
  "Nerve Sheath Tumor"                     = "Tumor de la vaina nerviosa",
  "Non-Hodgkin Lymphoma"                   = "Linfoma no Hodgkin",
  "Non-Small Cell Lung Cancer"             = "Cancer de pulmon de celulas no pequenas (CPCNP)",
  "Other Cancer"                           = "Otro cancer",
  "Ovarian Cancer"                         = "Cancer de ovario",
  "Ovarian/Fallopian Tube Cancer"          = "Cancer de ovario / trompa de Falopio",
  "Pancreatic Cancer"                      = "Cancer de pancreas",
  "Parathyroid Cancer"                     = "Cancer paratiroideo",
  "Penile Cancer"                          = "Cancer de pene",
  "Peripheral Nervous System"              = "Sistema nervioso periferico",
  "Peripheral Nervous System Cancer"       = "Cancer del sistema nervioso periferico",
  "Peritoneal Cancer"                      = "Cancer peritoneal",
  "Pheochromocytoma"                       = "Feocromocitoma",
  "Pineal Tumor"                           = "Tumor pineal",
  "Pleural Cancer"                         = "Cancer pleural",
  "Posttransplant Lymphoproliferative Disorders" = "Trastorno linfoproliferativo post-trasplante",
  "Primary CNS Melanocytic Tumors"         = "Tumores melanociticos primarios del SNC",
  "Prostate Cancer"                        = "Cancer de prostata",
  "Renal Cell Carcinoma"                   = "Carcinoma de celulas renales",
  "Renal Neuroendocrine Tumor"             = "Tumor neuroendocrino renal",
  "Retinoblastoma"                         = "Retinoblastoma",
  "Rhabdoid Cancer"                        = "Cancer rabdoide",
  "Salivary Gland Cancer"                  = "Cancer de glandulas salivales",
  "Sellar Tumor"                           = "Tumor selar",
  "Sex Cord Stromal Tumor"                 = "Tumor de cordones sexuales / estroma",
  "Sialoblastoma"                          = "Sialoblastoma",
  "Skin Cancer"                            = "Cancer de piel",
  "Small Bowel Cancer"                     = "Cancer de intestino delgado",
  "Small Cell Lung Cancer"                 = "Cancer de pulmon de celulas pequenas (CPCP)",
  "Soft Tissue Cancer"                     = "Cancer de tejidos blandos",
  "Soft Tissue Sarcoma"                    = "Sarcoma de tejidos blandos",
  "T-Lymphoblastic Leukemia/Lymphoma"      = "Leucemia/linfoma linfoblastico T",
  "Testicular Cancer"                      = "Cancer testicular",
  "Thymic Cancer"                          = "Cancer timico",
  "Thymic Tumor"                           = "Tumor timico",
  "Thyroid Cancer"                         = "Cancer de tiroides",
  "Tubular Adenoma of the Colon"           = "Adenoma tubular de colon",
  "Undifferentiated Embryonal Sarcoma of the Liver" = "Sarcoma embrionario indiferenciado del higado",
  "Uterine Cancer"                         = "Cancer uterino",
  "Uterine Sarcoma"                        = "Sarcoma uterino",
  "Vaginal Cancer"                         = "Cancer vaginal",
  "Vulvar Carcinoma"                       = "Carcinoma vulvar",
  "Vulvar/Vaginal Cancer"                  = "Cancer vulvar / vaginal",
  "Wilms Tumor"                            = "Tumor de Wilms"
)

#' Returns a named vector: names = Spanish display label, values = English
#' OncoTree code. The selectizeInput stores the *value* (English), so
#' downstream queries and analytics keep their canonical key.
lookup_oncotree <- function() {
  f <- .local_extdata("tumorlist.csv")
  if (!nzchar(f)) return(character(0))
  x <- tryCatch(utils::read.csv(f, fileEncoding = "UTF-8-BOM"),
                error = function(e) tryCatch(utils::read.csv(f),
                                             error = function(e) NULL))
  if (is.null(x) || ncol(x) < 2) return(character(0))
  vals <- sort(unique(x[[2]][nzchar(x[[2]])]))
  labels <- vapply(vals, function(v) {
    # `[[` errors on missing keys for named char vectors; use `[` + is.na.
    es <- unname(.ONCOTREE_ES[v])
    if (!is.na(es) && nzchar(es)) es else v
  }, character(1), USE.NAMES = FALSE)
  out <- vals
  names(out) <- labels
  # Sort by Spanish label so the dropdown is alphabetical for the user.
  out[order(names(out))]
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
