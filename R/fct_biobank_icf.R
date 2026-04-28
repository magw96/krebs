#' biobank_icf_docx: genera un machote de Consentimiento Informado (ICF)
#' pre-llenado con los datos del paciente para imprimir y firmar en consulta.
#'
#' Cumple con los requisitos minimos de la NOM-012-SSA3-2012 y la
#' Ley General de Salud para investigacion en seres humanos.
#' El documento es para impresion en hoja membretada institucional.
#'
#' @param out_path     Ruta destino del archivo .docx
#' @param patient      Lista con $mrn, $nombre, $sexo, $edad, $tipo_cancer
#' @param bio_subject  Lista con $hospital_code, $bio_subject_id (opcional)
#' @return out_path

biobank_icf_docx <- function(out_path, patient = NULL, bio_subject = NULL) {
  if (!requireNamespace("officer", quietly = TRUE))
    stop("Instale 'officer' para generar el ICF: install.packages('officer')")

  pname  <- patient$nombre %||% "_______________________________"
  pmrn   <- patient$mrn    %||% "_______________"
  psex   <- patient$sexo   %||% "____"
  page   <- if (!is.null(patient$edad) && !is.na(patient$edad))
              sprintf("%d anos", as.integer(patient$edad))
            else "____ anos"
  pdx    <- patient$tipo_cancer %||% "_______________________________"
  bsid   <- bio_subject$bio_subject_id %||% "(se asignara al firmar)"
  hosp   <- bio_subject$hospital_code  %||% "____"
  hoy    <- format(Sys.Date(), "%Y-%m-%d")

  doc <- officer::read_docx()

  doc <- officer::body_add_par(doc,
    "CONSENTIMIENTO INFORMADO PARA DONACION DE MUESTRAS BIOLOGICAS AL BIOBANCO ONCOLOGICO KREBS",
    style = "heading 1")

  doc <- officer::body_add_par(doc,
    sprintf("Protocolo IRB: TDM-CEI-2026-V1   |   Hospital: %s   |   Version: ICF-Krebs-v1.0-2026",
            hosp),
    style = "Normal")

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "1. Datos del participante", style = "heading 2")

  ptbl <- data.frame(
    Campo = c("Nombre completo", "MRN (registro hospitalario)",
              "Sexo", "Edad", "Diagnostico oncologico",
              "ID pseudonimizado (BIOID)", "Fecha de la consulta"),
    Valor = c(pname, pmrn, psex, page, pdx, bsid, hoy),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, ptbl, style = "table_template")

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "2. Naturaleza y proposito", style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Se le invita a donar muestras biologicas (tejido tumoral, ",
           "tejido normal pareado, sangre, plasma, suero, ADN o ARN ",
           "extraidos) que seran almacenadas en el Biobanco Oncologico ",
           "Krebs. El proposito es apoyar investigacion biomedica ",
           "presente y futura para mejorar el diagnostico, pronostico y ",
           "tratamiento del cancer."),
    style = "Normal")

  doc <- officer::body_add_par(doc, "3. Procedimiento", style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Las muestras se obtendran durante procedimientos clinicos ",
           "ya programados (cirugia, biopsia, extraccion de sangre). ",
           "No implica intervenciones adicionales que aumenten el riesgo ",
           "de su tratamiento medico."),
    style = "Normal")

  doc <- officer::body_add_par(doc, "4. Riesgos y beneficios", style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Riesgos: minimos, asociados al procedimiento clinico ",
           "ya indicado. La donacion no genera riesgos adicionales. ",
           "Beneficios: no hay beneficio directo inmediato. La ",
           "investigacion derivada puede beneficiar a futuros pacientes."),
    style = "Normal")

  doc <- officer::body_add_par(doc, "5. Confidencialidad y proteccion de datos",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Sus datos seran tratados conforme a la LFPDPPP. Su identidad ",
           "se sustituira por un identificador pseudonimizado (BIOID). ",
           "Solo personal autorizado del biobanco podra vincular el BIOID ",
           "con su MRN, mediante un keystore cifrado. Los investigadores ",
           "que accedan a las muestras solo veran datos pseudonimizados."),
    style = "Normal")

  doc <- officer::body_add_par(doc, "6. Alcance del consentimiento (marque)",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    "[ ] Autorizo el uso de mis muestras para investigacion oncologica general (broad consent).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] Autorizo estudios genomicos / secuenciacion masiva.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] Autorizo ser re-contactado para estudios futuros relacionados.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] Autorizo compartir muestras (anonimizadas) con colaboradores externos bajo MTA.",
    style = "Normal")

  doc <- officer::body_add_par(doc, "7. Derechos del participante",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Puede retirar este consentimiento en cualquier momento sin ",
           "afectar su atencion medica. La retirada implica que las ",
           "muestras no usadas seran destruidas; los datos ya generados ",
           "permaneceran anonimizados en estudios concluidos."),
    style = "Normal")

  doc <- officer::body_add_par(doc, "8. Custodio del biobanco",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    "Nombre: _________________________________   Tel: _______________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Correo institucional: __________________________________________",
    style = "Normal")

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "9. Firmas", style = "heading 2")

  firmas <- data.frame(
    Rol = c("Participante", "Testigo 1", "Testigo 2",
            "Investigador / clinico responsable"),
    Nombre = rep("_______________________________________", 4L),
    Firma  = rep("__________________  Fecha: ____________", 4L),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, firmas, style = "table_template")

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Documento generado por Krebs V0.2 el ", hoy,
           ". Imprima en hoja membretada institucional. ",
           "Conserve la copia firmada y registre la version y fecha en el sistema."),
    style = "Normal")

  print(doc, target = out_path)
  invisible(out_path)
}
