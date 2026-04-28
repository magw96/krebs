#' biobank_icf_docx: genera un machote de Consentimiento Informado (ICF)
#' pre-llenado con los datos del paciente para imprimir y firmar en consulta.
#'
#' Cumple con los requisitos minimos de la NOM-012-SSA3-2012, la Ley General
#' de Salud (titulo quinto, investigacion en seres humanos), la LFPDPPP y los
#' lineamientos internacionales de OMS / Declaracion de Helsinki / CIOMS para
#' biobancos oncologicos.
#'
#' El documento es para impresion en hoja membretada institucional.
#'
#' @param out_path     Ruta destino del archivo .docx
#' @param patient      Lista con $mrn, $nombre, $sexo, $edad, $tipo_cancer
#' @param bio_subject  Lista con $hospital_code, $bio_subject_id (opcional)
#' @return out_path

biobank_icf_docx <- function(out_path, patient = NULL, bio_subject = NULL) {
  # Fallback robusto: si officer no esta disponible (ej. PCC sin la
  # dependencia instalada todavia), generamos un .txt con el mismo
  # contenido en lugar de fallar el download y dejar al navegador
  # con "Site wasn't available".
  if (!requireNamespace("officer", quietly = TRUE)) {
    txt_path <- sub("\\.docx?$", ".txt", out_path)
    if (txt_path == out_path) txt_path <- paste0(out_path, ".txt")
    .biobank_icf_txt(txt_path, patient, bio_subject)
    file.copy(txt_path, out_path, overwrite = TRUE)
    return(invisible(out_path))
  }

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

  # ============================================================
  # ENCABEZADO
  # ============================================================
  doc <- officer::body_add_par(doc,
    "CONSENTIMIENTO INFORMADO PARA DONACION DE MUESTRAS BIOLOGICAS AL BIOBANCO ONCOLOGICO KREBS",
    style = "heading 1")

  doc <- officer::body_add_par(doc,
    sprintf("Protocolo IRB: TDM-CEI-2026-V1   |   Hospital: %s   |   Version del documento: ICF-Krebs-v1.0-2026   |   Fecha: %s",
            hosp, hoy),
    style = "Normal")

  doc <- officer::body_add_par(doc,
    paste0("Documento de consentimiento informado para la donacion ",
           "voluntaria de muestras biologicas y datos clinicos asociados ",
           "con fines de investigacion biomedica oncologica."),
    style = "Normal")

  # ============================================================
  # 1. DATOS DEL PARTICIPANTE
  # ============================================================
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "1. Datos del participante",
                               style = "heading 2")

  ptbl <- data.frame(
    Campo = c("Nombre completo", "MRN (registro hospitalario)",
              "Sexo", "Edad", "Diagnostico oncologico",
              "ID pseudonimizado (BIOID)", "Hospital de atencion",
              "Fecha de la consulta"),
    Valor = c(pname, pmrn, psex, page, pdx, bsid, hosp, hoy),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, ptbl, style = "table_template")

  # ============================================================
  # 2. INVITACION
  # ============================================================
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "2. Invitacion a participar",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Se le invita a participar de manera voluntaria como donante de ",
           "muestras biologicas y datos clinicos al Biobanco Oncologico Krebs, ",
           "una infraestructura institucional de investigacion que conserva, ",
           "administra y distribuye material biologico humano de pacientes ",
           "con cancer para apoyar proyectos de investigacion biomedica ",
           "aprobados por el Comite de Etica en Investigacion (CEI) y por ",
           "el Comite de Investigacion correspondiente. ",
           "Antes de aceptar, lea cuidadosamente este documento, pregunte ",
           "todo lo que considere necesario y tomese el tiempo que requiera ",
           "para decidir."),
    style = "Normal")

  # ============================================================
  # 3. PROPOSITO Y FINES DE LA INVESTIGACION
  # ============================================================
  doc <- officer::body_add_par(doc, "3. Proposito y fines de la investigacion",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("El proposito del Biobanco Oncologico Krebs es generar un acervo ",
           "de muestras biologicas y datos clinicos de alta calidad que ",
           "permita responder preguntas cientificas dirigidas a:"),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "a) Comprender los mecanismos biologicos, geneticos y moleculares del cancer.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "b) Identificar biomarcadores diagnosticos, pronosticos y predictivos de respuesta al tratamiento.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "c) Desarrollar y validar nuevas pruebas diagnosticas y terapias dirigidas (medicina de precision).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "d) Estudiar la epidemiologia molecular y los factores de riesgo del cancer en la poblacion mexicana.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "e) Apoyar proyectos academicos, de tesis, de posgrado y publicaciones cientificas revisadas por pares.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "f) Habilitar colaboraciones nacionales e internacionales bajo Acuerdos de Transferencia de Material (MTA).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Cada uso especifico de sus muestras requerira la aprobacion ",
           "previa del Comite de Etica en Investigacion y del Comite ",
           "Cientifico del biobanco. Ningun proyecto podra utilizar sus ",
           "muestras fuera del alcance que usted autorice en este documento."),
    style = "Normal")

  # ============================================================
  # 4. NATURALEZA VOLUNTARIA
  # ============================================================
  doc <- officer::body_add_par(doc, "4. Naturaleza voluntaria de la participacion",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Su participacion es totalmente voluntaria. Si decide no donar, ",
           "su atencion medica, derechos asistenciales y relacion con su ",
           "medico tratante no se veran afectados de ninguna forma. Tampoco ",
           "afectara la cobertura de su seguro ni el acceso a servicios ",
           "hospitalarios."),
    style = "Normal")

  # ============================================================
  # 5. PROCEDIMIENTO Y TIPOS DE MUESTRA
  # ============================================================
  doc <- officer::body_add_par(doc, "5. Procedimiento y tipos de muestra",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Las muestras se obtendran exclusivamente durante procedimientos ",
           "clinicos ya programados (cirugia oncologica, biopsia diagnostica, ",
           "extraccion de sangre indicada por su medico). No se realizaran ",
           "intervenciones adicionales ni se aumentara el riesgo de su ",
           "tratamiento medico. Las muestras que se podran almacenar incluyen:"),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Tejido tumoral fresco, congelado o en bloque de parafina (FFPE).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Tejido normal pareado adyacente al tumor.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Sangre periferica completa, plasma y suero.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Acidos nucleicos extraidos (ADN genomico, ARN, ADN tumoral circulante).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Cuando aplique, otros fluidos (orina, liquido pleural, ascitis, LCR) obtenidos por indicacion clinica.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Las muestras se acompanaran de datos clinicos relevantes ",
           "(diagnostico, estadificacion, tratamiento, evolucion, ",
           "histopatologia, estudios de imagen y laboratorio), siempre ",
           "vinculados unicamente al BIOID y nunca a su nombre."),
    style = "Normal")

  # ============================================================
  # 6. RIESGOS Y BENEFICIOS
  # ============================================================
  doc <- officer::body_add_par(doc, "6. Riesgos y beneficios",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Riesgos: la donacion no agrega riesgos fisicos a los ya ",
           "asociados al procedimiento clinico que de cualquier manera ",
           "se realizaria. El principal riesgo es la potencial perdida ",
           "de confidencialidad, que el biobanco mitiga mediante ",
           "pseudonimizacion criptografica (BIOID), almacenamiento ",
           "cifrado del mapeo identidad-muestra, control de acceso por ",
           "roles y registro auditable de cada acceso."),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Beneficios: la donacion no representa un beneficio directo ",
           "inmediato para usted. La investigacion derivada puede ",
           "beneficiar a futuros pacientes con cancer al mejorar el ",
           "diagnostico, el pronostico y los tratamientos disponibles."),
    style = "Normal")

  # ============================================================
  # 7. TIEMPO DE ALMACENAMIENTO
  # ============================================================
  doc <- officer::body_add_par(doc, "7. Tiempo de almacenamiento",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Las muestras y los datos asociados se almacenaran en el ",
           "Biobanco Oncologico Krebs por un periodo inicial de hasta ",
           "veinte (20) anos contados a partir de la fecha de firma de ",
           "este consentimiento, plazo que podra renovarse por periodos ",
           "iguales si el biobanco se mantiene activo y los proyectos de ",
           "investigacion asi lo requieran. Antes de cada renovacion el ",
           "Comite de Etica revisara la pertinencia cientifica del acervo. ",
           "Si el biobanco cesara operaciones, sus muestras seran ",
           "destruidas siguiendo los procedimientos institucionales para ",
           "residuos peligrosos biologico-infecciosos (NOM-087-SEMARNAT-SSA1-2002), ",
           "o transferidas a otro biobanco autorizado, decision que sera ",
           "comunicada al Comite de Etica."),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Usted puede solicitar en cualquier momento la destruccion de ",
           "las muestras no utilizadas (ver seccion 10. Derechos)."),
    style = "Normal")

  # ============================================================
  # 8. CONFIDENCIALIDAD
  # ============================================================
  doc <- officer::body_add_par(doc, "8. Confidencialidad y proteccion de datos personales",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Sus datos personales seran tratados conforme a la Ley Federal ",
           "de Proteccion de Datos Personales en Posesion de los Particulares ",
           "(LFPDPPP) y su reglamento. Su identidad sera sustituida por un ",
           "identificador pseudonimizado (BIOID) generado mediante un ",
           "hash criptografico HMAC-SHA256. Solo el Custodio del biobanco ",
           "y el personal autorizado podran vincular el BIOID con su MRN ",
           "mediante un keystore cifrado simetricamente. Los investigadores ",
           "que reciban acceso a las muestras solo veran el BIOID y datos ",
           "clinicos pseudonimizados; no podran reidentificarlo. Cada acceso ",
           "queda registrado en una bitacora de auditoria inalterable."),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Las publicaciones cientificas, informes y presentaciones que ",
           "deriven del uso de las muestras nunca incluiran datos que ",
           "permitan identificarlo."),
    style = "Normal")

  # ============================================================
  # 9. ALCANCE DEL CONSENTIMIENTO (CHECKBOXES)
  # ============================================================
  doc <- officer::body_add_par(doc, "9. Alcance del consentimiento (marque las opciones que autoriza)",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Marque cada casilla solo si esta de acuerdo. Puede aceptar ",
           "algunos usos y rechazar otros. Puede tambien modificar estas ",
           "autorizaciones en el futuro contactando al Custodio del biobanco."),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.1  Autorizo el uso de mis muestras para investigacion oncologica general (consentimiento amplio / broad consent), siempre que cada proyecto sea aprobado por el Comite de Etica.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.2  Autorizo estudios genomicos, de secuenciacion masiva (NGS), exoma, transcriptoma, epigenoma o paneles dirigidos sobre mi material biologico.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.3  Autorizo el desarrollo y validacion de modelos de aprendizaje automatico / inteligencia artificial sobre mis datos pseudonimizados.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.4  Autorizo ser re-contactado por el biobanco para invitarme a estudios futuros relacionados con mi diagnostico o para confirmar / actualizar este consentimiento.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.5  Autorizo compartir mis muestras y datos pseudonimizados con colaboradores academicos externos (nacionales o internacionales) bajo un Acuerdo de Transferencia de Material (MTA) aprobado por la institucion.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.6  Autorizo el uso de mis muestras y datos pseudonimizados en proyectos colaborativos con empresas farmaceuticas, biotecnologicas o de diagnostico, bajo MTA, sin que esto genere derechos comerciales a mi favor.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "[ ] 9.7  Autorizo que, si durante la investigacion se obtienen hallazgos clinicamente significativos y accionables sobre mi salud, el biobanco pueda contactar a mi medico tratante para informarle (devolucion de hallazgos secundarios).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Renuncia a derechos comerciales: entiendo que las muestras se ",
           "donan sin contraprestacion economica y que cualquier desarrollo, ",
           "patente o producto derivado de la investigacion no genera ",
           "derechos comerciales ni regalias a mi favor."),
    style = "Normal")

  # ============================================================
  # 10. DERECHOS DEL PARTICIPANTE
  # ============================================================
  doc <- officer::body_add_par(doc, "10. Derechos del participante (ARCO + retiro)",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("De acuerdo con la LFPDPPP, en cualquier momento usted puede ",
           "ejercer sus derechos de Acceso, Rectificacion, Cancelacion y ",
           "Oposicion (ARCO) sobre sus datos personales, asi como retirar ",
           "este consentimiento, sin que ello afecte su atencion medica."),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Acceso: solicitar conocer que datos suyos conserva el biobanco.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Rectificacion: corregir datos inexactos o incompletos.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Cancelacion: solicitar la eliminacion de sus datos personales y la destruccion de sus muestras no utilizadas.",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "- Oposicion: retirar autorizaciones especificas (por ejemplo, retirar el permiso de re-contacto sin retirar la donacion completa).",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("La retirada del consentimiento implica que las muestras no ",
           "utilizadas seran destruidas. Los datos y resultados ya generados ",
           "y publicados antes de la retirada permaneceran de manera ",
           "anonimizada en estudios concluidos, dado que tecnicamente no ",
           "es posible eliminarlos de publicaciones cientificas ya hechas."),
    style = "Normal")

  # ============================================================
  # 11. COMPENSACION
  # ============================================================
  doc <- officer::body_add_par(doc, "11. Compensacion economica",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("La donacion es estrictamente altruista y no genera ningun ",
           "pago, compensacion economica, regalia ni beneficio material ",
           "para usted o sus familiares. El biobanco tampoco le cobrara ",
           "por almacenar sus muestras."),
    style = "Normal")

  # ============================================================
  # 12. DEVOLUCION DE RESULTADOS
  # ============================================================
  doc <- officer::body_add_par(doc, "12. Devolucion de resultados",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("La mayoria de los resultados de investigacion no se devuelven ",
           "individualmente porque su validez clinica no esta establecida. ",
           "Si autorizo la opcion 9.7, el biobanco podra notificar a mi ",
           "medico tratante en caso de hallazgos secundarios accionables ",
           "(por ejemplo, variantes germinales con implicaciones para ",
           "familiares directos), siempre con asesoramiento genetico ",
           "previo. Los resultados agregados (no individuales) podran ",
           "consultarse en publicaciones cientificas."),
    style = "Normal")

  # ============================================================
  # 13. CUSTODIO
  # ============================================================
  doc <- officer::body_add_par(doc, "13. Custodio del biobanco y datos de contacto",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    "Custodio responsable: _________________________________________________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Cargo institucional:  _________________________________________________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Telefono de contacto: _________________   Correo: _____________________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Para ejercer sus derechos ARCO, retirar el consentimiento, ",
           "actualizar autorizaciones o resolver dudas relacionadas con ",
           "el biobanco, contacte al Custodio."),
    style = "Normal")

  # ============================================================
  # 14. COMITE DE ETICA
  # ============================================================
  doc <- officer::body_add_par(doc, "14. Comite de Etica en Investigacion (CEI)",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Si tiene dudas sobre sus derechos como participante o desea ",
           "presentar una queja independiente, puede contactar al Comite ",
           "de Etica en Investigacion de la institucion:"),
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Comite de Etica:      _________________________________________________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Numero de registro CONBIOETICA: _____________   COFEPRIS: _____________",
    style = "Normal")
  doc <- officer::body_add_par(doc,
    "Telefono / correo:    _________________________________________________",
    style = "Normal")

  # ============================================================
  # 15. DECLARACION DEL PARTICIPANTE
  # ============================================================
  doc <- officer::body_add_par(doc, "15. Declaracion del participante",
                               style = "heading 2")
  doc <- officer::body_add_par(doc,
    paste0("Declaro que he leido (o me ha sido leido) este documento, ",
           "que se me han explicado en lenguaje comprensible los fines, ",
           "procedimientos, riesgos, beneficios, alcance y duracion del ",
           "almacenamiento de mis muestras, asi como mis derechos. Se me ",
           "ha dado oportunidad de hacer preguntas y todas han sido ",
           "respondidas de manera satisfactoria. Se me entrega una copia ",
           "firmada de este documento. Acepto participar voluntariamente."),
    style = "Normal")

  # ============================================================
  # 16. FIRMAS
  # ============================================================
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc, "16. Firmas", style = "heading 2")

  firmas <- data.frame(
    Rol = c("Participante (donante)",
            "Representante legal (si aplica)",
            "Testigo 1",
            "Testigo 2",
            "Investigador / clinico responsable",
            "Custodio del biobanco"),
    Nombre = rep("_______________________________________", 6L),
    Firma  = rep("__________________  Fecha: ____________", 6L),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, firmas, style = "table_template")

  # ============================================================
  # PIE
  # ============================================================
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_par(doc,
    paste0("Documento generado por el sistema Krebs V0.2 el ", hoy,
           ". Imprima en hoja membretada institucional. ",
           "Conserve la copia firmada por el participante en el expediente ",
           "del biobanco y registre el numero de version y fecha en el ",
           "modulo de Consentimientos del sistema."),
    style = "Normal")

  print(doc, target = out_path)
  invisible(out_path)
}

# ---- Fallback texto plano (si officer no esta instalado) -------------------
.biobank_icf_txt <- function(out_path, patient = NULL, bio_subject = NULL) {
  pname <- patient$nombre %||% "_______________________________"
  pmrn  <- patient$mrn    %||% "_______________"
  psex  <- patient$sexo   %||% "____"
  page  <- if (!is.null(patient$edad) && !is.na(patient$edad))
             sprintf("%d anos", as.integer(patient$edad))
           else "____ anos"
  pdx   <- patient$tipo_cancer %||% "_______________________________"
  bsid  <- bio_subject$bio_subject_id %||% "(se asignara al firmar)"
  hosp  <- bio_subject$hospital_code  %||% "____"
  hoy   <- format(Sys.Date(), "%Y-%m-%d")

  txt <- c(
    "CONSENTIMIENTO INFORMADO PARA DONACION DE MUESTRAS BIOLOGICAS",
    "BIOBANCO ONCOLOGICO KREBS",
    sprintf("Protocolo IRB: TDM-CEI-2026-V1   Hospital: %s   Version: ICF-Krebs-v1.0-2026   Fecha: %s",
            hosp, hoy),
    "",
    "1. Datos del participante",
    sprintf("   Nombre:       %s", pname),
    sprintf("   MRN:          %s", pmrn),
    sprintf("   Sexo:         %s    Edad: %s", psex, page),
    sprintf("   Diagnostico:  %s", pdx),
    sprintf("   BIOID:        %s", bsid),
    sprintf("   Hospital:     %s", hosp),
    sprintf("   Fecha:        %s", hoy),
    "",
    "2. Invitacion a participar",
    "   Donacion voluntaria al Biobanco Oncologico Krebs para investigacion",
    "   biomedica aprobada por el Comite de Etica.",
    "",
    "3. Proposito y fines de la investigacion",
    "   a) Comprender mecanismos biologicos del cancer.",
    "   b) Identificar biomarcadores diagnosticos / pronosticos / predictivos.",
    "   c) Desarrollar y validar nuevas terapias y pruebas diagnosticas.",
    "   d) Estudiar la epidemiologia molecular del cancer en Mexico.",
    "   e) Apoyar tesis, posgrado y publicaciones cientificas.",
    "   f) Habilitar colaboraciones nacionales e internacionales bajo MTA.",
    "",
    "4. Naturaleza voluntaria",
    "   No participar no afecta su atencion medica.",
    "",
    "5. Procedimiento y tipos de muestra",
    "   Tejido tumoral, tejido normal pareado, sangre, plasma, suero,",
    "   ADN/ARN extraidos, otros fluidos por indicacion clinica.",
    "   Obtenidas durante procedimientos clinicos ya programados.",
    "",
    "6. Riesgos y beneficios",
    "   Riesgos minimos (perdida de confidencialidad mitigada por BIOID).",
    "   Sin beneficio directo; beneficio potencial para futuros pacientes.",
    "",
    "7. Tiempo de almacenamiento",
    "   Hasta 20 anos desde la firma, renovable previa revision del CEI.",
    "   Puede solicitar la destruccion en cualquier momento.",
    "",
    "8. Confidencialidad (LFPDPPP)",
    "   BIOID via HMAC-SHA256. Mapeo cifrado bajo custodia exclusiva.",
    "   Bitacora de auditoria de cada acceso.",
    "",
    "9. Alcance del consentimiento (marque con X)",
    "   [ ] 9.1  Investigacion oncologica general (broad consent)",
    "   [ ] 9.2  Estudios genomicos / secuenciacion masiva (NGS)",
    "   [ ] 9.3  Modelos de IA / aprendizaje automatico",
    "   [ ] 9.4  Re-contacto para estudios futuros",
    "   [ ] 9.5  Compartir con colaboradores externos bajo MTA",
    "   [ ] 9.6  Uso por empresas farmaceuticas / biotech bajo MTA",
    "   [ ] 9.7  Devolucion de hallazgos secundarios accionables",
    "   Renuncia a derechos comerciales: donacion altruista, sin regalias.",
    "",
    "10. Derechos ARCO + retiro",
    "    Acceso, Rectificacion, Cancelacion, Oposicion. Retiro sin afectar",
    "    su atencion medica.",
    "",
    "11. Compensacion economica",
    "    Donacion altruista, sin pago ni regalias.",
    "",
    "12. Devolucion de resultados",
    "    Solo hallazgos accionables si autorizo opcion 9.7.",
    "",
    "13. Custodio del biobanco",
    "    Nombre: _______________________  Cargo: _______________________",
    "    Tel:    _______________________  Correo: ______________________",
    "",
    "14. Comite de Etica en Investigacion",
    "    Comite: _________________________   CONBIOETICA: ____________",
    "    Tel/Correo: ____________________________________________________",
    "",
    "15. Declaracion del participante",
    "    He leido el documento; se me explicaron fines, procedimientos,",
    "    riesgos, beneficios, alcance y duracion. Acepto participar.",
    "",
    "16. Firmas",
    "    Participante:        _______________________  Fecha: __________",
    "    Repr. legal (si aplica): ___________________  Fecha: __________",
    "    Testigo 1:           _______________________  Fecha: __________",
    "    Testigo 2:           _______________________  Fecha: __________",
    "    Investigador:        _______________________  Fecha: __________",
    "    Custodio biobanco:   _______________________  Fecha: __________",
    "",
    paste0("Generado por Krebs V0.2 el ", hoy,
           " (fallback texto: 'officer' no instalado en el servidor).")
  )
  writeLines(txt, out_path, useBytes = TRUE)
  invisible(out_path)
}
