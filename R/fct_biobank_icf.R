#' biobank_icf_docx: genera un machote de Consentimiento Informado (ICF)
#' pre-llenado con los datos del paciente para imprimir y firmar en consulta.
#'
#' Cumple con los requisitos minimos de la NOM-012-SSA3-2012, la Ley General
#' de Salud (titulo quinto, investigacion en seres humanos), el Reglamento
#' de la LGS en materia de investigacion para la salud, la LFPDPPP, asi como
#' los lineamientos internacionales de la Declaracion de Helsinki (AMM 2013),
#' las guias CIOMS 2016 y la ISBER Best Practices 2018 para biobancos.
#'
#' Genera un .docx con formato (titulo, encabezados color navy, parrafos
#' justificados, tablas, saltos de pagina, encabezado y pie con numero de
#' pagina). Si officer no esta disponible, cae a un .txt equivalente.
#'
#' @param out_path     Ruta destino del archivo .docx
#' @param patient      Lista con $mrn, $nombre, $sexo, $edad, $tipo_cancer
#' @param bio_subject  Lista con $hospital_code, $bio_subject_id (opcional)
#' @return out_path

biobank_icf_docx <- function(out_path, patient = NULL, bio_subject = NULL) {

  # Fallback robusto: si officer no esta disponible (ej. PCC sin la
  # dependencia instalada todavia), generamos un .txt con el mismo
  # contenido en lugar de fallar el download.
  if (!requireNamespace("officer", quietly = TRUE)) {
    txt_path <- sub("\\.docx?$", ".txt", out_path)
    if (txt_path == out_path) txt_path <- paste0(out_path, ".txt")
    .biobank_icf_txt(txt_path, patient, bio_subject)
    file.copy(txt_path, out_path, overwrite = TRUE)
    return(invisible(out_path))
  }

  # ---- Datos pre-llenados --------------------------------------------------
  pname  <- patient$nombre %||% "_______________________________"
  pmrn   <- patient$mrn    %||% "_______________"
  psex   <- patient$sexo   %||% "____"
  page   <- if (!is.null(patient$edad) && !is.na(patient$edad))
              sprintf("%d a\u00f1os", as.integer(patient$edad))
            else "____ a\u00f1os"
  pdx    <- patient$tipo_cancer %||% "_______________________________"
  bsid   <- bio_subject$bio_subject_id %||% "(se asignar\u00e1 al firmar)"
  hosp   <- bio_subject$hospital_code  %||% "____"
  hoy    <- format(Sys.Date(), "%Y-%m-%d")

  # ---- Paleta y estilos tipograficos --------------------------------------
  navy <- "#0d2c54"; navy2 <- "#14365e"; teal <- "#2e7d6b"

  fmt_title <- officer::fp_text(font.family = "Calibri", font.size = 18,
                                bold = TRUE, color = navy)
  fmt_subtitle <- officer::fp_text(font.family = "Calibri", font.size = 11,
                                   italic = TRUE, color = navy2)
  fmt_h1 <- officer::fp_text(font.family = "Calibri", font.size = 14,
                             bold = TRUE, color = navy)
  fmt_h2 <- officer::fp_text(font.family = "Calibri", font.size = 12,
                             bold = TRUE, color = navy2)
  fmt_body <- officer::fp_text(font.family = "Calibri", font.size = 11,
                               color = "#1c1c1c")
  fmt_bold <- officer::fp_text(font.family = "Calibri", font.size = 11,
                               bold = TRUE, color = "#1c1c1c")
  fmt_small <- officer::fp_text(font.family = "Calibri", font.size = 9,
                                italic = TRUE, color = "#5a5a5a")
  fmt_chip <- officer::fp_text(font.family = "Consolas", font.size = 10,
                               bold = TRUE, color = teal)

  par_just <- officer::fp_par(text.align = "justify", padding.bottom = 4,
                              padding.top = 2, line_spacing = 1.15)
  par_left <- officer::fp_par(text.align = "left", padding.bottom = 2,
                              line_spacing = 1.15)
  par_indent <- officer::fp_par(text.align = "justify", padding.bottom = 2,
                                padding.left = 24, line_spacing = 1.15)
  par_h1 <- officer::fp_par(text.align = "left", padding.top = 12,
                            padding.bottom = 6,
                            border.bottom = officer::fp_border(color = navy,
                                                               width = 1))
  par_h2 <- officer::fp_par(text.align = "left", padding.top = 8,
                            padding.bottom = 4)
  par_center <- officer::fp_par(text.align = "center", padding.bottom = 6)

  # Helpers para no repetir
  add_h1 <- function(d, txt) {
    officer::body_add_fpar(d, officer::fpar(officer::ftext(txt, fmt_h1),
                                            fp_p = par_h1))
  }
  add_h2 <- function(d, txt) {
    officer::body_add_fpar(d, officer::fpar(officer::ftext(txt, fmt_h2),
                                            fp_p = par_h2))
  }
  add_p <- function(d, txt, indent = FALSE) {
    p <- if (indent) par_indent else par_just
    officer::body_add_fpar(d, officer::fpar(officer::ftext(txt, fmt_body),
                                            fp_p = p))
  }
  add_bullet <- function(d, label, body) {
    # "label" en negrita, luego el texto
    officer::body_add_fpar(d, officer::fpar(
      officer::ftext(label, fmt_bold),
      officer::ftext(body,  fmt_body),
      fp_p = par_indent))
  }

  doc <- officer::read_docx()

  # =========================================================================
  # PORTADA
  # =========================================================================
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("CONSENTIMIENTO INFORMADO", fmt_title),
    fp_p = par_center))
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("Donaci\u00f3n de muestras biol\u00f3gicas y datos cl\u00ednicos al Biobanco Oncol\u00f3gico Krebs",
                   fmt_subtitle),
    fp_p = par_center))
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # Tabla de metadatos
  meta <- data.frame(
    Campo = c("Protocolo IRB", "Versi\u00f3n del documento",
              "Hospital", "Fecha de impresi\u00f3n",
              "Marco normativo"),
    Valor = c("TDM-CEI-2026-V1",
              "ICF-Krebs-v1.0-2026",
              hosp,
              hoy,
              "NOM-012-SSA3-2012 \u00b7 LGS T\u00edtulo Quinto \u00b7 LFPDPPP \u00b7 Helsinki 2013 \u00b7 CIOMS 2016 \u00b7 ISBER 2018"),
    stringsAsFactors = FALSE)
  doc <- officer::body_add_table(doc, meta, style = "Light Grid Accent 1",
                                 first_column = TRUE)
  doc <- officer::body_add_par(doc, "", style = "Normal")

  doc <- add_p(doc,
    paste0("Documento de consentimiento informado para la donaci\u00f3n ",
           "voluntaria de muestras biol\u00f3gicas y datos cl\u00ednicos ",
           "asociados, con fines de investigaci\u00f3n biom\u00e9dica ",
           "oncol\u00f3gica. Lea cuidadosamente, pregunte cuanto necesite ",
           "y conserve una copia firmada."))
  doc <- add_p(doc,
    paste0("Este documento sigue la estructura recomendada por la IARC ",
           "(Common Minimum Technical Standards and Protocols for Biobanks ",
           "Dedicated to Cancer Research, 2017, Anexo 3) y se divide en dos ",
           "partes: la PARTE A es un folleto informativo que explica el ",
           "biobanco, los usos previstos, los riesgos y sus derechos; la ",
           "PARTE B es el formulario de consentimiento donde usted decide ",
           "qu\u00e9 autoriza y firma."))

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # PARTE A - FOLLETO INFORMATIVO
  # =========================================================================
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("PARTE A \u2014 FOLLETO INFORMATIVO",
                   officer::fp_text(font.family = "Calibri", font.size = 16,
                                    bold = TRUE, color = teal)),
    fp_p = par_center))
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("Lea esta secci\u00f3n antes de firmar la Parte B",
                   fmt_subtitle),
    fp_p = par_center))
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # =========================================================================
  # 1. DATOS DEL PARTICIPANTE
  # =========================================================================
  doc <- add_h1(doc, "1. Datos del participante")

  ptbl <- data.frame(
    Campo = c("Nombre completo", "MRN (registro hospitalario)",
              "Sexo", "Edad", "Diagn\u00f3stico oncol\u00f3gico",
              "ID pseudonimizado (BIOID)", "Hospital de atenci\u00f3n",
              "Fecha de la consulta"),
    Valor = c(pname, pmrn, psex, page, pdx, bsid, hosp, hoy),
    stringsAsFactors = FALSE)
  doc <- officer::body_add_table(doc, ptbl, style = "Light Grid Accent 1",
                                 first_column = TRUE)

  # =========================================================================
  # 2. MARCO ETICO Y NORMATIVO
  # =========================================================================
  doc <- add_h1(doc, "2. Marco \u00e9tico y normativo")
  doc <- add_p(doc,
    paste0("El Biobanco Oncol\u00f3gico Krebs opera bajo los principios ",
           "bio\u00e9ticos cl\u00e1sicos:"))
  doc <- add_bullet(doc, "Autonom\u00eda: ",
    "su participaci\u00f3n es libre, informada y revocable en cualquier momento.")
  doc <- add_bullet(doc, "Beneficencia: ",
    "el biobanco busca generar conocimiento que beneficie a futuros pacientes con c\u00e1ncer.")
  doc <- add_bullet(doc, "No maleficencia: ",
    "minimiza riesgos f\u00edsicos, psicol\u00f3gicos y sociales mediante pseudonimizaci\u00f3n criptogr\u00e1fica y control de acceso.")
  doc <- add_bullet(doc, "Justicia: ",
    "garantiza acceso equitativo de la comunidad cient\u00edfica y evita la explotaci\u00f3n de poblaciones vulnerables.")
  doc <- add_p(doc,
    paste0("Adicionalmente cumple con la Declaraci\u00f3n de Helsinki ",
           "(Asociaci\u00f3n M\u00e9dica Mundial, revisi\u00f3n 2013), las ",
           "Pautas \u00c9ticas Internacionales para la Investigaci\u00f3n ",
           "Biom\u00e9dica en Seres Humanos del Consejo de Organizaciones ",
           "Internacionales de las Ciencias M\u00e9dicas (CIOMS, 2016), las ",
           "ISBER Best Practices for Repositories (2018) y la normativa ",
           "mexicana aplicable: Ley General de Salud (T\u00edtulo Quinto), ",
           "su Reglamento en materia de investigaci\u00f3n, NOM-012-SSA3-2012, ",
           "NOM-035-SSA3-2012, LFPDPPP y, cuando aplique, NOM-220-SSA1-2016."))

  # =========================================================================
  # 3. INVITACION A PARTICIPAR
  # =========================================================================
  doc <- add_h1(doc, "3. Invitaci\u00f3n a participar")
  doc <- add_p(doc,
    paste0("Se le invita a participar voluntariamente como donante de ",
           "muestras biol\u00f3gicas y datos cl\u00ednicos al Biobanco ",
           "Oncol\u00f3gico Krebs. Un biobanco es una infraestructura ",
           "institucional sin fines de lucro que conserva, administra y ",
           "distribuye material biol\u00f3gico humano y datos asociados ",
           "para apoyar proyectos de investigaci\u00f3n biom\u00e9dica ",
           "previamente aprobados por un Comit\u00e9 de \u00c9tica en ",
           "Investigaci\u00f3n (CEI) y un Comit\u00e9 Cient\u00edfico."))
  doc <- add_p(doc,
    paste0("La donaci\u00f3n es independiente del tratamiento que reciba: ",
           "no condiciona ning\u00fan procedimiento ni decisi\u00f3n ",
           "cl\u00ednica. Su m\u00e9dico tratante puede o no ser uno de ",
           "los investigadores que utilicen las muestras."))

  # =========================================================================
  # 4. NATURALEZA VOLUNTARIA
  # =========================================================================
  doc <- add_h1(doc, "4. Naturaleza voluntaria de la participaci\u00f3n")
  doc <- add_p(doc,
    paste0("Su participaci\u00f3n es totalmente voluntaria. Si decide no ",
           "donar, su atenci\u00f3n m\u00e9dica, derechos asistenciales y ",
           "relaci\u00f3n con su m\u00e9dico tratante no se ver\u00e1n ",
           "afectados de ninguna forma. Tampoco afectar\u00e1 la cobertura ",
           "de su seguro ni el acceso a servicios hospitalarios. Puede ",
           "cambiar de opini\u00f3n y retirar el consentimiento en ",
           "cualquier momento sin necesidad de justificaci\u00f3n."))

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # 5. PROPOSITO DEL BIOBANCO
  # =========================================================================
  doc <- add_h1(doc, "5. Prop\u00f3sito del Biobanco Oncol\u00f3gico Krebs")
  doc <- add_p(doc,
    paste0("El Biobanco Oncol\u00f3gico Krebs tiene como prop\u00f3sito ",
           "general construir un acervo institucional de muestras biol\u00f3gicas ",
           "humanas y datos cl\u00ednicos vinculados, de alta calidad ",
           "anal\u00edtica y con trazabilidad completa, que permita acelerar ",
           "la investigaci\u00f3n traslacional sobre el c\u00e1ncer en ",
           "M\u00e9xico y reducir la dependencia de cohortes biol\u00f3gicas ",
           "extranjeras que no representan la diversidad gen\u00e9tica de ",
           "nuestra poblaci\u00f3n."))
  doc <- add_p(doc,
    paste0("El acervo se administra con criterios cient\u00edficos, ",
           "\u00e9ticos y de equidad, asegurando que cada proyecto que ",
           "acceda a las muestras justifique su pertinencia, presente ",
           "metodolog\u00eda v\u00e1lida y demuestre que no existen ",
           "alternativas menos invasivas para responder a la pregunta ",
           "de investigaci\u00f3n."))

  # =========================================================================
  # 6. OBJETIVOS CIENTIFICOS
  # =========================================================================
  doc <- add_h1(doc, "6. Objetivos cient\u00edficos espec\u00edficos")
  doc <- add_p(doc,
    "El biobanco apoyar\u00e1 proyectos dirigidos, entre otros, a:")
  doc <- add_bullet(doc, "a) Bases moleculares del c\u00e1ncer: ",
    "estudiar mutaciones som\u00e1ticas y germinales, alteraciones epigen\u00e9ticas, expresi\u00f3n g\u00e9nica y v\u00edas de se\u00f1alizaci\u00f3n implicadas en la oncog\u00e9nesis y la progresi\u00f3n tumoral.")
  doc <- add_bullet(doc, "b) Biomarcadores: ",
    "identificar y validar marcadores diagn\u00f3sticos, pron\u00f3sticos y predictivos de respuesta al tratamiento en sangre y en tejido tumoral.")
  doc <- add_bullet(doc, "c) Medicina de precisi\u00f3n: ",
    "desarrollar y validar pruebas diagn\u00f3sticas y terapias dirigidas, incluyendo paneles de NGS, perfiles transcript\u00f3micos y modelos preditivos basados en inteligencia artificial.")
  doc <- add_bullet(doc, "d) Epidemiolog\u00eda molecular: ",
    "describir la frecuencia y distribuci\u00f3n de variantes oncog\u00e9nicas en la poblaci\u00f3n mexicana y compararla con cohortes internacionales.")
  doc <- add_bullet(doc, "e) Microambiente tumoral e inmunolog\u00eda: ",
    "caracterizar la respuesta inmune, el microbioma y las interacciones c\u00e9lula-estroma asociadas al c\u00e1ncer.")
  doc <- add_bullet(doc, "f) Resistencia y recurrencia: ",
    "estudiar mecanismos de resistencia a quimioterapia, terapia dirigida e inmunoterapia, as\u00ed como recurrencia tumoral.")
  doc <- add_bullet(doc, "g) Modelos preclinicos: ",
    "generar organoides, xenoinjertos derivados de paciente (PDX) o cultivos primarios para evaluar nuevas terapias.")
  doc <- add_bullet(doc, "h) Formaci\u00f3n acad\u00e9mica: ",
    "apoyar tesis de pregrado, posgrado y residencias m\u00e9dicas en oncolog\u00eda, anatom\u00eda patol\u00f3gica y biolog\u00eda molecular.")
  doc <- add_bullet(doc, "i) Colaboraciones: ",
    "habilitar redes nacionales e internacionales de investigaci\u00f3n bajo Acuerdos de Transferencia de Material (MTA) y c\u00f3digos de gobernanza.")
  doc <- add_p(doc,
    paste0("Cada proyecto requiere aprobaci\u00f3n previa del Comit\u00e9 ",
           "de \u00c9tica y del Comit\u00e9 Cient\u00edfico del biobanco. ",
           "Ning\u00fan proyecto podr\u00e1 utilizar sus muestras fuera ",
           "del alcance que usted autorice en la secci\u00f3n 14 de este ",
           "documento."))

  # =========================================================================
  # 7. PROCEDIMIENTO Y TIPOS DE MUESTRA
  # =========================================================================
  doc <- add_h1(doc, "7. Procedimiento y tipos de muestra")
  doc <- add_p(doc,
    paste0("Las muestras se obtendr\u00e1n exclusivamente durante ",
           "procedimientos cl\u00ednicos ya programados (cirug\u00eda ",
           "oncol\u00f3gica, biopsia diagn\u00f3stica, extracci\u00f3n de ",
           "sangre indicada por su m\u00e9dico). No se realizar\u00e1n ",
           "intervenciones adicionales ni se aumentar\u00e1 el riesgo de ",
           "su tratamiento m\u00e9dico. Las muestras que podr\u00e1n ",
           "almacenarse incluyen:"))
  doc <- add_p(doc,
    "\u2022 Tejido tumoral fresco, congelado o en bloque de parafina (FFPE).",
    indent = TRUE)
  doc <- add_p(doc,
    "\u2022 Tejido normal pareado adyacente al tumor.",
    indent = TRUE)
  doc <- add_p(doc,
    "\u2022 Sangre perif\u00e9rica completa, plasma y suero.",
    indent = TRUE)
  doc <- add_p(doc,
    "\u2022 \u00c1cidos nucleicos extra\u00eddos: ADN gen\u00f3mico, ARN, ADN tumoral circulante (ctDNA).",
    indent = TRUE)
  doc <- add_p(doc,
    "\u2022 Cuando aplique, otros fluidos (orina, l\u00edquido pleural, ascitis, l\u00edquido cefalorraqu\u00eddeo) obtenidos por indicaci\u00f3n cl\u00ednica.",
    indent = TRUE)
  doc <- add_p(doc,
    paste0("Las muestras se acompa\u00f1ar\u00e1n de datos cl\u00ednicos ",
           "relevantes (diagn\u00f3stico, estadificaci\u00f3n, tratamiento, ",
           "evoluci\u00f3n, histopatolog\u00eda, estudios de imagen y de ",
           "laboratorio) vinculados \u00fanicamente al BIOID y nunca a su ",
           "nombre o n\u00famero de expediente."))

  # =========================================================================
  # 8. CALIDAD Y TRAZABILIDAD
  # =========================================================================
  doc <- add_h1(doc, "8. Calidad y trazabilidad de las muestras")
  doc <- add_p(doc,
    paste0("Cada muestra se procesa siguiendo Procedimientos Normalizados ",
           "de Operaci\u00f3n (SOPs) basados en ISBER Best Practices 2018: ",
           "tiempo isqu\u00e9mico controlado, temperatura monitoreada en ",
           "tiempo real, alicuotaci\u00f3n para minimizar ciclos de ",
           "congelaci\u00f3n-descongelaci\u00f3n, y registro electr\u00f3nico ",
           "inmutable de cada paso (cadena de custodia). Esto garantiza ",
           "que los resultados de investigaci\u00f3n derivados sean ",
           "reproducibles y de calidad publicable."))

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # 9. RIESGOS
  # =========================================================================
  doc <- add_h1(doc, "9. Riesgos")
  doc <- add_p(doc,
    paste0("La donaci\u00f3n no agrega riesgos f\u00edsicos a los ya ",
           "asociados al procedimiento cl\u00ednico que de cualquier manera ",
           "se realizar\u00eda. Los principales riesgos residuales son:"))
  doc <- add_bullet(doc, "P\u00e9rdida de confidencialidad: ",
    "mitigada por pseudonimizaci\u00f3n criptogr\u00e1fica (BIOID generado v\u00eda HMAC-SHA256), almacenamiento cifrado del mapeo identidad-muestra, control de acceso por roles y auditor\u00eda inmutable de cada acceso.")
  doc <- add_bullet(doc, "Hallazgos incidentales: ",
    "podr\u00edan surgir en estudios genet\u00edcos hallazgos sobre predisposici\u00f3n a otras enfermedades. Solo se le notificar\u00e1n los hallazgos accionables y siempre con asesor\u00eda gen\u00e9tica previa.")
  doc <- add_bullet(doc, "Reidentificaci\u00f3n por datos gen\u00f3micos: ",
    "los datos gen\u00f3micos son intr\u00ednsecamente \u00fanicos. El biobanco aplica controles de acceso y nunca publica datos gen\u00f3micos crudos en repositorios abiertos sin pol\u00edticas de acceso controlado (dbGaP, EGA o equivalente nacional).")
  doc <- add_bullet(doc, "Discriminaci\u00f3n: ",
    "ning\u00fan resultado individual ser\u00e1 compartido con su empleador, aseguradora ni terceros, dada la sensibilidad social y legal de los datos gen\u00e9ticos (excepcionalismo gen\u00e9tico).")

  # =========================================================================
  # 10. BENEFICIOS
  # =========================================================================
  doc <- add_h1(doc, "10. Beneficios")
  doc <- add_p(doc,
    paste0("La donaci\u00f3n no representa un beneficio directo inmediato ",
           "para usted. Los beneficios esperados son indirectos y a mediano ",
           "o largo plazo: la investigaci\u00f3n derivada puede mejorar el ",
           "diagn\u00f3stico, el pron\u00f3stico y los tratamientos ",
           "disponibles para futuros pacientes con c\u00e1ncer, ",
           "particularmente en M\u00e9xico, donde existe sub-representaci\u00f3n ",
           "en cohortes internacionales. Su participaci\u00f3n contribuye a ",
           "que la medicina de precisi\u00f3n sea m\u00e1s equitativa."))

  # =========================================================================
  # 11. TIEMPO DE ALMACENAMIENTO
  # =========================================================================
  doc <- add_h1(doc, "11. Tiempo y condiciones de almacenamiento")
  doc <- add_p(doc,
    paste0("Las muestras y los datos asociados se almacenar\u00e1n por un ",
           "periodo inicial de hasta veinte (20) a\u00f1os contados desde ",
           "la fecha de firma de este consentimiento. El plazo podr\u00e1 ",
           "renovarse por periodos iguales si el biobanco se mantiene ",
           "activo y los proyectos as\u00ed lo requieren. Antes de cada ",
           "renovaci\u00f3n, el Comit\u00e9 de \u00c9tica revisar\u00e1 la ",
           "pertinencia cient\u00edfica del acervo."))
  doc <- add_p(doc,
    paste0("Las muestras se conservan a temperaturas controladas (-80 \u00b0C, ",
           "-196 \u00b0C en nitr\u00f3geno l\u00edquido o 4 \u00b0C seg\u00fan ",
           "el tipo) en instalaciones con respaldo el\u00e9ctrico, monitoreo ",
           "24/7 y planes de contingencia documentados."))
  doc <- add_p(doc,
    paste0("Si el biobanco cesara operaciones, sus muestras ser\u00e1n: ",
           "(i) destruidas conforme a la NOM-087-SEMARNAT-SSA1-2002 ",
           "(residuos peligrosos biol\u00f3gico-infecciosos), o ",
           "(ii) transferidas a otro biobanco autorizado, decisi\u00f3n que ",
           "ser\u00e1 comunicada al Comit\u00e9 de \u00c9tica. Usted puede ",
           "solicitar en cualquier momento la destrucci\u00f3n de las ",
           "muestras no utilizadas (secci\u00f3n 16. Derechos)."))
  doc <- add_h2(doc, "11.1 Disposici\u00f3n de muestras en caso de fallecimiento")
  doc <- add_p(doc,
    paste0("Salvo que usted o su representante legal indiquen lo contrario, ",
           "su consentimiento contin\u00faa vigente despu\u00e9s de su ",
           "fallecimiento, conforme a la pr\u00e1ctica internacional de ",
           "biobancos oncol\u00f3gicos. Sus muestras y datos seguir\u00e1n ",
           "siendo utilizados \u00fanicamente con los alcances que usted ",
           "haya autorizado en la secci\u00f3n 14, dado que el valor ",
           "cient\u00edfico de los biobancos depende del seguimiento a ",
           "largo plazo. Sus familiares directos pueden, en cualquier ",
           "momento, solicitar al Custodio la destrucci\u00f3n de las ",
           "muestras no utilizadas o el cese del re-contacto a familiares."))

  # =========================================================================
  # 12. CONFIDENCIALIDAD
  # =========================================================================
  doc <- add_h1(doc, "12. Confidencialidad y protecci\u00f3n de datos personales")
  doc <- add_p(doc,
    paste0("Sus datos personales ser\u00e1n tratados conforme a la Ley ",
           "Federal de Protecci\u00f3n de Datos Personales en Posesi\u00f3n ",
           "de los Particulares (LFPDPPP) y su reglamento. Los datos de ",
           "salud son considerados sensibles y por ello reciben las ",
           "siguientes medidas reforzadas:"))
  doc <- add_bullet(doc, "Pseudonimizaci\u00f3n: ",
    "su identidad se sustituye por un BIOID generado mediante HMAC-SHA256 con sal institucional.")
  doc <- add_bullet(doc, "Cifrado: ",
    "el mapeo BIOID \u2194 MRN se conserva cifrado simet\u00e9ricamente, con clave bajo custodia exclusiva del Custodio del biobanco.")
  doc <- add_bullet(doc, "Acceso por roles: ",
    "los investigadores reciben \u00fanicamente datos pseudonimizados; nunca su nombre, direcci\u00f3n ni n\u00famero de expediente.")
  doc <- add_bullet(doc, "Auditor\u00eda: ",
    "cada acceso queda registrado en una bit\u00e1cora inmutable, revisada peri\u00f3dicamente.")
  doc <- add_bullet(doc, "Publicaciones: ",
    "los art\u00edculos cient\u00edficos, informes y presentaciones nunca incluir\u00e1n datos que permitan identificarlo.")

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # 13. ASPECTOS ETICOS ESPECIALES
  # =========================================================================
  doc <- add_h1(doc, "13. Aspectos \u00e9ticos especiales")
  doc <- add_h2(doc, "13.1 Vulnerabilidad")
  doc <- add_p(doc,
    paste0("Si usted pertenece a un grupo que pudiera considerarse ",
           "vulnerable (menor de edad, persona con capacidad disminuida ",
           "para decidir, comunidad ind\u00edgena, persona privada de la ",
           "libertad), el biobanco aplica salvaguardas adicionales: ",
           "consentimiento del representante legal, asentimiento del menor ",
           "cuando sea posible, materiales en lengua materna y revisi\u00f3n ",
           "espec\u00edfica del Comit\u00e9 de \u00c9tica."))
  doc <- add_h2(doc, "13.2 Excepcionalismo gen\u00e9tico")
  doc <- add_p(doc,
    paste0("Los datos gen\u00e9ticos pueden afectar tambi\u00e9n a sus ",
           "familiares biol\u00f3gicos. El biobanco no compartir\u00e1 ",
           "informaci\u00f3n gen\u00e9tica individual con familiares sin su ",
           "consentimiento explicito. Si autoriza la opci\u00f3n 14.7 ",
           "(hallazgos accionables), se ofrecer\u00e1 asesor\u00eda gen\u00e9tica ",
           "para discutir las implicaciones familiares."))
  doc <- add_h2(doc, "13.3 Soberan\u00eda de datos y equidad")
  doc <- add_p(doc,
    paste0("Las muestras y datos pseudonimizados que salgan del pa\u00eds ",
           "lo har\u00e1n bajo MTA que garantizan: (i) prop\u00f3sito de uso ",
           "espec\u00edfico, (ii) prohibici\u00f3n de re-transferencia sin ",
           "autorizaci\u00f3n, (iii) reconocimiento de la institucio\u0301n ",
           "mexicana en publicaciones, y (iv) acceso retorno-equitativo a ",
           "los hallazgos para la investigaci\u00f3n nacional."))
  doc <- add_h2(doc, "13.4 Conflicto de inter\u00e9s")
  doc <- add_p(doc,
    paste0("Los investigadores que accedan al biobanco declaran sus ",
           "conflictos de inter\u00e9s al Comit\u00e9 de \u00c9tica. Las ",
           "muestras no se otorgar\u00e1n a proyectos con conflictos no ",
           "resueltos. Los pacientes no tienen obligaci\u00f3n de participar ",
           "en proyectos espec\u00edficos."))
  doc <- add_h2(doc, "13.5 Compromiso de no comercializaci\u00f3n directa")
  doc <- add_p(doc,
    paste0("Las muestras no se venden. El biobanco puede recuperar ",
           "costos operativos (procesamiento, almacenamiento, env\u00edo) ",
           "pero no obtiene lucro de su distribuci\u00f3n."))

  # =========================================================================
  # 14. ALCANCE DEL CONSENTIMIENTO
  # =========================================================================
  doc <- add_h1(doc, "14. Alcance del consentimiento")
  doc <- add_p(doc,
    paste0("Marque cada casilla \u00fanicamente si est\u00e1 de acuerdo. ",
           "Puede aceptar algunos usos y rechazar otros, y puede modificar ",
           "estas autorizaciones en el futuro contactando al Custodio."))
  doc <- add_p(doc,
    "[ ] 14.1  Autorizo el uso de mis muestras para investigaci\u00f3n oncol\u00f3gica general (consentimiento amplio / broad consent), siempre que cada proyecto sea aprobado por el Comit\u00e9 de \u00c9tica y el Comit\u00e9 Cient\u00edfico del biobanco.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.2  Autorizo estudios gen\u00f3micos: secuenciaci\u00f3n masiva (NGS), exoma, gen\u00f3mica completa, transcript\u00f3mica, epigen\u00f3mica o paneles dirigidos.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.3  Autorizo el desarrollo y validaci\u00f3n de modelos de aprendizaje autom\u00e1tico / inteligencia artificial sobre mis datos pseudonimizados.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.4  Autorizo la generaci\u00f3n de modelos prec\u00ednicos derivados (organoides, l\u00edneas celulares, xenoinjertos PDX).",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.5  Autorizo ser re-contactado por el biobanco para invitarme a estudios futuros relacionados con mi diagn\u00f3stico o para confirmar / actualizar este consentimiento.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.6  Autorizo compartir mis muestras y datos pseudonimizados con colaboradores acad\u00e9micos externos (nacionales o internacionales) bajo MTA aprobado por la instituci\u00f3n.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.7  Autorizo el uso de mis muestras y datos pseudonimizados en proyectos colaborativos con empresas farmac\u00e9uticas, biotecnol\u00f3gicas o de diagn\u00f3stico, bajo MTA, sin que esto genere derechos comerciales a mi favor.",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.8  Autorizo que, si durante la investigaci\u00f3n se obtienen hallazgos cl\u00ednicamente significativos y accionables sobre mi salud, el biobanco contacte a mi m\u00e9dico tratante para informarle (devoluci\u00f3n de hallazgos secundarios).",
    indent = TRUE)
  doc <- add_p(doc,
    "[ ] 14.9  Autorizo el dep\u00f3sito de mis datos gen\u00f3micos pseudonimizados en repositorios cient\u00edficos de acceso controlado (dbGaP, EGA o equivalente nacional).",
    indent = TRUE)
  doc <- add_p(doc,
    paste0("Renuncia a derechos comerciales: entiendo que las muestras se ",
           "donan sin contraprestaci\u00f3n econ\u00f3mica y que cualquier ",
           "desarrollo, patente o producto derivado de la investigaci\u00f3n ",
           "no genera derechos comerciales ni regal\u00edas a mi favor."))

  # =========================================================================
  # 15. COMPARTICION
  # =========================================================================
  doc <- add_h1(doc, "15. Compartici\u00f3n de muestras y datos")
  doc <- add_h2(doc, "15.1 Principios FAIR y soberan\u00eda nacional")
  doc <- add_p(doc,
    paste0("El biobanco adopta los principios FAIR (Findable, Accessible, ",
           "Interoperable, Reusable) para maximizar el valor cient\u00edfico ",
           "de los datos. Toda compartici\u00f3n se rige por un Acuerdo de ",
           "Transferencia de Material (MTA) institucional aprobado por el ",
           "Comit\u00e9 Cient\u00edfico y por el Comit\u00e9 de \u00c9tica, que ",
           "respeta los derechos de los donantes, la soberan\u00eda nacional ",
           "sobre el material biol\u00f3gico humano y el reconocimiento ",
           "institucional en publicaciones derivadas."))
  doc <- add_h2(doc, "15.2 Transferencia internacional de muestras y datos")
  doc <- add_p(doc,
    paste0("La transferencia de muestras o datos pseudonimizados fuera del ",
           "territorio nacional solo proceder\u00e1 si usted lo autoriza ",
           "expl\u00edcitamente (opciones 14.6 y/o 14.7) y \u00fanicamente bajo ",
           "MTA que garanticen los siguientes m\u00ednimos:"))
  doc <- add_bullet(doc, "Prop\u00f3sito espec\u00edfico: ",
    "el material recibido se usa exclusivamente para el proyecto descrito en el MTA, sin reutilizaci\u00f3n para otros fines sin nueva autorizaci\u00f3n.")
  doc <- add_bullet(doc, "No re-transferencia: ",
    "el receptor no puede transferir el material a terceros sin autorizaci\u00f3n previa por escrito del biobanco.")
  doc <- add_bullet(doc, "Equivalencia regulatoria: ",
    "el receptor debe operar bajo un marco de \u00e9tica e investigaci\u00f3n equivalente o m\u00e1s estricto que el mexicano (Helsinki, CIOMS, GDPR cuando aplique).")
  doc <- add_bullet(doc, "Reconocimiento institucional: ",
    "la instituci\u00f3n mexicana origen es co-autora, co-licenciante o reconocida formalmente en publicaciones, patentes y reportes.")
  doc <- add_bullet(doc, "Retorno equitativo: ",
    "los hallazgos derivados se comparten en formato accesible para investigadores nacionales y, cuando aplique, se ofrecen capacitaci\u00f3n y transferencia tecnol\u00f3gica.")
  doc <- add_bullet(doc, "Destrucci\u00f3n al cierre: ",
    "el material residual se destruye o se devuelve al biobanco al finalizar el proyecto.")
  doc <- add_p(doc,
    paste0("Las transferencias internacionales se notifican al CEI y se ",
           "registran en el cat\u00e1logo p\u00fablico del biobanco (sin datos ",
           "individuales)."))
  doc <- add_h2(doc, "15.3 Repositorios de acceso controlado")
  doc <- add_p(doc,
    paste0("Si autoriza la opci\u00f3n 14.9, sus datos gen\u00f3micos ",
           "pseudonimizados podr\u00e1n depositarse en repositorios cient\u00edficos ",
           "de acceso controlado (por ejemplo, dbGaP de los NIH, EGA del EBI, ",
           "BBMRI-ERIC, o repositorios mexicanos equivalentes). El acceso ",
           "requiere aprobaci\u00f3n de un Comit\u00e9 de Acceso a Datos (DAC) ",
           "y MTA por proyecto. Nunca se publican datos crudos en ",
           "repositorios abiertos sin control de acceso."))

  # =========================================================================
  # 16. DERECHOS
  # =========================================================================
  doc <- add_h1(doc, "16. Derechos del participante (ARCO + retiro)")
  doc <- add_p(doc,
    paste0("De acuerdo con la LFPDPPP, en cualquier momento usted puede ",
           "ejercer sus derechos ARCO sobre sus datos personales, as\u00ed ",
           "como retirar este consentimiento, sin que ello afecte su ",
           "atenci\u00f3n m\u00e9dica:"))
  doc <- add_bullet(doc, "Acceso: ",
    "solicitar conocer qu\u00e9 datos suyos conserva el biobanco.")
  doc <- add_bullet(doc, "Rectificaci\u00f3n: ",
    "corregir datos inexactos o incompletos.")
  doc <- add_bullet(doc, "Cancelaci\u00f3n: ",
    "solicitar la eliminaci\u00f3n de sus datos personales y la destrucci\u00f3n de sus muestras no utilizadas.")
  doc <- add_bullet(doc, "Oposici\u00f3n: ",
    "retirar autorizaciones espec\u00edficas (por ejemplo, retirar el permiso de re-contacto sin retirar la donaci\u00f3n completa).")
  doc <- add_p(doc,
    paste0("La retirada del consentimiento implica que las muestras no ",
           "utilizadas ser\u00e1n destruidas. Los datos y resultados ya ",
           "publicados antes de la retirada permanecer\u00e1n de manera ",
           "anonimizada en estudios concluidos, dado que t\u00e9cnicamente ",
           "no es posible eliminarlos de publicaciones cient\u00edficas ",
           "ya difundidas."))

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # 17. COMPENSACION
  # =========================================================================
  doc <- add_h1(doc, "17. Compensaci\u00f3n econ\u00f3mica")
  doc <- add_p(doc,
    paste0("La donaci\u00f3n es estrictamente altruista y no genera ",
           "ning\u00fan pago, compensaci\u00f3n econ\u00f3mica, regal\u00eda ",
           "ni beneficio material para usted o sus familiares. El biobanco ",
           "tampoco le cobrar\u00e1 por almacenar sus muestras."))

  # =========================================================================
  # 18. DEVOLUCION DE RESULTADOS
  # =========================================================================
  doc <- add_h1(doc, "18. Devoluci\u00f3n de resultados")
  doc <- add_p(doc,
    paste0("La mayor\u00eda de los resultados de investigaci\u00f3n no se ",
           "devuelven individualmente porque su validez cl\u00ednica no ",
           "est\u00e1 establecida (los m\u00e9todos pueden ser experimentales). ",
           "Si autoriza la opci\u00f3n 14.8, el biobanco podr\u00e1 notificar ",
           "a su m\u00e9dico tratante hallazgos secundarios accionables ",
           "(por ejemplo, variantes germinales con implicaciones para ",
           "familiares directos), siempre con asesor\u00eda gen\u00e9tica ",
           "previa. Los resultados agregados (no individuales) podr\u00e1n ",
           "consultarse en publicaciones cient\u00edficas y en informes ",
           "comunitarios del biobanco."))

  # =========================================================================
  # 19. GOBERNANZA, FINANCIAMIENTO Y CONTACTO
  # =========================================================================
  doc <- add_h1(doc, "19. Gobernanza, financiamiento y contacto")
  doc <- add_h2(doc, "19.1 Custodio del biobanco")
  doc <- add_p(doc, "Custodio responsable: _________________________________________________")
  doc <- add_p(doc, "Cargo institucional:  _________________________________________________")
  doc <- add_p(doc, "Tel\u00e9fono de contacto: _________________   Correo: ____________________")
  doc <- add_p(doc,
    paste0("Para ejercer sus derechos ARCO, retirar el consentimiento, ",
           "actualizar autorizaciones o resolver dudas, contacte al ",
           "Custodio."))
  doc <- add_h2(doc, "19.2 Financiamiento del biobanco")
  doc <- add_p(doc,
    paste0("El Biobanco Oncol\u00f3gico Krebs se financia con fondos ",
           "institucionales (presupuesto hospitalario y universitario) y, ",
           "cuando aplique, con donaciones filantr\u00f3picas y subvenciones ",
           "de investigaci\u00f3n nacionales (CONAHCYT, Secretar\u00eda de ",
           "Salud) o internacionales (NIH, IARC, fundaciones acad\u00e9micas) ",
           "obtenidas mediante concurso. Los proyectos que reciban muestras ",
           "pueden cubrir costos de procesamiento y env\u00edo, pero el ",
           "biobanco no opera con fines de lucro y los ingresos se ",
           "reinvierten en infraestructura, control de calidad y ",
           "trazabilidad. Los conflictos de inter\u00e9s se declaran al CEI."))
  doc <- add_h2(doc, "19.3 Quejas, reclamos y comunicaci\u00f3n con el participante")
  doc <- add_p(doc,
    paste0("Si tiene una queja relacionada con el manejo de sus muestras o ",
           "datos, puede dirigirla en el siguiente orden: ",
           "(i) al Custodio del biobanco (secci\u00f3n 19.1) para resoluci\u00f3n ",
           "directa; ",
           "(ii) al Comit\u00e9 de \u00c9tica en Investigaci\u00f3n de la ",
           "instituci\u00f3n (secci\u00f3n 20) si la respuesta del Custodio no ",
           "es satisfactoria; ",
           "(iii) a la Comisi\u00f3n Nacional de Bio\u00e9tica (CONBIOETICA) ",
           "o a la COFEPRIS, en \u00faltima instancia, conforme al marco ",
           "regulatorio vigente. ",
           "Toda queja se documenta en bit\u00e1cora y se responde por ",
           "escrito en un plazo m\u00e1ximo de quince (15) d\u00edas h\u00e1biles. ",
           "Adicionalmente, el biobanco publica un informe anual en su ",
           "p\u00e1gina institucional con resultados agregados, proyectos ",
           "activos y composici\u00f3n del acervo."))

  # =========================================================================
  # 20. CEI
  # =========================================================================
  doc <- add_h1(doc, "20. Comit\u00e9 de \u00c9tica en Investigaci\u00f3n (CEI)")
  doc <- add_p(doc,
    paste0("Si tiene dudas sobre sus derechos o desea presentar una queja ",
           "independiente, puede contactar al Comit\u00e9 de \u00c9tica en ",
           "Investigaci\u00f3n de la instituci\u00f3n:"))
  doc <- add_p(doc, "Comit\u00e9 de \u00c9tica:      _________________________________________________")
  doc <- add_p(doc, "Registro CONBIOETICA: _____________   Registro COFEPRIS: _____________")
  doc <- add_p(doc, "Tel\u00e9fono / correo:    _________________________________________________")

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # PARTE B - FORMULARIO DE CONSENTIMIENTO
  # =========================================================================
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("PARTE B \u2014 FORMULARIO DE CONSENTIMIENTO",
                   officer::fp_text(font.family = "Calibri", font.size = 16,
                                    bold = TRUE, color = teal)),
    fp_p = par_center))
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext("Firme \u00fanicamente despu\u00e9s de leer la Parte A y aclarar dudas",
                   fmt_subtitle),
    fp_p = par_center))
  doc <- officer::body_add_par(doc, "", style = "Normal")

  # =========================================================================
  # 21. DECLARACION
  # =========================================================================
  doc <- add_h1(doc, "21. Declaraci\u00f3n del participante")
  doc <- add_p(doc,
    paste0("Declaro que he le\u00eddo (o me ha sido le\u00eddo) este ",
           "documento, que se me han explicado en lenguaje comprensible ",
           "los objetivos, fines, procedimientos, riesgos, beneficios, ",
           "alcance y duraci\u00f3n del almacenamiento de mis muestras, ",
           "as\u00ed como mis derechos. Se me ha dado oportunidad de hacer ",
           "preguntas y todas han sido respondidas de manera satisfactoria. ",
           "Se me entrega una copia firmada de este documento. Acepto ",
           "participar voluntariamente."))

  # =========================================================================
  # 22. FIRMAS
  # =========================================================================
  doc <- add_h1(doc, "22. Firmas")
  firmas <- data.frame(
    Rol = c("Participante (donante)",
            "Representante legal (si aplica)",
            "Testigo 1",
            "Testigo 2",
            "Investigador / cl\u00ednico responsable",
            "Custodio del biobanco"),
    Nombre = rep("_______________________________________", 6L),
    Firma  = rep("__________________  Fecha: ____________", 6L),
    stringsAsFactors = FALSE)
  doc <- officer::body_add_table(doc, firmas, style = "Light Grid Accent 1",
                                 first_column = TRUE)

  doc <- officer::body_add_break(doc)

  # =========================================================================
  # 23. GLOSARIO
  # =========================================================================
  doc <- add_h1(doc, "23. Glosario de t\u00e9rminos clave")
  glos <- data.frame(
    Termino = c(
      "Biobanco",
      "Muestra biol\u00f3gica",
      "Pseudonimizaci\u00f3n",
      "BIOID",
      "HMAC-SHA256",
      "NGS",
      "ctDNA",
      "FFPE",
      "Organoides / PDX",
      "MTA",
      "FAIR",
      "dbGaP / EGA",
      "Hallazgo accionable",
      "Asesor\u00eda gen\u00e9tica",
      "ARCO",
      "CEI / CONBIOETICA / COFEPRIS",
      "Broad consent",
      "Soberan\u00eda de datos"
    ),
    Definicion = c(
      "Infraestructura institucional sin fines de lucro que recolecta, procesa, almacena y distribuye muestras biol\u00f3gicas humanas y datos asociados para investigaci\u00f3n.",
      "Tejido, sangre, fluidos o derivados (ADN, ARN, prote\u00ednas) provenientes de un participante.",
      "T\u00e9cnica que sustituye la identidad por un c\u00f3digo (BIOID); el v\u00ednculo se conserva cifrado y bajo control restringido.",
      "Identificador pseudonimizado del biobanco con formato {INST}-{SUBJ}-{COL}-{TIPO}-{ALI} (ej. HSPA-7K3M9PQ2-001-FFPE-A01).",
      "Funci\u00f3n criptogr\u00e1fica (Hash-based Message Authentication Code) que genera el BIOID de manera reproducible pero irreversible sin la sal institucional.",
      "Next-Generation Sequencing: secuenciaci\u00f3n masiva paralela de ADN/ARN (paneles, exoma, genoma, transcriptoma).",
      "ADN tumoral circulante en sangre; permite biopsia l\u00edquida no invasiva.",
      "Formalin-Fixed Paraffin-Embedded: bloques de parafina del laboratorio de patolog\u00eda.",
      "Modelos prec\u00ednicos derivados de paciente: organoides 3D in vitro y xenoinjertos en rat\u00f3n inmunodeficiente (PDX).",
      "Material Transfer Agreement: contrato que rige la transferencia de muestras entre instituciones.",
      "Findable, Accessible, Interoperable, Reusable: principios para datos cient\u00edficos abiertos pero gobernados.",
      "Repositorios internacionales de datos gen\u00f3micos con acceso controlado por DAC (NIH y EBI respectivamente).",
      "Resultado gen\u00e9tico cl\u00ednicamente significativo cuya devoluci\u00f3n cambia el manejo m\u00e9dico (por ej., variantes en BRCA1/2, Lynch).",
      "Consulta especializada que explica las implicaciones m\u00e9dicas y familiares de un resultado gen\u00e9tico antes y despu\u00e9s de comunicarlo.",
      "Acceso, Rectificaci\u00f3n, Cancelaci\u00f3n y Oposici\u00f3n: derechos sobre datos personales bajo la LFPDPPP.",
      "Comit\u00e9 de \u00c9tica en Investigaci\u00f3n local; Comisi\u00f3n Nacional de Bio\u00e9tica; Comisi\u00f3n Federal para la Protecci\u00f3n contra Riesgos Sanitarios.",
      "Consentimiento amplio: autorizaci\u00f3n para usos futuros de investigaci\u00f3n no especificados al momento de la donaci\u00f3n, sujetos a aprobaci\u00f3n del CEI.",
      "Principio que afirma que las muestras y datos generados en M\u00e9xico son patrimonio nacional y su salida y uso se rigen por marcos institucionales y MTA."
    ),
    stringsAsFactors = FALSE
  )
  doc <- officer::body_add_table(doc, glos, style = "Light Grid Accent 1",
                                 first_column = TRUE)

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(doc, officer::fpar(
    officer::ftext(paste0("Documento generado por el sistema Krebs V0.2 el ",
                          hoy, ". Estructura basada en IARC Common Minimum ",
                          "Technical Standards and Protocols for Biobanks ",
                          "Dedicated to Cancer Research (2017), Anexo 3. ",
                          "Imprima en hoja membretada institucional. ",
                          "Conserve la copia firmada en el expediente del ",
                          "biobanco y registre la versi\u00f3n y fecha en el ",
                          "m\u00f3dulo de Consentimientos del sistema."),
                   fmt_small),
    fp_p = par_just))

  print(doc, target = out_path)
  invisible(out_path)
}

# ---- Fallback texto plano (si officer no esta instalado) -------------------
.biobank_icf_txt <- function(out_path, patient = NULL, bio_subject = NULL) {
  pname <- patient$nombre %||% "_______________________________"
  pmrn  <- patient$mrn    %||% "_______________"
  psex  <- patient$sexo   %||% "____"
  page  <- if (!is.null(patient$edad) && !is.na(patient$edad))
             sprintf("%d a\u00f1os", as.integer(patient$edad))
           else "____ a\u00f1os"
  pdx   <- patient$tipo_cancer %||% "_______________________________"
  bsid  <- bio_subject$bio_subject_id %||% "(se asignar\u00e1 al firmar)"
  hosp  <- bio_subject$hospital_code  %||% "____"
  hoy   <- format(Sys.Date(), "%Y-%m-%d")

  txt <- c(
    "CONSENTIMIENTO INFORMADO PARA DONACI\u00d3N DE MUESTRAS BIOL\u00d3GICAS",
    "BIOBANCO ONCOL\u00d3GICO KREBS",
    sprintf("Protocolo IRB: TDM-CEI-2026-V1   Hospital: %s   Versi\u00f3n: ICF-Krebs-v1.0-2026   Fecha: %s",
            hosp, hoy),
    "Marco normativo: NOM-012-SSA3-2012 \u00b7 LGS T\u00edtulo Quinto \u00b7 LFPDPPP \u00b7 Helsinki 2013 \u00b7 CIOMS 2016 \u00b7 ISBER 2018",
    "Estructura: IARC Common Minimum Technical Standards (2017), Anexo 3.",
    "",
    "================ PARTE A \u2014 FOLLETO INFORMATIVO ================",
    "",
    "1. Datos del participante",
    sprintf("   Nombre:       %s", pname),
    sprintf("   MRN:          %s", pmrn),
    sprintf("   Sexo:         %s    Edad: %s", psex, page),
    sprintf("   Diagn\u00f3stico:  %s", pdx),
    sprintf("   BIOID:        %s", bsid),
    sprintf("   Hospital:     %s", hosp),
    sprintf("   Fecha:        %s", hoy),
    "",
    "2. Marco \u00e9tico (autonom\u00eda, beneficencia, no maleficencia, justicia).",
    "3. Invitaci\u00f3n voluntaria al Biobanco Oncol\u00f3gico Krebs.",
    "4. Naturaleza voluntaria: no participar no afecta su atenci\u00f3n.",
    "5. Prop\u00f3sito: acervo institucional para investigaci\u00f3n traslacional.",
    "6. Objetivos cient\u00edficos: bases moleculares, biomarcadores, medicina",
    "   de precisi\u00f3n, epidemiolog\u00eda molecular, microambiente, resistencia,",
    "   modelos prec\u00ednicos, formaci\u00f3n acad\u00e9mica, colaboraciones bajo MTA.",
    "7. Procedimiento: tejido tumoral / normal pareado / sangre / plasma /",
    "   suero / ADN-ARN / fluidos por indicaci\u00f3n cl\u00ednica.",
    "8. Calidad y trazabilidad bajo SOPs ISBER 2018.",
    "9. Riesgos: confidencialidad (mitigada por BIOID HMAC-SHA256),",
    "   hallazgos incidentales, reidentificaci\u00f3n gen\u00f3mica, discriminaci\u00f3n.",
    "10. Beneficios: indirectos, para futuros pacientes; equidad en MX.",
    "11. Almacenamiento: hasta 20 a\u00f1os renovables; -80\u00b0C / -196\u00b0C /",
    "    NOM-087-SEMARNAT-SSA1-2002 al destruir.",
    "    11.1 Disposici\u00f3n post-mortem: el consentimiento contin\u00faa vigente",
    "         tras el fallecimiento; familiares pueden solicitar destrucci\u00f3n.",
    "12. Confidencialidad LFPDPPP: pseudonimizaci\u00f3n + cifrado + roles +",
    "    auditor\u00eda; nunca publicar identificadores.",
    "13. Aspectos \u00e9ticos especiales:",
    "    13.1 Vulnerabilidad (menores, comunidades ind\u00edgenas, etc.).",
    "    13.2 Excepcionalismo gen\u00e9tico (impacto familiar).",
    "    13.3 Soberan\u00eda de datos: MTA con prop\u00f3sito espec\u00edfico, sin",
    "         re-transferencia, reconocimiento institucional, retorno equitativo.",
    "    13.4 Conflicto de inter\u00e9s: declarado al CEI.",
    "    13.5 Compromiso de no comercializaci\u00f3n directa.",
    "",
    "14. Alcance del consentimiento (marque con X)",
    "    [ ] 14.1  Investigaci\u00f3n oncol\u00f3gica general (broad consent)",
    "    [ ] 14.2  Estudios gen\u00f3micos / NGS / exoma / transcriptoma",
    "    [ ] 14.3  Modelos de IA / aprendizaje autom\u00e1tico",
    "    [ ] 14.4  Modelos prec\u00ednicos (organoides / PDX / l\u00edneas)",
    "    [ ] 14.5  Re-contacto para estudios futuros",
    "    [ ] 14.6  Compartir con colaboradores externos bajo MTA",
    "    [ ] 14.7  Uso por farmac\u00e9uticas / biotech bajo MTA",
    "    [ ] 14.8  Devoluci\u00f3n de hallazgos secundarios accionables",
    "    [ ] 14.9  Dep\u00f3sito en repositorios de acceso controlado (dbGaP, EGA)",
    "    Renuncia a derechos comerciales: donaci\u00f3n altruista, sin regal\u00edas.",
    "",
    "15. Compartici\u00f3n de muestras y datos:",
    "    15.1 FAIR + soberan\u00eda nacional bajo MTA institucional.",
    "    15.2 Transferencia internacional: solo con autorizaci\u00f3n 14.6/14.7,",
    "         prop\u00f3sito espec\u00edfico, sin re-transferencia, equivalencia",
    "         regulatoria, reconocimiento institucional, retorno equitativo,",
    "         destrucci\u00f3n al cierre del proyecto.",
    "    15.3 Repositorios de acceso controlado (dbGaP/EGA/BBMRI) con DAC.",
    "16. Derechos ARCO + retiro: Acceso, Rectificaci\u00f3n, Cancelaci\u00f3n, Oposici\u00f3n.",
    "17. Compensaci\u00f3n econ\u00f3mica: ninguna; donaci\u00f3n altruista.",
    "18. Devoluci\u00f3n de resultados: solo accionables si autoriza opci\u00f3n 14.8.",
    "19. Gobernanza, financiamiento y contacto",
    "    19.1 Custodio del biobanco",
    "         Nombre: _______________________  Cargo: _______________________",
    "         Tel:    _______________________  Correo: ______________________",
    "    19.2 Financiamiento: institucional + subvenciones (CONAHCYT/SS/NIH/IARC),",
    "         sin fines de lucro; ingresos reinvertidos en infraestructura.",
    "    19.3 Quejas: Custodio \u2192 CEI \u2192 CONBIOETICA/COFEPRIS;",
    "         respuesta por escrito en 15 d\u00edas h\u00e1biles.",
    "20. Comit\u00e9 de \u00c9tica en Investigaci\u00f3n",
    "    CEI: _________________________  CONBIOETICA: ___________________",
    "    Tel/Correo: _____________________________________________________",
    "",
    "================ PARTE B \u2014 FORMULARIO DE CONSENTIMIENTO ================",
    "",
    "21. Declaraci\u00f3n del participante: he comprendido y acepto participar.",
    "",
    "22. Firmas",
    "    Participante:           _______________________  Fecha: __________",
    "    Repr. legal (si aplica): ______________________  Fecha: __________",
    "    Testigo 1:              _______________________  Fecha: __________",
    "    Testigo 2:              _______________________  Fecha: __________",
    "    Investigador:           _______________________  Fecha: __________",
    "    Custodio biobanco:      _______________________  Fecha: __________",
    "",
    "23. Glosario de t\u00e9rminos clave",
    "    Biobanco: infraestructura institucional para acervo y distribuci\u00f3n.",
    "    BIOID: ID pseudonimizado {INST}-{SUBJ}-{COL}-{TIPO}-{ALI}.",
    "    HMAC-SHA256: hash criptogr\u00e1fico irreversible que genera el BIOID.",
    "    NGS: secuenciaci\u00f3n masiva (paneles, exoma, genoma, transcriptoma).",
    "    ctDNA: ADN tumoral circulante (biopsia l\u00edquida).",
    "    FFPE: bloques de tejido fijado en formalina y embebido en parafina.",
    "    Organoides / PDX: modelos prec\u00ednicos derivados de paciente.",
    "    MTA: Material Transfer Agreement entre instituciones.",
    "    FAIR: Findable, Accessible, Interoperable, Reusable.",
    "    dbGaP / EGA: repositorios gen\u00f3micos con acceso controlado.",
    "    Hallazgo accionable: variante con impacto en manejo cl\u00ednico.",
    "    ARCO: Acceso, Rectificaci\u00f3n, Cancelaci\u00f3n y Oposici\u00f3n (LFPDPPP).",
    "    Broad consent: consentimiento amplio a usos futuros aprobados por CEI.",
    "",
    paste0("Generado por Krebs V0.2 el ", hoy,
           " (fallback texto: 'officer' no instalado en el servidor).")
  )
  # UTF-8 explicito para acentos
  con <- file(out_path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(txt), con, useBytes = TRUE)
  invisible(out_path)
}
