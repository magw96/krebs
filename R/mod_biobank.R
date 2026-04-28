#' mod_biobank: tab dedicado al banco de tejidos.
#'
#' Tres flujos en una sola pagina:
#'   1. Buscar paciente (reutiliza search_patients/load_patient).
#'   2. Consentimiento informado (crear / ver / descargar machote DOCX).
#'   3. Recoleccion de muestras (BIOID + aliquotas + etiqueta PDF).
#'
#' Layout: 2 columnas arriba (busqueda | identidad pseudonimizada),
#'         1 columna abajo con tabBox de 3 pestanas.
#'
#' La trazabilidad clinica/banco se mantiene via biobank_subject_link
#' (mapping cifrado MRN <-> bio_subject_id).

mod_biobank_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("vials"), " Biobanco"),
    shiny::p(class = "text-muted",
      "Registro y seguimiento de muestras biologicas. ",
      "Cada paciente recibe un identificador pseudonimizado (BIOID) ",
      "que separa la identidad clinica de la muestra."),

    # ---- Top row: search | patient identity ---------------------------------
    shiny::fluidRow(
      shiny::column(6,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("magnifying-glass"),
                                  " Buscar paciente"),
          width = 12, status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          shiny::textInput(ns("q"), NULL,
            placeholder = "MRN o nombre (>= 2 caracteres)",
            width = "100%"),
          shiny::div(style = "max-height:200px;overflow:auto;",
            DT::DTOutput(ns("results_tbl")))
        )
      ),
      shiny::column(6,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("user-shield"),
                                  " Sujeto del biobanco"),
          width = 12, status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          shiny::uiOutput(ns("subject_card"))
        )
      )
    ),

    # ---- Below: tabs --------------------------------------------------------
    shiny::fluidRow(
      shiny::column(12,
        bs4Dash::tabBox(
          id = ns("tabs"), width = 12, side = "left",
          status = "info", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("flask"), " Banco de tejidos"),

          # ---- TAB 1: Consentimientos ---------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("file-signature"),
                                   " Consentimientos"),
            shiny::p(class = "text-muted small",
              "Cada muestra debe estar respaldada por un consentimiento ",
              "informado vigente. Aqui puede registrar el ICF firmado y ",
              "descargar un machote para la consulta."),

            shiny::fluidRow(
              shiny::column(6,
                shiny::h5(shiny::icon("clipboard-list"), " Vigentes"),
                DT::DTOutput(ns("consents_tbl"))),
              shiny::column(6,
                shiny::h5(shiny::icon("plus"), " Registrar consentimiento firmado"),
                shiny::textInput(ns("c_template"), "Plantilla / version",
                                 value = "ICF-Krebs-v1.0-2026"),
                shiny::dateInput(ns("c_signed_dt"), "Fecha de firma",
                                 value = Sys.Date()),
                shinyWidgets::awesomeCheckbox(ns("c_genomics"),
                  "Permite estudios genomicos", value = TRUE),
                shinyWidgets::awesomeCheckbox(ns("c_recontact"),
                  "Permite re-contacto", value = FALSE),
                shiny::textAreaInput(ns("c_notes"), "Notas (opcional)",
                                     rows = 2, width = "100%"),
                shiny::actionButton(ns("c_save"),
                  shiny::tagList(shiny::icon("save"),
                                 " Registrar consentimiento"),
                  class = "btn-success")
              )
            ),
            shiny::hr(),
            shiny::div(class = "alert alert-info small",
              shiny::icon("file-word"),
              " Para impresion en consulta, descargue el machote ICF ",
              "pre-llenado con los datos del paciente:"),
            shiny::uiOutput(ns("icf_template_dl"))
          ),

          # ---- TAB 2: Recoleccion -------------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("vial"), " Recoleccion"),
            shiny::p(class = "text-muted small",
              "Genere BIOIDs trazables y etiquetas Code128 imprimibles. ",
              "Requiere un consentimiento vigente seleccionado."),

            shiny::fluidRow(
              shiny::column(6,
                shinyWidgets::pickerInput(ns("r_consent_id"),
                  "Consentimiento aplicable",
                  choices = character(0))),
              shiny::column(3,
                shinyWidgets::pickerInput(ns("r_sample_type"),
                  "Tipo de muestra",
                  choices = biobank_sample_types(),
                  selected = "TUM",
                  options = list(`live-search` = TRUE))),
              shiny::column(3,
                shiny::numericInput(ns("r_n_aliquots"),
                  "N. de aliquotas", value = 1, min = 1, max = 50))
            ),

            shiny::fluidRow(
              shiny::column(6,
                shinyWidgets::pickerInput(ns("r_anatomic_site"),
                  "Sitio anatomico (ICD-O-3 topografia)",
                  choices = c("(seleccione)" = "", lookup_sites()),
                  selected = "",
                  options = list(`live-search` = TRUE,
                                 `actions-box` = FALSE,
                                 size = 12))),
              shiny::column(3,
                shinyWidgets::pickerInput(ns("r_preservation"),
                  "Preservacion",
                  choices = c("(seleccione)" = "",
                              "FFPE"        = "FFPE",
                              "Snap frozen" = "SNAP_FROZEN",
                              "OCT"         = "OCT",
                              "RNA later"   = "RNA_LATER",
                              "Viable"      = "VIABLE",
                              "Otro fijado" = "FIXED_OTHER"),
                  selected = "")),
              shiny::column(3,
                shinyWidgets::pickerInput(ns("r_storage_temp"),
                  "Temperatura",
                  choices = c("(seleccione)"="",
                              "RT","+4","-20","-80","-150","-196"),
                  selected = ""))
            ),

            shiny::fluidRow(
              shiny::column(6,
                shiny::dateInput(ns("r_collection_dt"),
                  "Fecha de colecta", value = Sys.Date())),
              shiny::column(6,
                shiny::textInput(ns("r_location"),
                  "Ubicacion (freezer / rack / box / pos)",
                  value = "", placeholder = "F1 / R3 / B07 / A4"))
            ),

            shiny::textAreaInput(ns("r_notes"), "Notas (opcional)",
                                 rows = 2, width = "100%"),

            shiny::div(class = "alert alert-secondary small",
              shiny::icon("circle-info"),
              " Protocolo IRB: ",
              shiny::strong("TDM-CEI-2026-V1"),
              ". El BIOID se genera automaticamente al registrar."),

            shiny::actionButton(ns("r_save"),
              shiny::tagList(shiny::icon("paper-plane"),
                             " Registrar y generar etiquetas"),
              class = "btn-success btn-lg"),
            shiny::div(style = "color:#c00", shiny::textOutput(ns("r_err"))),
            shiny::uiOutput(ns("r_label_dl"))
          ),

          # ---- TAB 3: Inventario --------------------------------------------
          shiny::tabPanel(
            title = shiny::tagList(shiny::icon("boxes-stacked"),
                                   " Inventario"),
            shiny::p(class = "text-muted small",
              "Muestras existentes para el sujeto seleccionado, ",
              "con su cadena de custodia."),
            DT::DTOutput(ns("specimens_tbl")),
            shiny::hr(),
            shiny::h6(shiny::icon("link"), " Cadena de custodia"),
            DT::DTOutput(ns("custody_tbl"))
          )
        )
      )
    )
  )
}

mod_biobank_server <- function(id, pool, user, prefill = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Search ------------------------------------------------------------
    selected_mrn <- shiny::reactiveVal(NULL)
    refresh_tick <- shiny::reactiveVal(0L)
    bump <- function() refresh_tick(shiny::isolate(refresh_tick()) + 1L)

    q_debounced <- shiny::debounce(shiny::reactive({ input$q %||% "" }), 300)

    results <- shiny::reactive({
      u <- user(); q <- q_debounced()
      if (is.null(u) || nchar(q) < 2)
        return(data.frame(mrn = character(0), display = character(0)))
      tryCatch(search_patients(pool, u, q),
               error = function(e) {
                 message("[biobank] search err: ", conditionMessage(e))
                 data.frame(mrn = character(0), display = character(0))
               })
    })

    output$results_tbl <- DT::renderDT({
      r <- results()
      if (nrow(r) == 0) {
        msg <- if (nchar(q_debounced()) < 2) "Escriba >= 2 caracteres."
               else "Sin resultados."
        return(DT::datatable(
          data.frame(Paciente = msg),
          rownames = FALSE, selection = "none", colnames = "",
          options = list(dom = "t", paging = FALSE, ordering = FALSE),
          style = "bootstrap4"))
      }
      DT::datatable(
        data.frame(Paciente = r$display),
        rownames = FALSE, selection = "single", colnames = "",
        options = list(dom = "t", paging = FALSE, ordering = FALSE,
                       columnDefs = list(list(targets = 0,
                                              className = "small"))),
        style = "bootstrap4"
      )
    })

    shiny::observeEvent(input$results_tbl_rows_selected, {
      r <- results(); sel <- input$results_tbl_rows_selected
      if (length(sel) == 0L || nrow(r) == 0) return()
      selected_mrn(r$mrn[sel])
    })

    # Deeplink prefill
    if (!is.null(prefill)) {
      shiny::observeEvent(prefill(), {
        u <- user(); p <- prefill()
        if (is.null(u) || is.null(p) || !nzchar(p$mrn %||% "")) return()
        shiny::updateTextInput(session, "q", value = p$mrn)
        selected_mrn(p$mrn)
      }, ignoreInit = TRUE)
    }

    # ---- Resolve patient + biobank subject ---------------------------------
    patient <- shiny::reactive({
      mrn <- selected_mrn(); u <- user()
      if (is.null(u) || is.null(mrn) || !nzchar(mrn)) return(NULL)
      tryCatch(load_patient(pool, u, mrn),
               error = function(e) NULL)
    })

    bio_subject <- shiny::reactive({
      refresh_tick()
      p <- patient(); u <- user()
      if (is.null(p) || is.null(u)) return(NULL)
      hosp <- resolve_hospital_code(pool, p$hospital_id)
      bsid_full <- tryCatch(
        biobank_subject_upsert(pool, p$mrn, hosp, u$email %||% "system"),
        error = function(e) {
          message("[biobank] subject upsert err: ", conditionMessage(e))
          NULL
        })
      if (is.null(bsid_full)) return(NULL)
      list(hospital_code = hosp,
           bio_subject_id = bsid_full,
           subj_short = sub(".*-", "", bsid_full))
    })

    # ---- Subject identity card --------------------------------------------
    output$subject_card <- shiny::renderUI({
      p <- patient(); bs <- bio_subject()
      if (is.null(p)) return(shiny::p(shiny::em("Seleccione un paciente.")))
      if (is.null(bs)) return(shiny::div(class = "alert alert-warning small",
        shiny::icon("triangle-exclamation"),
        " No se pudo generar el sujeto del biobanco. ",
        "Verifique las variables KREBS_BIOBANK_HMAC_SALT y ",
        "KREBS_BIOBANK_ENC_KEY."))
      shiny::tagList(
        shiny::h5(shiny::strong(p$nombre)),
        shiny::tags$dl(class = "row",
          shiny::tags$dt(class = "col-5", "MRN"),
          shiny::tags$dd(class = "col-7", p$mrn),
          shiny::tags$dt(class = "col-5", "Hospital"),
          shiny::tags$dd(class = "col-7", bs$hospital_code),
          shiny::tags$dt(class = "col-5", "BIO Subject ID"),
          shiny::tags$dd(class = "col-7",
            shiny::tags$code(bs$bio_subject_id)),
          shiny::tags$dt(class = "col-5", "Sexo / Edad"),
          shiny::tags$dd(class = "col-7",
            sprintf("%s / %d", p$sexo %||% "--", p$edad %||% NA_integer_)),
          shiny::tags$dt(class = "col-5", "Tipo cancer"),
          shiny::tags$dd(class = "col-7", p$tipo_cancer %||% "--")
        ),
        shiny::div(class = "small text-muted",
          shiny::icon("shield-halved"),
          " Identidad pseudonimizada via HMAC-SHA256. ",
          "El mapping MRN<->BIOID esta cifrado en la base de datos.")
      )
    })

    # ---- Consents ---------------------------------------------------------
    consents <- shiny::reactive({
      refresh_tick()
      bs <- bio_subject(); if (is.null(bs)) return(NULL)
      tryCatch(DBI::dbGetQuery(pool,
        "SELECT id, template_version, signed_dt,
                permits_genomics, permits_recontact,
                withdrawn_dt, irb_protocol
           FROM biobank_consents
          WHERE bio_subject_id = $1
          ORDER BY signed_dt DESC",
        params = list(bs$bio_subject_id)),
        error = function(e) NULL)
    })

    output$consents_tbl <- DT::renderDT({
      cs <- consents()
      if (is.null(cs) || nrow(cs) == 0) {
        return(DT::datatable(
          data.frame(Mensaje = "Sin consentimientos registrados."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          style = "bootstrap4"))
      }
      cs$Estado <- ifelse(is.na(cs$withdrawn_dt), "Vigente", "Retirado")
      DT::datatable(
        cs[, c("template_version","signed_dt","Estado",
               "permits_genomics","permits_recontact","irb_protocol")],
        rownames = FALSE, selection = "none",
        colnames = c("Plantilla","Firmado","Estado",
                     "Genomica","Re-contacto","IRB"),
        options = list(pageLength = 5),
        style = "bootstrap4")
    })

    # Refresh consent dropdown in Recoleccion tab
    shiny::observe({
      cs <- consents()
      if (is.null(cs) || nrow(cs) == 0) {
        shinyWidgets::updatePickerInput(session, "r_consent_id",
          choices = c("(no hay consentimientos vigentes)" = ""))
        return()
      }
      vigentes <- cs[is.na(cs$withdrawn_dt), , drop = FALSE]
      if (nrow(vigentes) == 0L) {
        shinyWidgets::updatePickerInput(session, "r_consent_id",
          choices = c("(no hay consentimientos vigentes)" = ""))
        return()
      }
      ch <- stats::setNames(vigentes$id,
        sprintf("%s (%s)", vigentes$template_version, vigentes$signed_dt))
      shinyWidgets::updatePickerInput(session, "r_consent_id", choices = ch)
    })

    # Save new consent
    shiny::observeEvent(input$c_save, {
      bs <- bio_subject(); u <- user()
      if (is.null(bs) || is.null(u)) {
        shiny::showNotification("Seleccione un paciente primero.",
                                type = "warning"); return()
      }
      tmpl <- nz(input$c_template) %||% "ICF-Krebs-v1.0-2026"
      tryCatch({
        new_id <- DBI::dbGetQuery(pool,
          "INSERT INTO biobank_consents
             (bio_subject_id, hospital_id, template_version, scope,
              permits_genomics, permits_recontact,
              signed_dt, irb_protocol, created_by)
           VALUES ($1, $2, $3, 'broad', $4, $5, $6,
                   'TDM-CEI-2026-V1', $7)
           RETURNING id",
          params = list(bs$bio_subject_id, bs$hospital_code, tmpl,
                        isTRUE(input$c_genomics),
                        isTRUE(input$c_recontact),
                        as.character(input$c_signed_dt %||% Sys.Date()),
                        u$email %||% "system"))[[1]]
        DBI::dbExecute(pool,
          "UPDATE biobank_subjects SET consent_id = $1
            WHERE bio_subject_id = $2 AND consent_id IS NULL",
          params = list(new_id, bs$bio_subject_id))
        shiny::showNotification(
          sprintf("Consentimiento %s registrado para %s.",
                  tmpl, bs$bio_subject_id),
          type = "message", duration = 6)
        # reset form fields so el siguiente registro empieza limpio
        shiny::updateTextInput(session, "c_template",
                               value = "ICF-Krebs-v1.0-2026")
        shiny::updateDateInput(session, "c_signed_dt", value = Sys.Date())
        shinyWidgets::updateAwesomeCheckbox(session, "c_genomics",
                                            value = TRUE)
        shinyWidgets::updateAwesomeCheckbox(session, "c_recontact",
                                            value = FALSE)
        shiny::updateTextAreaInput(session, "c_notes", value = "")
        bump()
      }, error = function(e) {
        shiny::showNotification(paste("Error:", conditionMessage(e)),
                                type = "error", duration = 10)
      })
    })

    # ICF template download (Word machote pre-filled)
    output$icf_template_dl <- shiny::renderUI({
      p <- patient(); if (is.null(p)) return(NULL)
      shiny::downloadButton(ns("icf_dl"),
        " Descargar machote ICF (DOCX)",
        icon = shiny::icon("file-word"),
        class = "btn-primary")
    })

    output$icf_dl <- shiny::downloadHandler(
      filename = function() {
        p <- patient()
        sprintf("ICF_Krebs_%s_%s.docx",
                p$mrn %||% "PACIENTE",
                format(Sys.Date(), "%Y%m%d"))
      },
      content = function(file) {
        p <- patient(); bs <- bio_subject()
        ok <- tryCatch({
          biobank_icf_docx(file, patient = p, bio_subject = bs); TRUE
        }, error = function(e) {
          message("[biobank] icf_docx err: ", conditionMessage(e))
          FALSE
        })
        if (!isTRUE(ok)) {
          # ultimo recurso: escribimos un .txt para que el download nunca
          # rompa con "Site wasn't available" en el navegador.
          .biobank_icf_txt(file, patient = p, bio_subject = bs)
        }
      }
    )

    # ---- Recoleccion ------------------------------------------------------
    r_err_rv <- shiny::reactiveVal("")
    output$r_err <- shiny::renderText(r_err_rv())
    last_label <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$r_save, {
      r_err_rv("")
      bs <- bio_subject(); u <- user(); p <- patient()
      if (is.null(bs) || is.null(u) || is.null(p)) {
        r_err_rv("Seleccione un paciente primero."); return()
      }
      if (!nzchar(input$r_consent_id %||% "")) {
        r_err_rv("Debe seleccionar un consentimiento vigente."); return()
      }
      shiny::req(input$r_sample_type, input$r_n_aliquots,
                 input$r_collection_dt)

      tryCatch({
        col_n <- bioid_next_collection(pool, bs$bio_subject_id,
                                       input$r_sample_type)
        bioids <- character(0)
        for (i in seq_len(as.integer(input$r_n_aliquots))) {
          bioid <- bioid_compose(bs$hospital_code, bs$subj_short,
                                 col_n, input$r_sample_type, i)
          DBI::dbExecute(pool,
            "INSERT INTO biobank_specimens
               (bioid, bio_subject_id, hospital_id, encounter_id,
                collection_dt, anatomic_site, sample_type,
                preservation, storage_temp,
                freezer, rack, box, position,
                status, notes, created_by)
             VALUES ($1, $2, $3, NULL, $4, $5, $6, $7, $8,
                     $9, $10, $11, $12, 'AVAILABLE', $13, $14)",
            params = list(bioid, bs$bio_subject_id, bs$hospital_code,
                          as.POSIXct(input$r_collection_dt),
                          site_label_to_topo(nz(input$r_anatomic_site)) %||%
                            nz(input$r_anatomic_site),
                          toupper(input$r_sample_type),
                          nz(input$r_preservation),
                          nz(input$r_storage_temp),
                          .loc_part(input$r_location, 1),
                          .loc_part(input$r_location, 2),
                          .loc_part(input$r_location, 3),
                          .loc_part(input$r_location, 4),
                          nz(input$r_notes),
                          u$email %||% "system"))

          DBI::dbExecute(pool,
            "INSERT INTO biobank_chain_of_custody
               (bioid, hospital_id, event, actor, to_location, notes)
             VALUES ($1, $2, 'COLLECTED', $3, $4, $5)",
            params = list(bioid, bs$hospital_code, u$email %||% "system",
                          nz(input$r_location), nz(input$r_notes)))

          bioids <- c(bioids, bioid)
        }

        # Generate label PDF for the first BIOID
        pdf_path <- tryCatch(
          biobank_label_pdf(bioids[1], input$r_sample_type,
                            input$r_collection_dt, bs$hospital_code),
          error = function(e) NULL)
        last_label(list(path = pdf_path,
                        bioids = bioids,
                        first = bioids[1]))

        shiny::showNotification(
          sprintf("Registradas %d aliquotas: %s ... %s",
                  length(bioids), bioids[1], bioids[length(bioids)]),
          type = "message", duration = 8)
        bump()
      }, error = function(e) {
        r_err_rv(paste("Error:", conditionMessage(e)))
        shiny::showNotification(paste("Error:", conditionMessage(e)),
                                type = "error", duration = 10)
      })
    })

    output$r_label_dl <- shiny::renderUI({
      l <- last_label(); if (is.null(l) || is.null(l$path)) return(NULL)
      shiny::tagList(
        shiny::hr(),
        shiny::div(class = "alert alert-success small",
          shiny::icon("circle-check"),
          " BIOIDs registrados: ",
          shiny::HTML(paste(sprintf(
            "<span class='badge badge-info'>%s</span>", l$bioids),
            collapse = " "))),
        shiny::downloadButton(ns("label_pdf_dl"),
          sprintf(" Descargar etiqueta (%s)", l$first),
          icon = shiny::icon("print"),
          class = "btn-success")
      )
    })

    output$label_pdf_dl <- shiny::downloadHandler(
      filename = function() {
        l <- last_label()
        sprintf("etiqueta_%s.pdf", l$first %||% "muestra")
      },
      content = function(file) {
        l <- last_label()
        if (!is.null(l$path) && file.exists(l$path))
          file.copy(l$path, file, overwrite = TRUE)
      }
    )

    # ---- Inventario --------------------------------------------------------
    specimens <- shiny::reactive({
      refresh_tick()
      bs <- bio_subject(); if (is.null(bs)) return(NULL)
      tryCatch(DBI::dbGetQuery(pool,
        "SELECT bioid, sample_type, anatomic_site, preservation,
                storage_temp,
                COALESCE(freezer,'')||'/'||COALESCE(rack,'')||'/'||
                COALESCE(box,'')||'/'||COALESCE(position,'') AS location,
                to_char(collection_dt,'YYYY-MM-DD') AS collected,
                status
           FROM biobank_specimens
          WHERE bio_subject_id = $1
          ORDER BY collection_dt DESC",
        params = list(bs$bio_subject_id)),
        error = function(e) NULL)
    })

    output$specimens_tbl <- DT::renderDT({
      sp <- specimens()
      if (is.null(sp) || nrow(sp) == 0) {
        return(DT::datatable(
          data.frame(Mensaje = "Sin muestras registradas para este sujeto."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          style = "bootstrap4"))
      }
      DT::datatable(
        sp,
        rownames = FALSE, selection = "single",
        colnames = c("BIOID","Tipo","Sitio","Preserv.","Temp",
                     "Ubicacion","Colectado","Estado"),
        options = list(pageLength = 8, scrollX = TRUE),
        style = "bootstrap4")
    })

    custody <- shiny::reactive({
      refresh_tick()
      sp <- specimens(); sel <- input$specimens_tbl_rows_selected
      if (is.null(sp) || nrow(sp) == 0 || length(sel) == 0L) return(NULL)
      bioid_pick <- sp$bioid[sel]
      tryCatch(DBI::dbGetQuery(pool,
        "SELECT to_char(event_at,'YYYY-MM-DD HH24:MI') AS event_at,
                event, actor, from_location, to_location, notes
           FROM biobank_chain_of_custody
          WHERE bioid = $1
          ORDER BY event_at",
        params = list(bioid_pick)),
        error = function(e) NULL)
    })

    output$custody_tbl <- DT::renderDT({
      ch <- custody()
      if (is.null(ch) || nrow(ch) == 0) {
        return(DT::datatable(
          data.frame(Mensaje = "Seleccione una muestra para ver su cadena de custodia."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          style = "bootstrap4"))
      }
      DT::datatable(ch, rownames = FALSE,
        colnames = c("Cuando","Evento","Actor","Desde","Hasta","Notas"),
        options = list(pageLength = 5),
        style = "bootstrap4")
    })
  })
}

# ---- helpers ---------------------------------------------------------------

# Parser muy simple de "F1 / R3 / B07 / A4" -> componente n
# (duplicado de mod_biobank_request.R; movido aqui en caso de que el
# request module se elimine en el futuro).
if (!exists(".loc_part")) {
  .loc_part <- function(s, n) {
    if (is.null(s) || !nzchar(s)) return(NA_character_)
    parts <- trimws(strsplit(s, "/", fixed = TRUE)[[1]])
    if (n > length(parts)) NA_character_ else parts[n]
  }
}
