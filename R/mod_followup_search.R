#' Tab 2 (NEW): patient search + add follow-up encounter.
#'
#' UX flow:
#'   1. Search by MRN or name (server-side fuzzy)
#'   2. Pick a patient -> right pane loads identity + history timeline
#'   3. Choose encounter type (recurrence/treatment/followup/death)
#'   4. Fill the shared encounter form -> insert -> timeline refreshes

mod_followup_search_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("clock-rotate-left"), " Seguimiento de paciente"),
    shiny::p("Busque por MRN o nombre, revise el historial, y agregue un nuevo evento clinico.",
             class = "text-muted"),

    shiny::fluidRow(
      shiny::column(4,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("magnifying-glass"), " Buscar paciente"),
          width = 12, status = "primary", solidHeader = TRUE,
          shiny::textInput(ns("q"), NULL,
            placeholder = "MRN o nombre (>= 2 caracteres)"),
          shiny::div(style = "max-height:240px;overflow:auto;",
            DT::DTOutput(ns("results_tbl"))),
          shiny::uiOutput(ns("patient_card"))
        )
      ),
      shiny::column(8,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("timeline"), " Historial"),
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          shinycssloaders::withSpinner(timevis::timevisOutput(ns("timeline"), height = "240px"),
                                       type = 5, color = "midnightblue", size = 0.5),
          shiny::br(),
          DT::DTOutput(ns("encounters_tbl")),
          shiny::br(),
          shiny::h5(shiny::icon("paperclip"), " Documentos adjuntos"),
          DT::DTOutput(ns("attachments_tbl")),
          shiny::uiOutput(ns("download_link"))
        ),

        shiny::conditionalPanel(
          condition = sprintf("output['%s']", ns("has_patient")),
          mod_encounter_form_ui(ns("enc"),
            allowed_types = c("recurrence","treatment","followup","death")),
          shiny::div(style = "color:#c00", shiny::textOutput(ns("submit_err"))),
          shiny::div(class = "krebs-sticky-submit",
            shiny::actionButton(ns("submit"),
              shiny::tagList(shiny::icon("paper-plane"), " Agregar evento"),
              class = "btn-success btn-lg krebs-submit-btn"),
            shiny::span(id = ns("submit_msg"), style = "display:none;",
                        shiny::icon("spinner", class = "fa-spin"), " Procesando..."),
            shiny::span(class = "kbd-hint",
              shiny::HTML("Atajo: <kbd>Ctrl</kbd>+<kbd>S</kbd>"))
          )
        )
      )
    )
  )
}

mod_followup_search_server <- function(id, pool, user, data_changed = NULL,
                                       prefill = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Search: textInput -> debounced DB query -> DT results ------------
    selected_mrn <- shiny::reactiveVal(NULL)

    q_debounced <- shiny::debounce(shiny::reactive({ input$q %||% "" }), 300)

    results <- shiny::reactive({
      u <- user(); q <- q_debounced()
      if (is.null(u) || nchar(q) < 2)
        return(data.frame(mrn=character(0), display=character(0)))
      tryCatch(search_patients(pool, u, q),
               error = function(e) {
                 message("[followup] search err: ", conditionMessage(e))
                 data.frame(mrn=character(0), display=character(0))
               })
    })

    output$results_tbl <- DT::renderDT({
      r <- results()
      if (nrow(r) == 0) {
        msg <- if (nchar(q_debounced()) < 2) "Escriba >= 2 caracteres."
               else "Sin resultados."
        return(DT::datatable(
          data.frame(Resultados = msg),
          rownames = FALSE, selection = "none",
          options = list(dom = "t", paging = FALSE, ordering = FALSE),
          style = "bootstrap4"))
      }
      DT::datatable(
        data.frame(Paciente = r$display),
        rownames = FALSE, selection = "single",
        colnames = "",
        options = list(dom = "t", paging = FALSE, ordering = FALSE,
                       columnDefs = list(list(targets = 0, className = "small"))),
        style = "bootstrap4"
      )
    })

    shiny::observeEvent(input$results_tbl_rows_selected, {
      r <- results(); sel <- input$results_tbl_rows_selected
      if (length(sel) == 0L || nrow(r) == 0) return()
      selected_mrn(r$mrn[sel])
    })

    # ---- Deeplink prefill: navbar quick-search + Mis pendientes ----------
    if (!is.null(prefill)) {
      shiny::observeEvent(prefill(), {
        u <- user(); p <- prefill()
        if (is.null(u) || is.null(p) || !nzchar(p$mrn %||% "")) return()
        shiny::updateTextInput(session, "q", value = p$mrn)
        selected_mrn(p$mrn)
      }, ignoreInit = TRUE)
    }

    # ---- Selected patient --------------------------------------------------
    patient <- shiny::reactive({
      mrn <- selected_mrn()
      u   <- user()
      if (is.null(u) || is.null(mrn) || !nzchar(mrn)) return(NULL)
      tryCatch(load_patient(pool, u, mrn),
               error = function(e) {
                 message("[followup] load_patient err: ", conditionMessage(e))
                 NULL
               })
    })

    output$has_patient <- shiny::reactive(!is.null(patient()))
    shiny::outputOptions(output, "has_patient", suspendWhenHidden = FALSE)

    # ---- Identity card -----------------------------------------------------
    output$patient_card <- shiny::renderUI({
      p <- patient(); if (is.null(p)) return(shiny::p(shiny::em("Seleccione un paciente.")))
      shiny::tagList(
        shiny::hr(),
        shiny::h5(shiny::icon("user"), " ", shiny::strong(p$nombre)),
        shiny::tags$dl(class = "row",
          shiny::tags$dt(class = "col-5", "MRN"),         shiny::tags$dd(class = "col-7", p$mrn),
          shiny::tags$dt(class = "col-5", "Sexo"),        shiny::tags$dd(class = "col-7", p$sexo),
          shiny::tags$dt(class = "col-5", "Edad"),        shiny::tags$dd(class = "col-7",
                                                            sprintf("%d", p$edad)),
          shiny::tags$dt(class = "col-5", "Fecha dx"),    shiny::tags$dd(class = "col-7",
                                                            as.character(p$fecha_dx %||% "--")),
          shiny::tags$dt(class = "col-5", "Tipo cancer"), shiny::tags$dd(class = "col-7",
                                                            p$tipo_cancer %||% "--"),
          shiny::tags$dt(class = "col-5", "Estado vital"),shiny::tags$dd(class = "col-7",
            shiny::tags$span(class = vital_class(p$vital_status),
                             p$vital_status %||% "desconocido")),
          shiny::tags$dt(class = "col-5", "Encuentros"),  shiny::tags$dd(class = "col-7", p$n_encuentros),
          shiny::tags$dt(class = "col-5", "Recurrencias"),shiny::tags$dd(class = "col-7", p$n_recurrencias)
        )
      )
    })

    # ---- Timeline ----------------------------------------------------------
    encounters <- shiny::reactive({
      p <- patient(); u <- user()
      if (is.null(p) || is.null(u)) return(NULL)
      load_encounters(pool, u, p$mrn)
    })

    output$timeline <- timevis::renderTimevis({
      e <- encounters(); if (is.null(e) || nrow(e) == 0) {
        return(timevis::timevis(
          data.frame(id=integer(0), content=character(0), start=character(0))
        ))
      }
      timevis::timevis(
        data.frame(
          id      = seq_len(nrow(e)),
          content = paste0("<b>", e$encounter_type, "</b>"),
          start   = as.character(e$encounter_date),
          group   = NA,
          style   = enc_style(e$encounter_type)
        ),
        options = list(stack = TRUE, zoomable = TRUE, height = "200px")
      )
    })

    output$encounters_tbl <- DT::renderDT({
      e <- encounters(); if (is.null(e) || nrow(e) == 0) return(NULL)
      cols_show <- intersect(
        c("encounter_date","encounter_type","line","treatment_intent",
          "tnm_t","tnm_n","tnm_m","primary_site","oncotree","chemo","radio",
          "complication","vital_status","notes"),
        names(e))
      DT::datatable(
        e[, cols_show, drop = FALSE],
        rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE),
        style = "bootstrap4"
      )
    })

    # ---- Attachments list (no blob bytes -- view excludes content) ---------
    attachments <- shiny::reactive({
      if (!is.null(data_changed)) data_changed()
      p <- patient(); u <- user()
      if (is.null(p) || is.null(u)) return(NULL)
      tryCatch(db_read(pool, u, "
        SELECT attachment_id, filename, mime, size_bytes,
               to_char(uploaded_at,'YYYY-MM-DD HH24:MI') AS uploaded_at
          FROM v_encounter_attachments_meta
         WHERE hospital_id=$1 AND mrn=$2
         ORDER BY uploaded_at DESC
         LIMIT 100",
        params = list(u$hospital_id, p$mrn)),
        error = function(e) NULL)
    })

    output$attachments_tbl <- DT::renderDT({
      a <- attachments(); if (is.null(a) || nrow(a) == 0) {
        return(DT::datatable(
          data.frame(Mensaje = "Sin documentos adjuntos."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE),
          style = "bootstrap4"))
      }
      a$size <- sprintf("%.1f KB", a$size_bytes / 1024)
      DT::datatable(
        a[, c("filename","mime","size","uploaded_at")],
        rownames = FALSE, selection = "single",
        colnames = c("Archivo","Tipo","Tamano","Subido"),
        options = list(pageLength = 5, dom = "tip"),
        style = "bootstrap4"
      )
    })

    # Single-row selection -> render a downloadHandler link.
    output$download_link <- shiny::renderUI({
      a <- attachments(); sel <- input$attachments_tbl_rows_selected
      if (is.null(a) || is.null(sel) || length(sel) == 0L) return(NULL)
      shiny::downloadLink(session$ns("dl_attachment"),
        label = shiny::tagList(shiny::icon("download"), " Descargar ",
                               shiny::strong(a$filename[sel])),
        class = "btn btn-sm btn-outline-primary mt-2")
    })

    output$dl_attachment <- shiny::downloadHandler(
      filename = function() {
        a <- attachments(); sel <- input$attachments_tbl_rows_selected
        if (is.null(a) || is.null(sel)) return("attachment.bin")
        a$filename[sel]
      },
      content = function(file) {
        a <- attachments(); sel <- input$attachments_tbl_rows_selected
        u <- user()
        if (is.null(a) || is.null(sel) || is.null(u)) return()
        rid <- a$attachment_id[sel]
        row <- db_read(pool, u,
          "SELECT content FROM encounter_attachments WHERE attachment_id=$1",
          params = list(rid))
        if (nrow(row) == 0) return()
        writeBin(row$content[[1]], file)
        audit_event(pool, u, "EXPORT",
                    target_table = "encounter_attachments",
                    target_id    = as.character(rid))
      }
    )

    # ---- Encounter form & submit -------------------------------------------
    enc <- mod_encounter_form_server("enc",
      patient = patient, pool = pool, user = user)

    err_rv <- shiny::reactiveVal("")
    output$submit_err <- shiny::renderText(err_rv())

    shiny::observe({
      # Validator is checked at submit time via enc$values(); gate only on
      # whether a patient is loaded. Avoids the "permanently locked" bug.
      shinyjs::toggleState("submit", condition = !is.null(patient()))
    })

    shiny::observeEvent(input$submit, {
      u <- user(); p <- patient()
      if (is.null(u) || is.null(p)) return()
      vals <- enc$values()
      if (is.null(vals)) { err_rv("Revise los campos del evento."); return() }
      vals$mrn         <- p$mrn
      vals$hospital_id <- u$hospital_id

      shinyjs::disable("submit"); shinyjs::show("submit_msg")
      on.exit({ shinyjs::enable("submit"); shinyjs::hide("submit_msg") }, add = TRUE)

      new_enc_id <- NULL
      tryCatch({
        with_tenant(pool, u, function(con) {
          new_enc_id <<- insert_encounter(con, u, vals)
        })
        # Attach files post-commit (a bad blob shouldn't kill the encounter).
        n_files <- tryCatch(
          enc$consume_files(new_enc_id, u$hospital_id, p$mrn),
          error = function(e) { message("[followup] attach: ", conditionMessage(e)); 0L })
        if (isTRUE(n_files > 0)) {
          shiny::showNotification(
            sprintf("%d archivo(s) adjuntados.", n_files),
            type = "message", duration = 3)
        }
        err_rv("")
        # Drop the autosaved draft and bump the user's recents cache.
        try(enc$on_submit_success(vals), silent = TRUE)
        if (!is.null(data_changed)) {
          data_changed(shiny::isolate(data_changed()) + 1L)
        }
        shiny::showNotification(
          sprintf("Evento agregado al expediente %s.", p$mrn),
          type = "message", duration = 4)
        # force refresh of timeline & history
        selected_mrn(p$mrn)
        enc$reset()
      }, error = function(e) {
        msg <- paste("Error:", conditionMessage(e))
        message("[followup] ", msg)
        err_rv(msg)
        shiny::showNotification(msg, type = "error", duration = 8)
      })
    })
  })
}

# ---- helpers ---------------------------------------------------------------

#' Search patients. Hospital scoping is handled by RLS (super_admin sees
#' all, regular users see only their hospital), so we don't filter on
#' hospital_id here -- doing so would break super_admin (NULL hospital_id).
#' Tries trigram similarity first (pg_trgm) and falls back to plain ILIKE
#' if the extension is missing.
search_patients <- function(pool, user, q) {
  q_low  <- tolower(q)
  q_like <- paste0("%", gsub("([%_])", "\\\\\\1", q_low), "%")

  rows <- tryCatch(
    db_read(pool, user, "
      SELECT mrn, nombre,
             to_char(fecha_nac,'YYYY-MM-DD') AS dob,
             sexo,
             similarity(lower(nombre), $1) AS sim
        FROM patient_identifiers
       WHERE mrn ILIKE $2 OR lower(nombre) % $1 OR lower(nombre) ILIKE $2
       ORDER BY (mrn = $3) DESC, sim DESC NULLS LAST
       LIMIT 25
    ", params = list(q_low, q_like, q)),
    error = function(e) {
      message("[search_patients] trigram fallback: ", conditionMessage(e))
      db_read(pool, user, "
        SELECT mrn, nombre,
               to_char(fecha_nac,'YYYY-MM-DD') AS dob,
               sexo
          FROM patient_identifiers
         WHERE mrn ILIKE $1 OR lower(nombre) ILIKE $1
         ORDER BY (mrn = $2) DESC, mrn ASC
         LIMIT 25
      ", params = list(q_like, q))
    })

  if (is.null(rows) || nrow(rows) == 0)
    return(data.frame(mrn = character(0), display = character(0)))
  rows$display <- sprintf("%s -- %s (%s, %s)",
                          rows$mrn, rows$nombre, rows$dob, rows$sexo)
  rows[, c("mrn","display")]
}

load_patient <- function(pool, user, mrn) {
  rows <- db_read(pool, user, "
    SELECT * FROM v_patient_summary
     WHERE hospital_id = $1 AND mrn = $2
  ", params = list(user$hospital_id, mrn))
  if (nrow(rows) == 0) return(NULL)
  p <- as.list(rows[1, ])
  p$nombre <- db_read(pool, user, "
    SELECT nombre FROM patient_identifiers
     WHERE hospital_id = $1 AND mrn = $2", params = list(user$hospital_id, mrn))$nombre
  p$min_event_date <- db_read(pool, user, "
    SELECT MIN(encounter_date) AS d FROM encounters
     WHERE hospital_id = $1 AND mrn = $2", params = list(user$hospital_id, mrn))$d
  p
}

load_encounters <- function(pool, user, mrn) {
  db_read(pool, user, "
    SELECT encounter_date, encounter_type, tnm_t, tnm_n, tnm_m,
           primary_site, oncotree, chemo, radio, complication,
           vital_status, notes
      FROM encounters
     WHERE hospital_id = $1 AND mrn = $2
     ORDER BY encounter_date ASC, created_at ASC
  ", params = list(user$hospital_id, mrn))
}

enc_style <- function(types) {
  pal <- c(initial_dx="background:#1f78b4;color:white;",
           recurrence="background:#e31a1c;color:white;",
           treatment ="background:#33a02c;color:white;",
           followup  ="background:#ff7f00;color:white;",
           death     ="background:#000000;color:white;")
  unname(pal[types])
}

vital_class <- function(s) {
  switch(s %||% "",
         vivo    = "badge bg-success",
         muerto  = "badge bg-dark",
         perdido = "badge bg-warning",
         "badge bg-secondary")
}
