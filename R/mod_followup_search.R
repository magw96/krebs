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
          shiny::selectizeInput(ns("search"), NULL, choices = NULL,
            options = list(
              placeholder = "MRN o nombre (>= 2 caracteres)",
              loadThrottle = 300,
              maxOptions = 25,
              create = FALSE
            )
          ),
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
          DT::DTOutput(ns("encounters_tbl"))
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

mod_followup_search_server <- function(id, pool, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Server-side search ------------------------------------------------
    shiny::observe({
      u <- user(); if (is.null(u)) return()
      shiny::updateSelectizeInput(session, "search",
        choices = NULL, server = TRUE,
        options = list(
          placeholder = "MRN o nombre (>= 2 caracteres)",
          loadThrottle = 300,
          load = I(""),  # we drive options below
          maxOptions = 25
        ))
    })

    # Debounced query each time the user types.
    shiny::observeEvent(input$search_search, {
      u <- user(); if (is.null(u)) return()
      q <- input$search_search %||% ""
      if (nchar(q) < 2) return()
      hits <- search_patients(pool, u, q)
      shiny::updateSelectizeInput(session, "search",
        choices  = stats::setNames(hits$mrn, hits$display),
        selected = input$search,
        server   = TRUE)
    }, ignoreInit = TRUE)

    # ---- Selected patient --------------------------------------------------
    patient <- shiny::reactive({
      mrn <- input$search
      u   <- user()
      if (is.null(u) || is.null(mrn) || !nzchar(mrn)) return(NULL)
      load_patient(pool, u, mrn)
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
      DT::datatable(
        e[, c("encounter_date","encounter_type","tnm_t","tnm_n","tnm_m",
              "primary_site","oncotree","chemo","radio","complication",
              "vital_status","notes")],
        rownames = FALSE, options = list(pageLength = 5, scrollX = TRUE),
        style = "bootstrap4"
      )
    })

    # ---- Encounter form & submit -------------------------------------------
    enc <- mod_encounter_form_server("enc",
      patient = patient, pool = pool, user = user)

    err_rv <- shiny::reactiveVal("")
    output$submit_err <- shiny::renderText(err_rv())

    shiny::observe({
      shinyjs::toggleState("submit",
        condition = !is.null(patient()) && isTRUE(enc$iv$is_valid()))
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

      tryCatch({
        with_tenant(pool, u, function(con) insert_encounter(con, u, vals))
        err_rv("")
        # Drop the autosaved draft and bump the user's recents cache.
        try(enc$on_submit_success(vals), silent = TRUE)
        shiny::showNotification("Evento agregado.", type = "message", duration = 3)
        # force refresh of timeline & history
        shiny::updateSelectizeInput(session, "search", selected = p$mrn)
        enc$reset()
      }, error = function(e) {
        err_rv(paste("Error:", conditionMessage(e)))
      })
    })
  })
}

# ---- helpers ---------------------------------------------------------------

#' Search patients within the user's hospital. Matches MRN exactly OR
#' name fuzzily via trigram similarity. Returns up to 25 rows.
search_patients <- function(pool, user, q) {
  q_low <- tolower(q)
  db_read(pool, user, "
    SELECT mrn,
           nombre,
           to_char(fecha_nac,'YYYY-MM-DD') AS dob,
           sexo,
           similarity(lower(nombre), $1) AS sim
      FROM patient_identifiers
     WHERE mrn = $2
        OR lower(nombre) % $1
     ORDER BY (mrn = $2) DESC, sim DESC NULLS LAST
     LIMIT 25
  ", params = list(q_low, q)) -> rows
  if (nrow(rows) == 0) return(data.frame(mrn=character(0), display=character(0)))
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
