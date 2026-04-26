#' Navbar patient quick-search.
#'
#' Persistent selectize input mounted in the dashboard header rightUi.
#' Server-side fuzzy match (re-uses search_patients() from mod_followup_search).
#' Picking a hit jumps to the Seguimiento tab and pre-loads that MRN.
#'
#' Keyboard: Ctrl/Cmd+K focuses the input (handled in krebs_shortcuts.js).

mod_quick_search_ui <- function(id) {
  ns <- shiny::NS(id)
  # bs4Dash::dashboardHeader(rightUi=...) wraps in <ul>, so each child MUST
  # be a <li>. Wrap the selectize in a list-item with the BS4 nav-item class.
  shiny::tags$li(
    class = "nav-item dropdown krebs-quicksearch",
    shiny::tags$label(`for` = ns("q"), class = "sr-only", "Buscar paciente"),
    shiny::selectizeInput(
      ns("q"),
      label = NULL,
      choices = NULL,
      options = list(
        placeholder = "Buscar (Ctrl+K)",
        loadThrottle = 300,
        maxOptions = 15,
        create = FALSE
      )
    )
  )
}

mod_quick_search_server <- function(id, pool, user, on_pick) {
  shiny::moduleServer(id, function(input, output, session) {

    # Preload every patient in the user's hospital (LIMIT 5000 as a guardrail).
    # Selectize then handles type-ahead and fuzzy match client-side, which
    # avoids the input$q_search round-trip that doesn't exist in stock Shiny.
    shiny::observe({
      u <- user(); if (is.null(u)) return()
      # No explicit hospital_id filter -- RLS handles tenant scoping (and
      # super_admin's hospital_id is NULL, which would zero-match here).
      pl <- tryCatch(
        db_read(pool, u, "
          SELECT mrn, nombre,
                 to_char(fecha_nac,'YYYY-MM-DD') AS dob, sexo
            FROM patient_identifiers
           ORDER BY nombre ASC
           LIMIT 5000"),
        error = function(e) {
          message("[quick] preload err: ", conditionMessage(e))
          NULL
        })
      if (is.null(pl) || nrow(pl) == 0) return()
      pl$display <- sprintf("%s -- %s (%s, %s)", pl$mrn, pl$nombre, pl$dob, pl$sexo)
      shiny::updateSelectizeInput(session, "q",
        choices  = stats::setNames(pl$mrn, pl$display),
        selected = character(0),
        server   = TRUE)
    })

    # When the user picks a result, fire the callback and reset the field.
    shiny::observeEvent(input$q, {
      mrn <- input$q
      if (is.null(mrn) || !nzchar(mrn)) return()
      on_pick(mrn)
      shiny::updateSelectizeInput(session, "q", selected = character(0))
    }, ignoreInit = TRUE)
  })
}
