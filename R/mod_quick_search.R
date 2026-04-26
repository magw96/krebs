#' Navbar patient quick-search.
#'
#' Persistent selectize input mounted in the dashboard header rightUi.
#' Server-side fuzzy match (re-uses search_patients() from mod_followup_search).
#' Picking a hit jumps to the Seguimiento tab and pre-loads that MRN.
#'
#' Keyboard: Ctrl/Cmd+K focuses the input (handled in krebs_shortcuts.js).

mod_quick_search_ui <- function(id) {
  ns <- shiny::NS(id)
  # Wrapped in a span so we can inline-style it inside the header.
  shiny::tags$span(
    class = "krebs-quicksearch",
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

    # Hydrate options server-side as the user types.
    shiny::observeEvent(input$q_search, {
      u <- user(); if (is.null(u)) return()
      q <- input$q_search %||% ""
      if (nchar(q) < 2) return()
      hits <- search_patients(pool, u, q)
      if (nrow(hits) == 0) return()
      shiny::updateSelectizeInput(session, "q",
        choices = stats::setNames(hits$mrn, hits$display),
        selected = input$q,
        server = TRUE)
    }, ignoreInit = TRUE)

    # When the user picks a result, fire the callback. The parent (app_server)
    # uses it to switch tabs and preload the picked MRN into Seguimiento.
    shiny::observeEvent(input$q, {
      mrn <- input$q
      if (is.null(mrn) || !nzchar(mrn)) return()
      on_pick(mrn)
      # Reset the navbar input so the next search starts empty. Guarded with
      # isolate so we don't loop on the observer.
      shiny::updateSelectizeInput(session, "q",
        choices = character(0), selected = character(0),
        server = TRUE)
    }, ignoreInit = TRUE)
  })
}
