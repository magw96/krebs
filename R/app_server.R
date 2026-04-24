#' Top-level server. Builds the DB pool, runs auth, dispatches modules.

app_server <- function(input, output, session) {

  # --- DB pool & one-time bootstrap ----------------------------------------
  pool <- db_pool()
  db_bootstrap(pool)
  shiny::onStop(function() pool::poolClose(pool))

  # --- Authentication (shinymanager-style, but DB-backed) -------------------
  user_rv <- shiny::reactiveVal(NULL)

  # Wire up login click handler ONCE (must not be inside an observer that
  # re-runs, otherwise we'd register a duplicate handler each time).
  mod_login_observers(input, output, session, pool, on_success = user_rv)

  # Open the login modal whenever there is no authenticated user. removeModal()
  # clears Bootstrap's backdrop, so the rest of the UI becomes interactive again.
  shiny::observe({
    if (is.null(user_rv())) {
      mod_login_show()
    } else {
      shiny::removeModal()
    }
  })

  # --- Header chip ----------------------------------------------------------
  output$user_chip <- shiny::renderUI({
    u <- user_rv(); if (is.null(u)) return(NULL)
    shiny::tags$span(
      class = "user-chip",
      shiny::icon("user-circle"), " ",
      shiny::strong(u$full_name), " (",
      shiny::tags$small(u$role), ") ",
      shiny::actionLink(session$ns("logout"), "Cerrar sesion",
                        icon = shiny::icon("door-open"))
    )
  })

  shiny::observeEvent(input$logout, {
    u <- user_rv()
    if (!is.null(u)) audit_event(pool, u, action = "LOGOUT",
                                 ip = session$request$REMOTE_ADDR)
    user_rv(NULL)
    session$reload()
  })

  # --- Mount modules (each gets pool + reactive user) -----------------------
  current_user <- shiny::reactive(user_rv())

  mod_register_new_server   ("register", pool = pool, user = current_user)
  mod_followup_search_server("followup", pool = pool, user = current_user)
  mod_dashboard_server      ("dash",     pool = pool, user = current_user)
  mod_admin_data_server     ("data",     pool = pool, user = current_user)
  mod_admin_users_server    ("admin",    pool = pool, user = current_user)
}
