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
  # We also toggle the `krebs-pre-auth` class on <body> so the CSS can hide
  # the bs4Dash chrome (sidebar/header/content) entirely while the user is
  # unauthenticated -- belt and braces alongside the :has() rule.
  shiny::observe({
    if (is.null(user_rv())) {
      shinyjs::addClass(selector = "body", class = "krebs-pre-auth")
      mod_login_show()
    } else {
      shinyjs::removeClass(selector = "body", class = "krebs-pre-auth")
      shiny::removeModal()
    }
  })

  # --- Header chip ----------------------------------------------------------
  output$user_chip <- shiny::renderUI({
    u <- user_rv(); if (is.null(u)) return(NULL)
    # The uiOutput container is already <li class="nav-item"> (set in app_ui.R)
    # so we render only the inner content here.
    shiny::tagList(
      shiny::tags$span(class = "user-chip",
        shiny::icon("user-circle"), " ",
        shiny::strong(u$full_name), " (",
        shiny::tags$small(u$role), ") ",
        shiny::actionLink(session$ns("logout"), "Cerrar sesion",
                          icon = shiny::icon("door-open"))
      )
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

  # Shared invalidation tick: bumped by register/followup after a successful
  # insert; observed by dashboard + data tabs so they refresh immediately
  # instead of showing stale cached results.
  data_changed <- shiny::reactiveVal(0L)

  # Cross-module deeplink: any caller (navbar quick-search, Mis pendientes
  # row click, ...) writes an MRN here, which switches to the Seguimiento
  # tab and pre-loads that patient.
  pick_patient <- shiny::reactiveVal(NULL)
  go_to_patient <- function(mrn) {
    if (is.null(mrn) || !nzchar(mrn)) return()
    pick_patient(list(mrn = mrn, ts = as.numeric(Sys.time())))
    bs4Dash::updateTabItems(session, "sidebar", selected = "tab_followup")
  }

  mod_quick_search_server   ("quick",    pool = pool, user = current_user,
                             on_pick    = go_to_patient)
  mod_home_server           ("home",     pool = pool, user = current_user,
                             on_pick    = go_to_patient,
                             data_changed = data_changed)
  mod_register_new_server   ("register", pool = pool, user = current_user,
                             data_changed = data_changed)
  mod_followup_search_server("followup", pool = pool, user = current_user,
                             data_changed = data_changed,
                             prefill      = pick_patient)
  mod_dashboard_server      ("dash",     pool = pool, user = current_user)
  mod_admin_data_server     ("data",     pool = pool, user = current_user,
                             data_changed = data_changed)
  mod_admin_users_server    ("admin",    pool = pool, user = current_user)
}
