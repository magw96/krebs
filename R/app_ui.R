#' Top-level UI. Thin shell: a bs4Dash skeleton that mounts each module.

app_ui <- function(request) {
  # Resolve the CSS so we can inline it. PCC sometimes loses the addResourcePath
  # mapping (different working directory than dev), so an external <link>
  # silently 404s and the theme never applies. Inlining is bulletproof.
  css_path <- ""
  for (cand in c(file.path("inst", "app", "www", "krebs.css"),
                 file.path("app", "www", "krebs.css"),
                 system.file("app", "www", "krebs.css", package = "krebs"))) {
    if (nzchar(cand) && file.exists(cand)) { css_path <- cand; break }
  }
  inline_css <- if (nzchar(css_path)) {
    paste(readLines(css_path, warn = FALSE, encoding = "UTF-8"),
          collapse = "\n")
  } else ""

  shiny::tagList(
    # shinyjs MUST be initialised at the very top of the document so that
    # toggleState/show/hide/reset calls in modules find their JS hook.
    shinyjs::useShinyjs(),
    # Pre-auth shield. Marks <html> as loading BEFORE any bs4Dash chrome
    # is parsed/painted, so the user never glimpses the dashboard while
    # Shiny boots and the login modal is still in flight. The class is
    # removed by app_server once the login modal is closed AND a user is
    # authenticated. We add the class via an inline script in the head
    # rather than a Shiny output so it is applied during HTML parse,
    # not after the websocket connects.
    shiny::tags$head(shiny::tags$script(shiny::HTML(
      "document.documentElement.classList.add('krebs-loading');"))),
    shiny::tags$head(
      # Keep the external link as a fallback (it will load if the resource
      # path is registered), and ALSO inline the file so the theme always
      # applies even when the static asset 404s.
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/krebs.css"),
      shiny::tags$style(type = "text/css", shiny::HTML(inline_css)),
      shiny::tags$link(rel = "icon", href = "www/krebs.svg"),
      shiny::tags$script(src = "www/krebs_shortcuts.js"),
      # Defensive: if Bootstrap leaves a stale modal-backdrop after removeModal()
      # the whole page becomes opaque/click-through-blocked. Force-hide any
      # backdrop unless an actual .modal.show is present.
      shiny::tags$style(shiny::HTML(
        ".modal-backdrop:not(.show), body:not(.modal-open) .modal-backdrop { display: none !important; }
         body:not(.modal-open) { overflow: auto !important; padding-right: 0 !important; }
         .kbd-flash { box-shadow: 0 0 0 4px rgba(40,167,69,0.35) !important; transition: box-shadow .2s; }"
      ))
    ),

    bs4Dash::dashboardPage(
      title = "Krebs",
      dark  = NULL,
      help  = NULL,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          title = "Krebs",
          color = "primary",
          image = "www/krebs.svg"
        ),
        skin  = "light",
        status = "primary",
        rightUi = shiny::tagList(
          mod_quick_search_ui("quick"),
          # bs4Dash::dashboardHeader requires every rightUi child to be a <li>.
          # Use a custom container so the uiOutput wrapper is itself an <li>
          # rather than the default <span>/<div>.
          shiny::uiOutput("user_chip",
                          container = function(...)
                            shiny::tags$li(class = "nav-item dropdown", ...))
        )
      ),

      sidebar = bs4Dash::dashboardSidebar(
        skin  = "light",
        bs4Dash::sidebarMenu(
          id = "sidebar",
          bs4Dash::menuItem("Mis pendientes",
            tabName = "tab_home", icon = shiny::icon("clipboard-check"),
            selected = TRUE),
          bs4Dash::menuItem("Registrar paciente",
            tabName = "tab_register", icon = shiny::icon("user-plus")),
          bs4Dash::menuItem("Seguimiento",
            tabName = "tab_followup", icon = shiny::icon("clock-rotate-left")),
          bs4Dash::menuItem("Visualizacion",
            tabName = "tab_dashboard", icon = shiny::icon("chart-line")),
          bs4Dash::menuItem("Datos / Descargas",
            tabName = "tab_data", icon = shiny::icon("table")),
          bs4Dash::menuItem("Administracion",
            tabName = "tab_admin", icon = shiny::icon("user-gear"))
        )
      ),

      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "tab_home",      mod_home_ui("home")),
          bs4Dash::tabItem(tabName = "tab_register",  mod_register_new_ui("register")),
          bs4Dash::tabItem(tabName = "tab_followup",  mod_followup_search_ui("followup")),
          bs4Dash::tabItem(tabName = "tab_dashboard", mod_dashboard_ui("dash")),
          bs4Dash::tabItem(tabName = "tab_data",      mod_admin_data_ui("data")),
          bs4Dash::tabItem(tabName = "tab_admin",     mod_admin_users_ui("admin"))
        )
      ),

      footer = bs4Dash::dashboardFooter(
        left  = "Krebs v0.2",
        right = format(Sys.Date(), "%Y")
      )
    ),

    # IMPORTANT: re-inject our theme CSS *after* dashboardPage() so it loads
    # after bs4Dash's own stylesheets and wins the cascade. Without this,
    # bs4Dash's inline theme strings override our `:root` custom-properties
    # and the page renders in the default light blue.
    shiny::tags$style(type = "text/css", shiny::HTML(inline_css))
  )
}
