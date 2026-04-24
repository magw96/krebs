#' Top-level UI. Thin shell: a bs4Dash skeleton that mounts each module.

app_ui <- function(request) {
  shiny::tagList(
    # shinyjs MUST be initialised at the very top of the document so that
    # toggleState/show/hide/reset calls in modules find their JS hook.
    shinyjs::useShinyjs(),
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "www/krebs.css"),
      shiny::tags$link(rel = "icon", href = "www/krebs.svg"),
      # Defensive: if Bootstrap leaves a stale modal-backdrop after removeModal()
      # the whole page becomes opaque/click-through-blocked. Force-hide any
      # backdrop unless an actual .modal.show is present.
      shiny::tags$style(shiny::HTML(
        ".modal-backdrop:not(.show), body:not(.modal-open) .modal-backdrop { display: none !important; }
         body:not(.modal-open) { overflow: auto !important; padding-right: 0 !important; }"
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
          shiny::uiOutput("user_chip", inline = TRUE)
        )
      ),

      sidebar = bs4Dash::dashboardSidebar(
        skin  = "light",
        bs4Dash::sidebarMenu(
          id = "sidebar",
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
    )
  )
}
