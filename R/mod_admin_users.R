#' Tab 5: administration.
#'
#' Sub-panels:
#'   - Hospitals (super_admin only)
#'   - Users   (admin / super_admin)
#'   - Audit log viewer (admin / super_admin)
#'
#' All write actions go through with_tenant() and write to audit_log.

mod_admin_users_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("user-gear"), " Administracion"),
    shiny::uiOutput(ns("denied")),
    shiny::conditionalPanel(
      condition = sprintf("output['%s']", ns("is_admin")),
      bs4Dash::tabsetPanel(
        id = ns("admin_tabs"),
        # ---- Users -----------------------------------------------------
        shiny::tabPanel("Usuarios", shiny::icon("users"),
          bs4Dash::box(width = 12, status = "primary", solidHeader = TRUE,
                       title = "Crear usuario",
            shiny::fluidRow(
              shiny::column(3, shiny::textInput(ns("u_username"), "Usuario")),
              shiny::column(3, shiny::textInput(ns("u_fullname"), "Nombre completo")),
              shiny::column(3, shiny::passwordInput(ns("u_pwd"), "Contrasena temporal")),
              shiny::column(3, shiny::selectInput(ns("u_role"), "Rol",
                choices = c("viewer","clinician","researcher","admin","super_admin"),
                selected = "clinician")),
              shiny::column(3, shiny::selectInput(ns("u_hospital"), "Hospital",
                                                  choices = NULL)),
              shiny::column(3, shiny::br(),
                            shiny::actionButton(ns("u_create"), "Crear",
                                                class = "btn-success"))
            ),
            shiny::div(style = "color:#c00", shiny::textOutput(ns("u_msg")))
          ),
          bs4Dash::box(width = 12, status = "info", solidHeader = TRUE,
                       title = "Usuarios existentes",
            DT::DTOutput(ns("users_tbl")))
        ),

        # ---- Hospitals (super_admin only) -----------------------------
        shiny::tabPanel("Hospitales", shiny::icon("hospital"),
          shiny::conditionalPanel(
            condition = sprintf("output['%s']", ns("is_super")),
            bs4Dash::box(width = 12, status = "primary", solidHeader = TRUE,
                         title = "Agregar hospital",
              shiny::fluidRow(
                shiny::column(3, shiny::textInput(ns("h_code"), "Codigo (corto)")),
                shiny::column(5, shiny::textInput(ns("h_name"), "Nombre")),
                shiny::column(2, shiny::textInput(ns("h_country"), "Pais", value = "MX")),
                shiny::column(2, shiny::br(),
                              shiny::actionButton(ns("h_create"), "Crear",
                                                  class = "btn-success"))
              ),
              shiny::div(style = "color:#c00", shiny::textOutput(ns("h_msg")))
            ),
            bs4Dash::box(width = 12, status = "info", solidHeader = TRUE,
                         title = "Hospitales",
              DT::DTOutput(ns("hosp_tbl")))
          )
        ),

        # ---- Audit log -------------------------------------------------
        shiny::tabPanel("Bitacora", shiny::icon("clipboard-list"),
          bs4Dash::box(width = 12, status = "warning", solidHeader = TRUE,
                       title = "Eventos recientes (ultimos 500)",
            DT::DTOutput(ns("audit_tbl")))
        )
      )
    )
  )
}

mod_admin_users_server <- function(id, pool, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    is_admin <- shiny::reactive({
      u <- user(); !is.null(u) && u$role %in% c("admin","super_admin")
    })
    is_super <- shiny::reactive({
      u <- user(); !is.null(u) && u$role == "super_admin"
    })
    output$is_admin <- shiny::reactive(is_admin())
    output$is_super <- shiny::reactive(is_super())
    shiny::outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "is_super", suspendWhenHidden = FALSE)

    output$denied <- shiny::renderUI({
      if (is_admin()) return(NULL)
      shiny::div(class = "alert alert-warning",
                 shiny::icon("ban"),
                 " Esta seccion requiere rol admin o super_admin.")
    })

    # ---- populate hospital dropdown -------------------------------------
    refresh_hospitals <- shiny::reactiveVal(0)
    shiny::observe({
      refresh_hospitals()
      u <- user(); if (is.null(u)) return()
      h <- pool::poolWithTransaction(pool, function(con) {
        DBI::dbGetQuery(con, "SELECT hospital_id, code || ' — ' || name AS label
                                FROM hospitals WHERE is_active ORDER BY code")
      })
      choices <- stats::setNames(h$hospital_id, h$label)
      # non-super admins can only assign within their own hospital
      if (!isTRUE(is_super())) {
        choices <- choices[choices == u$hospital_id]
      }
      shiny::updateSelectInput(session, "u_hospital", choices = choices)
    })

    # ---- create user ----------------------------------------------------
    u_msg <- shiny::reactiveVal("")
    output$u_msg <- shiny::renderText(u_msg())
    shiny::observeEvent(input$u_create, {
      u <- user(); if (!is_admin()) return()
      if (!nzchar(input$u_username) || !nzchar(input$u_fullname) ||
          !nzchar(input$u_pwd) || nchar(input$u_pwd) < 8) {
        u_msg("Complete todos los campos. Contrasena minima 8 caracteres."); return()
      }
      tryCatch({
        with_tenant(pool, u, function(con) {
          hash <- scrypt::hashPassword(input$u_pwd)
          DBI::dbExecute(con, "INSERT INTO users
            (username, full_name, pwd_hash, role, hospital_id)
            VALUES ($1,$2,$3,$4,$5)",
            params = list(input$u_username, input$u_fullname, hash,
                          input$u_role,
                          if (input$u_role == "super_admin") NA
                          else as.integer(input$u_hospital)))
          audit_write(con, u, "INSERT", "users",
                      target_id = input$u_username,
                      diff = list(after = list(role = input$u_role)))
        })
        u_msg("")
        shiny::showNotification("Usuario creado.", type = "message")
        refresh_hospitals(refresh_hospitals() + 1)
      }, error = function(e) u_msg(paste("Error:", conditionMessage(e))))
    })

    # ---- users table ----------------------------------------------------
    output$users_tbl <- DT::renderDT({
      u <- user(); if (!is_admin()) return(NULL)
      where <- if (isTRUE(is_super())) "" else
        sprintf("WHERE hospital_id = %d", as.integer(u$hospital_id))
      d <- pool::poolWithTransaction(pool, function(con) {
        DBI::dbGetQuery(con, paste("
          SELECT u.user_id, u.username, u.full_name, u.role,
                 h.code AS hospital, u.is_active, u.last_login_at
            FROM users u LEFT JOIN hospitals h USING (hospital_id)",
          where, "ORDER BY u.created_at DESC"))
      })
      DT::datatable(d, rownames = FALSE, style = "bootstrap4",
                    options = list(pageLength = 15, scrollX = TRUE))
    })

    # ---- create hospital (super_admin) ----------------------------------
    h_msg <- shiny::reactiveVal("")
    output$h_msg <- shiny::renderText(h_msg())
    shiny::observeEvent(input$h_create, {
      u <- user(); if (!is_super()) return()
      if (!nzchar(input$h_code) || !nzchar(input$h_name)) {
        h_msg("Codigo y nombre requeridos."); return()
      }
      tryCatch({
        with_tenant(pool, u, function(con) {
          DBI::dbExecute(con,
            "INSERT INTO hospitals(code, name, country) VALUES ($1,$2,$3)",
            params = list(input$h_code, input$h_name,
                          input$h_country %||% "MX"))
          audit_write(con, u, "INSERT", "hospitals",
                      target_id = input$h_code)
        })
        h_msg("")
        shiny::showNotification("Hospital creado.", type = "message")
        refresh_hospitals(refresh_hospitals() + 1)
      }, error = function(e) h_msg(paste("Error:", conditionMessage(e))))
    })

    output$hosp_tbl <- DT::renderDT({
      if (!is_super()) return(NULL)
      d <- pool::poolWithTransaction(pool, function(con) {
        DBI::dbGetQuery(con, "SELECT hospital_id, code, name, country,
                                     is_active, created_at FROM hospitals
                                ORDER BY created_at DESC")
      })
      DT::datatable(d, rownames = FALSE, style = "bootstrap4",
                    options = list(pageLength = 10))
    })

    # ---- audit log -----------------------------------------------------
    output$audit_tbl <- DT::renderDT({
      if (!is_admin()) return(NULL)
      u <- user()
      where <- if (isTRUE(is_super())) "" else
        sprintf("WHERE hospital_id = %d", as.integer(u$hospital_id))
      d <- pool::poolWithTransaction(pool, function(con) {
        DBI::dbGetQuery(con, paste("
          SELECT at, actor_name, action, target_table, target_id, ip
            FROM audit_log",
          where, "ORDER BY at DESC LIMIT 500"))
      })
      DT::datatable(d, rownames = FALSE, style = "bootstrap4",
                    options = list(pageLength = 25, scrollX = TRUE))
    })
  })
}
