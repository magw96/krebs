#' Login modal. Validates against the `users` table; on success calls
#' `on_success(user_list)` so app_server can populate user_rv().
#'
#' Two functions because Shiny needs the observers wired up exactly ONCE at
#' app start (not every time the modal is shown). `mod_login_show()` only
#' opens the dialog (idempotent); `mod_login_observers()` registers the
#' click/auth handler with direct access to top-level input/output.

mod_login_show <- function() {
  ver  <- tryCatch(as.character(utils::packageVersion("krebs")),
                   error = function(e) "0.2.0")
  yr   <- format(Sys.Date(), "%Y")

  shiny::showModal(shiny::modalDialog(
    title = NULL, easyClose = FALSE, footer = NULL, size = "s",
    shiny::div(class = "krebs-login-card",
      shiny::tags$img(src = "www/krebs.svg",
                      class = "krebs-logo", alt = "Krebs"),
      shiny::h2(class = "krebs-app-name", "Krebs"),
      shiny::p(class = "krebs-app-tag",
               "Plataforma clinica de oncologia"),
      shiny::p(class = "krebs-login-prompt",
               "Inicie sesion para continuar"),
      shiny::textInput("krebs_login_user", "Usuario",
                       placeholder = "su.usuario"),
      shiny::passwordInput("krebs_login_pwd", "Contrasena"),
      shiny::uiOutput("krebs_login_err"),
      shiny::actionButton("krebs_login_btn",
        shiny::tagList(shiny::icon("right-to-bracket"), " Entrar"),
        class = "btn-primary", width = "100%")
    ),
    shiny::tags$div(class = "krebs-login-footer",
      shiny::span(class = "krebs-version",
                  sprintf("Krebs v%s", ver)),
      "  ",
      shiny::HTML("&middot;"),
      "  ",
      shiny::span(sprintf("(c) %s", yr)),
      shiny::span(class = "krebs-disclaimer",
        "Uso restringido a personal autorizado. ",
        "Datos protegidos bajo confidencialidad medica. ",
        "Toda accion queda registrada en bitacora de auditoria.")
    )
  ))
}

mod_login_observers <- function(input, output, session, pool, on_success) {
  err_rv <- shiny::reactiveVal("")
  output$krebs_login_err <- shiny::renderUI({
    if (!nzchar(err_rv())) return(NULL)
    shiny::div(class = "alert alert-danger", style = "margin-top:10px;",
               shiny::icon("triangle-exclamation"), " ", err_rv())
  })

  shiny::observeEvent(input$krebs_login_btn, {
    u <- input$krebs_login_user %||% ""
    p <- input$krebs_login_pwd  %||% ""
    if (!nzchar(u) || !nzchar(p)) {
      err_rv("Ingrese usuario y contrasena."); return()
    }
    row <- tryCatch(
      pool::poolWithTransaction(pool, function(con) {
        DBI::dbGetQuery(con,
          "SELECT user_id, username, full_name, pwd_hash, role,
                  hospital_id, is_active
             FROM users WHERE username = $1",
          params = list(u))
      }),
      error = function(e) {
        message("[login] DB error: ", conditionMessage(e)); NULL
      }
    )
    if (is.null(row) || nrow(row) == 0 || !isTRUE(row$is_active[1])) {
      err_rv("Usuario invalido o inactivo."); return()
    }
    if (!isTRUE(scrypt::verifyPassword(row$pwd_hash[1], p))) {
      err_rv("Contrasena incorrecta."); return()
    }
    user <- as.list(row[1, , drop = FALSE])
    user$pwd_hash <- NULL
    audit_event(pool, user, action = "LOGIN",
                ip = session$request$REMOTE_ADDR)
    pool::poolWithTransaction(pool, function(con) {
      DBI::dbExecute(con,
        "UPDATE users SET last_login_at = now() WHERE user_id = $1",
        params = list(user$user_id))
    })
    err_rv("")
    shiny::removeModal()
    on_success(user)
  })
}
