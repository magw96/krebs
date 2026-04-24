#' Tab 4: data table + downloads.
#'
#' Researcher role sees the de-identified view (v_clinical_deidentified).
#' Other roles see encounters joined to identifiers (subject to RLS).
#' Every download writes an EXPORT row in audit_log.

mod_admin_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("table"), " Datos y descargas"),
    shiny::p("Las descargas quedan registradas en la bitacora de auditoria.",
             class = "text-muted"),
    bs4Dash::box(width = 12, status = "primary", solidHeader = TRUE,
                 title = shiny::tagList(shiny::icon("download"), " Exportar"),
      shiny::fluidRow(
        shiny::column(4, shinyWidgets::radioGroupButtons(ns("scope"), "Vista",
          choices = c("Encuentros con identidad" = "full",
                      "De-identificada"           = "deid"),
          selected = "deid", justified = TRUE, size = "sm")),
        shiny::column(4, shiny::downloadButton(ns("dl_csv"),  "CSV",   class = "btn-success")),
        shiny::column(4, shiny::downloadButton(ns("dl_xlsx"), "Excel", class = "btn-success"))
      )
    ),

    bs4Dash::box(width = 12, status = "info", solidHeader = TRUE,
                 title = shiny::tagList(shiny::icon("table"), " Vista previa"),
      DT::DTOutput(ns("tbl"))
    )
  )
}

mod_admin_data_server <- function(id, pool, user) {
  shiny::moduleServer(id, function(input, output, session) {

    tbl <- shiny::reactive({
      u <- user(); if (is.null(u)) return(NULL)
      can_full <- u$role %in% c("clinician","admin","super_admin")
      scope    <- input$scope %||% "deid"
      if (scope == "full" && !can_full) scope <- "deid"
      if (scope == "deid") {
        db_read(pool, u, "SELECT * FROM v_clinical_deidentified ORDER BY encounter_date DESC LIMIT 1000")
      } else {
        db_read(pool, u, "
          SELECT pi.nombre, pi.mrn, pi.sexo, pi.fecha_nac,
                 e.* FROM patient_identifiers pi
            JOIN encounters e USING (hospital_id, mrn)
            ORDER BY e.encounter_date DESC LIMIT 1000")
      }
    })

    output$tbl <- DT::renderDT({
      d <- tbl(); if (is.null(d)) return(NULL)
      DT::datatable(d, rownames = FALSE, style = "bootstrap4", filter = "top",
                    options = list(scrollX = TRUE, pageLength = 25))
    })

    output$dl_csv <- shiny::downloadHandler(
      filename = function() sprintf("krebs_%s_%s.csv", input$scope %||% "deid",
                                    format(Sys.time(), "%Y%m%d-%H%M%S")),
      content  = function(file) {
        d <- tbl()
        utils::write.csv(d, file, row.names = FALSE)
        u <- user()
        if (!is.null(u)) audit_event(pool, u, "EXPORT",
                                     target_table = "encounters",
                                     target_id    = input$scope %||% "deid")
      }
    )
    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() sprintf("krebs_%s_%s.xlsx", input$scope %||% "deid",
                                    format(Sys.time(), "%Y%m%d-%H%M%S")),
      content  = function(file) {
        d <- tbl()
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(d, file)
        } else {
          # fall back to CSV with .xlsx extension warning
          utils::write.csv(d, file, row.names = FALSE)
        }
        u <- user()
        if (!is.null(u)) audit_event(pool, u, "EXPORT",
                                     target_table = "encounters",
                                     target_id    = input$scope %||% "deid")
      }
    )
  })
}
