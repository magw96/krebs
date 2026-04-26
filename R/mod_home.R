#' Tab 0 (NEW): Mis pendientes - clinician worklist.
#'
#' Three SQL-driven worklists, each rendered as a clickable DT:
#'   1. Followup overdue   - vivos sin encuentro en >180 dias
#'   2. Recurrencia abierta - encounter_type=recurrence en los ultimos 90 dias
#'                            sin un encuentro `treatment` posterior
#'   3. Defunciones sin cierre - vital_status=muerto pero sin encounter_type=death
#'
#' Clicking a row triggers `on_pick(mrn)` which the parent uses to deeplink into
#' Seguimiento.

# Days threshold for "overdue followup" (chosen by the user during build).
.OVERDUE_DAYS <- 180L

mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("clipboard-check"), " Mis pendientes"),
    shiny::p(class = "text-muted",
      "Tareas que requieren su atencion. Haga clic en cualquier paciente ",
      "para abrir su expediente en Seguimiento."),

    # KPI row -----------------------------------------------------------------
    shiny::fluidRow(
      shiny::column(4, bs4Dash::valueBoxOutput(ns("kpi_overdue"), width = 12)),
      shiny::column(4, bs4Dash::valueBoxOutput(ns("kpi_recurrence"), width = 12)),
      shiny::column(4, bs4Dash::valueBoxOutput(ns("kpi_death"), width = 12))
    ),

    # Worklists ---------------------------------------------------------------
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("hourglass-half"),
        sprintf(" Seguimiento vencido (>%d dias sin encuentro)", .OVERDUE_DAYS)),
      width = 12, status = "warning", solidHeader = TRUE, collapsible = TRUE,
      DT::DTOutput(ns("tbl_overdue"))
    ),
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("triangle-exclamation"),
        " Recurrencias recientes sin tratamiento"),
      width = 12, status = "danger", solidHeader = TRUE, collapsible = TRUE,
      DT::DTOutput(ns("tbl_recur"))
    ),
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("file-circle-xmark"),
        " Defunciones sin cierre formal"),
      width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
      DT::DTOutput(ns("tbl_death"))
    )
  )
}

mod_home_server <- function(id, pool, user, on_pick, data_changed = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- Reactive worklists (depend on data_changed for fresh refresh) ------
    overdue <- shiny::reactive({
      if (!is.null(data_changed)) data_changed()
      u <- user(); if (is.null(u)) return(NULL)
      load_overdue(pool, u, .OVERDUE_DAYS)
    })
    recur_open <- shiny::reactive({
      if (!is.null(data_changed)) data_changed()
      u <- user(); if (is.null(u)) return(NULL)
      load_open_recurrences(pool, u)
    })
    deaths_open <- shiny::reactive({
      if (!is.null(data_changed)) data_changed()
      u <- user(); if (is.null(u)) return(NULL)
      load_open_deaths(pool, u)
    })

    # ---- KPI value boxes ----------------------------------------------------
    output$kpi_overdue <- bs4Dash::renderValueBox({
      n <- nrow(overdue() %||% data.frame())
      bs4Dash::valueBox(
        value = n, subtitle = "Seguimientos vencidos",
        icon = shiny::icon("hourglass-half"), color = "warning"
      )
    })
    output$kpi_recurrence <- bs4Dash::renderValueBox({
      n <- nrow(recur_open() %||% data.frame())
      bs4Dash::valueBox(
        value = n, subtitle = "Recurrencias abiertas",
        icon = shiny::icon("triangle-exclamation"), color = "danger"
      )
    })
    output$kpi_death <- bs4Dash::renderValueBox({
      n <- nrow(deaths_open() %||% data.frame())
      bs4Dash::valueBox(
        value = n, subtitle = "Defunciones sin cierre",
        icon = shiny::icon("file-circle-xmark"), color = "info"
      )
    })

    # ---- Tables -------------------------------------------------------------
    render_worklist <- function(reactive_df, empty_msg) {
      DT::renderDT({
        d <- reactive_df()
        if (is.null(d) || nrow(d) == 0) {
          return(DT::datatable(
            data.frame(Mensaje = empty_msg),
            rownames = FALSE, options = list(dom = "t", paging = FALSE),
            style = "bootstrap4"))
        }
        DT::datatable(d, rownames = FALSE, selection = "single",
          style = "bootstrap4",
          options = list(pageLength = 10, scrollX = TRUE,
                         order = list(list(0, "asc"))))
      })
    }
    output$tbl_overdue <- render_worklist(overdue,
      "Sin pacientes con seguimiento vencido. Felicidades.")
    output$tbl_recur <- render_worklist(recur_open,
      "Sin recurrencias abiertas pendientes de tratamiento.")
    output$tbl_death <- render_worklist(deaths_open,
      "Sin defunciones pendientes de cierre.")

    # ---- Row click -> deeplink into Seguimiento -----------------------------
    pick_from <- function(reactive_df, row_input) {
      shiny::observeEvent(row_input(), {
        d <- reactive_df(); r <- row_input()
        if (is.null(d) || is.null(r) || length(r) == 0L) return()
        if (r > nrow(d)) return()
        on_pick(d$mrn[r])
      })
    }
    pick_from(overdue,     shiny::reactive(input$tbl_overdue_rows_selected))
    pick_from(recur_open,  shiny::reactive(input$tbl_recur_rows_selected))
    pick_from(deaths_open, shiny::reactive(input$tbl_death_rows_selected))
  })
}

# ---- Worklist queries -------------------------------------------------------

#' Patients alive whose most recent encounter is older than `days`.
load_overdue <- function(pool, user, days) {
  db_read(pool, user, sprintf("
    WITH latest AS (
      SELECT DISTINCT ON (e.hospital_id, e.mrn)
             e.hospital_id, e.mrn, e.encounter_date, e.encounter_type,
             e.vital_status
        FROM encounters e
       ORDER BY e.hospital_id, e.mrn, e.encounter_date DESC, e.created_at DESC
    )
    SELECT pi.mrn,
           pi.nombre,
           l.encounter_date AS ultimo_encuentro,
           (CURRENT_DATE - l.encounter_date)::INT AS dias_sin_visita,
           v.tipo_cancer,
           v.fecha_dx
      FROM latest l
      JOIN patient_identifiers pi
        ON pi.hospital_id = l.hospital_id AND pi.mrn = l.mrn
 LEFT JOIN v_patient_summary v
        ON v.hospital_id = l.hospital_id AND v.mrn = l.mrn
     WHERE COALESCE(l.vital_status, 'vivo') <> 'muerto'
       AND (CURRENT_DATE - l.encounter_date) > %d
     ORDER BY dias_sin_visita DESC
     LIMIT 200
  ", as.integer(days)))
}

#' Recurrence encounters in the last 90 days that have NO subsequent
#' treatment encounter (i.e. nothing was started in response).
load_open_recurrences <- function(pool, user) {
  db_read(pool, user, "
    SELECT pi.mrn,
           pi.nombre,
           r.encounter_date AS fecha_recurrencia,
           (CURRENT_DATE - r.encounter_date)::INT AS dias_desde_recurrencia,
           r.primary_site,
           r.notes
      FROM encounters r
      JOIN patient_identifiers pi
        ON pi.hospital_id = r.hospital_id AND pi.mrn = r.mrn
     WHERE r.encounter_type = 'recurrence'
       AND r.encounter_date >= CURRENT_DATE - INTERVAL '90 days'
       AND NOT EXISTS (
         SELECT 1 FROM encounters t
          WHERE t.hospital_id    = r.hospital_id
            AND t.mrn            = r.mrn
            AND t.encounter_type = 'treatment'
            AND t.encounter_date >= r.encounter_date
       )
     ORDER BY r.encounter_date DESC
     LIMIT 200
  ")
}

#' vital_status='muerto' on any encounter, but no encounter_type='death'
#' has been registered to formally close the record.
load_open_deaths <- function(pool, user) {
  db_read(pool, user, "
    SELECT DISTINCT ON (pi.hospital_id, pi.mrn)
           pi.mrn,
           pi.nombre,
           e.encounter_date AS fecha_referencia,
           e.death_date,
           e.death_cause
      FROM encounters e
      JOIN patient_identifiers pi
        ON pi.hospital_id = e.hospital_id AND pi.mrn = e.mrn
     WHERE e.vital_status = 'muerto'
       AND NOT EXISTS (
         SELECT 1 FROM encounters d
          WHERE d.hospital_id    = e.hospital_id
            AND d.mrn            = e.mrn
            AND d.encounter_type = 'death'
       )
     ORDER BY pi.hospital_id, pi.mrn, e.encounter_date DESC
     LIMIT 200
  ")
}
