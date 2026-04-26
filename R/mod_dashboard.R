#' Tab 3: visualizations.
#'
#' Single global filter strip at the top (cancer type, date range, sex, age range).
#' All charts respect those filters via a shared reactive `filtered()`.
#'
#' Charts implemented:
#'   1. KM overall survival (functional)
#'   2. Time-to-recurrence (functional)
#'   3. Stage distribution at dx (stacked bar)
#'   4. Treatment-journey Sankey
#'   5. Age pyramid by sex
#'   6. Cases-by-state choropleth (only if mxmaps available)
#'   7. Time series of new cases
#'   8. Hospital LOS distribution
#'   9. Stats panel (N, median IQR, normality)

mod_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("chart-line"), " Visualizacion"),

    bs4Dash::box(
      title = shiny::tagList(shiny::icon("filter"), " Filtros"),
      width = 12, status = "secondary", solidHeader = TRUE, collapsible = TRUE,
      shiny::fluidRow(
        shiny::column(3, shiny::selectizeInput(ns("f_cancer"), "Tipo de cancer",
                                              choices = NULL, multiple = TRUE,
                                              options = list(placeholder = "Todos"))),
        shiny::column(3, shiny::dateRangeInput(ns("f_date"),
                                               "Rango de diagnostico (opcional)",
                                               start = NA, end = NA,
                                               language = "es")),
        shiny::column(2, shinyWidgets::pickerInput(ns("f_sex"), "Sexo",
                                                   choices = c("M","F","Otro"),
                                                   selected = c("M","F","Otro"),
                                                   multiple = TRUE)),
        shiny::column(2, shiny::sliderInput(ns("f_age"), "Edad", min = 0, max = 100,
                                            value = c(0, 100))),
        shiny::column(2, shiny::uiOutput(ns("filter_summary")))
      )
    ),

    shiny::fluidRow(
      shiny::column(6,
        bs4Dash::box(title = "Estadisticas descriptivas", width = 12, status = "info",
                     shiny::tableOutput(ns("stats_panel")))),
      shiny::column(6,
        bs4Dash::box(title = "Distribucion por edad y sexo (piramide)", width = 12,
                     status = "info",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_pyramid"), height = 320),
                                                  type = 5, color = "midnightblue", size = 0.5)))
    ),

    shiny::fluidRow(
      shiny::column(6,
        bs4Dash::box(title = "Supervivencia global (Kaplan-Meier)", width = 12, status = "primary",
                     shinycssloaders::withSpinner(shiny::plotOutput(ns("p_km"), height = 360),
                                                  type = 5))),
      shiny::column(6,
        bs4Dash::box(title = "Tiempo a recurrencia", width = 12, status = "primary",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_ttr"), height = 360),
                                                  type = 5)))
    ),

    shiny::fluidRow(
      shiny::column(6,
        bs4Dash::box(title = "Distribucion de estadio al dx", width = 12, status = "primary",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_stage"), height = 360),
                                                  type = 5))),
      shiny::column(6,
        bs4Dash::box(title = "Casos nuevos por mes", width = 12, status = "primary",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_ts"), height = 360),
                                                  type = 5)))
    ),

    shiny::fluidRow(
      shiny::column(12,
        bs4Dash::box(title = "Trayectoria de tratamiento (Sankey)", width = 12, status = "warning",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_sankey"), height = 420),
                                                  type = 5)))
    ),

    shiny::fluidRow(
      shiny::column(6,
        bs4Dash::box(title = "Estancia hospitalaria por procedimiento", width = 12, status = "info",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_los"), height = 360),
                                                  type = 5))),
      shiny::column(6,
        bs4Dash::box(title = "Casos por estado (Mexico)", width = 12, status = "info",
                     shinycssloaders::withSpinner(plotly::plotlyOutput(ns("p_map"), height = 360),
                                                  type = 5)))
    )
  )
}

mod_dashboard_server <- function(id, pool, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate cancer-type filter from DB
    shiny::observe({
      u <- user(); if (is.null(u)) return()
      types <- db_read(pool, u, "
        SELECT DISTINCT oncotree FROM encounters
         WHERE encounter_type = 'initial_dx' AND oncotree IS NOT NULL
         ORDER BY oncotree")$oncotree
      shiny::updateSelectizeInput(session, "f_cancer", choices = types, server = TRUE)
    })

    # ---- Single shared filtered dataset ----------------------------------
    # Defensive: returns an empty data.frame on any failure (missing inputs,
    # not-yet-ready filters, DB hiccup) so downstream renderers can show
    # "Sin datos" instead of "An error has occurred".
    filtered <- shiny::reactive({
      u <- user(); if (is.null(u)) return(NULL)
      tryCatch({
        where <- "WHERE encounter_type = 'initial_dx'"
        params <- list()
        i <- 0
        add <- function(clause, val) {
          i <<- i + 1L
          where <<- paste(where, "AND", sub("\\$\\?", paste0("$", i), clause))
          params[[i]] <<- val
        }
        # Date range -- optional. Apply only when both endpoints valid.
        if (length(input$f_date) == 2 && all(!is.na(input$f_date))) {
          add("encounter_date >= $?", as.character(input$f_date[1]))
          add("encounter_date <= $?", as.character(input$f_date[2]))
        }
        if (length(input$f_cancer)) {
          add("oncotree = ANY($?)", input$f_cancer)
        }
        # Sex -- optional. Skip filter when nothing selected (= "all").
        if (length(input$f_sex) > 0 && length(input$f_sex) < 3) {
          add("pi.sexo = ANY($?)", input$f_sex)
        }
        # Age -- only constrain when slider is moved off the full 0-100 range.
        age_lo <- if (length(input$f_age) >= 1) as.integer(input$f_age[1]) else 0L
        age_hi <- if (length(input$f_age) >= 2) as.integer(input$f_age[2]) else 100L
        if (age_lo > 0L || age_hi < 100L) {
          add("((e.encounter_date - pi.fecha_nac)/365) BETWEEN $? AND $?", age_lo)
          # second placeholder for the same BETWEEN clause
          i <<- i + 1L
          where <<- sub("\\$\\?", paste0("$", i), where)
          params[[i]] <<- age_hi
        }
        sql <- paste("
          SELECT e.*, pi.sexo, pi.fecha_nac,
                 (e.encounter_date - pi.fecha_nac)/365 AS edad_dx
            FROM encounters e
            JOIN patient_identifiers pi USING (hospital_id, mrn) ",
          where, sep = "")
        db_read(pool, u, sql, params = params)
      }, error = function(e) {
        message("[dashboard] filtered() error: ", conditionMessage(e))
        data.frame()
      })
    })

    output$filter_summary <- shiny::renderUI({
      d <- filtered()
      n <- if (is.null(d) || !is.data.frame(d)) 0L else nrow(d)
      shiny::tags$h5(shiny::tags$span(class = "badge bg-primary",
                                      sprintf("N = %d pacientes", n)))
    })

    # ---- Stats panel ------------------------------------------------------
    output$stats_panel <- shiny::renderTable({
      tryCatch({
        d <- filtered()
        if (is.null(d) || nrow(d) == 0) {
          return(data.frame(Mensaje = "Sin datos para los filtros seleccionados."))
        }
        vars <- list(
          "Edad al dx"       = d$edad_dx,
          "LOS (dias)"       = d$los_days,
          "Dosis radio (Gy)" = d$radio_dose_gy,
          "Ciclos quimio"    = d$chemo_cycles
        )
        do.call(rbind, lapply(names(vars), function(nm) {
          x <- as.numeric(vars[[nm]]); x <- x[!is.na(x)]
          if (length(x) < 2) return(data.frame(Variable = nm, N = length(x),
                                               Mediana = NA, IQR = NA, Normal = NA))
          norm_p <- if (length(x) >= 3 && length(x) <= 5000)
                      suppressWarnings(stats::shapiro.test(x)$p.value) else NA
          data.frame(
            Variable = nm,
            N        = length(x),
            Mediana  = round(median(x), 2),
            IQR      = sprintf("%.1f - %.1f", quantile(x, 0.25), quantile(x, 0.75)),
            Normal   = ifelse(is.na(norm_p), "--", ifelse(norm_p > 0.05, "si", "no"))
          )
        }))
      }, error = function(e) {
        message("[dashboard] stats_panel error: ", conditionMessage(e))
        data.frame(Mensaje = paste("Error al calcular estadisticas:",
                                   conditionMessage(e)))
      })
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ---- KM survival ------------------------------------------------------
    output$p_km <- shiny::renderPlot({
      tryCatch({
        u <- user(); if (is.null(u)) return(plot_empty("Sin sesion"))
        df <- db_read(pool, u, "
          WITH dx AS (
            SELECT hospital_id, mrn, MIN(encounter_date) AS fdx
              FROM encounters WHERE encounter_type='initial_dx'
              GROUP BY hospital_id, mrn),
          last AS (
            SELECT hospital_id, mrn,
                   MAX(encounter_date) AS flast,
                   BOOL_OR(vital_status='muerto') AS dead,
                   MAX(death_date) AS fdeath
              FROM encounters GROUP BY hospital_id, mrn)
          SELECT pi.sexo,
                 COALESCE(last.fdeath, last.flast) - dx.fdx AS days,
                 CASE WHEN last.dead THEN 1 ELSE 0 END AS event
            FROM patient_identifiers pi
            JOIN dx   USING (hospital_id, mrn)
            LEFT JOIN last USING (hospital_id, mrn)")
        if (is.null(df) || nrow(df) < 5) {
          return(plot_empty("Datos insuficientes para curva KM (>= 5 pacientes)"))
        }
        df$days  <- as.numeric(df$days)
        df$event <- as.integer(df$event)
        df <- df[df$days > 0 & !is.na(df$days), ]
        if (nrow(df) < 5) return(plot_empty("Datos insuficientes para curva KM"))
        fit <- survival::survfit(survival::Surv(days/30.44, event) ~ sexo, data = df)
        survminer::ggsurvplot(fit, data = df, pval = TRUE, conf.int = FALSE,
                              risk.table = FALSE, xlab = "Meses",
                              ylab = "Probabilidad de supervivencia",
                              ggtheme = ggplot2::theme_minimal())$plot
      }, error = function(e) {
        message("[dashboard] p_km error: ", conditionMessage(e))
        plot_empty(paste("Error KM:", conditionMessage(e)))
      })
    })

    # ---- Time to recurrence -----------------------------------------------
    output$p_ttr <- plotly::renderPlotly({
      tryCatch({
        u <- user(); if (is.null(u)) return(plot_empty_ly("Sin sesion"))
        df <- db_read(pool, u, "
          WITH dx AS (
            SELECT hospital_id, mrn, MIN(encounter_date) AS fdx, oncotree
              FROM encounters WHERE encounter_type='initial_dx'
              GROUP BY hospital_id, mrn, oncotree),
          rec AS (
            SELECT hospital_id, mrn, MIN(encounter_date) AS frec
              FROM encounters WHERE encounter_type='recurrence'
              GROUP BY hospital_id, mrn)
          SELECT dx.oncotree,
                 (rec.frec - dx.fdx)::int AS days_to_recurrence
            FROM dx JOIN rec USING (hospital_id, mrn)
           WHERE rec.frec > dx.fdx")
        if (is.null(df) || nrow(df) == 0) {
          return(plot_empty_ly("Sin recurrencias registradas aun"))
        }
        df$months <- df$days_to_recurrence / 30.44
        p <- ggplot2::ggplot(df, ggplot2::aes(x = oncotree, y = months)) +
          ggplot2::geom_violin(fill = "#1f78b4", alpha = 0.6) +
          ggplot2::geom_jitter(width = 0.1, alpha = 0.5, size = 1) +
          ggplot2::labs(x = NULL, y = "Meses al primer recurrencia") +
          ggplot2::theme_minimal() +
          ggplot2::coord_flip()
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_ttr error: ", conditionMessage(e))
        plot_empty_ly(paste("Error tiempo a recurrencia:", conditionMessage(e)))
      })
    })

    # ---- Stage distribution -----------------------------------------------
    output$p_stage <- plotly::renderPlotly({
      tryCatch({
        d <- filtered(); if (is.null(d) || nrow(d) == 0) return(plot_empty_ly("Sin datos"))
        d$year <- format(as.Date(d$encounter_date), "%Y")
        tbl <- as.data.frame(table(year = d$year, stage = d$tnm_t))
        if (nrow(tbl) == 0) return(plot_empty_ly("Sin datos de estadificacion"))
        p <- ggplot2::ggplot(tbl, ggplot2::aes(x = year, y = Freq, fill = stage)) +
          ggplot2::geom_col(position = "fill") +
          ggplot2::scale_y_continuous(labels = scales::percent) +
          ggplot2::labs(x = NULL, y = NULL, fill = "T") +
          ggplot2::theme_minimal()
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_stage error: ", conditionMessage(e))
        plot_empty_ly(paste("Error estadio:", conditionMessage(e)))
      })
    })

    # ---- New cases time series --------------------------------------------
    output$p_ts <- plotly::renderPlotly({
      tryCatch({
        d <- filtered(); if (is.null(d) || nrow(d) == 0) return(plot_empty_ly("Sin datos"))
        d$mo <- format(as.Date(d$encounter_date), "%Y-%m-01")
        tbl <- as.data.frame(table(mo = d$mo))
        if (nrow(tbl) == 0) return(plot_empty_ly("Sin datos"))
        tbl$mo <- as.Date(tbl$mo)
        p <- ggplot2::ggplot(tbl, ggplot2::aes(x = mo, y = Freq)) +
          ggplot2::geom_line(color = "#1f78b4") +
          ggplot2::geom_point(color = "#1f78b4") +
          ggplot2::labs(x = NULL, y = "Casos nuevos") +
          ggplot2::theme_minimal()
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_ts error: ", conditionMessage(e))
        plot_empty_ly(paste("Error serie temporal:", conditionMessage(e)))
      })
    })

    # ---- Sankey: dx -> tx -> outcome --------------------------------------
    output$p_sankey <- plotly::renderPlotly({
      tryCatch({
        d <- filtered(); if (is.null(d) || nrow(d) == 0) return(plot_empty_ly("Sin datos"))
        chemo <- if (is.null(d$chemo)) FALSE else !is.na(d$chemo) & as.logical(d$chemo)
        radio <- if (is.null(d$radio)) FALSE else !is.na(d$radio) & as.logical(d$radio)
        d$tx <- ifelse(chemo & radio, "Quimio+Radio",
                ifelse(chemo, "Quimio",
                ifelse(radio, "Radio", "Sin tratamiento")))
        d$outcome <- ifelse(is.na(d$vital_status), "Vivo (sin seguimiento)", d$vital_status)
        flows1 <- as.data.frame(table(source = d$oncotree, target = d$tx))
        flows2 <- as.data.frame(table(source = d$tx, target = d$outcome))
        flows  <- rbind(flows1, flows2)
        flows  <- flows[flows$Freq > 0, ]
        if (nrow(flows) == 0) return(plot_empty_ly("Sin trayectorias para mostrar"))
        nodes  <- unique(c(as.character(flows$source), as.character(flows$target)))
        idx    <- function(x) match(as.character(x), nodes) - 1L
        plotly::plot_ly(type = "sankey", orientation = "h",
          node = list(label = nodes, pad = 15, thickness = 18,
                      line = list(color = "black", width = 0.5)),
          link = list(source = idx(flows$source), target = idx(flows$target),
                      value  = flows$Freq))
      }, error = function(e) {
        message("[dashboard] p_sankey error: ", conditionMessage(e))
        plot_empty_ly(paste("Error Sankey:", conditionMessage(e)))
      })
    })

    # ---- Age pyramid ------------------------------------------------------
    output$p_pyramid <- plotly::renderPlotly({
      tryCatch({
        d <- filtered(); if (is.null(d) || nrow(d) == 0) return(plot_empty_ly("Sin datos"))
        d <- d[!is.na(d$edad_dx) & !is.na(d$sexo), ]
        if (nrow(d) == 0) return(plot_empty_ly("Sin datos de edad/sexo"))
        d$age_bin <- cut(as.numeric(d$edad_dx),
                         breaks = seq(0, 100, by = 10), include.lowest = TRUE)
        tbl <- as.data.frame(table(age_bin = d$age_bin, sexo = d$sexo))
        tbl$Freq <- ifelse(tbl$sexo == "M", -tbl$Freq, tbl$Freq)
        p <- ggplot2::ggplot(tbl, ggplot2::aes(x = age_bin, y = Freq, fill = sexo)) +
          ggplot2::geom_col() +
          ggplot2::scale_fill_manual(values = c(M = "#1f78b4", F = "#e31a1c", Otro = "#999999")) +
          ggplot2::coord_flip() +
          ggplot2::scale_y_continuous(labels = abs) +
          ggplot2::labs(x = "Edad", y = "N", fill = "Sexo") +
          ggplot2::theme_minimal()
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_pyramid error: ", conditionMessage(e))
        plot_empty_ly(paste("Error piramide:", conditionMessage(e)))
      })
    })

    # ---- LOS by procedure -------------------------------------------------
    output$p_los <- plotly::renderPlotly({
      tryCatch({
        d <- filtered(); if (is.null(d) || nrow(d) == 0) return(plot_empty_ly("Sin datos"))
        if (is.null(d$los_days)) return(plot_empty_ly("Sin datos de hospitalizacion"))
        d <- d[!is.na(d$los_days) & d$los_days > 0, ]
        if (nrow(d) == 0) return(plot_empty_ly("Sin datos de hospitalizacion"))
        if (is.null(d$surgery_cpt))
          return(plot_empty_ly("Sin procedimientos registrados"))
        d$proc <- vapply(d$surgery_cpt, function(x)
                           if (length(x)) as.character(x[1]) else NA_character_,
                         character(1))
        d <- d[!is.na(d$proc) & nzchar(d$proc), ]
        if (nrow(d) == 0) return(plot_empty_ly("Sin procedimientos registrados"))
        p <- ggplot2::ggplot(d, ggplot2::aes(x = proc, y = los_days)) +
          ggplot2::geom_boxplot(fill = "#33a02c", alpha = 0.6) +
          ggplot2::coord_flip() +
          ggplot2::labs(x = NULL, y = "Dias") +
          ggplot2::theme_minimal()
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_los error: ", conditionMessage(e))
        plot_empty_ly(paste("Error LOS:", conditionMessage(e)))
      })
    })

    # ---- Mexico choropleth ------------------------------------------------
    output$p_map <- plotly::renderPlotly({
      tryCatch({
        if (!requireNamespace("mxmaps", quietly = TRUE))
          return(plot_empty_ly("Mapa no disponible (mxmaps no instalado)"))
        u <- user(); if (is.null(u)) return(plot_empty_ly("Sin sesion"))
        df <- db_read(pool, u, "
          SELECT pi.estado_n AS state_name, COUNT(*) AS n
            FROM patient_identifiers pi
            JOIN encounters e USING (hospital_id, mrn)
           WHERE e.encounter_type = 'initial_dx'
           GROUP BY pi.estado_n")
        if (is.null(df) || nrow(df) == 0) return(plot_empty_ly("Sin datos"))
        d2 <- merge(mxmaps::df_mxstate_2020, df, by = "state_name", all.x = TRUE)
        d2$value <- d2$n
        p <- mxmaps::mxstate_choropleth(d2[, c("region","value")])
        plotly::ggplotly(p)
      }, error = function(e) {
        message("[dashboard] p_map error: ", conditionMessage(e))
        plot_empty_ly(paste("Error mapa:", conditionMessage(e)))
      })
    })
  })
}

# ---- helpers ---------------------------------------------------------------

plot_empty <- function(msg) {
  ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 5, color = "gray40")
}
plot_empty_ly <- function(msg) plotly::ggplotly(plot_empty(msg))
