#' Shared encounter-form module.
#'
#' Used by:
#'   * mod_register_new   -> encounter_type = 'initial_dx'
#'   * mod_followup_search -> encounter_type in ('recurrence','treatment','followup','death')
#'
#' Conditional sections show/hide based on encounter_type.

# ---- UI ---------------------------------------------------------------------

mod_encounter_form_ui <- function(id, allowed_types = c("initial_dx","recurrence",
                                                        "treatment","followup","death")) {
  ns <- shiny::NS(id)
  type_labels <- c(
    initial_dx  = "Diagnóstico inicial",
    recurrence  = "Recurrencia",
    treatment   = "Nuevo tratamiento",
    followup    = "Seguimiento clínico",
    death       = "Defunción"
  )

  type_selector <- if (length(allowed_types) == 1L) {
    shiny::tagList(
      shiny::tags$div(
        class = "alert alert-primary py-2 mb-2 text-center",
        shiny::tags$strong(unname(type_labels[allowed_types]))
      ),
      shinyjs::hidden(
        shiny::textInput(ns("encounter_type"), label = NULL,
                         value = allowed_types[1])
      )
    )
  } else {
    shinyWidgets::radioGroupButtons(
      ns("encounter_type"), label = NULL,
      choices  = type_labels[allowed_types],
      selected = allowed_types[1],
      size     = "sm", justified = TRUE
    )
  }

  shiny::tagList(
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("notes-medical"), " Tipo de evento"),
      width = 12, collapsible = FALSE, status = "primary", solidHeader = TRUE,
      type_selector,
      shiny::dateInput(ns("encounter_date"), "Fecha del evento",
                       value = Sys.Date(), max = Sys.Date(), language = "es")
    ),

    # ---- Diagnosis context (initial_dx only) ---------------------------
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'initial_dx'", ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("magnifying-glass-chart"),
                               " Contexto del diagnóstico"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(4,
            shiny::dateInput(ns("first_symptom_date"), "Fecha del primer síntoma",
                             value = NULL, max = Sys.Date(), language = "es")),
          shiny::column(4,
            shinyWidgets::pickerInput(ns("dx_method"), "Método diagnóstico",
              choices = c("Biopsia"   = "biopsia",
                          "Citología" = "citologia",
                          "Imagen"    = "imagen",
                          "Clínico"   = "clinico",
                          "Quirúrgico" = "quirurgico"),
              selected = NULL, options = list(`live-search` = TRUE))),
          shiny::column(4,
            shinyWidgets::pickerInput(ns("referral_source"), "Origen de referencia",
              choices = c("Urgencias"       = "urgencias",
                          "Consulta externa" = "consulta",
                          "Referido externo" = "referido",
                          "Tamizaje"        = "tamizaje",
                          "Auto-referido"   = "auto"),
              selected = NULL, options = list(`live-search` = TRUE)))
        ),
        shiny::fluidRow(
          shiny::column(12,
            shinyWidgets::pickerInput(ns("imaging_at_dx"),
              "Imagen al diagnóstico",
              choices = c("TC","RM","PET-CT","US","Gammagrafía","Mamografía",
                          "Endoscopia","Ninguna"),
              multiple = TRUE, options = list(`live-search` = TRUE,
                                              `actions-box` = TRUE)))
        ),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::awesomeCheckbox(ns("family_history_cancer"),
              "Antecedente familiar de cáncer", FALSE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("family_history_cancer")),
              shiny::textInput(ns("family_history_detail"),
                "Detalle (parentesco / sitio)",
                placeholder = "Madre — mama; abuelo — colon"))
          ),
          shiny::column(6,
            shinyWidgets::awesomeCheckbox(ns("prior_cancer"),
              "Cáncer previo", FALSE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("prior_cancer")),
              shiny::textInput(ns("prior_cancer_site"),
                "Sitio del cáncer previo"))
          )
        )
      )
    ),

    # ---- Tumor characterization ----------------------------------------
    shiny::conditionalPanel(
      condition = sprintf("['initial_dx','recurrence'].indexOf(input['%s']) > -1",
                          ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("dna"), " Caracterización del tumor"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::selectizeInput(ns("primary_site"),
                              "Localización anatómica (ICD-O-3)",
                              choices = NULL,
                              options = list(placeholder = "Buscar sitio…")),
        shiny::selectizeInput(ns("oncotree"), "Tipo de cáncer (OncoTree)",
                              choices = NULL),
        shiny::selectizeInput(ns("icdo3_morph"),
                              "Subtipo histológico (ICD-O-3)",
                              choices = NULL),
        shiny::fluidRow(
          shiny::column(12,
            shiny::tags$label(class = "control-label", "Grado tumoral"),
            shinyWidgets::radioGroupButtons(ns("tumor_grade"), label = NULL,
              choices = c("G1","G2","G3","G4","GX"),
              selected = character(0), size = "sm", justified = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(12,
            shiny::tags$label(class = "control-label",
                              "ECOG performance status"),
            shinyWidgets::radioGroupButtons(ns("ecog_ps"), label = NULL,
              choices = c("0","1","2","3","4"),
              selected = character(0), size = "sm", justified = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("hpv"), "VPH",
              choices = c("Desconocido" = "desconocido",
                          "Positivo"    = "positivo",
                          "Negativo"    = "negativo"),
              selected = "desconocido")),
          shiny::column(6, style = "padding-top:30px;",
            shinyWidgets::awesomeCheckbox(ns("bilateral"),
                                          "Tumor bilateral", FALSE))
        )
      ),

    # ---- TNM staging ---------------------------------------------------
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("layer-group"), " Clasificación TNM"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(8,
            shiny::tags$label(class = "control-label", "T (tumor primario)"),
            shinyWidgets::radioGroupButtons(ns("tnm_t"), label = NULL,
              choices = c("TX","Tis","T1","T2","T3","T4"),
              selected = character(0), size = "sm", justified = TRUE)),
          shiny::column(4,
            shiny::tags$label(class = "control-label", "Base T"),
            shinyWidgets::radioGroupButtons(ns("tnm_t_basis"), label = NULL,
              choices = c("Clínico" = "clinico", "Patológico" = "patologico"),
              selected = character(0), size = "sm", justified = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(8,
            shiny::tags$label(class = "control-label", "N (ganglios)"),
            shinyWidgets::radioGroupButtons(ns("tnm_n"), label = NULL,
              choices = c("NX","N0","N1","N2","N3"),
              selected = character(0), size = "sm", justified = TRUE)),
          shiny::column(4,
            shiny::tags$label(class = "control-label", "Base N"),
            shinyWidgets::radioGroupButtons(ns("tnm_n_basis"), label = NULL,
              choices = c("Clínico" = "clinico", "Patológico" = "patologico"),
              selected = character(0), size = "sm", justified = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(8,
            shiny::tags$label(class = "control-label", "M (metástasis)"),
            shinyWidgets::radioGroupButtons(ns("tnm_m"), label = NULL,
              choices = c("MX","M0","M1"),
              selected = character(0), size = "sm", justified = TRUE)),
          shiny::column(4,
            shiny::tags$label(class = "control-label", "Resumen"),
            shiny::div(class = "form-control text-center",
                       style = "font-weight:600;",
                       shiny::textOutput(ns("tnm_str"), inline = TRUE)))
        )
      ),

    # ---- Biomarkers ----------------------------------------------------
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("flask-vial"),
                               " Biomarcadores y marcadores tumorales"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::textAreaInput(ns("biomarkers_raw"),
          "Biomarcadores moleculares — uno por línea, formato \"clave: valor\"",
          rows = 3,
          placeholder = "ER: positivo\nPR: negativo\nHER2: 2+\nKRAS: G12D"),
        shiny::textAreaInput(ns("tumor_markers_raw"),
          "Marcadores tumorales séricos — uno por línea, valor numérico",
          rows = 2,
          placeholder = "CEA: 4.2\nCA125: 35")
      )
    ),

    # ---- Systemic treatment --------------------------------------------
    shiny::conditionalPanel(
      condition = sprintf("['initial_dx','recurrence','treatment'].indexOf(input['%s']) > -1",
                          ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("syringe"), " Tratamiento sistémico"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shinyWidgets::awesomeCheckbox(ns("chemo"), "Quimioterapia", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("chemo")),
          shiny::div(class = "ml-4",
            shinyWidgets::radioGroupButtons(ns("chemo_intent"), "Intención",
              choices = c("Neoadyuvante" = "neoadyuvante",
                          "Adyuvante"    = "adyuvante",
                          "Paliativo"    = "paliativo"),
              selected = character(0), size = "sm", justified = TRUE),
            shiny::selectizeInput(ns("chemo_drugs"), "Medicamentos",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "Buscar fármacos…")),
            shiny::fluidRow(
              shiny::column(6,
                shiny::numericInput(ns("chemo_cycles"), "Número de ciclos",
                                    value = NA, min = 1, max = 50)),
              shiny::column(6,
                shinyWidgets::pickerInput(ns("chemo_response"), "Respuesta",
                  choices = c("Completa"   = "completa",
                              "Parcial"    = "parcial",
                              "Estable"    = "estable",
                              "Progresión" = "progresion"),
                  selected = NULL))
            )
          )
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("hormonal_therapy"),
                                      "Terapia hormonal", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("hormonal_therapy")),
          shiny::div(class = "ml-4",
            shiny::textInput(ns("hormonal_drug"), "Fármaco hormonal",
              placeholder = "Tamoxifeno, letrozol, leuprolide…"))
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("targeted_therapy"),
                                      "Terapia dirigida", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("targeted_therapy")),
          shiny::div(class = "ml-4",
            shiny::textInput(ns("targeted_drug"), "Fármaco dirigido",
              placeholder = "Trastuzumab, imatinib, osimertinib…"))
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("immunotherapy"),
                                      "Inmunoterapia", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("immunotherapy")),
          shiny::div(class = "ml-4",
            shiny::textInput(ns("immuno_drug"), "Fármaco de inmunoterapia",
              placeholder = "Pembrolizumab, nivolumab, atezolizumab…"))
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("radio"), "Radioterapia", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("radio")),
          shiny::div(class = "ml-4",
            shiny::numericInput(ns("radio_dose_gy"), "Dosis total (Gy)",
                                value = NA, min = 0, step = 0.5))
        )
      ),

    # ---- Surgery -------------------------------------------------------
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("hospital"), " Cirugía"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::selectizeInput(ns("surgery_cpt"), "Procedimiento (CPT)",
                              choices = NULL, multiple = TRUE,
                              options = list(placeholder = "Buscar procedimiento…")),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("surgery_intent"), "Intención",
              choices = c("Curativa"   = "curativa",
                          "Paliativa"  = "paliativa",
                          "Diagnóstica" = "diagnostica"),
              selected = NULL)),
          shiny::column(6,
            shinyWidgets::pickerInput(ns("surgery_margin"),
              "Márgenes quirúrgicos",
              choices = c("R0","R1","R2"),
              selected = NULL))
        ),
        shiny::fluidRow(
          shiny::column(6,
            shiny::dateInput(ns("surgery_date"), "Fecha de cirugía",
                             value = NULL, max = Sys.Date(), language = "es")),
          shiny::column(6,
            shiny::dateInput(ns("discharge_date"), "Fecha de alta",
                             value = NULL, max = Sys.Date(), language = "es"))
        ),
        shiny::fluidRow(
          shiny::column(4,
            shiny::numericInput(ns("lymph_nodes_examined"),
                                "Ganglios examinados",
                                value = NA, min = 0, max = 200)),
          shiny::column(4,
            shiny::numericInput(ns("lymph_nodes_positive"),
                                "Ganglios positivos",
                                value = NA, min = 0, max = 200)),
          shiny::column(4,
            shinyWidgets::pickerInput(ns("complication"), "Complicación",
              choices = c("Ninguna" = "ninguna",
                          "Menor"   = "menor",
                          "Mayor"   = "mayor"),
              selected = "ninguna"))
        )
      )
    ),

    # ---- Vital status (followup, death) --------------------------------
    shiny::conditionalPanel(
      condition = sprintf("['followup','death'].indexOf(input['%s']) > -1",
                          ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("heart-pulse"), " Estado vital"),
        width = 12, collapsible = FALSE, status = "warning", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(4,
            shinyWidgets::radioGroupButtons(ns("vital_status"), "Estado",
              choices = c("Vivo"    = "vivo",
                          "Muerto"  = "muerto",
                          "Perdido" = "perdido"),
              selected = character(0), size = "sm", justified = TRUE)
          ),
          shiny::column(4,
            shiny::dateInput(ns("death_date"), "Fecha de defunción",
                             value = NULL, max = Sys.Date(), language = "es")
          ),
          shiny::column(4,
            shiny::textInput(ns("death_cause"), "Causa")
          )
        )
      )
    ),

    bs4Dash::box(
      title = shiny::tagList(shiny::icon("note-sticky"), " Notas"),
      width = 12, collapsible = TRUE, status = "secondary", solidHeader = TRUE,
      shiny::textAreaInput(ns("notes"), NULL, rows = 3,
                           placeholder = "Observaciones libres")
    )
  )
}

# ---- Server -----------------------------------------------------------------

mod_encounter_form_server <- function(id, patient = function() NULL,
                                      pool = NULL, user = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate selectizes server-side (memory-friendly).
    shiny::observe({
      shiny::updateSelectizeInput(session, "primary_site",
        choices = lookup_sites(), server = TRUE)
      shiny::updateSelectizeInput(session, "oncotree",
        choices = lookup_oncotree(), server = TRUE)
      icdo3 <- lookup_icdo3()
      if ("Histology.Behavior.Description" %in% names(icdo3)) {
        shiny::updateSelectizeInput(session, "icdo3_morph",
          choices = sort(unique(icdo3$Histology.Behavior.Description)),
          server = TRUE)
      }
      drugs <- lookup_drugs()
      if ("x" %in% names(drugs)) {
        shiny::updateSelectizeInput(session, "chemo_drugs",
          choices = sort(drugs$x), server = TRUE)
      }
      cpt <- lookup_cpt()
      if ("PROCEDURE.DESCRIPTION" %in% names(cpt)) {
        shiny::updateSelectizeInput(session, "surgery_cpt",
          choices = sort(cpt$PROCEDURE.DESCRIPTION), server = TRUE)
      }
    })

    # Live TNM string.
    output$tnm_str <- shiny::renderText({
      paste0(input$tnm_t %||% "?", input$tnm_n %||% "?", input$tnm_m %||% "?")
    })

    iv <- make_encounter_validator(input, patient)
    iv$enable()

    # Build the row to insert. Returns NULL if invalid.
    values <- shiny::reactive({
      if (!iv$is_valid()) return(NULL)
      u <- if (is.function(user)) user() else user
      p <- if (is.function(patient)) patient() else patient
      list(
        hospital_id    = u$hospital_id,
        mrn            = p$mrn %||% input$mrn,
        encounter_type = input$encounter_type,
        encounter_date = input$encounter_date,

        tnm_t          = nz(input$tnm_t),
        tnm_n          = nz(input$tnm_n),
        tnm_m          = nz(input$tnm_m),
        tnm_t_basis    = nz(input$tnm_t_basis),
        tnm_n_basis    = nz(input$tnm_n_basis),
        primary_site   = nz(input$primary_site),
        oncotree       = nz(input$oncotree),
        icdo3_morph    = nz(input$icdo3_morph),
        bilateral      = isTRUE(input$bilateral),
        tumor_grade    = nz(input$tumor_grade),
        ecog_ps        = as_int(input$ecog_ps),
        hpv            = nz(input$hpv),

        dx_method             = nz(input$dx_method),
        imaging_at_dx         = if (length(input$imaging_at_dx)) input$imaging_at_dx else NA,
        biomarkers            = parse_kv_json(input$biomarkers_raw,    numeric_values = FALSE),
        tumor_markers         = parse_kv_json(input$tumor_markers_raw, numeric_values = TRUE),
        family_history_cancer = isTRUE(input$family_history_cancer),
        family_history_detail = nz(input$family_history_detail),
        prior_cancer          = isTRUE(input$prior_cancer),
        prior_cancer_site     = nz(input$prior_cancer_site),
        first_symptom_date    = as_date(input$first_symptom_date),
        referral_source       = nz(input$referral_source),

        chemo            = isTRUE(input$chemo),
        chemo_intent     = nz(input$chemo_intent),
        chemo_drugs      = if (length(input$chemo_drugs)) input$chemo_drugs else NA,
        chemo_cycles     = as_int(input$chemo_cycles),
        chemo_response   = nz(input$chemo_response),
        radio            = isTRUE(input$radio),
        radio_dose_gy    = as_num(input$radio_dose_gy),
        hormonal_therapy = isTRUE(input$hormonal_therapy),
        hormonal_drug    = nz(input$hormonal_drug),
        targeted_therapy = isTRUE(input$targeted_therapy),
        targeted_drug    = nz(input$targeted_drug),
        immunotherapy    = isTRUE(input$immunotherapy),
        immuno_drug      = nz(input$immuno_drug),

        surgery_cpt          = if (length(input$surgery_cpt)) input$surgery_cpt else NA,
        surgery_intent       = nz(input$surgery_intent),
        surgery_margin       = nz(input$surgery_margin),
        lymph_nodes_examined = as_int(input$lymph_nodes_examined),
        lymph_nodes_positive = as_int(input$lymph_nodes_positive),
        surgery_date         = as_date(input$surgery_date),
        discharge_date       = as_date(input$discharge_date),
        complication         = nz(input$complication),

        vital_status   = nz(input$vital_status),
        death_date     = as_date(input$death_date),
        death_cause    = nz(input$death_cause),
        notes          = nz(input$notes),
        created_by     = u$user_id
      )
    })

    reset <- function() {
      shinyjs::reset(session$ns(""))
    }

    list(values = values, iv = iv, reset = reset, input = input)
  })
}

# ----- typing helpers --------------------------------------------------------
nz       <- function(x) if (is.null(x) || !nzchar(as.character(x))) NA else x
as_int   <- function(x) if (is.null(x) || is.na(suppressWarnings(as.integer(x)))) NA_integer_ else as.integer(x)
as_num   <- function(x) if (is.null(x) || is.na(suppressWarnings(as.numeric(x)))) NA_real_   else as.numeric(x)
as_date  <- function(x) if (is.null(x) || is.na(x)) NA         else as.Date(x)

#' Parse a "key: value\nkey: value" textarea into a JSON object string.
#' Returns NA if input is empty.
parse_kv_json <- function(raw, numeric_values = FALSE) {
  if (is.null(raw) || !nzchar(raw)) return(NA)
  lines <- strsplit(raw, "[\r\n]+")[[1]]
  lines <- trimws(lines); lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NA)
  out <- list()
  for (ln in lines) {
    pair <- strsplit(ln, ":", fixed = TRUE)[[1]]
    if (length(pair) < 2) next
    k <- trimws(pair[1])
    v <- trimws(paste(pair[-1], collapse = ":"))
    if (numeric_values) {
      vn <- suppressWarnings(as.numeric(v))
      out[[k]] <- if (is.na(vn)) v else vn
    } else {
      out[[k]] <- v
    }
  }
  if (!length(out)) return(NA)
  jsonlite::toJSON(out, auto_unbox = TRUE)
}
