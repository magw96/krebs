#' Shared encounter-form module.
#'
#' Used by:
#'   * mod_register_new   -> encounter_type = 'initial_dx'
#'   * mod_followup_search -> encounter_type in ('recurrence','treatment','followup','death')
#'
#' All Spanish accents in this file are written as \uXXXX escapes so the file
#' parses identically under any locale (PCC sometimes runs in C / POSIX).
#' All radio inputs use shiny::radioButtons(inline = TRUE) -- native HTML
#' <input type=radio>, BS-version-agnostic -- instead of
#' shinyWidgets::radioGroupButtons (BS3-only, broken under bs4Dash).

# ---- UI ---------------------------------------------------------------------

mod_encounter_form_ui <- function(id, allowed_types = c("initial_dx","recurrence",
                                                        "treatment","followup","death")) {
  ns <- shiny::NS(id)
  type_labels <- c(
    initial_dx  = "Diagnostico inicial",
    recurrence  = "Recurrencia",
    treatment   = "Nuevo tratamiento",
    followup    = "Seguimiento clinico",
    death       = "Defuncion"
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
    shiny::radioButtons(ns("encounter_type"), label = NULL,
      choices  = type_labels[allowed_types],
      selected = allowed_types[1],
      inline   = TRUE)
  }

  shiny::tagList(
    # Autosave-draft restore callout (filled in by the server when a draft
    # exists for this user x patient x encounter_type).
    shiny::uiOutput(ns("draft_callout")),

    # ---- Caracteristicas al diagnostico (merged tipo + contexto) -------
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("magnifying-glass-chart"),
                             " Caracteristicas al diagnostico"),
      width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
      type_selector,
      shiny::fluidRow(
        shiny::column(3,
          shiny::dateInput(ns("encounter_date"), "Fecha del evento",
                           value = Sys.Date(), max = Sys.Date(),
                           language = "es")),
        shiny::column(3,
          shinyWidgets::pickerInput(ns("dx_method"), "Metodo diagnostico",
            choices = c("Biopsia"      = "biopsia",
                        "Citologia"    = "citologia",
                        "Imagen"       = "imagen",
                        "Clinico"      = "clinico",
                        "Quirurgico"   = "quirurgico"),
            selected = NULL, options = list(`live-search` = TRUE))),
        shiny::column(3,
          shinyWidgets::pickerInput(ns("referral_source"), "Origen de referencia",
            choices = c("Urgencias"        = "urgencias",
                        "Consulta externa" = "consulta",
                        "Referido externo" = "referido",
                        "Tamizaje"         = "tamizaje",
                        "Auto-referido"    = "auto"),
            selected = NULL, options = list(`live-search` = TRUE))),
        shiny::column(3,
          shiny::radioButtons(ns("ecog_ps"),
            label = shiny::tagList(
              "ECOG performance status ",
              shiny::tags$span(
                `data-bs-toggle` = "tooltip",
                `data-bs-placement` = "right",
                `data-bs-html` = "true",
                title = paste(
                  "0 = Asintomatico, totalmente activo.<br>",
                  "1 = Sintomas leves, ambulatorio, trabajo ligero.<br>",
                  "2 = Ambulatorio &gt;50% del dia, no trabaja.<br>",
                  "3 = Encamado o en silla &gt;50% del dia.<br>",
                  "4 = Completamente discapacitado, encamado.<br>",
                  "5 = Muerto."),
                style = "cursor:help; color:#0d6efd;",
                shiny::icon("circle-info"))
            ),
            choices = c("0","1","2","3","4"),
            selected = character(0), inline = TRUE))
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'initial_dx'", ns("encounter_type")),
        shiny::fluidRow(
          shiny::column(12,
            shinyWidgets::pickerInput(ns("imaging_at_dx"),
              "Imagen al diagnostico",
              choices = c("TC","RM","PET-CT","US",
                          "Gammagrafia","Mamografia",
                          "Endoscopia","Ninguna"),
              multiple = TRUE, options = list(`live-search` = TRUE,
                                              `actions-box` = TRUE)))
        ),
        # Family history: pick relative first (unlocks the cancer-type
        # field). Limits free-text and structures the input.
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("family_history_relative"),
              "Antecedente familiar de cancer (parentesco)",
              choices = c("Sin antecedente"     = "",
                          "Padre"               = "padre",
                          "Madre"               = "madre",
                          "Hermano(a)"          = "hermano",
                          "Hijo(a)"             = "hijo",
                          "Abuelo(a) paterno"   = "abuelo_p",
                          "Abuelo(a) materno"   = "abuelo_m",
                          "Tio(a) paterno"      = "tio_p",
                          "Tio(a) materno"      = "tio_m",
                          "Otro"                = "otro"),
              selected = ""),
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] && input['%s'] !== ''",
                                  ns("family_history_relative"),
                                  ns("family_history_relative")),
              shiny::textInput(ns("family_history_detail"),
                "Tipo de cancer del familiar",
                placeholder = "Ej: mama, colon, prostata"))
          ),
          shiny::column(6,
            shinyWidgets::awesomeCheckbox(ns("prior_cancer"),
              "Cancer previo", FALSE),
            shiny::conditionalPanel(
              condition = sprintf("input['%s']", ns("prior_cancer")),
              shiny::textInput(ns("prior_cancer_site"),
                "Sitio del cancer previo"))
          )
        )
      )
    ),

    # ---- Tumor characterization ----------------------------------------
    shiny::conditionalPanel(
      condition = sprintf("['initial_dx','recurrence'].indexOf(input['%s']) > -1",
                          ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("dna"),
                               " Caracterizacion del tumor"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::selectizeInput(ns("primary_site"),
                              "Localizacion anatomica (ICD-O-3)",
                              choices = NULL,
                              options = list(placeholder = "Buscar sitio...")),
        shiny::selectizeInput(ns("oncotree"), "Tipo de cancer (OncoTree)",
                              choices = NULL),
        shiny::selectizeInput(ns("icdo3_morph"),
                              "Subtipo histologico (ICD-O-3)",
                              choices = NULL),
        shiny::fluidRow(
          shiny::column(12, style = "padding-top:8px;",
            shinyWidgets::awesomeCheckbox(ns("bilateral"),
                                          "Tumor bilateral", FALSE))
        )
      ),

    # ---- TNM staging ---------------------------------------------------
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("layer-group"),
                               " Clasificacion TNM"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(8,
            shiny::radioButtons(ns("tnm_t"), "T (tumor primario)",
              choices = c("TX","Tis","T1","T2","T3","T4"),
              selected = character(0), inline = TRUE)),
          shiny::column(4,
            shiny::radioButtons(ns("tnm_t_basis"), "Base T",
              choices = c("Clinico" = "clinico",
                          "Patologico" = "patologico"),
              selected = character(0), inline = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(8,
            shiny::radioButtons(ns("tnm_n"), "N (ganglios)",
              choices = c("NX","N0","N1","N2","N3"),
              selected = character(0), inline = TRUE)),
          shiny::column(4,
            shiny::radioButtons(ns("tnm_n_basis"), "Base N",
              choices = c("Clinico" = "clinico",
                          "Patologico" = "patologico"),
              selected = character(0), inline = TRUE))
        ),
        shiny::fluidRow(
          shiny::column(8,
            shiny::radioButtons(ns("tnm_m"), "M (metastasis)",
              choices = c("MX","M0","M1"),
              selected = character(0), inline = TRUE)),
          shiny::column(4,
            shiny::tags$label(class = "control-label", "Resumen"),
            shiny::div(class = "form-control text-center",
                       style = "font-weight:600;",
                       shiny::textOutput(ns("tnm_str"), inline = TRUE)))
        )
      ),

    # ---- Cancer-specific compartment (driven by OncoTree pick) ---------
      shiny::uiOutput(ns("cancer_specific")),

    # ---- Biomarkers (fallback only - structured panel is above) --------
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("flask-vial"),
                               " Biomarcadores adicionales (opcional)"),
        width = 12, collapsible = TRUE, collapsed = TRUE,
        status = "secondary", solidHeader = TRUE,
        shiny::helpText(shiny::icon("circle-info"),
          " Use este espacio SOLO para hallazgos no cubiertos por el panel ",
          "molecular/patologico de arriba (ej. mutaciones raras, paneles NGS ",
          "ampliados). Los biomarcadores estandar ya estan estructurados arriba."),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("extra_marker_key"),
              "Biomarcador",
              choices = c("",
                "ATM","BRCA1","BRCA2","PALB2","CHEK2","RAD51",
                "TP53","RB1","PTEN","APC","SMAD4","CDKN2A","STK11",
                "MYC","NOTCH1","NOTCH2","EZH2","KMT2D","ARID1A",
                "MET","RET","NTRK1","NTRK2","NTRK3","FGFR1","FGFR3",
                "PIK3CA","AKT1","mTOR","ESR1","AR-V7",
                "TMB (mut/Mb)","HRD score","LOH (%)","BRCAness",
                "Otro (ver notas)"),
              options = list(`live-search` = TRUE,
                             `none-selected-text` = "Seleccione marcador"))),
          shiny::column(6,
            shiny::textInput(ns("extra_marker_value"),
              "Resultado",
              placeholder = "mutado / wildtype / valor numerico"))
        ),
        shiny::actionButton(ns("extra_marker_add"),
          shiny::tagList(shiny::icon("plus"), " Agregar"),
          class = "btn btn-sm btn-outline-primary"),
        shiny::hr(),
        shiny::tags$label("Lista capturada"),
        shiny::verbatimTextOutput(ns("extra_markers_preview"),
                                  placeholder = TRUE),
        # Hidden mirror of the captured list (the textarea form keeps the
        # existing merge_biomarkers_json() pipeline working unchanged).
        shinyjs::hidden(
          shiny::textAreaInput(ns("biomarkers_raw"), NULL, value = "", rows = 1)
        ),
        shiny::hr(),
        shiny::tags$label("Marcadores tumorales sericos (opcional)"),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("tm_cea"),
            "CEA (ng/mL)", value = NA, min = 0, step = 0.1)),
          shiny::column(4, shiny::numericInput(ns("tm_ca125"),
            "CA-125 (U/mL)", value = NA, min = 0, step = 1)),
          shiny::column(4, shiny::numericInput(ns("tm_ca199"),
            "CA 19-9 (U/mL)", value = NA, min = 0, step = 1))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("tm_ca153"),
            "CA 15-3 (U/mL)", value = NA, min = 0, step = 1)),
          shiny::column(4, shiny::numericInput(ns("tm_afp"),
            "AFP (ng/mL)", value = NA, min = 0, step = 0.1)),
          shiny::column(4, shiny::numericInput(ns("tm_bhcg"),
            "beta-hCG (mUI/mL)", value = NA, min = 0, step = 0.1))
        ),
        shinyjs::hidden(
          shiny::textAreaInput(ns("tumor_markers_raw"), NULL, value = "", rows = 1)
        )
      )
    ),

    # ---- Systemic treatment --------------------------------------------
    shiny::conditionalPanel(
      condition = sprintf("['initial_dx','recurrence','treatment'].indexOf(input['%s']) > -1",
                          ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("syringe"),
                               " Tratamiento sistemico"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shinyWidgets::awesomeCheckbox(ns("chemo"), "Quimioterapia", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("chemo")),
          shiny::div(class = "ml-4",
            shiny::radioButtons(ns("chemo_intent"), "Intencion",
              choices = c("Neoadyuvante" = "neoadyuvante",
                          "Adyuvante"    = "adyuvante",
                          "Paliativo"    = "paliativo"),
              selected = character(0), inline = TRUE),
            shiny::selectizeInput(ns("chemo_drugs"), "Medicamentos",
              choices = NULL, multiple = TRUE,
              options = list(placeholder = "Buscar farmacos...")),
            shiny::fluidRow(
              shiny::column(6,
                shiny::numericInput(ns("chemo_cycles"), "Numero de ciclos",
                                    value = NA, min = 1, max = 50)),
              shiny::column(6,
                shinyWidgets::pickerInput(ns("chemo_response"), "Respuesta",
                  choices = c("Completa"           = "completa",
                              "Parcial"            = "parcial",
                              "Estable"            = "estable",
                              "Progresion"    = "progresion"),
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
            shinyWidgets::pickerInput(ns("hormonal_drug"),
              "Farmaco(s) hormonal(es)",
              choices = character(0), multiple = TRUE,
              options = list(`live-search` = TRUE,
                             `actions-box` = TRUE,
                             `selected-text-format` = "count > 2",
                             `none-selected-text` = "Buscar farmaco...",
                             size = 10)),
            shiny::textInput(ns("hormonal_drug_other"),
              "Otro (texto libre, opcional)",
              placeholder = "Escribe si no aparece arriba"))
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("targeted_therapy"),
                                      "Terapia dirigida", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("targeted_therapy")),
          shiny::div(class = "ml-4",
            shinyWidgets::pickerInput(ns("targeted_drug"),
              "Farmaco(s) dirigido(s)",
              choices = character(0), multiple = TRUE,
              options = list(`live-search` = TRUE,
                             `actions-box` = TRUE,
                             `selected-text-format` = "count > 2",
                             `none-selected-text` = "Buscar farmaco...",
                             size = 10)),
            shiny::textInput(ns("targeted_drug_other"),
              "Otro (texto libre, opcional)",
              placeholder = "Escribe si no aparece arriba"))
        ),
        shiny::hr(),
        shinyWidgets::awesomeCheckbox(ns("immunotherapy"),
                                      "Inmunoterapia", FALSE),
        shiny::conditionalPanel(
          condition = sprintf("input['%s']", ns("immunotherapy")),
          shiny::div(class = "ml-4",
            shinyWidgets::pickerInput(ns("immuno_drug"),
              "Farmaco(s) de inmunoterapia",
              choices = character(0), multiple = TRUE,
              options = list(`live-search` = TRUE,
                             `actions-box` = TRUE,
                             `selected-text-format` = "count > 2",
                             `none-selected-text` = "Buscar farmaco...",
                             size = 10)),
            shiny::textInput(ns("immuno_drug_other"),
              "Otro (texto libre, opcional)",
              placeholder = "Escribe si no aparece arriba"))
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
        title = shiny::tagList(shiny::icon("hospital"), " Cirugia"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
        shinyWidgets::pickerInput(ns("surgery_cpt"),
          "Procedimiento oncologico",
          choices = lookup_oncology_procedures(),
          multiple = TRUE,
          options = list(`live-search` = TRUE,
                         `actions-box` = TRUE,
                         `selected-text-format` = "count > 2",
                         `none-selected-text` = "Buscar procedimiento...",
                         `live-search-placeholder` = "Buscar...",
                         size = 12)),
        shiny::textInput(ns("surgery_other"),
          "Otro procedimiento (texto libre, opcional)",
          placeholder = "Si no aparece arriba, descrribalo aqui"),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("surgery_intent"), "Intencion",
              choices = c("Curativa"            = "curativa",
                          "Paliativa"           = "paliativa",
                          "Diagnostica"    = "diagnostica"),
              selected = NULL)),
          shiny::column(6,
            shinyWidgets::pickerInput(ns("surgery_margin"),
              "Margenes quirurgicos",
              choices = c("R0","R1","R2"),
              selected = NULL))
        ),
        shiny::fluidRow(
          shiny::column(6,
            shiny::dateInput(ns("surgery_date"), "Fecha de cirugia",
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
            shinyWidgets::pickerInput(ns("complication"), "Complicacion",
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
            shiny::radioButtons(ns("vital_status"), "Estado",
              choices = c("Vivo"    = "vivo",
                          "Muerto"  = "muerto",
                          "Perdido" = "perdido"),
              selected = character(0), inline = TRUE)
          ),
          shiny::column(4,
            shiny::dateInput(ns("death_date"), "Fecha de defuncion",
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

    # User helper -- coerce reactive / scalar / NULL to a list with $user_id.
    .u <- function() if (is.function(user)) user() else user
    .has_pool <- function() !is.null(pool) && inherits(pool, c("Pool","R6"))

    # Resolve recent-values lists once per session (cheap query). Wrapped in a
    # reactive so they refresh after submit if needed.
    recents <- shiny::reactive({
      u <- .u()
      if (is.null(u) || !.has_pool()) return(list())
      out <- list()
      for (k in c("oncotree","primary_site","icdo3_morph",
                  "chemo_drugs","surgery_cpt")) {
        out[[k]] <- tryCatch(db_recent_values(pool, u, k, n = 8),
                             error = function(e) character(0))
      }
      out
    })

    # Populate selectizes server-side (memory-friendly), prepending the user's
    # "Recientes" group to the four heaviest pickers.
    shiny::observe({
      r <- recents()
      sites <- lookup_sites()
      shiny::updateSelectizeInput(session, "primary_site",
        choices = .with_recent_optgroup(sites, r$primary_site), server = TRUE)
      onco <- lookup_oncotree()
      shiny::updateSelectizeInput(session, "oncotree",
        choices = .with_recent_optgroup(onco, r$oncotree), server = TRUE)
      icdo3 <- lookup_icdo3()
      morph_col <- intersect(c("Histology.Behavior.Description",
                               "Histology/Behavior Description"), names(icdo3))
      if (!length(morph_col)) {
        morph_col <- grep("histology", names(icdo3),
                          ignore.case = TRUE, value = TRUE)
      }
      if (length(morph_col)) {
        morph <- sort(unique(icdo3[[morph_col[1]]]))
        shiny::updateSelectizeInput(session, "icdo3_morph",
          choices = .with_recent_optgroup(morph, r$icdo3_morph), server = TRUE)
      }
      drugs <- lookup_drugs()
      if ("x" %in% names(drugs)) {
        ch <- sort(drugs$x)
        shiny::updateSelectizeInput(session, "chemo_drugs",
          choices = .with_recent_optgroup(ch, r$chemo_drugs), server = TRUE)
      }
      # Surgery procedures: curated, grouped by anatomic site. We keep
      # recents at the top by prepending a "Recientes" group when present.
      proc <- lookup_oncology_procedures()
      if (length(proc)) {
        recents_proc <- intersect(as.character(r$surgery_cpt %||% character(0)),
                                  unname(unlist(proc)))
        if (length(recents_proc)) {
          proc <- c(list(Recientes = recents_proc), proc)
        }
        shinyWidgets::updatePickerInput(session, "surgery_cpt", choices = proc,
          selected = intersect(input$surgery_cpt %||% character(0),
                               unname(unlist(proc))))
      }
    })

    # Live TNM string.
    output$tnm_str <- shiny::renderText({
      paste0(input$tnm_t %||% "?", input$tnm_n %||% "?", input$tnm_m %||% "?")
    })

    # ---- Extra biomarkers (structured -> hidden textarea) ------------------
    extra_markers <- shiny::reactiveVal(list())

    shiny::observeEvent(input$extra_marker_add, {
      k <- input$extra_marker_key %||% ""
      v <- input$extra_marker_value %||% ""
      if (!nzchar(k) || !nzchar(v)) return()
      cur <- extra_markers()
      cur[[k]] <- trimws(v)
      extra_markers(cur)
      shinyWidgets::updatePickerInput(session, "extra_marker_key", selected = "")
      shiny::updateTextInput(session, "extra_marker_value", value = "")
    })

    output$extra_markers_preview <- shiny::renderText({
      m <- extra_markers()
      if (!length(m)) return("(sin biomarcadores adicionales)")
      paste(sprintf("%s: %s", names(m), unlist(m)), collapse = "\n")
    })

    # Mirror extra_markers + structured tumor markers into the hidden
    # textareas that the existing merge/parse pipeline reads at submit.
    shiny::observe({
      m <- extra_markers()
      txt <- if (length(m))
        paste(sprintf("%s: %s", names(m), unlist(m)), collapse = "\n") else ""
      shiny::updateTextAreaInput(session, "biomarkers_raw", value = txt)
    })

    shiny::observe({
      pairs <- list(
        CEA   = input$tm_cea,   `CA-125` = input$tm_ca125,
        `CA 19-9` = input$tm_ca199, `CA 15-3` = input$tm_ca153,
        AFP   = input$tm_afp,   `beta-hCG` = input$tm_bhcg
      )
      pairs <- pairs[vapply(pairs, function(x)
        !is.null(x) && !is.na(x) && length(x) == 1, logical(1))]
      txt <- if (length(pairs))
        paste(sprintf("%s: %s", names(pairs), unlist(pairs)), collapse = "\n") else ""
      shiny::updateTextAreaInput(session, "tumor_markers_raw", value = txt)
    })

    # Cancer-specific compartment: react to the OncoTree pick.
    cancer_cat <- shiny::reactive({ cancer_category(input$oncotree) })
    output$cancer_specific <- shiny::renderUI({
      cancer_specific_ui(ns, cancer_cat())
    })

    # Refresh the therapy drug pickers whenever the cancer category changes,
    # filtered to the NCCN/ESMO drugs relevant to that category.
    shiny::observe({
      cat <- cancer_cat()
      shinyWidgets::updatePickerInput(session, "hormonal_drug",
        choices = lookup_therapy_drugs(cat, "hormonal"),
        selected = intersect(input$hormonal_drug %||% character(0),
                             lookup_therapy_drugs(cat, "hormonal")))
      shinyWidgets::updatePickerInput(session, "targeted_drug",
        choices = lookup_therapy_drugs(cat, "targeted"),
        selected = intersect(input$targeted_drug %||% character(0),
                             lookup_therapy_drugs(cat, "targeted")))
      shinyWidgets::updatePickerInput(session, "immuno_drug",
        choices = lookup_therapy_drugs(cat, "immuno"),
        selected = intersect(input$immuno_drug %||% character(0),
                             lookup_therapy_drugs(cat, "immuno")))
    })

    # ---- Autosave drafts ---------------------------------------------------
    # Identify the draft slot. mrn = "" for the "register new patient" case
    # before the user has typed an MRN.
    draft_key <- shiny::reactive({
      p <- if (is.function(patient)) patient() else patient
      list(mrn = as.character(p$mrn %||% ""),
           etype = as.character(input$encounter_type %||% ""))
    })

    draft_dismissed <- shiny::reactiveVal(FALSE)
    draft_loaded    <- shiny::reactiveVal(FALSE)

    # Restore-callout: shown when a draft exists for this slot.
    output$draft_callout <- shiny::renderUI({
      if (!.has_pool() || is.null(.u())) return(NULL)
      if (isTRUE(draft_dismissed())) return(NULL)
      k <- draft_key(); if (!nzchar(k$etype)) return(NULL)
      d <- tryCatch(db_load_draft(pool, .u(), k$mrn, k$etype),
                    error = function(e) NULL)
      if (is.null(d)) return(NULL)
      shiny::div(class = "alert alert-info py-2 d-flex align-items-center",
        style = "gap:10px;",
        shiny::icon("clock-rotate-left"),
        shiny::span(sprintf("Hay un borrador guardado de este formulario (%s).",
                            human_ago(d$updated_at))),
        shiny::actionButton(ns("draft_restore"), "Restaurar",
          class = "btn btn-sm btn-primary ml-auto"),
        shiny::actionButton(ns("draft_discard"), "Descartar",
          class = "btn btn-sm btn-outline-secondary"))
    })

    shiny::observeEvent(input$draft_restore, {
      k <- draft_key()
      d <- tryCatch(db_load_draft(pool, .u(), k$mrn, k$etype),
                    error = function(e) NULL)
      if (is.null(d) || is.null(d$payload)) return()
      .restore_inputs(session, d$payload)
      draft_loaded(TRUE)
      draft_dismissed(TRUE)
      shiny::showNotification("Borrador restaurado.",
                              type = "message", duration = 3)
    })

    shiny::observeEvent(input$draft_discard, {
      k <- draft_key()
      try(db_delete_draft(pool, .u(), k$mrn, k$etype), silent = TRUE)
      draft_dismissed(TRUE)
    })

    # Debounced autosave. Snapshots all input values, strips ephemeral keys,
    # and upserts on change. Skipped when no DB / no user / form looks empty.
    draft_payload <- shiny::reactive({
      vals <- shiny::reactiveValuesToList(input)
      # Drop non-form keys
      drop_pat <- "^(submit|reset_form|draft_(restore|discard)|tabs?_)"
      vals[grepl(drop_pat, names(vals))] <- NULL
      vals
    }) |> shiny::debounce(2000)

    shiny::observe({
      if (!.has_pool() || is.null(.u())) return()
      k <- draft_key(); if (!nzchar(k$etype)) return()
      payload <- draft_payload()
      # Skip autosave for empty / brand-new forms (avoids spamming the table
      # the moment a user opens "Registrar paciente").
      if (.payload_is_empty(payload)) return()
      # Cap payload size at ~64 KB to be safe.
      json <- jsonlite::toJSON(payload, auto_unbox = TRUE,
                               null = "null", force = TRUE)
      if (nchar(json) > 65536) return()
      try(db_save_draft(pool, .u(), k$mrn, k$etype, payload), silent = TRUE)
    })

    iv <- make_encounter_validator(input, patient)
    iv$enable()

    # Build the row to insert. Hard requirement: encounter_date. Other rules
    # (death/discharge dates, chemo intent) surface as red text via the
    # validator's UI feedback but do not block submission -- the DB and
    # validate_new_patient() catch real problems.
    values <- shiny::reactive({
      if (is.null(input$encounter_date) || is.na(input$encounter_date) ||
          !nzchar(as.character(input$encounter_date))) return(NULL)
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
        biomarkers            = merge_biomarkers_json(
                                  input$biomarkers_raw,
                                  cancer_specific_values(input, cancer_cat())),
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
        hormonal_drug    = combine_drugs(input$hormonal_drug,
                                         input$hormonal_drug_other),
        targeted_therapy = isTRUE(input$targeted_therapy),
        targeted_drug    = combine_drugs(input$targeted_drug,
                                         input$targeted_drug_other),
        immunotherapy    = isTRUE(input$immunotherapy),
        immuno_drug      = combine_drugs(input$immuno_drug,
                                         input$immuno_drug_other),

        surgery_cpt          = combine_drugs(input$surgery_cpt, input$surgery_other),
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

    # Called by the parent module after a successful db_insert: drops the
    # autosaved draft and bumps the recents cache for tracked fields.
    on_submit_success <- function(values_list) {
      k <- draft_key()
      if (.has_pool() && !is.null(.u())) {
        try(db_delete_draft(pool, .u(), k$mrn, k$etype), silent = TRUE)
        try(db_track_recents(pool, .u(), values_list), silent = TRUE)
      }
      draft_dismissed(TRUE)
    }

    list(values = values, iv = iv, reset = reset, input = input,
         on_submit_success = on_submit_success)
  })
}

# ----- autosave / recents helpers --------------------------------------------

#' Build a selectize choices object that prepends "Recientes" above the
#' full list. Returns the flat vector if there are no recents.
.with_recent_optgroup <- function(full, recent) {
  full <- as.character(full)
  recent <- intersect(as.character(recent), full)
  if (!length(recent)) return(full)
  list(Recientes = recent, Todos = setdiff(full, recent))
}

#' Heuristic: is this payload "empty enough" that we should not autosave?
#' A snapshot with only the auto-defaulted encounter_date counts as empty.
.payload_is_empty <- function(payload) {
  if (!length(payload)) return(TRUE)
  meaningful <- payload[!names(payload) %in%
                        c("encounter_date", "encounter_type")]
  for (v in meaningful) {
    if (is.null(v)) next
    if (is.logical(v)) { if (any(v, na.rm = TRUE)) return(FALSE); next }
    if (is.character(v)) { if (any(nzchar(v))) return(FALSE); next }
    if (is.numeric(v))   { if (any(!is.na(v) & v != 0)) return(FALSE); next }
    return(FALSE)
  }
  TRUE
}

#' Push a list of (input id -> value) pairs back into the form. Heuristic
#' dispatch by value type, since we don't know which widget owns each id.
.restore_inputs <- function(session, payload) {
  for (key in names(payload)) {
    v <- payload[[key]]
    if (is.null(v)) next
    if (length(v) > 1L) {
      try(shiny::updateSelectizeInput(session, key, selected = unlist(v)),
          silent = TRUE)
      try(shinyWidgets::updatePickerInput(session, key, selected = unlist(v)),
          silent = TRUE)
      next
    }
    if (is.logical(v)) {
      try(shinyWidgets::updateAwesomeCheckbox(session, key, value = isTRUE(v)),
          silent = TRUE)
      try(shiny::updateCheckboxInput(session, key, value = isTRUE(v)),
          silent = TRUE)
      next
    }
    if (is.numeric(v)) {
      try(shiny::updateNumericInput(session, key, value = v), silent = TRUE)
      next
    }
    sv <- as.character(v)
    # Date-shaped strings -> dateInput; otherwise text/select fall-throughs.
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", sv)) {
      try(shiny::updateDateInput(session, key, value = as.Date(sv)),
          silent = TRUE)
    }
    try(shiny::updateTextInput(session, key, value = sv),     silent = TRUE)
    try(shiny::updateTextAreaInput(session, key, value = sv), silent = TRUE)
    try(shiny::updateSelectizeInput(session, key, selected = sv),
        silent = TRUE)
    try(shinyWidgets::updatePickerInput(session, key, selected = sv),
        silent = TRUE)
    try(shiny::updateRadioButtons(session, key, selected = sv), silent = TRUE)
  }
}

# ----- typing helpers --------------------------------------------------------
nz       <- function(x) if (is.null(x) || !nzchar(as.character(x))) NA else x
as_int   <- function(x) if (is.null(x) || is.na(suppressWarnings(as.integer(x)))) NA_integer_ else as.integer(x)
as_num   <- function(x) if (is.null(x) || is.na(suppressWarnings(as.numeric(x)))) NA_real_   else as.numeric(x)
as_date  <- function(x) if (is.null(x) || is.na(x)) NA         else as.Date(x)

#' Merge a multi-select picker value with a free-text "Otro" field into a
#' single comma-separated string suitable for the existing TEXT column.
combine_drugs <- function(picked, other) {
  parts <- c(picked, if (!is.null(other) && nzchar(other)) trimws(other))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(NA_character_)
  paste(parts, collapse = ", ")
}

#' Merge free-text biomarkers with the cancer-specific structured list.
#' Returns a JSON object string, or NA if both sources are empty.
merge_biomarkers_json <- function(raw, cs_list = list()) {
  base <- list()
  if (!is.null(raw) && nzchar(raw)) {
    lines <- strsplit(raw, "[\r\n]+")[[1]]
    lines <- trimws(lines); lines <- lines[nzchar(lines)]
    for (ln in lines) {
      pair <- strsplit(ln, ":", fixed = TRUE)[[1]]
      if (length(pair) < 2) next
      base[[trimws(pair[1])]] <- trimws(paste(pair[-1], collapse = ":"))
    }
  }
  merged <- utils::modifyList(base, cs_list %||% list())
  if (!length(merged)) return(NA)
  jsonlite::toJSON(merged, auto_unbox = TRUE)
}

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
