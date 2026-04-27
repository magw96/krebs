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

  # ---- Step 1: type selector --------------------------------------------
  # Single-type callers (Registrar -> initial_dx) skip the wizard entirely
  # and just stamp the value via a hidden input.
  type_selector_card <- if (length(allowed_types) == 1L) {
    shinyjs::hidden(
      shiny::textInput(ns("encounter_type"), label = NULL,
                       value = allowed_types[1])
    )
  } else {
    # IMPORTANT: shiny treats `choices = c(name=value)` as
    # name -> displayed, value -> returned. Our `type_labels` is
    # c(code = "Spanish label"), so we use choiceNames/choiceValues
    # to make the labels Spanish but keep the saved value as the code.
    # We use plain shiny::radioButtons (not shinyWidgets) because the
    # latter had click-handler issues on bs4Dash (see prior task #9/#13).
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("list-check"),
                             " Paso 1: \u00bfQue desea registrar?"),
      width = 12, collapsible = FALSE, status = "primary", solidHeader = TRUE,
      shiny::div(class = "krebs-typepicker",
        shiny::radioButtons(
          inputId      = ns("encounter_type"),
          label        = NULL,
          choiceNames  = unname(type_labels[allowed_types]),
          choiceValues = allowed_types,
          selected     = character(0),
          inline       = TRUE
        )
      ),
      shiny::div(class = "text-muted small",
        shiny::icon("info-circle"), " ",
        "Seleccione un tipo de evento para desbloquear los campos correspondientes.")
    )
  }

  # ---- Step 2 visibility: only after a type has been picked --------------
  # For single-type callers the value is always set, so this is no-op.
  step2_visible_when <- sprintf(
    "typeof input['%s'] !== 'undefined' && input['%s'] !== ''",
    ns("encounter_type"), ns("encounter_type"))
  initial_or_recurrence <- sprintf(
    "['initial_dx','recurrence'].indexOf(input['%s']) > -1",
    ns("encounter_type"))

  shiny::tagList(
    # Autosave-draft restore callout (filled in by the server when a draft
    # exists for this user x patient x encounter_type).
    shiny::uiOutput(ns("draft_callout")),

    type_selector_card,

    # ---- Step 2: datos basicos del evento (always after type pick) ------
    # Title is per-type ("Datos de diagnostico oncologico" for initial_dx,
    # "Datos de la recurrencia" for recurrence, etc.) -- rendered server-side.
    shiny::conditionalPanel(condition = step2_visible_when,
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("clipboard-list"),
                             shiny::textOutput(ns("step2_title"), inline = TRUE)),
      width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
      shiny::fluidRow(
        shiny::column(3,
          shiny::dateInput(ns("encounter_date"), "Fecha del evento",
                           value = Sys.Date(), max = Sys.Date(),
                           language = "es")),
        shiny::column(3,
          shiny::conditionalPanel(condition = initial_or_recurrence,
          shinyWidgets::pickerInput(ns("dx_method"), "Metodo diagnostico",
            choices = c("Biopsia"      = "biopsia",
                        "Citologia"    = "citologia",
                        "Imagen"       = "imagen",
                        "Clinico"      = "clinico",
                        "Quirurgico"   = "quirurgico"),
            selected = NULL, options = list(`live-search` = TRUE)))),
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] == 'initial_dx'", ns("encounter_type")),
          shinyWidgets::pickerInput(ns("referral_source"), "Origen de referencia",
            choices = c("Urgencias"        = "urgencias",
                        "Consulta externa" = "consulta",
                        "Referido externo" = "referido",
                        "Tamizaje"         = "tamizaje",
                        "Auto-referido"    = "auto"),
            selected = NULL, options = list(`live-search` = TRUE)))),
        shiny::column(3,
          shiny::conditionalPanel(
            condition = sprintf("input['%s'] != 'death'", ns("encounter_type")),
          shiny::radioButtons(ns("ecog_ps"),
            label = shiny::tagList(
              "ECOG performance status ",
              # bs4Dash ships Bootstrap 4 (jQuery tooltips) -- use both BS4
              # `data-toggle` AND BS5 `data-bs-toggle` so this works on either
              # framework. krebs_shortcuts.js initialises both engines.
              shiny::tags$span(
                `data-toggle`       = "tooltip",
                `data-bs-toggle`    = "tooltip",
                `data-placement`    = "left",
                `data-bs-placement` = "left",
                `data-html`         = "true",
                `data-bs-html`      = "true",
                title = paste0(
                  "<div style='text-align:left'>",
                  "<b>0</b> Asintomatico, totalmente activo<br>",
                  "<b>1</b> Sintomas leves, ambulatorio, trabajo ligero<br>",
                  "<b>2</b> Ambulatorio &gt;50% del dia, no trabaja<br>",
                  "<b>3</b> Encamado o en silla &gt;50% del dia<br>",
                  "<b>4</b> Completamente discapacitado, encamado<br>",
                  "<b>5</b> Muerto",
                  "</div>"),
                style = "cursor:help; color:#0d2c54;",
                shiny::icon("circle-info"))
            ),
            choices = c("0","1","2","3","4"),
            selected = character(0), inline = TRUE)))
      ),
      # ---- Death-specific block (cause classification) ------------------
      # When this encounter is a death, the encounter_date IS the death
      # date and vital_status is implicitly 'muerto' (set server-side at
      # submit). The clinician only needs to classify the cause as
      # related vs unrelated to the cancer + a free-text detail. Avoids
      # the redundant "Estado vital" box for death encounters.
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'death'", ns("encounter_type")),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(5,
            shinyWidgets::pickerInput(ns("death_relation"),
              "Relacion con la enfermedad oncologica",
              choices = c("(seleccione)"        = "",
                          "Relacionada"         = "relacionada",
                          "No relacionada"      = "no_relacionada",
                          "Desconocida"         = "desconocida"),
              selected = "")),
          shiny::column(7,
            shiny::textInput(ns("death_cause_detail"),
              "Causa especifica (opcional)",
              placeholder = "Ej: progresion sistemica, sepsis, IAM, etc."))
        )
      )
    )), # closes Step-2 bs4Dash::box + Step-2 conditionalPanel

    # ---- Antecedentes (only at initial diagnosis) -----------------------
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'initial_dx'", ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("clipboard-user"),
                               " Antecedentes y contexto"),
        width = 12, collapsible = TRUE, status = "primary", solidHeader = TRUE,
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

    # ---- Recurrence / progression specifics -----------------------------
    # Only meaningful when this encounter IS a recurrence/progression. The
    # TNM box below is reused (it doubles as rTNM). ECOG already lives on
    # each encounter row, so capturing it inside step-2 does NOT overwrite
    # the initial_dx ECOG -- it is a separate row in the encounters table.
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'recurrence'", ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("triangle-exclamation"),
                               " Datos de la recurrencia / progresion"),
        width = 12, collapsible = TRUE, status = "danger", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(4,
            shinyWidgets::pickerInput(ns("recurrence_type"),
              "Patron de recurrencia",
              choices = c("(seleccione)"   = "",
                          "Local"          = "local",
                          "Regional (ganglionar)" = "regional",
                          "A distancia (metastasica)" = "distancia",
                          "Mixta (local + distancia)" = "mixta",
                          "Desconocida"    = "desconocida"),
              selected = "")),
          shiny::column(4,
            shinyWidgets::pickerInput(ns("recurrence_confirmation"),
              "Metodo de confirmacion",
              choices = c("(seleccione)"           = "",
                          "Imagen"                 = "imagen",
                          "Biopsia (histologica)"  = "biopsia",
                          "Citologia"              = "citologia",
                          "Quirurgica"             = "quirurgico",
                          "Marcador serico"        = "marcador_serologico",
                          "Clinico"                = "clinico"),
              selected = "")),
          shiny::column(4,
            shinyWidgets::awesomeCheckbox(ns("biopsy_done"),
              "Re-biopsia obtenida (re-perfilado molecular)", FALSE))
        ),
        # Sites of distant disease (only when distancia / mixta) -----------
        shiny::conditionalPanel(
          condition = sprintf(
            "['distancia','mixta'].indexOf(input['%s']) > -1",
            ns("recurrence_type")),
          shinyWidgets::pickerInput(ns("recurrence_sites"),
            "Sitios de enfermedad a distancia",
            choices = c("Cerebro / SNC"            = "cerebro",
                        "Hueso"                    = "hueso",
                        "Pulmon"                   = "pulmon",
                        "Higado"                   = "higado",
                        "Peritoneo"                = "peritoneo",
                        "Pleura"                   = "pleura",
                        "Ganglios distantes"       = "ganglios_distantes",
                        "Suprarrenal"              = "suprarrenal",
                        "Piel / partes blandas"    = "piel",
                        "Ovario"                   = "ovario",
                        "Otro"                     = "otro"),
            multiple = TRUE,
            options = list(`actions-box` = TRUE,
                           `live-search` = TRUE,
                           `selected-text-format` = "count > 2",
                           `none-selected-text` = "Seleccione sitio(s)"))
        ),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("prior_treatment_response"),
              "Respuesta a la ultima linea de tratamiento",
              choices = c("(seleccione)"  = "",
                          "Completa"      = "completa",
                          "Parcial"       = "parcial",
                          "Estable"       = "estable",
                          "Progresion"    = "progresion",
                          "No evaluable"  = "no_evaluable"),
              selected = ""))
        ),
        shiny::div(class = "alert alert-info small mb-0",
          shiny::icon("circle-info"), " ",
          shiny::strong("Re-estadiaje:"),
          " complete a continuacion el rTNM, ECOG actual y el panel ",
          "molecular si hubo nueva biopsia. ",
          "Estos valores se guardan en este encuentro y NO sobreescriben ",
          "los del diagnostico inicial.")
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
    # Title flips to "rTNM (re-estadiaje)" for recurrence encounters so the
    # clinician sees clearly that they are restaging, not editing the
    # initial dx TNM.
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("layer-group"),
                               shiny::textOutput(ns("tnm_title"), inline = TRUE)),
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

    # ---- Treatment line + intent (only for treatment encounters) -------
    # Tracking 1L/2L/3L lines and their intent is the cornerstone of
    # downstream PFS / OS-by-line analyses. We surface it as the FIRST
    # control inside any treatment encounter so it's never forgotten.
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'treatment'", ns("encounter_type")),
      bs4Dash::box(
        title = shiny::tagList(shiny::icon("layer-group"),
                               " Linea e intencion del tratamiento"),
        width = 12, collapsible = FALSE, status = "primary", solidHeader = TRUE,
        shiny::fluidRow(
          shiny::column(3,
            shiny::numericInput(ns("line"),
              shiny::tagList(
                "Linea de tratamiento ",
                shiny::tags$span(
                  `data-toggle` = "tooltip", `data-bs-toggle` = "tooltip",
                  `data-placement` = "top", `data-bs-placement` = "top",
                  `data-html` = "true", `data-bs-html` = "true",
                  title = paste0(
                    "<div style='text-align:left'>",
                    "<b>1L</b> Tratamiento inicial post-diagnostico<br>",
                    "<b>2L</b> Tras progresion o intolerancia a 1L<br>",
                    "<b>3L+</b> Lineas subsecuentes",
                    "</div>"),
                  style = "cursor:help; color:#0d2c54;",
                  shiny::icon("circle-info"))
              ),
              value = 1, min = 1, max = 20, step = 1)),
          shiny::column(5,
            shiny::radioButtons(ns("treatment_intent"), "Intencion",
              choices = c("Curativo"     = "curativo",
                          "Adyuvante"    = "adyuvante",
                          "Neoadyuvante" = "neoadyuvante",
                          "Paliativo"    = "paliativo",
                          "Mantenimiento"= "mantenimiento"),
              selected = character(0), inline = TRUE)),
          shiny::column(4,
            shiny::div(class = "text-muted small mt-2",
              shiny::icon("circle-info"), " ",
              shiny::textOutput(ns("line_hint"), inline = TRUE)))
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
              choices = c("(sin especificar)" = "",
                          "Ninguna"           = "ninguna",
                          "Menor"             = "menor",
                          "Mayor"             = "mayor"),
              selected = ""))
        )
      )
    ),

    # ---- Vital status (followup ONLY) ----------------------------------
    # For 'death' encounters the cause is captured inside the step-2 box
    # via the structured 'death_relation' picker, and vital_status /
    # death_date are derived server-side -- so this whole section is
    # hidden for death to avoid the duplicated "Datos de la defuncion"
    # + "Estado vital" boxes.
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'followup'", ns("encounter_type")),
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
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'muerto'", ns("vital_status")),
              shiny::dateInput(ns("death_date"), "Fecha de defuncion",
                               value = NULL, max = Sys.Date(), language = "es"))
          ),
          shiny::column(4,
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'muerto'", ns("vital_status")),
              shiny::textInput(ns("death_cause"), "Causa"))
          )
        )
      )
    ),

    bs4Dash::box(
      title = shiny::tagList(shiny::icon("note-sticky"), " Notas"),
      width = 12, collapsible = TRUE, status = "secondary", solidHeader = TRUE,
      shiny::textAreaInput(ns("notes"), NULL, rows = 3,
                           placeholder = "Observaciones libres")
    ),

    # ---- Attachments (PDF / PNG / JPG, up to 10 MB each) ----------------
    # Files are stored as bytea in encounter_attachments and inserted AFTER
    # the encounter row commits successfully (the parent module triggers the
    # upload via enc$consume_files()). This keeps the form usable even when
    # the user attaches nothing, and avoids orphan blobs on validation fail.
    bs4Dash::box(
      title = shiny::tagList(shiny::icon("paperclip"), " Documentos adjuntos"),
      width = 12, collapsible = TRUE, status = "secondary", solidHeader = TRUE,
      shiny::p(class = "text-muted small mb-2",
        "Adjunte reportes de patologia, imagenologia o laboratorios. ",
        shiny::strong("Maximo 10 MB por archivo."),
        " Formatos: PDF, PNG, JPG."),
      shiny::fileInput(ns("attachments"), NULL,
                       multiple = TRUE,
                       accept = c("application/pdf", "image/png", "image/jpeg"),
                       buttonLabel = shiny::tagList(shiny::icon("upload"),
                                                    " Seleccionar"),
                       placeholder = "Sin archivos"),
      shiny::div(style = "color:#a83232", shiny::textOutput(ns("att_err")))
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

    # ---- Step 2 dynamic title (per encounter type) ------------------------
    output$step2_title <- shiny::renderText({
      switch(input$encounter_type %||% "",
        initial_dx = " Datos de diagnostico oncologico",
        recurrence = " Datos de la recurrencia",
        treatment  = " Datos del tratamiento",
        followup   = " Datos del seguimiento",
        death      = " Datos de la defuncion",
        " Datos del evento")
    })
    shiny::outputOptions(output, "step2_title", suspendWhenHidden = FALSE)

    # TNM title -- becomes "rTNM (re-estadiaje)" on recurrence rows so the
    # clinician sees this is a re-stage, not an edit of the dx TNM.
    output$tnm_title <- shiny::renderText({
      if (isTRUE(input$encounter_type == "recurrence"))
        " Re-estadiaje rTNM (recurrencia / progresion)"
      else
        " Clasificacion TNM"
    })
    shiny::outputOptions(output, "tnm_title", suspendWhenHidden = FALSE)

    # ---- Auto-prefill downstream dates from encounter_date ---------------
    # When the user enters the dx/event date, mirror it into surgery_date,
    # discharge_date and death_date *only if those are still empty*. Avoids
    # overwriting a date the clinician deliberately set, but spares them
    # from re-typing the most common value (same day as the event).
    # Also enforces "no date earlier than the dx date" by bumping `min`.
    shiny::observeEvent(input$encounter_date, {
      d <- input$encounter_date
      if (is.null(d) || is.na(d)) return()

      # Update min for downstream dates so the picker visually rejects
      # earlier values (server still re-checks at submit time).
      shiny::updateDateInput(session, "surgery_date",   min = d, max = Sys.Date())
      shiny::updateDateInput(session, "discharge_date", min = d, max = Sys.Date())
      shiny::updateDateInput(session, "death_date",     min = d, max = Sys.Date())

      # Prefill the *value* only when surgery/discharge/death are still blank
      # AND only on encounter types where it makes sense.
      etype <- input$encounter_type %||% ""
      if (etype %in% c("initial_dx","recurrence","treatment")) {
        if (is.null(input$surgery_date)   || is.na(input$surgery_date))
          shiny::updateDateInput(session, "surgery_date",   value = d)
        if (is.null(input$discharge_date) || is.na(input$discharge_date))
          shiny::updateDateInput(session, "discharge_date", value = d)
      }
      if (etype == "death") {
        if (is.null(input$death_date) || is.na(input$death_date))
          shiny::updateDateInput(session, "death_date", value = d)
      }
    }, ignoreInit = TRUE)

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

        # Vital-status fields. For 'death' encounters everything is
        # derived from the death-specific block: vital_status is
        # implicitly 'muerto', death_date == encounter_date, and
        # death_cause is the structured "{relation}: {detail}" string.
        # For 'followup' the clinician may pick vivo/muerto/perdido and
        # only when muerto fill in the date+cause fields directly.
        vital_status   = if (isTRUE(input$encounter_type == "death"))
                           "muerto" else nz(input$vital_status),
        death_date     = if (isTRUE(input$encounter_type == "death"))
                           as_date(input$encounter_date)
                         else as_date(input$death_date),
        death_cause    = if (isTRUE(input$encounter_type == "death")) {
                           rel <- nz(input$death_relation)
                           det <- nz(input$death_cause_detail)
                           parts <- c(rel, det)
                           parts <- parts[nzchar(parts)]
                           if (length(parts)) paste(parts, collapse = ": ") else NA_character_
                         } else nz(input$death_cause),
        notes          = nz(input$notes),

        # Treatment-line tracking (nullable; only meaningful when
        # encounter_type == 'treatment').
        line             = if (isTRUE(input$encounter_type == "treatment"))
                             as_int(input$line) else NA,
        treatment_intent = if (isTRUE(input$encounter_type == "treatment"))
                             nz(input$treatment_intent) else NA,

        # Recurrence-specific fields (nullable; only meaningful when
        # encounter_type == 'recurrence'). recurrence_sites is an array.
        recurrence_type          = if (isTRUE(input$encounter_type == "recurrence"))
                                     nz(input$recurrence_type) else NA,
        recurrence_sites         = if (isTRUE(input$encounter_type == "recurrence")
                                       && length(input$recurrence_sites))
                                     input$recurrence_sites else NA,
        recurrence_confirmation  = if (isTRUE(input$encounter_type == "recurrence"))
                                     nz(input$recurrence_confirmation) else NA,
        biopsy_done              = if (isTRUE(input$encounter_type == "recurrence"))
                                     isTRUE(input$biopsy_done) else NA,
        prior_treatment_response = if (isTRUE(input$encounter_type == "recurrence"))
                                     nz(input$prior_treatment_response) else NA,
        time_to_recurrence_days  = if (isTRUE(input$encounter_type == "recurrence")) {
                                     dx <- suppressWarnings(as.Date(p$fecha_dx %||% NA))
                                     ev <- suppressWarnings(as.Date(input$encounter_date))
                                     if (!is.na(dx) && !is.na(ev))
                                       as.integer(ev - dx) else NA_integer_
                                   } else NA,

        created_by     = u$user_id
      )
    })

    # ---- Treatment-line auto-suggestion ------------------------------------
    # When the user switches to a treatment encounter, pre-fill the line
    # number with (max existing line for this patient) + 1 so they don't
    # have to remember whether this is 2L or 3L.
    output$line_hint <- shiny::renderText({
      if (!isTRUE(input$encounter_type == "treatment")) return("")
      p <- if (is.function(patient)) patient() else patient
      if (is.null(p) || is.null(p$mrn)) return("Linea 1L sugerida (paciente nuevo).")
      u <- .u(); if (is.null(u) || !.has_pool()) return("")
      max_line <- tryCatch(
        db_read(pool, u,
          "SELECT COALESCE(MAX(line),0) AS m FROM encounters
            WHERE hospital_id=$1 AND mrn=$2 AND encounter_type='treatment'",
          params = list(u$hospital_id, p$mrn))$m,
        error = function(e) 0L)
      sprintf("Sugerencia: %dL (basada en lineas previas registradas).",
              as.integer(max_line) + 1L)
    })

    shiny::observeEvent(list(input$encounter_type, patient()), {
      if (!isTRUE(input$encounter_type == "treatment")) return()
      p <- if (is.function(patient)) patient() else patient
      if (is.null(p) || is.null(p$mrn)) {
        shiny::updateNumericInput(session, "line", value = 1)
        return()
      }
      u <- .u(); if (is.null(u) || !.has_pool()) return()
      max_line <- tryCatch(
        db_read(pool, u,
          "SELECT COALESCE(MAX(line),0) AS m FROM encounters
            WHERE hospital_id=$1 AND mrn=$2 AND encounter_type='treatment'",
          params = list(u$hospital_id, p$mrn))$m,
        error = function(e) 0L)
      shiny::updateNumericInput(session, "line",
                                value = as.integer(max_line) + 1L)
    }, ignoreInit = FALSE)

    # ---- Attachment validation (10 MB cap, allowed MIME types) -------------
    .ATT_MAX <- 10L * 1024L * 1024L
    .ATT_OK_MIME <- c("application/pdf", "image/png", "image/jpeg")
    att_err_rv <- shiny::reactiveVal("")
    output$att_err <- shiny::renderText(att_err_rv())

    shiny::observeEvent(input$attachments, {
      f <- input$attachments
      if (is.null(f) || nrow(f) == 0) { att_err_rv(""); return() }
      msgs <- character(0)
      for (i in seq_len(nrow(f))) {
        if (!(f$type[i] %in% .ATT_OK_MIME)) {
          msgs <- c(msgs, sprintf("'%s': tipo %s no permitido.",
                                  f$name[i], f$type[i]))
        } else if (f$size[i] > .ATT_MAX) {
          msgs <- c(msgs, sprintf("'%s': %.1f MB excede el limite de 10 MB.",
                                  f$name[i], f$size[i]/1024/1024))
        }
      }
      att_err_rv(paste(msgs, collapse = " "))
    })

    # Consumed by the parent module *after* the encounter row commits.
    # Returns the number of attachments successfully written.
    consume_files <- function(encounter_id, hospital_id, mrn) {
      f <- input$attachments
      if (is.null(f) || nrow(f) == 0) return(0L)
      u <- .u(); if (is.null(u) || !.has_pool()) return(0L)
      n_ok <- 0L
      for (i in seq_len(nrow(f))) {
        if (!(f$type[i] %in% .ATT_OK_MIME)) next
        if (f$size[i] > .ATT_MAX) next
        bytes <- tryCatch(readBin(f$datapath[i], what = "raw",
                                  n = as.integer(f$size[i])),
                          error = function(e) NULL)
        if (is.null(bytes)) next
        try({
          with_tenant(pool, u, function(con) {
            DBI::dbExecute(con,
              "INSERT INTO encounter_attachments
                 (encounter_id, hospital_id, mrn, filename, mime,
                  size_bytes, content, uploaded_by)
               VALUES ($1,$2,$3,$4,$5,$6,$7,$8)",
              params = list(encounter_id, hospital_id, mrn,
                            f$name[i], f$type[i],
                            as.integer(f$size[i]),
                            list(bytes), u$user_id))
          })
          n_ok <- n_ok + 1L
        }, silent = TRUE)
      }
      n_ok
    }

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
         on_submit_success = on_submit_success,
         consume_files = consume_files)
  })
}

# ----- autosave / recents helpers --------------------------------------------

#' Build a selectize choices object that prepends "Recientes" above the
#' full list. Returns the flat vector if there are no recents.
#'
#' Preserves names() if `full` is a named vector (label = value), as is
#' the case for the OncoTree picker where Spanish labels map to English
#' codes. Without this, as.character() would strip names and the user
#' would see raw codes instead of translations.
.with_recent_optgroup <- function(full, recent) {
  if (is.list(full)) return(full)              # already grouped
  if (is.null(full) || !length(full)) return(character(0))
  recent <- as.character(recent)
  recent <- recent[nzchar(recent)]
  codes  <- unname(as.character(full))
  rec_in <- intersect(recent, codes)
  if (!length(rec_in)) return(full)
  if (!is.null(names(full))) {
    rec_idx <- match(rec_in, full)
    rec_idx <- rec_idx[!is.na(rec_idx)]
    if (!length(rec_idx)) return(full)
    rec_full <- full[rec_idx]
    rest     <- full[setdiff(seq_along(full), rec_idx)]
    return(list(Recientes = rec_full, Todos = rest))
  }
  list(Recientes = rec_in, Todos = setdiff(codes, rec_in))
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
