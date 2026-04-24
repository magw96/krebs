#' Tab 1: register a NEW patient + their initial diagnosis encounter.
#'
#' Two side-by-side panels:
#'   - Left: identidad (PHI -> patient_identifiers)  + estilo de vida
#'   - Right: encounter form (initial_dx only)       -> encounters
#'
#' Both inserts happen in a single transaction; if the encounter insert fails
#' the patient row is rolled back too.

mod_register_new_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("user-plus"), " Registrar paciente nuevo"),
    shiny::p("Capture el expediente, datos demograficos y el diagnostico inicial.",
             class = "text-muted"),
    shiny::fluidRow(

      # --- LEFT column: identidad + estilo de vida -----------------------
      shiny::column(5,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("id-card"), " Identidad"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shiny::textInput(ns("mrn"),
            shiny::HTML("MRN / Expediente <span style='color:red'>*</span>"),
            placeholder = "solo numeros, ej. 3825369"),
          shiny::textInput(ns("nombre"),
            shiny::HTML("Nombre completo <span style='color:red'>*</span>"),
            placeholder = "Sin acentos"),
          shiny::textInput(ns("curp"), "CURP",
                           placeholder = "18 caracteres"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::dateInput(ns("fecha_nac"), "Fecha de nacimiento",
                               value = "1980-01-01", max = Sys.Date(), language = "es")),
            shiny::column(6,
              shinyWidgets::radioGroupButtons(ns("sexo"), "Sexo",
                choices = c("M","F","Otro"),
                selected = character(0), size = "sm", justified = TRUE))
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::textInput(ns("telefono"), "Telefono", placeholder = "10 digitos")),
            shiny::column(6,
              shiny::textInput(ns("email"), "Correo electronico"))
          ),
          shiny::selectizeInput(ns("estado_n"),    "Estado de nacimiento",    choices = NULL),
          shiny::selectizeInput(ns("municipio_n"), "Municipio de nacimiento", choices = NULL),
          shiny::fluidRow(
            shiny::column(6,
              shinyWidgets::pickerInput(ns("insurance"), "Cobertura medica",
                choices = c("IMSS","ISSSTE","INSABI/Bienestar","Privado",
                            "Gastos de bolsillo","Otro","Ninguna"),
                selected = NULL, options = list(`live-search` = TRUE))),
            shiny::column(6,
              shinyWidgets::pickerInput(ns("estado_civil"), "Estado civil",
                choices = c("soltero","casado","union libre","divorciado","viudo"),
                selected = NULL))
          ),
          shiny::fluidRow(
            shiny::column(6,
              shinyWidgets::pickerInput(ns("escolaridad"), "Escolaridad",
                choices = c("ninguna","primaria","secundaria","preparatoria",
                            "licenciatura","posgrado"),
                selected = NULL)),
            shiny::column(6,
              shiny::textInput(ns("ocupacion"), "Ocupacion"))
          ),
          shiny::div(style = "color:#c00", shiny::textOutput(ns("identity_err")))
        ),

        bs4Dash::box(
          title = shiny::tagList(shiny::icon("heart"), " Estilo de vida"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("peso"), "Peso (kg)",
                                  value = 70,  min = 5, max = 250)),
            shiny::column(6,
              shiny::numericInput(ns("estatura"), "Estatura (cm)",
                                  value = 170, min = 50, max = 240))
          ),
          shinyWidgets::radioGroupButtons(ns("smoking"), "Tabaquismo (cig/dia)",
            choices = c("No","1-10","10-20","20+"), selected = "No",
            size = "xs", justified = TRUE),
          shinyWidgets::radioGroupButtons(ns("alcohol"), "Alcohol (bebidas/sem)",
            choices = c("No","1-6","7-13","14+"), selected = "No",
            size = "xs", justified = TRUE),
          shinyWidgets::radioGroupButtons(ns("physical_activity"),
            "Actividad fisica",
            choices = c("ninguna","leve","moderada","vigorosa"),
            selected = "ninguna", size = "xs", justified = TRUE),
          shinyWidgets::radioGroupButtons(ns("drugs"),
            "Uso de drogas recreativas",
            choices = c("ninguna","ocasional","frecuente"),
            selected = "ninguna", size = "xs", justified = TRUE)
        ),

        bs4Dash::box(
          title = shiny::tagList(shiny::icon("notes-medical"), " Comorbilidades"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
          shiny::selectizeInput(ns("comorbidities"),
            "ICD-11 (multiples)", choices = NULL, multiple = TRUE,
            options = list(placeholder = "diabetes, hipertension, ..."))
        )
      ),

      # --- RIGHT column: encounter form (initial_dx) ---------------------
      shiny::column(7,
        mod_encounter_form_ui(ns("enc"), allowed_types = "initial_dx")
      )
    ),

    shiny::hr(),
    shiny::div(style = "text-align:center;",
      shiny::actionButton(ns("submit"),
        shiny::tagList(shiny::icon("paper-plane"), " Registrar"),
        class = "btn-success btn-lg"),
      shiny::span(id = ns("submit_msg"), style = "display:none; margin-left:10px;",
                  shiny::icon("spinner", class = "fa-spin"), " Procesando..."),
      shiny::div(id = ns("ok_msg"), style = "display:none; margin-top:15px;",
        shiny::h4(shiny::icon("circle-check", style = "color:green"),
                  " Paciente registrado"),
        shiny::actionLink(ns("reset_form"), "Registrar otro paciente"))
    )
  )
}

mod_register_new_server <- function(id, pool, user) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # populate state/municipio/comorbidity dropdowns
    shiny::observe({
      shiny::updateSelectizeInput(session, "estado_n",
        choices = lookup_states(), server = TRUE)
      shiny::updateSelectizeInput(session, "municipio_n",
        choices = lookup_municipios(), server = TRUE)
      icd <- tryCatch(lookup_icd11(), error = function(e) NULL)
      if (!is.null(icd)) {
        # accept either a data.frame with a description column or a vector
        ch <- if (is.data.frame(icd)) {
          col <- intersect(c("Title","label","name","description"), names(icd))
          if (length(col)) icd[[col[1]]] else as.character(icd[[1]])
        } else as.character(icd)
        shiny::updateSelectizeInput(session, "comorbidities",
          choices = sort(unique(ch)), server = TRUE)
      }
    })

    enc <- mod_encounter_form_server("enc",
                                     patient = function() NULL,
                                     pool = pool, user = user)

    err_rv <- shiny::reactiveVal("")
    output$identity_err <- shiny::renderText(err_rv())

    # gate the submit button
    shiny::observe({
      ok <- nzchar(input$mrn %||% "") &&
            nzchar(input$nombre %||% "") &&
            !is.null(input$sexo) &&
            !is.null(input$fecha_nac) &&
            isTRUE(enc$iv$is_valid())
      shinyjs::toggleState("submit", condition = ok)
    })

    shiny::observeEvent(input$submit, {
      u <- user(); if (is.null(u)) return()
      shinyjs::disable("submit"); shinyjs::show("submit_msg")
      on.exit({ shinyjs::enable("submit"); shinyjs::hide("submit_msg") }, add = TRUE)

      issues <- validate_new_patient(pool, u, input$mrn, input$fecha_nac)
      if (length(issues) > 0) { err_rv(paste(issues, collapse = " ")); return() }
      err_rv("")

      vals <- enc$values()
      if (is.null(vals)) {
        err_rv("Revise los campos del diagnostico inicial."); return()
      }
      vals$mrn         <- input$mrn
      vals$hospital_id <- u$hospital_id

      tryCatch({
        with_tenant(pool, u, function(con) {
          # 1) patient identifier (PHI)
          DBI::dbExecute(con,
            "INSERT INTO patient_identifiers
               (hospital_id, mrn, nombre, fecha_nac, sexo,
                estado_n, municipio_n,
                insurance, ocupacion, escolaridad, estado_civil,
                telefono, email, curp,
                created_by)
             VALUES ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15)",
            params = list(u$hospital_id, input$mrn, input$nombre,
                          as.character(input$fecha_nac), input$sexo,
                          input$estado_n %||% NA, input$municipio_n %||% NA,
                          input$insurance %||% NA, input$ocupacion %||% NA,
                          input$escolaridad %||% NA, input$estado_civil %||% NA,
                          input$telefono %||% NA, input$email %||% NA,
                          input$curp %||% NA,
                          u$user_id))
          audit_write(con, u, "INSERT", "patient_identifiers",
                      target_id = paste(u$hospital_id, input$mrn, sep = ":"),
                      diff = list(after = list(mrn = input$mrn,
                                               nombre = input$nombre)))

          # 2) lifestyle row
          DBI::dbExecute(con,
            "INSERT INTO lifestyle
               (hospital_id, mrn, weight_kg, height_cm,
                smoking, alcohol, physical_activity, drugs)
             VALUES ($1,$2,$3,$4,$5,$6,$7,$8)",
            params = list(u$hospital_id, input$mrn,
                          input$peso, input$estatura,
                          input$smoking, input$alcohol,
                          input$physical_activity %||% NA,
                          input$drugs %||% NA))

          # 3) comorbidities (multi)
          if (length(input$comorbidities)) {
            for (c in input$comorbidities) {
              DBI::dbExecute(con,
                "INSERT INTO comorbidities (hospital_id, mrn, icd11_code)
                 VALUES ($1,$2,$3)",
                params = list(u$hospital_id, input$mrn, c))
            }
          }

          # 4) initial_dx encounter
          insert_encounter(con, u, vals)
        })
        shinyjs::hide("submit"); shinyjs::show("ok_msg")
      },
      error = function(e) {
        err_rv(paste("Error al guardar:", conditionMessage(e)))
      })
    })

    shiny::observeEvent(input$reset_form, {
      shinyjs::reset(ns(""))
      enc$reset()
      shinyjs::show("submit"); shinyjs::hide("ok_msg")
    })
  })
}

#' Insert one encounter row (used by both register_new and followup_search).
#' Caller must already be inside `with_tenant()`.
insert_encounter <- function(con, user, vals) {
  cols <- c(
    "hospital_id","mrn","encounter_type","encounter_date",
    "tnm_t","tnm_n","tnm_m","tnm_t_basis","tnm_n_basis",
    "primary_site","oncotree","icdo3_morph","bilateral",
    "tumor_grade","ecog_ps","hpv",
    "dx_method","imaging_at_dx","biomarkers","tumor_markers",
    "family_history_cancer","family_history_detail",
    "prior_cancer","prior_cancer_site",
    "first_symptom_date","referral_source",
    "chemo","chemo_intent","chemo_drugs","chemo_cycles","chemo_response",
    "radio","radio_dose_gy",
    "hormonal_therapy","hormonal_drug",
    "targeted_therapy","targeted_drug",
    "immunotherapy","immuno_drug",
    "surgery_cpt","surgery_intent","surgery_margin",
    "lymph_nodes_examined","lymph_nodes_positive",
    "surgery_date","discharge_date","complication",
    "vital_status","death_date","death_cause",
    "notes","created_by"
  )
  # fill any missing keys with NA so the param list is positional & complete
  for (k in cols) if (is.null(vals[[k]])) vals[[k]] <- NA
  vals <- vals[cols]

  # arrays must go in as Postgres array literals
  vals$chemo_drugs   <- pg_text_array(vals$chemo_drugs)
  vals$surgery_cpt   <- pg_text_array(vals$surgery_cpt)
  vals$imaging_at_dx <- pg_text_array(vals$imaging_at_dx)

  placeholders <- paste0("$", seq_along(cols))
  sql <- paste0("INSERT INTO encounters (",
                paste(cols, collapse = ", "),
                ") VALUES (", paste(placeholders, collapse = ", "),
                ") RETURNING encounter_id")

  res <- DBI::dbGetQuery(con, sql, params = unname(vals))
  audit_write(con, user, "INSERT", "encounters",
              target_id = as.character(res$encounter_id[1]),
              diff = list(after = list(type = vals$encounter_type,
                                       date = as.character(vals$encounter_date))))
  res$encounter_id[1]
}

pg_text_array <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) return(NA)
  paste0("{", paste(gsub('"', '\\\\"', x), collapse = ","), "}")
}
