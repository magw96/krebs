#' Tab 1: register a NEW patient + their initial diagnosis encounter.
#'
#' Two side-by-side panels:
#'   - Left: identidad (PHI -> patient_identifiers)  + datos clinicos
#'   - Right: encounter form (initial_dx only)       -> encounters
#'
#' Both inserts happen in a single transaction; if the encounter insert fails
#' the patient row is rolled back too.
#'
#' All Spanish accents are written as \uXXXX so the file parses identically
#' under any locale (PCC sometimes runs in C / POSIX).

mod_register_new_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3(shiny::icon("user-plus"), " Registrar paciente nuevo"),
    shiny::p("Capture el expediente, datos demograficos y el ",
             "diagnostico inicial.",
             class = "text-muted"),
    shiny::fluidRow(

      # --- TOP row: identidad (8 cols) + datos clinicos (4 cols) --------
      shiny::column(8,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("id-card"), " Identidad"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shiny::textInput(ns("mrn"),
            shiny::HTML("MRN / Expediente <span style='color:red'>*</span>"),
            placeholder = "Solo numeros, ej. 3825369"),
          shiny::textInput(ns("nombre"),
            shiny::HTML("Nombre completo <span style='color:red'>*</span>"),
            placeholder = "Apellidos y nombre"),
          shiny::textInput(ns("curp"), "CURP",
                           placeholder = "18 caracteres"),
          shiny::fluidRow(
            shiny::column(6,
              shiny::dateInput(ns("fecha_nac"), "Fecha de nacimiento",
                               value = "1980-01-01", max = Sys.Date(),
                               language = "es")),
            shiny::column(6,
              shiny::radioButtons(ns("sexo"), "Sexo",
                choices = c("M","F","Otro"),
                selected = character(0), inline = TRUE))
          ),
          shiny::fluidRow(
            shiny::column(6,
              shiny::textInput(ns("telefono"), "Telefono",
                               placeholder = "10 digitos")),
            shiny::column(6,
              shiny::textInput(ns("email"), "Correo electronico"))
          ),
          shiny::selectizeInput(ns("estado_n"),    "Estado de nacimiento",
            choices = c("", lookup_states())),
          shiny::selectizeInput(ns("municipio_n"), "Municipio de nacimiento",
            choices = "",
            options = list(placeholder = "Seleccione primero el estado")),
          shiny::fluidRow(
            shiny::column(6,
              shinyWidgets::pickerInput(ns("insurance"),
                "Cobertura medica",
                choices = c("IMSS"               = "IMSS",
                            "ISSSTE"             = "ISSSTE",
                            "INSABI / Bienestar" = "INSABI",
                            "Privado"            = "privado",
                            "Gastos de bolsillo" = "bolsillo",
                            "Otro"               = "otro",
                            "Ninguna"            = "ninguna"),
                selected = NULL, options = list(`live-search` = TRUE))),
            shiny::column(6,
              shinyWidgets::pickerInput(ns("estado_civil"), "Estado civil",
                choices = c("Soltero"            = "soltero",
                            "Casado"             = "casado",
                            "Union libre"   = "union_libre",
                            "Divorciado"         = "divorciado",
                            "Viudo"              = "viudo"),
                selected = NULL))
          ),
          shiny::fluidRow(
            shiny::column(6,
              shinyWidgets::pickerInput(ns("escolaridad"), "Escolaridad",
                choices = c("Ninguna"      = "ninguna",
                            "Primaria"     = "primaria",
                            "Secundaria"   = "secundaria",
                            "Preparatoria" = "preparatoria",
                            "Licenciatura" = "licenciatura",
                            "Posgrado"     = "posgrado"),
                selected = NULL)),
            shiny::column(6,
              shiny::textInput(ns("ocupacion"), "Ocupacion"))
          ),
          shiny::div(style = "color:#c00", shiny::textOutput(ns("identity_err")))
        )
      ),

      shiny::column(4,
        bs4Dash::box(
          title = shiny::tagList(shiny::icon("heart-pulse"), " Datos clinicos"),
          width = 12, status = "primary", solidHeader = TRUE, collapsible = TRUE,
          # Comorbilidades up top -- they're the highest-yield clinical
          # info on this card, so the clinician sees them first.
          shiny::tags$label(class = "control-label",
            shiny::icon("notes-medical"), " Comorbilidades"),
          shiny::selectizeInput(ns("comorbidities"),
            "ICD-11 (multiples)", choices = NULL, multiple = TRUE,
            options = list(placeholder = "Diabetes, hipertension, ...")),
          shiny::hr(),
          shiny::fluidRow(
            shiny::column(6,
              shiny::numericInput(ns("peso"), "Peso (kg)",
                                  value = 70,  min = 5, max = 250)),
            shiny::column(6,
              shiny::numericInput(ns("estatura"), "Estatura (cm)",
                                  value = 170, min = 50, max = 240))
          ),
          shiny::radioButtons(ns("smoking"),
            "Tabaquismo (cigarrillos / dia)",
            choices = c("No","1-10","10-20","20+"), selected = "No",
            inline = TRUE),
          shiny::radioButtons(ns("alcohol"),
            "Alcohol (bebidas / semana)",
            choices = c("No","1-6","7-13","14+"), selected = "No",
            inline = TRUE),
          shiny::radioButtons(ns("physical_activity"),
            "Actividad fisica",
            choices = c("Ninguna"  = "ninguna",
                        "Leve"     = "leve",
                        "Moderada" = "moderada",
                        "Vigorosa" = "vigorosa"),
            selected = "ninguna", inline = TRUE),
          shiny::helpText("ECOG performance status se captura en el ",
                          shiny::tags$em("encuentro clinico"), " (debajo).")
        )
      ),

      # --- BOTTOM row: encounter form (initial_dx) ----------------------
      shiny::column(12,
        mod_encounter_form_ui(ns("enc"), allowed_types = "initial_dx")
      )
    ),

    shiny::div(class = "krebs-sticky-submit",
      shiny::actionButton(ns("submit"),
        shiny::tagList(shiny::icon("paper-plane"), " Registrar"),
        class = "btn-success btn-lg krebs-submit-btn"),
      shiny::span(id = ns("submit_msg"),
                  style = "display:none;",
                  shiny::icon("spinner", class = "fa-spin"), " Procesando..."),
      shiny::span(class = "kbd-hint",
        shiny::HTML("Atajo: <kbd>Ctrl</kbd>+<kbd>S</kbd>")),
      shiny::div(id = ns("ok_msg"),
                 style = "display:none; width:100%; text-align:center;",
        shiny::h4(shiny::icon("circle-check", style = "color:green"),
                  " Paciente registrado"),
        shiny::actionLink(ns("reset_form"), "Registrar otro paciente"))
    )
  )
}

mod_register_new_server <- function(id, pool, user, data_changed = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Populate comorbidities once at startup. Estado is preloaded in UI.
    shiny::observe({
      icd <- tryCatch(lookup_icd11(), error = function(e) NULL)
      if (!is.null(icd)) {
        ch <- if (is.data.frame(icd)) {
          col <- intersect(c("Title","label","name","description"), names(icd))
          if (length(col)) icd[[col[1]]] else as.character(icd[[1]])
        } else as.character(icd)
        shiny::updateSelectizeInput(session, "comorbidities",
          choices = sort(unique(ch)), server = TRUE)
      }
    })

    # Cascading municipios: refilter every time estado_n changes (client-side
    # so options appear on click without typing).
    shiny::observeEvent(input$estado_n, {
      mun <- if (nzchar(input$estado_n %||% "")) lookup_municipios(input$estado_n)
             else character(0)
      shiny::updateSelectizeInput(session, "municipio_n",
        choices = c("", mun), selected = "")
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    enc <- mod_encounter_form_server("enc",
                                     patient = function() NULL,
                                     pool = pool, user = user)

    err_rv <- shiny::reactiveVal("")
    output$identity_err <- shiny::renderText(err_rv())

    # gate the submit button -- only the four PHI-required fields. Validator
    # for the encounter form is checked at submit time (enc$values() returns
    # NULL on failure, and validate_new_patient() runs MRN/dob checks).
    shiny::observe({
      ok <- nzchar(input$mrn %||% "") &&
            nzchar(input$nombre %||% "") &&
            !is.null(input$sexo) &&
            !is.null(input$fecha_nac)
      shinyjs::toggleState("submit", condition = ok)
    })

    shiny::observeEvent(input$submit, {
      u <- user(); if (is.null(u)) return()
      shinyjs::disable("submit"); shinyjs::show("submit_msg")
      on.exit({ shinyjs::enable("submit"); shinyjs::hide("submit_msg") },
              add = TRUE)

      issues <- validate_new_patient(pool, u, input$mrn, input$fecha_nac)
      if (length(issues) > 0) { err_rv(paste(issues, collapse = " ")); return() }
      err_rv("")

      vals <- enc$values()
      if (is.null(vals)) {
        err_rv("Revise los campos del diagnostico inicial."); return()
      }
      vals$mrn         <- input$mrn
      vals$hospital_id <- u$hospital_id

      # Hard precondition: every multi-tenant insert needs a hospital_id.
      # Super-admins may have hospital_id = NULL, in which case we can't
      # write rows owned by no tenant -- ask them to switch hospital first.
      if (is.null(u$hospital_id) || is.na(u$hospital_id)) {
        err_rv("Su usuario no tiene hospital asignado. Pida al administrador asignarle un hospital antes de registrar pacientes.")
        return()
      }

      message(sprintf("[register] inserting mrn=%s hospital_id=%s user=%s",
                      input$mrn, u$hospital_id, u$user_id))

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
                smoking, alcohol, physical_activity)
             VALUES ($1,$2,$3,$4,$5,$6,$7)",
            params = list(u$hospital_id, input$mrn,
                          input$peso, input$estatura,
                          input$smoking, input$alcohol,
                          input$physical_activity %||% NA))

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
        # Drop the autosaved draft and bump the user's recents cache.
        try(enc$on_submit_success(vals), silent = TRUE)
        # Bump the cross-module trigger so dashboard + data tabs refresh.
        if (!is.null(data_changed)) {
          data_changed(shiny::isolate(data_changed()) + 1L)
        }
        shiny::showNotification(
          paste0("Paciente ", input$mrn, " guardado en la base de datos."),
          type = "message", duration = 4)
        shinyjs::hide("submit"); shinyjs::show("ok_msg")
      },
      error = function(e) {
        msg <- paste("Error al guardar:", conditionMessage(e))
        message("[register] ", msg)
        err_rv(msg)
        shiny::showNotification(msg, type = "error", duration = 8)
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
  for (k in cols) if (is.null(vals[[k]])) vals[[k]] <- NA
  vals <- vals[cols]

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
