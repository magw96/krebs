#' mod_biobank_request: dialog para registrar muestras desde un encuentro
#'
#' UX:
#'   * Embed: shiny::actionButton (icono de tubo) en el form de encuentro.
#'   * Click -> modalDialog: tipo de muestra, n. de aliquotas, sitio anatomico,
#'     consentimiento (existente o crear nuevo), notas.
#'   * Submit -> upsert sujeto -> insert specimens (1 por aliquota) ->
#'     append COLLECTED en chain_of_custody -> genera PDF de etiquetas.
#'
#' Devuelve un reactive con los BIOIDs creados (para que el modulo padre
#' los persista en encounters.specimen_bioids).

mod_biobank_request_ui <- function(id, label = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(ns("open"),
      label = shiny::tagList(shiny::icon("vial-circle-check"),
                              " Recolectar muestra para banco"),
      class = "btn-outline-info btn-sm"),
    shiny::uiOutput(ns("recent_bioids_ui"))
  )
}

mod_biobank_request_server <- function(id, pool, user, patient,
                                       encounter_id = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    created <- shiny::reactiveVal(character(0))

    # ---- Open modal -------------------------------------------------------
    shiny::observeEvent(input$open, {
      p <- if (is.function(patient)) patient() else patient
      if (is.null(p) || !nzchar(p$mrn %||% "")) {
        shiny::showNotification("Selecciona un paciente primero.",
                                type = "warning")
        return()
      }

      # Cargar consentimientos vigentes para ese sujeto si ya existe
      hosp <- toupper(p$hospital_id %||% "HSPA")
      bsid <- tryCatch(pseudonymize_mrn(p$mrn, hosp),
                       error = function(e) NULL)

      consent_choices <- c("(crear nuevo)" = "__new__")
      if (!is.null(bsid)) {
        full_id <- paste0(hosp, "-", bsid)
        cs <- tryCatch(DBI::dbGetQuery(pool,
          "SELECT id, template_version, signed_dt
             FROM biobank_consents
            WHERE bio_subject_id = $1
              AND withdrawn_dt IS NULL
            ORDER BY signed_dt DESC",
          params = list(full_id)), error = function(e) NULL)
        if (!is.null(cs) && nrow(cs) > 0) {
          extra <- stats::setNames(cs$id,
            sprintf("%s (%s)", cs$template_version, cs$signed_dt))
          consent_choices <- c(extra, consent_choices)
        }
      }

      shiny::showModal(shiny::modalDialog(
        title = shiny::tagList(shiny::icon("vial"),
                                " Recolectar muestra para banco"),
        size = "l", easyClose = FALSE,
        shiny::p(shiny::strong("Paciente:"), " ", p$nombre,
                 "  -  MRN ", p$mrn, "  -  ", hosp, class = "small text-muted"),

        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("sample_type"),
              "Tipo de muestra",
              choices = biobank_sample_types(),
              selected = "TUM",
              options = list(`live-search` = TRUE))),
          shiny::column(3,
            shiny::numericInput(ns("n_aliquots"),
              "N. de aliquotas", value = 1, min = 1, max = 50)),
          shiny::column(3,
            shiny::dateInput(ns("collection_dt"),
              "Fecha de colecta", value = Sys.Date()))
        ),

        shiny::fluidRow(
          shiny::column(6,
            shiny::textInput(ns("anatomic_site"),
              "Sitio anatomico (libre o ICD-O-3 topo)",
              value = "")),
          shiny::column(6,
            shinyWidgets::pickerInput(ns("preservation"),
              "Preservacion",
              choices = c("(seleccione)" = "",
                          "FFPE"        = "FFPE",
                          "Snap frozen" = "SNAP_FROZEN",
                          "OCT"         = "OCT",
                          "RNA later"   = "RNA_LATER",
                          "Viable"      = "VIABLE",
                          "Otro fijado" = "FIXED_OTHER"),
              selected = ""))
        ),

        shiny::fluidRow(
          shiny::column(4,
            shinyWidgets::pickerInput(ns("storage_temp"),
              "Temperatura",
              choices = c("(seleccione)"="", "RT","+4","-20","-80","-150","-196"),
              selected = "")),
          shiny::column(8,
            shiny::textInput(ns("location"),
              "Ubicacion (freezer / rack / box / pos)",
              value = "", placeholder = "F1 / R3 / B07 / A4"))
        ),

        shiny::hr(),
        shiny::h6(shiny::icon("file-signature"), " Consentimiento informado"),
        shiny::fluidRow(
          shiny::column(6,
            shinyWidgets::pickerInput(ns("consent_id"),
              "Consentimiento",
              choices = consent_choices,
              selected = consent_choices[[1]])),
          shiny::column(6,
            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == '__new__'", ns("consent_id")),
              shiny::textInput(ns("consent_template"),
                "Plantilla / version",
                value = "ICF-Krebs-v1.0-2026"),
              shiny::dateInput(ns("consent_signed_dt"),
                "Fecha de firma", value = Sys.Date()),
              shinyWidgets::awesomeCheckbox(ns("permits_genomics"),
                "Permite estudios genomicos", value = TRUE),
              shinyWidgets::awesomeCheckbox(ns("permits_recontact"),
                "Permite re-contacto", value = FALSE)
            ))
        ),

        shiny::textAreaInput(ns("notes"), "Notas (opcional)",
                             rows = 2, width = "100%"),

        shiny::div(class = "alert alert-info small",
          shiny::icon("circle-info"),
          " Protocolo IRB: ",
          shiny::strong("TDM-CEI-2026-V1"),
          ". El BIOID se genera automaticamente."),

        footer = shiny::tagList(
          shiny::modalButton("Cancelar"),
          shiny::actionButton(ns("save"),
            shiny::tagList(shiny::icon("paper-plane"), " Registrar y generar etiquetas"),
            class = "btn-success")
        )
      ))
    })

    # ---- Save -------------------------------------------------------------
    shiny::observeEvent(input$save, {
      shiny::req(input$sample_type, input$n_aliquots, input$collection_dt)
      p <- if (is.function(patient)) patient() else patient
      u <- if (is.function(user))    user()    else user
      if (is.null(p) || is.null(u)) return()

      hosp <- toupper(p$hospital_id %||% "HSPA")

      bsid_full <- tryCatch(
        biobank_subject_upsert(pool, p$mrn, hosp, u$email %||% "system"),
        error = function(e) {
          shiny::showNotification(paste("Error sujeto:", conditionMessage(e)),
                                  type = "error", duration = 8)
          NULL
        })
      if (is.null(bsid_full)) return()

      # Consentimiento
      consent_id <- input$consent_id
      if (identical(consent_id, "__new__")) {
        consent_id <- DBI::dbGetQuery(pool,
          "INSERT INTO biobank_consents
             (bio_subject_id, hospital_id, template_version, scope,
              permits_genomics, permits_recontact,
              signed_dt, irb_protocol, created_by)
           VALUES ($1, $2, $3, 'broad', $4, $5, $6, 'TDM-CEI-2026-V1', $7)
           RETURNING id",
          params = list(bsid_full, hosp,
                        (nz(input$consent_template) %||% "ICF-Krebs-v1.0-2026"),
                        isTRUE(input$permits_genomics),
                        isTRUE(input$permits_recontact),
                        as.character(input$consent_signed_dt %||% Sys.Date()),
                        u$email %||% "system"))[[1]]
        DBI::dbExecute(pool,
          "UPDATE biobank_subjects SET consent_id = $1 WHERE bio_subject_id = $2",
          params = list(consent_id, bsid_full))
      }

      # Generar BIOIDs por aliquota
      col_n <- bioid_next_collection(pool, bsid_full, input$sample_type)
      bioids <- character(0)
      for (i in seq_len(as.integer(input$n_aliquots))) {
        bioid <- bioid_compose(hosp, sub(".*-", "", bsid_full),
                               col_n, input$sample_type, i)
        DBI::dbExecute(pool,
          "INSERT INTO biobank_specimens
             (bioid, bio_subject_id, hospital_id, encounter_id,
              collection_dt, anatomic_site, sample_type,
              preservation, storage_temp,
              freezer, rack, box, position,
              status, notes, created_by)
           VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9,
                   $10, $11, $12, $13, 'AVAILABLE', $14, $15)",
          params = list(bioid, bsid_full, hosp,
                        if (!is.null(encounter_id) && is.function(encounter_id))
                          encounter_id() else encounter_id,
                        as.POSIXct(input$collection_dt),
                        nz(input$anatomic_site),
                        toupper(input$sample_type),
                        nz(input$preservation),
                        nz(input$storage_temp),
                        .loc_part(input$location, 1),
                        .loc_part(input$location, 2),
                        .loc_part(input$location, 3),
                        .loc_part(input$location, 4),
                        nz(input$notes), u$email %||% "system"))

        DBI::dbExecute(pool,
          "INSERT INTO biobank_chain_of_custody
             (bioid, hospital_id, event, actor, to_location, notes)
           VALUES ($1, $2, 'COLLECTED', $3, $4, $5)",
          params = list(bioid, hosp, u$email %||% "system",
                        nz(input$location), nz(input$notes)))

        bioids <- c(bioids, bioid)
      }

      created(c(created(), bioids))

      # Generar PDF con etiquetas
      pdf_path <- tryCatch(
        biobank_label_pdf(bioids[1], input$sample_type,
                          input$collection_dt, hosp),
        error = function(e) NULL)

      shiny::removeModal()
      shiny::showNotification(
        sprintf("Registradas %d aliquotas: %s ... %s",
                length(bioids), bioids[1], bioids[length(bioids)]),
        type = "message", duration = 8)
    })

    # ---- UI: chips con los BIOIDs ya creados ------------------------------
    output$recent_bioids_ui <- shiny::renderUI({
      ids <- created()
      if (!length(ids)) return(NULL)
      shiny::div(class = "small text-muted",
        shiny::icon("vial"), " Muestras en este encuentro: ",
        shiny::HTML(paste(sprintf(
          "<span class='badge badge-info'>%s</span>", ids), collapse = " ")))
    })

    # Devolver al modulo padre
    return(created)
  })
}

# ---- helpers internos ------------------------------------------------------

# Parser muy simple de "F1 / R3 / B07 / A4" -> componente n
.loc_part <- function(s, n) {
  if (is.null(s) || !nzchar(s)) return(NA_character_)
  parts <- trimws(strsplit(s, "/", fixed = TRUE)[[1]])
  if (n > length(parts)) NA_character_ else parts[n]
}

# nz() y %||% se importan del scope global del paquete
# (definidos en mod_encounter_form.R y fct_db.R respectivamente).
