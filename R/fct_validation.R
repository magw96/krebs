#' Cross-field validators used by the encounter form module.
#'
#' Returned validators are shinyvalidate::InputValidator objects that the
#' module wires into its inputs. Pure functions of inputs - no DB access.

#' Build the validator for the encounter form.
#'
#' @param input  The module's input (NS-resolved).
#' @param patient  reactive() returning the loaded patient (NULL on Tab 1).
#' @return InputValidator
make_encounter_validator <- function(input, patient = function() NULL) {
  iv <- shinyvalidate::InputValidator$new()

  # MRN: numeric, 4-15 digits.
  iv$add_rule("mrn", function(value) {
    if (!nzchar(value %||% "")) return("MRN requerido")
    if (!grepl("^[0-9]{4,15}$", value)) return("MRN debe ser numerico (4-15 digitos)")
    NULL
  })

  iv$add_rule("encounter_date", shinyvalidate::sv_required("Fecha del evento requerida"))

  # Death date >= all prior encounter dates and >= encounter_date.
  iv$add_rule("death_date", function(value) {
    if (is.null(value) || is.na(value)) return(NULL)
    p <- patient()
    if (!is.null(p) && !is.null(p$min_event_date) && !is.na(p$min_event_date)) {
      if (as.Date(value) < as.Date(p$min_event_date)) {
        return("Fecha de defuncion anterior al primer evento clinico")
      }
    }
    if (!is.null(input$encounter_date) && as.Date(value) < as.Date(input$encounter_date)) {
      return("Fecha de defuncion anterior a la fecha del evento")
    }
    NULL
  })

  # Surgery before discharge.
  iv$add_rule("discharge_date", function(value) {
    if (is.null(value) || is.na(value)) return(NULL)
    if (!is.null(input$surgery_date) && !is.na(input$surgery_date) &&
        as.Date(value) < as.Date(input$surgery_date)) {
      return("Fecha de alta anterior a la fecha de cirugia")
    }
    NULL
  })

  # If chemo is checked, intent is required.
  iv$add_rule("chemo_intent", function(value) {
    if (isTRUE(input$chemo) && (is.null(value) || !nzchar(value)))
      return("Indique intencion de quimioterapia")
    NULL
  })

  iv
}

#' Validate a hospital MRN at insertion time. Returns a character() of issues.
validate_new_patient <- function(pool, user, mrn, fecha_nac) {
  issues <- character(0)
  if (!grepl("^[0-9]{4,15}$", mrn %||% "")) {
    issues <- c(issues, "MRN debe ser numerico (4-15 digitos).")
  }
  if (is.null(fecha_nac) || is.na(fecha_nac)) {
    issues <- c(issues, "Fecha de nacimiento requerida.")
  } else if (as.Date(fecha_nac) > Sys.Date()) {
    issues <- c(issues, "Fecha de nacimiento posterior a hoy.")
  }
  # Check duplicate within this hospital.
  if (length(issues) == 0) {
    n <- db_read(pool, user,
      "SELECT COUNT(*) AS n FROM patient_identifiers
        WHERE hospital_id = $1 AND mrn = $2",
      params = list(user$hospital_id, mrn))$n
    if (n > 0) issues <- c(issues, "Ya existe un paciente con ese MRN en su hospital.")
  }
  issues
}
