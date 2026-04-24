#' App-level config. Read once at startup. Reads from `inst/golem-config.yml`
#' under the active R_CONFIG_ACTIVE profile.

#' @export
get_golem_config <- function(value, config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
                             use_parent = TRUE) {
  config::get(value = value, config = config,
              file   = app_sys("golem-config.yml"),
              use_parent = use_parent)
}

#' Resolve a path inside the installed package's inst/ directory
#' (or, in dev, inst/ in the source tree).
#' @export
app_sys <- function(...) {
  p <- system.file(..., package = "krebs")
  if (nzchar(p)) return(p)
  # Dev fallback (devtools::load_all):
  file.path("inst", ...)
}
