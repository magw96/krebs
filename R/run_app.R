#' Run the Krebs Shiny app.
#' @export
run_app <- function(...) {
  # Make /inst/app/www available at the URL prefix "www/" so app_ui can link
  # to "www/krebs.css", "www/help.html", etc.  Works whether we're running
  # from a checkout (inst/app/www) or installed package (app/www).
  www_dir <- NULL
  candidates <- c(
    file.path("inst", "app", "www"),     # source checkout
    system.file("app", "www", package = "krebs")  # installed package
  )
  for (cand in candidates) {
    if (nzchar(cand) && dir.exists(cand)) { www_dir <- cand; break }
  }
  if (!is.null(www_dir)) {
    try(shiny::addResourcePath("www", www_dir), silent = TRUE)
  }

  shiny::shinyApp(
    ui     = app_ui,
    server = app_server,
    options = list(...)
  )
}
