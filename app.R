# Entry point for shinyapps.io / Posit Connect / Shiny Server.
# Loads the package (devtools::load_all in dev, library() in prod) and runs the app.

if (Sys.getenv("R_CONFIG_ACTIVE", "dev") == "dev" && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  # In production, the package is installed via renv. Source the R/ files directly
  # so we don't need an install step on the deploy server.
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = FALSE)
}

run_app()
