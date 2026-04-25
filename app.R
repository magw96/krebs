# Entry point for shinyapps.io / Posit Connect / Shiny Server.
# Loads the package (devtools::load_all in dev, library() in prod) and runs the app.

# --- Force UTF-8 locale BEFORE source()-ing any R file -----------------------
# PCC images often boot in C / POSIX, which makes R fall back to "<U+00ED>"
# style glyph escapes when rendering Unicode strings into HTML attributes
# (pickerInput choices, etc). Try the most portable UTF-8 locales in order.
local({
  for (loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
    ok <- tryCatch(Sys.setlocale("LC_ALL", loc), error = function(e) "")
    if (nzchar(ok)) break
  }
  Sys.setenv(LANG = "en_US.UTF-8", LC_ALL = "en_US.UTF-8")
  options(encoding = "UTF-8")
})

if (Sys.getenv("R_CONFIG_ACTIVE", "dev") == "dev" && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".", quiet = TRUE)
} else {
  # In production, the package is installed via renv. Source the R/ files directly
  # so we don't need an install step on the deploy server.
  for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
    source(f, local = FALSE, encoding = "UTF-8")
  }
}

run_app()
