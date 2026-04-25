# --- App startup hardening (runs BEFORE shiny boots) ------------------------
# 1) Force a UTF-8 capable locale. PCC images may set LANG="UTF-8.UTF-8"
#    (malformed) or leave it unset, which collapses LC_CTYPE to C and breaks
#    parse() on strings with accented characters.
local({
  for (loc in c("en_US.UTF-8", "C.UTF-8", "en_US.utf8", "C.utf8")) {
    ok <- tryCatch(Sys.setlocale("LC_ALL", loc), error = function(e) "")
    if (nzchar(ok)) break
  }
  Sys.setenv(LANG = "en_US.UTF-8")
})

# 2) Disable shiny's loadSupport() auto-sourcing of R/. app.R sources files
#    explicitly with encoding="UTF-8" in a guaranteed order.
options(shiny.autoload.r = FALSE)

# --- renv (must stay last) --------------------------------------------------
source("renv/activate.R")
