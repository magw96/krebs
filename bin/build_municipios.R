#!/usr/bin/env Rscript
# One-time helper: build inst/extdata/mx_municipios.csv from the mxmaps
# package so the deployed app no longer needs mxmaps installed at runtime.
#
# Usage (run once locally where mxmaps IS installed):
#   Rscript bin/build_municipios.R
#
# If mxmaps isn't installed locally either, install it from GitHub first:
#   remotes::install_github("diegovalle/mxmaps")

if (!requireNamespace("mxmaps", quietly = TRUE)) {
  stop("Install mxmaps first: remotes::install_github('diegovalle/mxmaps')")
}
df  <- mxmaps::df_mxmunicipio_2020
out <- data.frame(
  state_code     = df$state_code,
  state_name     = df$state_name,
  municipio_code = df$municipio_code,
  municipio_name = df$municipio_name,
  stringsAsFactors = FALSE
)
out <- out[order(out$state_name, out$municipio_name), ]
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
utils::write.csv(out, "inst/extdata/mx_municipios.csv",
                 row.names = FALSE, fileEncoding = "UTF-8")
message("wrote ", nrow(out), " rows to inst/extdata/mx_municipios.csv")
