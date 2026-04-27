# data-raw/build_mx_municipios.R
#
# Regenerate the bundled Mexican municipios CSV from the `mxmaps` package.
# Run locally on a machine where mxmaps is installed -- Posit Connect Cloud
# does not install mxmaps reliably (heavy sf / geojsonio deps + private
# Remotes), so the bundled CSV is the only source the deployed app sees.
#
#   USAGE (from project root):
#     R -e 'source("data-raw/build_mx_municipios.R")'
#
#   OUTPUT:
#     inst/extdata/mx_municipios.csv  -- 2 columns: state_name, municipio_name
#                                        ~2,469 rows (one per municipio).
#     inst/extdata/mx_states.csv      -- 32 rows: state_name, region (INEGI code).
#                                        Useful if we ever wire a real choropleth.
#
# After running, commit both CSVs:
#     git add inst/extdata/mx_municipios.csv inst/extdata/mx_states.csv
#     git commit -m "data: refresh mx_municipios.csv from mxmaps"

if (!requireNamespace("mxmaps", quietly = TRUE)) {
  stop("mxmaps is not installed. Install it locally with:\n",
       "  install.packages('remotes')\n",
       "  remotes::install_github('diegovalle/mxmaps')",
       call. = FALSE)
}

out_dir <- "inst/extdata"
if (!dir.exists(out_dir)) {
  stop("Run this script from the project root (cwd should contain inst/).",
       call. = FALSE)
}

# -- Municipios --------------------------------------------------------------
m <- mxmaps::df_mxmunicipio_2020
keep <- c("state_name", "municipio_name")
miss <- setdiff(keep, names(m))
if (length(miss)) stop("mxmaps schema changed; missing: ",
                       paste(miss, collapse = ", "), call. = FALSE)

mun <- unique(m[, keep, drop = FALSE])
mun <- mun[order(mun$state_name, mun$municipio_name), ]
utils::write.csv(mun, file.path(out_dir, "mx_municipios.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Wrote %d rows to %s/mx_municipios.csv",
                nrow(mun), out_dir))

# -- States (with INEGI region code, used by mxstate_choropleth) -------------
s <- mxmaps::df_mxstate_2020
keep_s <- intersect(c("state_name", "region", "state_abbr_official"), names(s))
sta <- unique(s[, keep_s, drop = FALSE])
sta <- sta[order(sta$state_name), ]
utils::write.csv(sta, file.path(out_dir, "mx_states.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")
message(sprintf("Wrote %d rows to %s/mx_states.csv",
                nrow(sta), out_dir))

invisible(list(municipios = mun, states = sta))
