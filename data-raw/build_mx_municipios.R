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
#     inst/extdata/mx_municipios.csv   -- 2 columns: state_name, municipio_name
#                                         ~2,469 rows (one per municipio).
#     inst/extdata/mx_states.csv       -- 32 rows: state_name, region (INEGI code).
#     inst/extdata/mx_states.geojson   -- 32 polygons, one per state, with
#                                         `state_name` + `region` properties.
#                                         Used by the dashboard choropleth so
#                                         we don't need mxmaps at runtime on PCC.
#
# After running, commit all three files:
#     git add inst/extdata/mx_municipios.csv \
#             inst/extdata/mx_states.csv \
#             inst/extdata/mx_states.geojson
#     git commit -m "data: refresh mx municipios + states + geojson from mxmaps"

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

# -- States GeoJSON (used by the dashboard choropleth at runtime) ------------
# We rebuild the geometries from `mxmaps::mxstate.map` (a fortified data.frame
# with one row per polygon vertex) using sf, then merge `state_name` from
# `df_mxstate_2020` so the deployed app can join on the same key it already
# stores in patient_identifiers.estado_n.
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("sf is required to dump mx_states.geojson. install.packages('sf')",
       call. = FALSE)
}

mp <- mxmaps::mxstate.map  # long/lat polygon vertices
mp <- mp[order(mp$region, mp$group, mp$order), ]

# mxstate.map is the ggplot2::fortify() output of a SpatialPolygonsDataFrame:
# `group` = one independent ring (states with islands have multiple groups);
# `region` = state code (one state may own several groups).
#
# Build a polygon per `group`, drop degenerate ones (<4 vertices = cannot form
# a closed ring), then aggregate groups into a MULTIPOLYGON per state.

per_group <- by(mp, mp$group, function(g) {
  if (nrow(g) < 4) return(NULL)
  ring <- cbind(g$long, g$lat)
  if (!isTRUE(all.equal(unname(ring[1, ]), unname(ring[nrow(ring), ]),
                        tolerance = 1e-9))) {
    ring <- rbind(ring, ring[1, , drop = FALSE])
  }
  list(region = as.character(g$region[1]),
       poly   = sf::st_polygon(list(ring)))
}, simplify = FALSE)
per_group <- Filter(Negate(is.null), per_group)

regions <- vapply(per_group, function(x) x$region, character(1))
state_codes <- sort(unique(regions))

mp_list <- lapply(state_codes, function(rc) {
  parts <- per_group[regions == rc]
  rings <- lapply(parts, function(p) list(p$poly[[1]]))
  sf::st_multipolygon(rings)
})

geom  <- sf::st_sfc(mp_list, crs = 4326)
attrs <- data.frame(region = state_codes, stringsAsFactors = FALSE)
attrs <- merge(attrs, sta, by = "region", all.x = TRUE, sort = FALSE)
attrs <- attrs[match(state_codes, attrs$region), , drop = FALSE]
mx_sf <- sf::st_sf(attrs, geometry = geom)

geojson_path <- file.path(out_dir, "mx_states.geojson")
if (file.exists(geojson_path)) file.remove(geojson_path)
sf::st_write(mx_sf, geojson_path, driver = "GeoJSON", quiet = TRUE)
message(sprintf("Wrote %d features to %s",
                nrow(mx_sf), geojson_path))

invisible(list(municipios = mun, states = sta, geojson = geojson_path))
