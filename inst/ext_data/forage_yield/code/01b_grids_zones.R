# =============================================================================
# 01b_grids_zones.R - Step 1: DMP grids and NUTS3 rasters (quick)
# 1) Reads the header of one DMP file of each product (1 km before 2014,
#    300 m from 2014) to define the European window of its grid, saved in
#    raw_data/grids/grid_<1km|300m>.rds. Nothing else is downloaded.
# 2) Turns each NUTS3 map (01a) into a raster on these grids (each pixel gets
#    the id of the NUTS3 containing its centre): raw_data/grids/nuts3_<v>_<grid>.tif,
#    with the id -> NUTS3 code table nuts3_<v>_ids.csv.
# Run after 01a. Existing files are skipped, unless the NUTS3 map is more
# recent (e.g. regions added from a fallback version, see 01a).
# =============================================================================
source("00_config.R")
setup_cdse_s3()

# ---- 1. DMP grids -----------------------------------------------------------
# One reference file per product (any dekad: all dekads share the same grid).
ref_year <- c(`1km` = 2010, `300m` = 2015)
for (g in names(ref_year)) {
  if (file.exists(grid_file(g))) next
  y <- ref_year[[g]]
  items <- stac_search(dmp_collection(y), sprintf("%d-06-01", y), sprintf("%d-06-30", y), asset = dmp_asset)
  if (!nrow(items)) stop("No DMP file found for the ", g, " grid")
  full <- rast(s3_to_vsi(items$href[1]))   # reads the header only
  grid <- make_eu_grid(full, eu_bbox)
  saveRDS(grid, grid_file(g))
  msg("Grid ", g, ": ", grid$nrow, " x ", grid$ncol, " pixels, resolution ", round(res(full)[1], 6), " deg")
}

# ---- 2. NUTS3 rasters on the grids ------------------------------------------
# One per NUTS version x grid actually used by the years in years_all.
combos <- unique(data.table(v = nuts_version_of_year(years_all),
                            g = vapply(years_all, grid_of_year, "")))
for (i in seq_len(nrow(combos))) {
  v <- combos$v[i]; g <- combos$g[i]
  map_file <- file.path(dir_nuts, sprintf("NUTS3_EU_%d.gpkg", v))
  nuts3 <- st_read(map_file, quiet = TRUE)
  outdated <- function(f) !file.exists(f) || file.mtime(f) < file.mtime(map_file)
  # id -> NUTS3 code table (same order as in the NUTS3 map)
  if (outdated(zone_lookup_file(v)))
    fwrite(data.table(zone = seq_len(nrow(nuts3)), nuts3 = nuts3$nuts_id, cntr = nuts3$cntr, name = nuts3$name),
           zone_lookup_file(v))
  if (!outdated(zone_file(v, g))) next
  msg("Rasterising NUTS3 ", v, " on the ", g, " grid")
  vv <- vect(nuts3); vv$zone <- seq_len(nrow(vv))
  # pixels outside all NUTS3 get 0 (= no region)
  rasterize(vv, template_from_grid(readRDS(grid_file(g))), field = "zone", background = 0,
            filename = zone_file(v, g), overwrite = TRUE,
            wopt = list(datatype = "INT2U", NAflag = 65535, gdal = c("COMPRESS=DEFLATE", "TILED=YES")))
}
msg("Done: grids and NUTS3 rasters in ", dir_grids)
