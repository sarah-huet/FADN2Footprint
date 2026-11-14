# =============================================================================
# 01c_masks.R - Step 1: forage-type cover fraction on the DMP grids
# For each forage type (grass, maize) and each year, the mask source follows
# CLAUDE.md (CLC closest vintage / HRL 2017+); see mask_source() in 00_config.R.
# Output: raw_data/masks/<forage>_<source><year>_<grid>.tif = share [0-1] of
# each DMP pixel covered by the forage type (used for the mask_min_cover rule
# and for areas).
#   grass, CLC : classes 231 + 321          (CLC 100 m, raw_data/CLC/)
#   grass, HRL : grassland 100 m (% cover)   (CDSE, 100 km tiles)
#   maize, CLC : classes 211 + 212
#   maize, HRL : crop types 10 m, class 1130 (CDSE, 100 km tiles), first
#                aggregated to 100 m shares in raw_data/masks/tiles_100m/
# Requires 01b (DMP grids). Existing outputs are skipped. The 10 m crop-type
# tiles are processed in parallel (n_workers in 00_config.R).
# =============================================================================
source("00_config.R")
setup_cdse_s3()

# ---- 1. Reference grids -----------------------------------------------------
# Masks are computed directly on the DMP grids (1 km before 2014, 300 m from
# 2014), defined by 01b_grids_zones.R.
templates <- list()
for (g in c("1km", "300m")) {
  if (!file.exists(grid_file(g))) stop("Missing ", grid_file(g), ": run 01b_grids_zones.R first")
  templates[[g]] <- template_from_grid(readRDS(grid_file(g)))
}

# ---- 2. Which masks are needed ----------------------------------------------
# One row per year x forage type: CLC (with its vintage) or HRL (with its year).
plan <- rbindlist(lapply(years_all, function(y) rbindlist(lapply(names(forage_mask), function(f) {
  s <- mask_source(y, f)
  data.table(year = y, forage = f, src_type = s$type, src_year = s$year, grid = grid_of_year(y))
}))))

# ---- 3. List the HRL tiles of each year needed (catalogue search, cached) --
# Function listing the 100 km tiles of a layer for one year (Europe, EPSG:3035;
# overseas tiles in UTM are dropped).
hrl_tiles <- function(f, y) {
  it <- stac_search(hrl_collections[[f]], sprintf("%d-01-01", y), sprintf("%d-12-31", y), bbox = eu_bbox)
  if (!nrow(it)) return(it)
  it <- it[grepl("3035", proj) | grepl("_03035_", id)]
  # some tiles exist in several revisions (e.g. ..._V01_R00 and ..._V01_R01):
  # keep only the latest one, otherwise both are mosaicked and the result
  # depends on their order
  it[, tile := sub(".*_(E[0-9]+N[0-9]+)_.*", "\\1", id)]
  it[, rev := id]   # ids differ only by version/revision, sorted alphabetically
  setorder(it, tile, -rev)
  n0 <- nrow(it); it <- it[, .SD[1], by = tile][, rev := NULL]
  if (nrow(it) < n0) msg("  ", f, " ", y, ": ", n0 - nrow(it), " older tile revisions dropped")
  it[, vsi := s3_to_vsi(href)]
  it
}
# If a year is not published yet (or its search fails), the latest available
# year is used instead (reported in outputs/step1/mask_sources_by_year.csv).
hrl_tile_list <- list(); hrl_available <- list()
for (f in names(hrl_collections)) {
  yrs <- sort(unique(plan[forage == f & src_type == "HRL", src_year]))
  for (y in yrs) {
    tl <- tryCatch(hrl_tiles(f, y), error = function(e) {
      warning("Catalogue search failed for ", f, " ", y, ": ", conditionMessage(e)); NULL })
    msg("HRL ", f, " ", y, ": ", if (is.null(tl)) "search failed" else paste(nrow(tl), "tiles"))
    if (!is.null(tl) && nrow(tl)) hrl_tile_list[[paste(f, y)]] <- tl
  }
  hrl_available[[f]] <- yrs[paste(f, yrs) %in% names(hrl_tile_list)]
  if (!length(hrl_available[[f]]) && any(plan$forage == f & plan$src_type == "HRL"))
    stop("No HRL tiles found for ", f, " (collection ", hrl_collections[[f]], "). ",
         "The Copernicus catalogue may be down: try again later.")
}
plan[src_type == "HRL", src_year_used := vapply(seq_len(.N), function(i) {
  a <- hrl_available[[forage[i]]]; max(a[a <= src_year[i]], min(a))
}, numeric(1))]
plan[src_type == "CLC", src_year_used := src_year]
plan[, mask_file := file.path(dir_masks, sprintf("%s_%s%d_%s.tif", forage, src_type, src_year_used, grid))]
write_csv(plan, file.path(dir_out1, "mask_sources_by_year.csv"))
if (plan[src_type == "HRL", any(src_year_used != src_year)])
  warning("HRL not available for some years; latest available year used (see outputs/step1/mask_sources_by_year.csv)")

# ---- 4. List the mask files still to compute (existing ones are skipped) ----
todo <- unique(plan[, .(forage, src_type, src_year_used, grid, mask_file)])
todo <- todo[!file.exists(mask_file)]

# ---- 5. CLC masks (years before the HRL) -------------------------------------
# CLC 100 m raster -> 1 for the forage classes, 0 for other classes ->
# average over each DMP pixel = share of the pixel covered by the forage type.
for (i in which(todo$src_type == "CLC")) {
  t <- todo[i]
  f <- clc_file(t$src_year_used)
  if (!file.exists(f)) stop("Missing ", f, ": download the CLC ", t$src_year_used,
                            " 100 m raster (GeoTIFF) from land.copernicus.eu and save it there")
  # 0/1 CLC raster of the forage classes (3 billion cells: ~10-30 min), saved
  # and reused for the other grid of the same vintage
  bin_file <- file.path(dir_masks, sprintf("clc_binary_%s_%d.tif", t$forage, t$src_year_used))
  if (file.exists(bin_file)) {
    b <- rast(bin_file)
  } else {
    msg("CLC ", t$src_year_used, " ", t$forage, ": 0/1 raster (long step, R looks idle: do not interrupt)")
    b <- clc_binary(rast(f), clc_classes[[t$forage]], bin_file)
  }
  msg("Mask ", basename(t$mask_file), ": averaging on the ", t$grid, " grid")
  fraction_on_grid(b, templates[[t$grid]], t$mask_file)
  msg("  done")
}

# ---- 6. HRL masks (2017+ layers, read from the Copernicus S3 storage) -------
# Grass: the 100 m layer already gives % grassland -> averaged on the DMP grid.
# Maize: the 10 m crop-type tiles are first turned into maize shares per 100 m
# cell (saved tile by tile, so an interrupted run restarts where it stopped),
# then averaged on the DMP grid.
for (i in which(todo$src_type == "HRL")) {
  t <- todo[i]
  msg("Mask ", basename(t$mask_file))
  tiles <- hrl_tile_list[[paste(t$forage, t$src_year_used)]]
  msg("  ", nrow(tiles), " tiles")
  if (t$forage == "grass") {
    # 100 m: 0-100 = % grassland, 255 = outside
    src <- vrt(tiles$vsi, filename = tempfile(fileext = ".vrt"),
               options = c("-srcnodata", "255", "-vrtnodata", "255"))
    fraction_on_grid(src, templates[[t$grid]], t$mask_file, divide_by = 100)
    next
  } else {
    # 10 m crop types: share of maize (1130) per 100 m cell, tile by tile
    out_dir <- file.path(dir_tiles, paste0("maize_", t$src_year_used))
    dir.create(out_dir, showWarnings = FALSE)
    tiles[, of := file.path(out_dir, paste0(id, "_100m.tif"))]
    todo_t <- tiles[!file.exists(of)]
    if (nrow(todo_t)) {
      msg("  ", nrow(todo_t), " tiles to process with ", n_workers, " workers")
      cl <- start_workers()
      # workers are always stopped, even after an error or an interruption
      status <- tryCatch(parallel::parLapplyLB(cl, split(todo_t[, .(vsi, of)], seq_len(nrow(todo_t))),
                                               worker_fun(cty_tile_share)),
                         finally = parallel::stopCluster(cl))
      failed <- unlist(status)[unlist(status) != "ok"]
      if (length(failed))
        stop(length(failed), " tile(s) could not be processed (network problem?):\n  ",
             paste(head(failed, 10), collapse = "\n  "),
             "\nRun 01c_masks.R again when the connection is back: finished tiles are kept.", call. = FALSE)
    }
    src <- vrt(tiles$of,
               filename = tempfile(fileext = ".vrt"))
  }
  fraction_on_grid(src, templates[[t$grid]], t$mask_file)
}
msg("Done: masks in ", dir_masks)
