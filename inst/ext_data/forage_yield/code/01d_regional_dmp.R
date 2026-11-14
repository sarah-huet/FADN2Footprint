# =============================================================================
# 01d_regional_dmp.R - Step 1, outputs 2 and 3: seasonal regional DMP
# Average DMP over the growing season (1 March - 30 September, tDM/ha) of
# grassland and maize pixels per NUTS2 and NUTS3 region, following
# "Mixed pixels and small samples" and "Growing season" in CLAUDE.md:
#   - only DMP pixels with more than mask_min_cover (90%) of their area covered
#     by the forage type are used
#   - pixels are assigned to the region containing their centre, using the
#     NUTS version applicable to the year
#   - a region with < min_pixels retained pixels (or no valid DMP) takes the
#     value of its parent: NUTS3 -> NUTS2 -> NUTS1 -> NUTS0; then, as a last
#     resort, the national DMP of the same forage type in the neighbouring
#     country (neighbour_country in 00_config.R). The level used is given in
#     column dmp_source; error if no usable neighbour
# Method (fast): instead of building DMP maps, each 10-daily DMP file of the
# season is read only where there are retained pixels, and summed directly per
# NUTS3 (one small table per dekad, saved in raw_data/dmp_dekad/). Dekads are
# processed in parallel. The seasonal value of a region is then
#   sum over dekads of (days x mean DMP of the region's valid pixels),
# scaled to the full season when some dekads have no valid data (left blank
# if valid dekads cover < 75% of the season), in kg -> t DM/ha.
# Also gives the Copernicus area of each forage type per NUTS3 (cover share x
# pixel area, all pixels), used in step 2.
# Requires 01a, 01b, 01c. Can be stopped and restarted: finished dekads are kept.
# Outputs (outputs/step1/):
#   regional_dmp_nuts3.csv, regional_dmp_nuts2.csv, dmp_files_used.csv,
#   pixels_retained_nuts2.csv / .xlsx (cells < 5 highlighted),
#   mask_sources_by_year.csv is written by 01c
# =============================================================================
source("00_config.R")
setup_cdse_s3()

plan <- fread(file.path(dir_out1, "mask_sources_by_year.csv"))   # from 01c
missing_masks <- plan[year %in% years_all & !file.exists(mask_file)]
if (nrow(missing_masks))
  stop("Masks missing for ", paste(unique(missing_masks$year), collapse = ", "), ": ",
       paste(unique(basename(missing_masks$mask_file)), collapse = ", "),
       ". Run 01c_masks.R until it finishes (status OK) before 01d.", call. = FALSE)

# ---- 1. Pixel index per year (local files only, a few minutes per year) ----
# Lists, block by block, the retained pixels of the year (grass or maize
# mask > mask_min_cover, inside an EU/UK NUTS3) and counts pixels and areas per NUTS3.
# The index (and the dekad tables computed from it) carry a tag that changes
# whenever the inputs change (NUTS3 raster, mask files, mask_min_cover), so
# results computed with older masks or rules are never reused by mistake.
index_tag <- function(y) {
  g <- grid_of_year(y); v <- nuts_version_of_year(y)
  f <- c(zone_file(v, g), plan[year == y & forage == "grass", mask_file],
         plan[year == y & forage == "maize", mask_file])
  info <- file.info(f)
  key <- paste(c(f, info$size, as.numeric(info$mtime), mask_min_cover), collapse = "|")
  tmp <- tempfile(); writeLines(key, tmp)
  substr(unname(tools::md5sum(tmp)), 1, 8)
}
tags <- setNames(vapply(years_all, index_tag, ""), years_all)
index_file <- function(y) file.path(dir_index, sprintf("index_%d_%s.rds", y, tags[[as.character(y)]]))
for (y in years_all) {
  if (file.exists(index_file(y))) next
  g <- grid_of_year(y); v <- nuts_version_of_year(y)
  msg("Pixel index ", y, " (", g, " grid, NUTS ", v, ")")
  grid <- readRDS(grid_file(g))
  idx <- build_year_index(zone_file(v, g),
                          plan[year == y & forage == "grass", mask_file],
                          plan[year == y & forage == "maize", mask_file], grid)
  idx$grid <- grid; idx$year <- y; idx$nuts_version <- v
  saveRDS(idx, index_file(y))
  msg("  ", length(idx$blocks), " blocks with retained pixels; ",
      sum(idx$stats[, "n_grass"]), " grass and ", sum(idx$stats[, "n_maize"]), " maize pixels")
}

# ---- 2. List the dekad files of the growing season (catalogue, no download)
tasks <- rbindlist(lapply(years_all, function(y) {
  items <- stac_search(dmp_collection(y), sprintf("%d-01-01", y), sprintf("%d-12-31", y), asset = dmp_asset)
  if (!nrow(items)) { warning("No DMP files found for ", y); return(NULL) }
  items <- select_dekads(items[year(end) == y])   # most consolidated version of each dekad
  # growing season only: dekads from 1 March to 30 September (21 dekads)
  items <- items[start >= as.Date(paste0(y, "-", season_start)) & end <= as.Date(paste0(y, "-", season_end))]
  if (nrow(items) < 21) warning(y, ": only ", nrow(items), " dekads found in the growing season")
  dir.create(file.path(dir_dekad, y, tags[[as.character(y)]]), recursive = TRUE, showWarnings = FALSE)
  items[, .(year = y, collection = dmp_collection(y), id, start, end, days, rt,
            vsi = s3_to_vsi(href), index_file = index_file(y),
            out_file = file.path(dir_dekad, y, tags[[as.character(y)]], paste0(id, ".rds")))]
}))
write_csv(tasks[, .(year, collection, id, start, end, days, rt)], file.path(dir_out1, "dmp_files_used.csv"))

# ---- 3. DMP sums per NUTS3 for each dekad (remote reads, in parallel) -------
todo <- tasks[!file.exists(out_file)]
msg(nrow(tasks) - nrow(todo), " dekads already done, ", nrow(todo), " to process with ", n_workers, " workers")
all_failed <- character(0)
if (nrow(todo)) {
  cl <- start_workers()
  # workers are always stopped, even after an error or an interruption
  tryCatch({
    t0 <- Sys.time()
    # process year by year, so that progress is visible and restarts lose little
    for (y in unique(todo$year)) {
      ty <- todo[year == y, .(year, vsi, index_file, out_file)]
      res <- unlist(parallel::parLapplyLB(cl, split(ty, seq_len(nrow(ty))), worker_fun(dekad_sums)))
      failed <- res[startsWith(res, "FAILED")]
      msg(y, " done (", nrow(ty) - length(failed), " of ", nrow(ty), " dekads), elapsed ",
          format(round(Sys.time() - t0, 1)))
      if (length(failed)) { msg("  failed: ", failed); all_failed <- c(all_failed, failed) }
    }
  }, finally = parallel::stopCluster(cl))
  # dekads already computed are kept: rerunning the script only redoes the failed ones
  if (length(all_failed))
    stop(length(all_failed), " dekad(s) could not be read after several attempts (network?). ",
         "Run 01d_regional_dmp.R again: only these dekads will be recomputed.")
}

# ---- 4. Read the dekad tables and attach NUTS3 / NUTS2 codes ---------------
dk <- rbindlist(lapply(seq_len(nrow(tasks)), function(i) {
  if (!file.exists(tasks$out_file[i])) return(NULL)
  cbind(readRDS(tasks$out_file[i]), year = tasks$year[i], days = tasks$days[i], id = tasks$id[i])
}))
lookup <- rbindlist(lapply(unique(nuts_version_of_year(years_all)), function(v)
  cbind(fread(zone_lookup_file(v), encoding = "UTF-8"), nuts_version = v)))
dk[, nuts_version := nuts_version_of_year(year)]
dk <- merge(dk, lookup[, .(nuts_version, zone, nuts3)], by = c("nuts_version", "zone"))
dk[, nuts2 := substr(nuts3, 1, 4)]

# ---- 5. Seasonal DMP per region, at every NUTS level --------------------------
# For one region: dekad mean = sum / number of valid pixels (sums and counts
# of its NUTS3 are added first for NUTS2, NUTS1 and NUTS0); seasonal value =
# sum(days x dekad mean), scaled by season_days / days with valid data;
# blank if valid dekads cover < 75% of the season.
dk[, `:=`(nuts1 = substr(nuts3, 1, 3), nuts0 = substr(nuts3, 1, 2))]
seasonal <- function(d, by) {
  d <- d[, .(dmp_sum = sum(dmp_sum), n_valid = sum(n_valid)), by = c(by, "year", "forage", "days", "id")]
  d <- d[, .(valid_days = sum(days), s = sum(days * dmp_sum / n_valid)), by = c(by, "year", "forage")]
  d[, dmp := fifelse(valid_days >= dmp_min_valid_share * season_days,
                     s * season_days / valid_days / 1000, NA_real_)]
  setnames(d, by, "code")
  d[, .(code, year, forage, dmp, valid_days)]
}
lev <- c("nuts3", "nuts2", "nuts1", "nuts0")
val <- rbindlist(lapply(lev, function(l) cbind(level = l, seasonal(dk, l))))

# ---- 6. Retained pixels and Copernicus areas per NUTS3 (from the index) ----
st3 <- rbindlist(lapply(years_all, function(y) {
  if (!file.exists(index_file(y))) return(NULL)
  idx <- readRDS(index_file(y)); s <- idx$stats
  z <- data.table(zone = seq_len(nrow(s)), year = y, nuts_version = idx$nuts_version)
  rbind(cbind(z, forage = "grass", n_pixels = s[, "n_grass"], copernicus_area_ha = s[, "area_grass"]),
        cbind(z, forage = "maize", n_pixels = s[, "n_maize"], copernicus_area_ha = s[, "area_maize"]))
}))
# all NUTS3 of the applicable version get a row (even without any pixel)
r3 <- merge(lookup, unique(st3[, .(year, nuts_version)]), by = "nuts_version", allow.cartesian = TRUE)
r3 <- rbind(copy(r3)[, forage := "grass"], copy(r3)[, forage := "maize"])
r3 <- merge(r3, st3, by = c("year", "nuts_version", "zone", "forage"), all.x = TRUE)
r3[is.na(n_pixels), `:=`(n_pixels = 0, copernicus_area_ha = 0)]
r3[, `:=`(nuts2 = substr(nuts3, 1, 4), nuts1 = substr(nuts3, 1, 3), nuts0 = substr(nuts3, 1, 2))]
r3 <- merge(r3, plan[, .(year, forage, mask = basename(mask_file))], by = c("year", "forage"), all.x = TRUE)

# Pixel counts at every level (sums of the NUTS3 counts)
npx <- rbindlist(lapply(lev, function(l)
  r3[, .(n_pixels = sum(n_pixels)), by = c(l, "year", "forage")][, level := l][]
  |> setnames(l, "code")))
val <- merge(npx, val, by = c("level", "code", "year", "forage"), all.x = TRUE)
# a level is usable if it has >= min_pixels retained pixels and a valid DMP value
val[, usable := n_pixels >= min_pixels & !is.na(dmp)]

# ---- 7. Last resort at national level: own country, then a neighbour ------
# Grass and maize: if the national value has < min_pixels (or no valid DMP),
# the national DMP of the same forage type in the neighbouring country (which
# must itself have >= min_pixels). Neighbours: neighbour_country in 00_config.R.
# Error if no usable neighbour.
last <- val[level == "nuts0",
            .(nuts0 = code, year, forage, l_dmp = fifelse(usable, dmp, NA_real_), l_src = "NUTS0")]
miss <- last[is.na(l_dmp), .(nuts0, year, forage, neighbour = unname(neighbour_country[nuts0]))]
if (nrow(miss)) {
  miss <- merge(miss, last[!is.na(l_dmp), .(neighbour = nuts0, year, forage, nb_dmp = l_dmp)],
                by = c("neighbour", "year", "forage"), all.x = TRUE)
  ok <- miss[!is.na(nb_dmp)]
  if (nrow(ok)) {
    last[ok, on = .(nuts0, year, forage),
         `:=`(l_dmp = i.nb_dmp, l_src = fifelse(i.forage == "grass", "NEIGHBOUR_GRASS", "NEIGHBOUR_MAIZE"))]
    msg("National DMP of the neighbouring country used for:")
    print(ok[, .(country = nuts0, forage, year, neighbour, dmp = nb_dmp)])
  }
  bad0 <- miss[is.na(nb_dmp)]
  if (nrow(bad0))
    stop("No usable national DMP and no usable neighbour (neighbour_country in 00_config.R) for: ",
         paste(bad0$nuts0, bad0$forage, bad0$year, collapse = ", "),
         ". Add the country, or another neighbour, to neighbour_country.", call. = FALSE)
}

# ---- 8. Fallback: value of the first usable level ---------------------------
# NUTS3 -> NUTS2 -> NUTS1 -> NUTS0 -> NUTS0 of the neighbouring country (same
# forage type); each level needs >= min_pixels retained pixels
resolve <- function(d, levels) {
  d <- copy(d)
  d[, `:=`(dmp = NA_real_, dmp_source = NA_character_)]
  for (l in levels) {
    u <- val[level == l, .(code, year, forage, u_ok = usable, u_dmp = dmp)]
    setnames(u, "code", l)
    d <- merge(d, u, by = c(l, "year", "forage"), all.x = TRUE)
    d[is.na(dmp) & u_ok %in% TRUE, `:=`(dmp = u_dmp, dmp_source = toupper(l))]
    d[, c("u_ok", "u_dmp") := NULL]
  }
  d <- merge(d, last, by = c("nuts0", "year", "forage"), all.x = TRUE)    # last: section 7
  d[is.na(dmp), `:=`(dmp = l_dmp, dmp_source = l_src)]
  d[, c("l_dmp", "l_src") := NULL]
  d
}
own <- function(l) val[level == l, .(code, year, forage, dmp_own = dmp)] |> setnames("code", l)

r3 <- resolve(r3, lev)
r3 <- merge(r3, own("nuts3"), by = c("nuts3", "year", "forage"), all.x = TRUE)

# NUTS2 table: counts and areas summed from NUTS3, names from the NUTS2 maps
r2 <- r3[, .(n_pixels = sum(n_pixels), copernicus_area_ha = sum(copernicus_area_ha)),
         by = .(year, nuts_version, forage, mask, nuts2, nuts1, nuts0, cntr)]
all2 <- rbindlist(lapply(unique(r3$nuts_version), function(v) {
  n <- st_read(file.path(dir_nuts, sprintf("NUTS2_EU_%d.gpkg", v)), quiet = TRUE)
  data.table(nuts2 = n$nuts_id, name = n$name, nuts_version = v)
}))
r2 <- merge(r2, all2, by = c("nuts2", "nuts_version"), all.x = TRUE)
r2 <- resolve(r2, lev[-1])
r2 <- merge(r2, own("nuts2"), by = c("nuts2", "year", "forage"), all.x = TRUE)

# ---- 9. Save the regional tables (names in ASCII, original in name_utf8) ----
r3[, `:=`(name_utf8 = name, name = to_ascii(name))]
r2[, `:=`(name_utf8 = name, name = to_ascii(name))]
cols3 <- c("year", "nuts_version", "cntr", "nuts2", "nuts3", "name", "forage", "mask",
           "n_pixels", "dmp_source", "dmp", "dmp_own", "copernicus_area_ha", "name_utf8")
write_csv(r3[, ..cols3][order(forage, nuts3, year)], file.path(dir_out1, "regional_dmp_nuts3.csv"))
cols2 <- c("year", "nuts_version", "cntr", "nuts2", "name", "forage", "mask",
           "n_pixels", "dmp_source", "dmp", "dmp_own", "copernicus_area_ha", "name_utf8")
write_csv(r2[, ..cols2][order(forage, nuts2, year)], file.path(dir_out1, "regional_dmp_nuts2.csv"))
msg("Level of the DMP value used (NUTS3 table):")
print(dcast(r3[, .N, by = .(forage, dmp_source)], forage ~ dmp_source, value.var = "N", fill = 0))

# ---- 10. Retained pixels per NUTS2 (Excel, cells < 5 highlighted in red) ---
# One row per NUTS2 x forage type, one column per year.
px <- dcast(r2, cntr + nuts2 + name + forage ~ year, value.var = "n_pixels")
setorder(px, forage, nuts2)
write_csv(px, file.path(dir_out1, "pixels_retained_nuts2.csv"))
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "pixels_retained_NUTS2")
openxlsx::writeData(wb, 1, px)
year_cols <- which(names(px) %in% as.character(years_all))
openxlsx::conditionalFormatting(wb, 1, cols = year_cols, rows = 2:(nrow(px) + 1),
                                rule = paste0("<", min_pixels),
                                style = openxlsx::createStyle(bgFill = "#F4B6B6"))
openxlsx::freezePane(wb, 1, firstRow = TRUE, firstCol = FALSE)
add_metadata_sheet(wb, "pixels_retained_nuts2.xlsx", list(pixels_retained_NUTS2 = px))
openxlsx::saveWorkbook(wb, file.path(dir_out1, "pixels_retained_nuts2.xlsx"), overwrite = TRUE)

low <- r2[n_pixels < min_pixels, .N, by = .(forage, year)]
msg("NUTS2 x year with < ", min_pixels, " retained pixels:"); print(low)
msg("Done: step 1 tables in ", dir_out1)
