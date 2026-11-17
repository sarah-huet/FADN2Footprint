# =============================================================================
# 01a_nuts_maps.R - Step 1, output 1: EU-wide NUTS2 and NUTS3 maps
# Downloads the GISCO EU-wide NUTS files (1:1 million, EPSG:4326) for every
# NUTS version used (see "NUTS maps" in CLAUDE.md) into raw_data/NUTS_maps/,
# keeps EU27 + UK, and reports countries missing from each version. A missing
# country listed in nuts_missing_fallback (00_config.R) gets the regions of the
# fallback version (UK in NUTS 2024: NUTS 2021 regions).
# =============================================================================
source("00_config.R")

# ---- 1. Which NUTS versions are needed --------------------------------------
# Each year uses the latest NUTS version not more recent than the year
# (nuts_version_of_year() in 00_config.R), e.g. 2015 -> NUTS 2013.
versions_used <- sort(unique(nuts_version_of_year(years_all)))
msg("NUTS versions used: ", paste(versions_used, collapse = ", "))

# ---- 2. Download the EU-wide GISCO files (skipped if already there) ---------
# One GeoPackage per version and level (2 = NUTS2, 3 = NUTS3).
for (v in sort(unique(c(versions_used, nuts_missing_fallback)))) for (lvl in 2:3) {
  f <- nuts_file(v, lvl)
  if (!file.exists(f)) {
    msg("Downloading NUTS ", v, " level ", lvl)
    download.file(nuts_url(v, lvl), f, mode = "wb", quiet = TRUE)
  }
}

# ---- 3. Function to read one GISCO file and keep what we need ---------------
# Returns one row per region with: NUTS code, country, name, level, version,
# source_version (= version, except for regions taken from a fallback version).
read_nuts <- function(v, lvl) {
  x <- st_read(nuts_file(v, lvl), quiet = TRUE)
  # upper-case attribute names only (renaming the geometry column breaks sf)
  geom_col <- attr(x, "sf_column")
  att <- setdiff(names(x), geom_col)
  names(x)[match(att, names(x))] <- toupper(att)
  # older versions may use GR (Greece) / GB (UK) instead of EL / UK
  x$CNTR_CODE[x$CNTR_CODE == "GR"] <- "EL"
  x$CNTR_CODE[x$CNTR_CODE == "GB"] <- "UK"
  # keep EU27 + UK only (drops candidate countries, EFTA, etc.)
  x <- x[x$CNTR_CODE %in% countries, ]
  # region name in Latin script when available
  name_col <- intersect(c("NAME_LATN", "NUTS_NAME"), names(x))[1]
  out <- st_sf(nuts_id = x$NUTS_ID, cntr = x$CNTR_CODE, name = x[[name_col]],
               level = lvl, version = v, source_version = v, geometry = st_geometry(x))
  st_make_valid(out)   # repairs invalid polygons that would break later steps
}

# ---- 4. Save the EU maps and check which countries each version covers ------
# Output files: raw_data/NUTS_maps/NUTS2_EU_<version>.gpkg and NUTS3_EU_<version>.gpkg
nuts <- list()
coverage <- list()
for (v in versions_used) {
  for (lvl in 2:3) {
    n <- read_nuts(v, lvl)
    # countries missing from this version with a fallback version: add their
    # regions from the fallback version (version = v, source_version = fallback)
    for (cty in setdiff(names(nuts_missing_fallback), unique(n$cntr))) {
      fb <- nuts_missing_fallback[[cty]]
      if (fb >= v) next
      add <- read_nuts(fb, lvl); add <- add[add$cntr == cty, ]
      add$version <- v
      n <- rbind(n, add)
      msg("NUTS ", v, " level ", lvl, ": ", cty, " missing, ", nrow(add), " regions taken from NUTS ", fb)
    }
    nuts[[paste(v, lvl)]] <- n
    # rewrite the map only if its list of regions changed, so that the NUTS3
    # rasters (01b) and the cached DMP sums (01d) of other years stay valid
    out_file <- file.path(dir_nuts, sprintf("NUTS%d_EU_%d.gpkg", lvl, v))
    same <- file.exists(out_file) && {
      old <- st_read(out_file, quiet = TRUE)
      identical(sort(old$nuts_id), sort(n$nuts_id))
    }
    if (!same) {
      st_write(n, out_file, delete_dsn = TRUE, quiet = TRUE)
      msg("Written: ", out_file)
    }
  }
  # countries of the EU27 + UK list absent from this version's NUTS3 map
  n3 <- nuts[[paste(v, 3)]]
  present <- unique(n3$cntr[n3$source_version == v])
  filled <- unique(n3$cntr[n3$source_version != v])
  coverage[[as.character(v)]] <- data.table(version = v,
                                            missing_countries = paste(setdiff(countries, present), collapse = " "),
                                            filled_from_earlier_version = paste(sprintf("%s (NUTS %d)", filled,
                                              nuts_missing_fallback[filled])[length(filled) > 0], collapse = " "))
}

# ---- 5. Report missing countries (not fixed unless a fallback is set) -------
coverage <- rbindlist(coverage)
write_csv(coverage, file.path(dir_out1, "nuts_versions_missing_countries.csv"))
print(coverage)
unfilled <- coverage[, .(left = paste(setdiff(strsplit(missing_countries, " ")[[1]],
                                               names(nuts_missing_fallback)), collapse = " ")), by = version]
if (any(unfilled$left != ""))
  warning("Some countries are missing from some NUTS versions without fallback (see table above). ",
          "Per CLAUDE.md, this is reported and not fixed: these countries get blanks for those years.")
msg("Done: EU NUTS2/NUTS3 maps in ", dir_nuts)
