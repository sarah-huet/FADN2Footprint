# =============================================================================
# 00_config.R - settings shared by all scripts (see CLAUDE.md)
# Sourced at the top of every script. Run scripts from the project root
# (open forage_yield.Rproj in RStudio).
# =============================================================================

# Full chain (each script is skipped quickly where its outputs are up to date):
# source("run_logged.R")
# run_chain(c("01a_nuts_maps.R", "01b_grids_zones.R", "01c_masks.R", "01d_regional_dmp.R",
#             "02_calibration_france.R", "03_prediction_eu.R"))

# ---- Packages ---------------------------------------------------------------
pkgs <- c("terra", "sf", "data.table", "readxl", "openxlsx", "stringi",
          "httr2", "jsonlite", "ggplot2")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  stop("Install missing packages first: install.packages(c(",
       paste0('"', missing_pkgs, '"', collapse = ", "), "))")
}
suppressPackageStartupMessages({
  library(terra); library(sf); library(data.table)
})
options(timeout = 3600)   # downloads (NUTS files) can take more than R's default 60 s
options(cli.progress_show_after = Inf)   # no retry progress bars in the logs

# ---- Folders ----------------------------------------------------------------
# All generated files go to subfolders (git-ignored); scripts stay at the root.
dir_raw      <- "raw_data"
dir_nuts     <- file.path(dir_raw, "NUTS_maps")
dir_clc      <- file.path(dir_raw, "CLC")          # CORINE rasters, downloaded by hand
dir_grids    <- file.path(dir_raw, "grids")        # DMP grids (1 km, 300 m) and NUTS3 rasters
dir_index    <- file.path(dir_raw, "pixel_index")  # retained pixels per year (from masks)
dir_dekad    <- file.path(dir_raw, "dmp_dekad")    # DMP sums per NUTS3 and dekad
dir_masks    <- file.path(dir_raw, "masks")        # mask fractions on the DMP grids
dir_tiles    <- file.path(dir_raw, "masks", "tiles_100m")  # intermediate CTY tiles
dir_out      <- "outputs"
dir_out1     <- file.path(dir_out, "step1")
dir_out2     <- file.path(dir_out, "step2")
dir_out3     <- file.path(dir_out, "step3")
dir_paper    <- file.path(dir_out, "paper")     # figures and tables of the working paper
for (d in c(dir_nuts, dir_clc, dir_grids, dir_index, dir_dekad, dir_masks, dir_tiles,
            dir_out1, dir_out2, dir_out3, dir_paper)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Keep track of the parameters: a copy of this file is saved as
# outputs/00_config.R each time a script runs (overwritten at each run; the
# parallel workers, which also load this file, do not write it).
if (!nzchar(Sys.getenv("FORAGE_WORKER")))
  writeLines(c(paste0("# Copy of 00_config.R saved on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      " (overwritten each time a script runs)"),
               readLines("00_config.R", warn = FALSE)),
             file.path(dir_out, "00_config.R"))

# French annual agricultural statistics (SAA, Agreste), departement level:
# - 2010-2025 series (current definitions), Excel file, sheets FOU and COP
# - before 2010: yearly files FDS_FOURRAGE_<year>.csv (forage) and
#   FDS_DEVELOPPE_<year>.csv (crops incl. wheat and grain maize), used for the
#   years saa_fds_years only (the 2010-2025 series is used from 2010)
dir_saa       <- file.path(dir_raw, "SAA")
saa_file      <- file.path(dir_saa, "SAA_2010-2025_provisoires_donnees_departementales.xlsx")
saa_fds_years <- 2004:2009
saa_fds_file  <- function(type, y)
  file.path(dir_saa, c(FOU = "FDS_FOURRAGE", COP = "FDS_DEVELOPPE_2")[[type]],
            sprintf("%s_%d.csv", c(FOU = "FDS_FOURRAGE", COP = "FDS_DEVELOPPE")[[type]], y))

# terra temporary files (large rasters): change if C: is short of space
dir.create(file.path(dir_raw, "terra_tmp"), showWarnings = FALSE)
terraOptions(tempdir = file.path(dir_raw, "terra_tmp"), memfrac = 0.6, progress = 0)

# ---- Years and periods ------------------------------------------------------
# Years processed (step 1) and predicted (step 3). For a quick test of the
# whole chain, use a single year, e.g. years_all <- 2017
years_all <- 2004:2024
period_of_year <- function(y) ifelse(y <= 2013, "A", ifelse(y <= 2016, "B", "C"))
# Period A: up to 2013 (DMP 1 km SPOT-VGT + CLC masks)
# Period B: 2014-2016 (DMP 300 m PROBA-V + HRL 2017 masks)
# Period C: 2017 onwards (DMP 300 m PROBA-V then Sentinel-3 OLCI from 2019 + year-specific
#           HRL masks)
# Labels used in figures and tables
period_label <- c(A = paste0(min(years_all), "-2013"), B = "2014-2016", C = paste0("2017-", max(years_all)))
# Changes in input data within or between periods (vertical lines in the figures)
input_breaks <- data.frame(
  year  = c(2014, 2017, 2019),
  label = c("DMP 1 km -> 300 m; CLC -> HRL masks",
            "Year-specific HRL masks (2014-2016 use the 2017 masks)", "Sentinel-3 OLCI replaces PROBA-V"))

# ---- Forage types -----------------------------------------------------------
forage_mask <- c(maize = "maize", grass = "grass")   # Copernicus masks

# ---- Dry matter productivity (CDSE STAC collections) -----------------------
# DMP is in kg DM/ha/day (10-daily). Seasonal sums are converted to tDM/ha.
dmp_collection <- function(y) {
  if (y <= 2013) "clms_dmp_global_1km_10daily_v2_cog"   # 1999-2020; files used here (2004-2013): SPOT-VGT
  else           "clms_dmp_global_300m_10daily_v2_cog"  # PROBA-V 2014-2018, Sentinel-3 OLCI 2019-
}
dmp_asset <- "dmp"
# A region-year is kept if the dekads with valid DMP in the region cover at
# least this share of the growing season (missing dekads filled by the mean).
dmp_min_valid_share <- 0.75

# ---- Growing season ---------------------------------------------------------
# DMP is summed only over the dekads from 1 March to 30 September (21 dekads,
# 214 days), for both grassland and maize. Values are in tDM/ha per season.
season_start <- "03-01"
season_end   <- "09-30"
season_days  <- as.integer(as.Date(paste0("2001-", season_end)) - as.Date(paste0("2001-", season_start))) + 1L

# ---- Computation settings ---------------------------------------------------
# DMP files are read by blocks of block_size x block_size pixels, only where
# there are retained pixels; dekads are processed in parallel by n_workers R
# processes (network-bound: more workers = faster, until the connection or
# the CDSE server saturates). Lower n_workers if the computer gets too slow.
block_size <- 512
n_workers  <- 4 #max(1, min(8, parallel::detectCores() - 1))

# Window read from the global DMP files (lon/lat): EU + UK incl. Azores,
# Madeira, Canaries and Cyprus. French outermost regions (DOM) are outside.
eu_bbox <- c(xmin = -32, ymin = 27, xmax = 45, ymax = 72)

# ---- Masks ------------------------------------------------------------------
mask_min_cover <- 0.9   # keep DMP pixels with more than this share (90%) covered by the
                        # forage type (any value >= 0.5: a pixel cannot count for both)
min_pixels     <- 30     # fewer retained pixels -> value of the parent region
                        # (NUTS3 -> NUTS2 -> NUTS1 -> NUTS0 -> neighbour, see below)
# Last resort at national level, using a designated neighbouring country: a
# country with < min_pixels pixels of a forage type takes the national DMP of
# its neighbour for that forage type (dmp_source = NEIGHBOUR_GRASS or
# NEIGHBOUR_MAIZE). With mask_min_cover = 0.9 this is needed for grass in MT
# (all years) and FI (2004-2008), and for maize in some years in MT, CY, FI, IE, LU.
neighbour_country <- c(MT = "IT", CY = "EL", FI = "SE", IE = "UK", LU = "BE")

clc_vintages <- c(2006, 2012, 2018)   # 2000 is never the closest for 2004+
clc_classes  <- list(grass = c(231, 321), maize = c(211, 212))
# CLC rasters downloaded by hand (100 m GeoTIFF, EPSG:3035) from
# https://land.copernicus.eu/en/products/corine-land-cover and unzipped in
# raw_data/CLC/ (version 2020_20u1; paths below). The 2018 vintage is never the
# closest one for the years using CLC (2004-2013).
clc_file <- function(v) {
  if (v == 2012) file.path(dir_clc, "u2018_clc2012_v2020_20u1_raster100m", "DATA", "U2018_CLC2012_V2020_20u1.tif")
  else           file.path(dir_clc, "u2012_clc2006_v2020_20u1_raster100m", "DATA", "U2012_CLC2006_V2020_20u1.tif")
}

closest_clc <- function(y) {
  d <- abs(clc_vintages - y)
  max(clc_vintages[d == min(d)])      # tie -> most recent vintage
}

hrl_collections <- c(
  grass = "clms_vlcc_grassland_europe_100m_yearly_v1",  # % permanent grassland per 100 m cell
  maize = "clms_vlcc_crop-types_europe_10m_yearly_v1"   # crop types, maize = 1130
)
cty_maize_code <- 1130

# First year using the HRL (2017+) mask instead of CLC, per forage type.
# Years between this and 2017 reuse the 2017 HRL mask.
hrl_switch_year <- c(grass = 2014, maize = 2014)

# Mask source for a given year and forage type
mask_source <- function(y, f) {
  if (y < hrl_switch_year[[f]]) list(type = "CLC", year = closest_clc(y))
  else                          list(type = "HRL", year = max(2017, y))
}
# NB: if the HRL is not yet available for a recent year, the most recent
# available HRL year is used (reported by 01c_masks.R).

# ---- NUTS -------------------------------------------------------------------
nuts_versions <- c(2003, 2006, 2010, 2013, 2016, 2021, 2024)
nuts_version_of_year <- function(y) vapply(y, function(x) max(nuts_versions[nuts_versions <= x]), numeric(1))
nuts_url <- function(v, level)
  sprintf("https://gisco-services.ec.europa.eu/distribution/v2/nuts/gpkg/NUTS_RG_01M_%d_4326_LEVL_%d.gpkg", v, level)
# Countries absent from a NUTS version: regions taken from an earlier version
# (UK: absent from NUTS 2024 after Brexit -> NUTS 2021 boundaries and codes,
# i.e. its last NUTS version, are added to the NUTS 2024 maps)
nuts_missing_fallback <- c(UK = 2021)
nuts_file <- function(v, level) file.path(dir_nuts, sprintf("NUTS_RG_01M_%d_4326_LEVL_%d.gpkg", v, level))

# EU27 + UK (Eurostat codes: EL = Greece, UK = United Kingdom)
countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","EL","ES","FI","FR","HR","HU",
               "IE","IT","LT","LU","LV","MT","NL","PL","PT","RO","SE","SI","SK","UK")

# ---- Copernicus Data Space credentials --------------------------------------
# S3 keys: create them at https://eodata-s3keysmanager.dataspace.copernicus.eu/panel/s3-credentials
# (log in with your CDSE account), then add to your .Renviron (never commit it):
#   CDSE_S3_KEY=...
#   CDSE_S3_SECRET=...
stac_url <- "https://stac.dataspace.copernicus.eu/v1"

source("00_functions.R")
source("00_metadata.R")
