# Estimate grassland productivity at NUTS3 x year level across Europe.
#
# Run from the package root. By default, download the Copernicus grassland
# raster collection (all available years from 2017 onwards), then stop.
# This collection describes grassland cover, not grassland productivity.
# Set download_only <- FALSE to run the separate productivity workflow.
# Its input files are deliberately configurable:
# the vector map must contain grassland polygons and a NUTS3 code attribute; the
# satellite file must contain one annual indicator summary per NUTS3; and the
# Agreste file must contain annual French grassland yields (or production and
# area from which those yields can be calculated).

# 1. Retrieve/read the grassland map ---------------------------------------
# https://land.copernicus.eu/en/products/high-resolution-layer-grasslands/grassland-2017-present-raster-10-m-europe-yearly
# S3 methodology: https://documentation.dataspace.copernicus.eu/APIs/S3.html
# Uncomment the following line to enable downloading the Copernicus grassland rasters
source("inst/scripts/grassland_download_hdr.R")
# Step 1: list what exists, WITHOUT downloading, to check sizes and names
prods <- cdse_query_products("grassland")
print(prods, n = 20)
cat(
    "Total size (GB):",
    round(sum(prods$content_length, na.rm = TRUE) / 1e9, 1),
    "\n"
)

# Step 2: filter by year 2020 (check the naming pattern in `prods$name` first)
sel <- prods |> filter(str_detect(name, "2020"))

# Step 3: download
cdse_download_all(sel, dest_dir = "data/raw/hrl_grassland")


#Next steps: unzip, build a mosaic, filter by confidence, aggregate to the resolution of your satellite indicator, then extract the indicator over grassland pixels. Here is a script for each stage. As before, I haven't run it on your files, so check the file names and raster value codes first.

## Step 0: Inspect before processing

library(terra)
library(stringr)

zips <- list.files(
    "data/raw/hrl_grassland",
    pattern = "\\.zip$",
    full.names = TRUE
)
length(zips)

# Unzip everything
for (z in zips) {
    unzip(z, exdir = "data/unzipped/hrl_grassland")
}

tifs <- list.files(
    "data/unzipped/hrl_grassland",
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE
)
head(tifs)

r <- rast(tifs[1])
print(r) # CRS (expected EPSG:3035), resolution (10 m), datatype
freq(r) # value codes: check what means grassland / non-grassland / no data

#Check `freq(r)` carefully. I'm assuming 1 = grassland, 0 = other, and a high value (such as 255) = no data, but the product documentation or the layer metadata is authoritative. Adjust the `GRASS_VALUE` below if needed.

## Step 1: Mosaic (virtual)

#A virtual raster avoids writing a huge file.

library(terra)

tifs <- list.files(
    "data/unzipped/hrl_grassland",
    pattern = "\\.tif$",
    recursive = TRUE,
    full.names = TRUE
)
# Keep only the grassland layer files, not the confidence layer, if they are in the same folder
# tifs <- tifs[!str_detect(tifs, "CONF|confidence")]

vrt_grass <- vrt(tifs, "data/processed/grass_2020.vrt", overwrite = TRUE)

#Do the same for the confidence layer if you downloaded it (`vrt_conf`).

## Step 2: Mask low-confidence pixels (optional)

GRASS_VALUE <- 1
CONF_MIN <- 50 # adjust to the scale of the confidence layer

grass <- vrt_grass == GRASS_VALUE # TRUE/FALSE grassland mask
# conf  <- vrt(conf_tifs)
# grass <- grass & (conf >= CONF_MIN)

## Step 3: Aggregate to the satellite resolution

#The goal is a grassland fraction per coarse pixel. The cleanest approach is to project the grassland mask onto the grid of your satellite indicator, so both layers line up exactly.
#
#First, get your indicator on a reference grid. Example with a NDVI raster you already have (`ndvi_ref`):

ndvi_ref <- rast("data/ndvi/ndvi_growing_season_2020.tif") # your indicator

# Fraction of 10 m grassland pixels in each coarse pixel (mean of 0/1 values)
grass_frac <- terra::project(
    as.numeric(grass), # 1 / 0
    ndvi_ref, # target grid (CRS, extent, resolution)
    method = "average" # average of the fine pixels = grassland fraction
)

writeRaster(
    grass_frac,
    "data/processed/grass_fraction_2020.tif",
    overwrite = TRUE
)

#For a very large extent, this can be memory-heavy. Two options:
#- Work **by country or tile**: crop `grass` to the extent of each country first (`crop()` with a NUTS or country polygon), process, and merge.
#- Use `terra::aggregate(grass, fact = 25, fun = "mean")` (for example 25 × 10 m = 250 m), then `resample()` or `project()` to the indicator grid. This is more efficient if the target resolution is a multiple of 10 m in EPSG:3035.

## Step 4: Intersect with the satellite indicator

# Keep only pixels that are mostly grassland
GRASS_FRAC_MIN <- 0.5
ndvi_grass <- mask(ndvi_ref, grass_frac >= GRASS_FRAC_MIN, maskvalues = FALSE)

Or
keep
the
fraction
as
a
weight
and
compute
a
weighted
mean
per
region.

## Step 5: Summarise by region (links to FADN)

#EU FADN works at the NUTS/region level, so use NUTS polygons from the `giscoR` package.

library(giscoR)
library(sf)

nuts2 <- gisco_get_nuts(year = "2021", nuts_level = 2, epsg = 3035)

# Mean NDVI over grassland pixels, per region
res <- terra::extract(
    ndvi_grass,
    vect(nuts2),
    fun = "mean",
    na.rm = TRUE
)
nuts2$ndvi_grass_mean <- res[, 2]

# Total grassland area per region (ha): fraction * pixel area
pix_ha <- prod(res(grass_frac)) / 1e4
area <- terra::extract(grass_frac, vect(nuts2), fun = "sum", na.rm = TRUE)
nuts2$grass_area_ha <- area[, 2] * pix_ha


## Points to decide

#1. **Indicator not yet chosen.** You haven't told me which indicator you will use (MODIS NDVI, Copernicus fAPAR, Sentinel-2). Tell me, and I will write the download code for it, as I did for the grassland layer.
#2. **Time integration.** Use the growing-season sum or mean of NDVI or fAPAR, for example April to October, rather than a single date.
#3. **Grassland year.** The HRL grassland map is yearly, so use the 2020 mask with 2020 satellite data. Compare a few years if you want to account for grassland rotation with crops.
#4. **Validation.** Compare the regional grassland area you get with Eurostat or CLC statistics to detect mask or reprojection problems.
#
#If you tell me the indicator and the spatial level you target (NUTS2, farm region, LPIS parcels), I'll write the next step.

# Requires AWS CLI v2: https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
# Generate S3 access/secret keys (not your CDSE password) at:
# https://eodata-s3keysmanager.dataspace.copernicus.eu/
# Configure AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY in your environment,
# or use an AWS CLI profile selected through AWS_PROFILE. Never save keys here.
# The bucket is "eodata"; the remaining path is the object-key prefix.
# WARNING: downloading every year at 10 m across Europe can require substantial
# disk space and transfer time. Re-running sync skips unchanged local files
# and retrieves newly published years; it does not delete local files.
download_grassland <- TRUE
download_only <- TRUE
grassland_s3_endpoint <- "https://eodata.dataspace.copernicus.eu"
grassland_s3_source <- paste0(
    "s3://eodata/CLMS/landcover_landuse/grassland_and_herbaceous/",
    "clms_vlcc_grassland_europe_10m_yearly_v1/"
)
grassland_download_dir <- file.path("inst", "ext_data", "copernicus_grassland")

# The downloaded rasters have no NUTS3 attributes. They are NOT substituted
# for this vector input: raster-to-NUTS3 aggregation is a separate step.
grassland_map_source <- file.path("inst", "ext_data", "grassland_nuts3.gpkg")
grassland_nuts3_column <- "NUTS_ID"

# 2. Estimate satellite-derived change relative to 2020 --------------------
satellite_indicator_file <- file.path(
    "inst",
    "ext_data",
    "satellite_nuts3_year.csv"
)
satellite_indicator_column <- "indicator"
years <- 2010:2025

# 3. Read annual French SAA Agreste productivity ---------------------------
# Source: https://agreste.agriculture.gouv.fr/agreste-web/disaron/SAA-SeriesLongues/detail/
# The input should be filtered to the relevant grassland/forage category and
# harmonized to Eurostat NUTS3 codes. Expected columns: nuts3, year, and either
# yield_t_ha, or Production_seche_volume plus Superficie_correspondante_hectare.
# The latter follows the SAA conversion used in this package: volume is in
# 100 kg and area is in hectares.
agreste_file <- file.path(
    "inst",
    "ext_data",
    "agreste_grassland_nuts3_2010_2025.csv"
)

# 4. Extrapolate annual productivity estimates to other European NUTS3 -------
# Non-French regions use the grassland-area-weighted French 2020 yield as the
# baseline, scaled by the region's satellite indicator relative to 2020.
# This is an indicator-relative proxy, not an independently observed yield.
output_file <- file.path(
    "inst",
    "ext_data",
    "grassland_productivity_nuts3_year.csv"
)

if (download_grassland) {
    aws_cli <- unname(Sys.which("aws"))
    if (!nzchar(aws_cli)) {
        stop(
            "AWS CLI v2 is required to retrieve the Copernicus grassland map. ",
            "Install it, restart R so that 'aws' is on PATH, and configure ",
            "your CDSE S3 credentials as described above.",
            call. = FALSE
        )
    }
    if (
        !dir.exists(grassland_download_dir) &&
            !dir.create(
                grassland_download_dir,
                recursive = TRUE,
                showWarnings = FALSE
            )
    ) {
        stop(
            "Could not create download directory: ",
            grassland_download_dir,
            call. = FALSE
        )
    }
    message(
        "Synchronizing all available Copernicus grassland years to ",
        grassland_download_dir,
        ". This may be a large download."
    )
    download_status <- system2(
        aws_cli,
        args = c(
            "s3",
            "sync",
            shQuote(grassland_s3_source),
            shQuote(normalizePath(grassland_download_dir, mustWork = TRUE)),
            "--endpoint-url",
            shQuote(grassland_s3_endpoint),
            "--region",
            "default",
            "--no-progress",
            "--no-cli-pager"
        )
    )
    if (download_status != 0L) {
        stop(
            "Copernicus S3 download failed (AWS CLI exit status ",
            download_status,
            "). See the CLI error above; check your CDSE S3 credentials, ",
            "network connection and available disk space. Re-run to retry.",
            call. = FALSE
        )
    }
    grassland_raster_files <- list.files(
        grassland_download_dir,
        pattern = "\\.tiff?$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
    )
    if (!length(grassland_raster_files)) {
        stop(
            "S3 sync completed but no GeoTIFF grassland maps were found in ",
            grassland_download_dir,
            ". Check the collection prefix and downloaded product format.",
            call. = FALSE
        )
    }
    message(
        "Grassland maps available: ",
        length(grassland_raster_files),
        " GeoTIFF file(s) in ",
        grassland_download_dir
    )
}

# Keep raster retrieval independent of the vector-based productivity inputs.
if (!download_only) {
    required_packages <- c("sf")
    missing_packages <- required_packages[
        !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
    ]
    if (length(missing_packages)) {
        stop(
            "Install required package(s): ",
            paste(missing_packages, collapse = ", "),
            call. = FALSE
        )
    }

    read_table <- function(path) {
        if (!file.exists(path)) {
            stop("Input file does not exist: ", path, call. = FALSE)
        }
        utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    }

    require_columns <- function(data, columns, label) {
        absent <- setdiff(columns, names(data))
        if (length(absent)) {
            stop(
                label,
                " is missing required column(s): ",
                paste(absent, collapse = ", "),
                call. = FALSE
            )
        }
    }

    normalize_codes <- function(codes, label) {
        codes <- trimws(as.character(codes))
        if (anyNA(codes) || any(!nzchar(codes))) {
            stop(
                label,
                " contains missing or blank NUTS3 codes.",
                call. = FALSE
            )
        }
        codes
    }

    # Read a local vector file or a URL supported by sf::st_read().
    grassland_map <- sf::st_read(grassland_map_source, quiet = TRUE)
    if (!grassland_nuts3_column %in% names(grassland_map)) {
        stop(
            "The grassland map must have a NUTS3 code attribute named '",
            grassland_nuts3_column,
            "'. Set grassland_nuts3_column to the correct field.",
            call. = FALSE
        )
    }
    if (is.na(sf::st_crs(grassland_map))) {
        stop(
            "The grassland map has no CRS; assign its correct CRS before use.",
            call. = FALSE
        )
    }
    if (
        any(
            !as.character(sf::st_geometry_type(grassland_map)) %in%
                c("POLYGON", "MULTIPOLYGON")
        )
    ) {
        stop(
            "The grassland map must contain polygon geometries.",
            call. = FALSE
        )
    }

    grassland_map$nuts3 <- normalize_codes(
        grassland_map[[grassland_nuts3_column]],
        "Grassland map"
    )
    grassland_map <- sf::st_transform(grassland_map, 3035)
    grassland_map$grassland_area_ha <-
        as.numeric(sf::st_area(grassland_map)) / 10000
    grassland_area <- stats::aggregate(
        grassland_area_ha ~ nuts3,
        data = sf::st_drop_geometry(grassland_map),
        FUN = sum
    )
    if (
        any(
            !is.finite(grassland_area$grassland_area_ha) |
                grassland_area$grassland_area_ha <= 0
        )
    ) {
        stop(
            "The grassland map contains invalid or zero NUTS3 area totals.",
            call. = FALSE
        )
    }

    satellite <- read_table(satellite_indicator_file)
    require_columns(
        satellite,
        c("nuts3", "year", satellite_indicator_column),
        "Satellite indicator table"
    )
    satellite$nuts3 <- normalize_codes(
        satellite$nuts3,
        "Satellite indicator table"
    )
    satellite$year <- suppressWarnings(as.integer(satellite$year))
    satellite$indicator <- suppressWarnings(
        as.numeric(satellite[[satellite_indicator_column]])
    )
    if (anyNA(satellite$year) || any(!satellite$year %in% years)) {
        stop(
            "Satellite indicator years must be integers from 2010 through 2025.",
            call. = FALSE
        )
    }
    if (any(!is.finite(satellite$indicator)) || any(satellite$indicator <= 0)) {
        stop(
            "Satellite indicators must be finite and positive for ratio-based comparison.",
            call. = FALSE
        )
    }
    if (anyDuplicated(satellite[c("nuts3", "year")])) {
        stop(
            "Satellite data must have one row per NUTS3 x year.",
            call. = FALSE
        )
    }

    agreste <- read_table(agreste_file)
    require_columns(agreste, c("nuts3", "year"), "Agreste table")
    agreste$nuts3 <- normalize_codes(agreste$nuts3, "Agreste table")
    agreste$year <- suppressWarnings(as.integer(agreste$year))
    if (anyNA(agreste$year) || any(!agreste$year %in% years)) {
        stop(
            "Agreste years must be integers from 2010 through 2025.",
            call. = FALSE
        )
    }
    if ("yield_t_ha" %in% names(agreste)) {
        agreste$yield_t_ha <- suppressWarnings(as.numeric(agreste$yield_t_ha))
    } else {
        require_columns(
            agreste,
            c("Production_seche_volume", "Superficie_correspondante_hectare"),
            "Agreste table"
        )
        production_100kg <- suppressWarnings(as.numeric(
            agreste$Production_seche_volume
        ))
        area_ha <- suppressWarnings(as.numeric(
            agreste$Superficie_correspondante_hectare
        ))
        if (
            any(!is.finite(production_100kg)) ||
                any(!is.finite(area_ha)) ||
                any(area_ha <= 0)
        ) {
            stop(
                "Agreste production and area must be finite, with area greater than zero.",
                call. = FALSE
            )
        }
        agreste$yield_t_ha <- production_100kg * 0.1 / area_ha
    }
    if (any(!is.finite(agreste$yield_t_ha)) || any(agreste$yield_t_ha < 0)) {
        stop(
            "Agreste yields must be finite and non-negative, in tonnes per hectare.",
            call. = FALSE
        )
    }
    if (anyDuplicated(agreste[c("nuts3", "year")])) {
        stop(
            "Agreste data must have one row per NUTS3 x year; filter/aggregate categories first.",
            call. = FALSE
        )
    }
    agreste <- agreste[
        startsWith(agreste$nuts3, "FR"),
        c("nuts3", "year", "yield_t_ha")
    ]

    map_codes <- grassland_area$nuts3
    expected <- expand.grid(
        nuts3 = map_codes,
        year = years,
        KEEP.OUT.ATTRS = FALSE,
        stringsAsFactors = FALSE
    )
    satellite_key <- paste(satellite$nuts3, satellite$year, sep = "\r")
    expected_key <- paste(expected$nuts3, expected$year, sep = "\r")
    satellite_match <- match(expected_key, satellite_key)
    if (anyNA(satellite_match)) {
        stop(
            "Satellite data are incomplete for the grassland map's NUTS3 x year panel; ",
            "missing ",
            sum(is.na(satellite_match)),
            " row(s).",
            call. = FALSE
        )
    }
    expected$indicator <- satellite$indicator[satellite_match]
    baseline <- expected[expected$year == 2020, c("nuts3", "indicator")]
    baseline_match <- match(expected$nuts3, baseline$nuts3)
    expected$satellite_index_2020 <-
        100 * expected$indicator / baseline$indicator[baseline_match]
    expected$satellite_change_pct <- expected$satellite_index_2020 - 100

    agreste_key <- paste(agreste$nuts3, agreste$year, sep = "\r")
    expected$agreste_yield_t_ha <- agreste$yield_t_ha[
        match(paste(expected$nuts3, expected$year, sep = "\r"), agreste_key)
    ]
    french_codes <- unique(expected$nuts3[startsWith(expected$nuts3, "FR")])
    french_baseline_rows <- expected[
        expected$year == 2020 & startsWith(expected$nuts3, "FR"),
    ]
    if (
        !length(french_codes) || anyNA(french_baseline_rows$agreste_yield_t_ha)
    ) {
        stop(
            "Agreste must provide a 2020 yield for every French NUTS3 unit in the map.",
            call. = FALSE
        )
    }
    french_baseline <- merge(
        french_baseline_rows[c("nuts3", "agreste_yield_t_ha")],
        grassland_area,
        by = "nuts3",
        all.x = TRUE,
        sort = FALSE
    )
    french_mean_yield_t_ha <- stats::weighted.mean(
        french_baseline$agreste_yield_t_ha,
        french_baseline$grassland_area_ha
    )

    expected$grassland_area_ha <- grassland_area$grassland_area_ha[
        match(expected$nuts3, grassland_area$nuts3)
    ]
    is_france <- startsWith(expected$nuts3, "FR")
    if (anyNA(expected$agreste_yield_t_ha[is_france])) {
        stop(
            "Agreste must provide a yield for every French NUTS3 unit and year in the map.",
            call. = FALSE
        )
    }
    expected$estimated_yield_t_ha <- ifelse(
        is_france,
        expected$agreste_yield_t_ha,
        french_mean_yield_t_ha * expected$satellite_index_2020 / 100
    )
    expected$yield_method <- ifelse(
        is_france,
        "Agreste observed",
        "French 2020 grassland-area-weighted mean x satellite relative index"
    )

    output <- expected[c(
        "nuts3",
        "year",
        "grassland_area_ha",
        "indicator",
        "satellite_index_2020",
        "satellite_change_pct",
        "estimated_yield_t_ha",
        "yield_method"
    )]
    names(output)[names(output) == "indicator"] <- satellite_indicator_column
    if (any(!is.finite(output$estimated_yield_t_ha))) {
        stop(
            "The estimated productivity output contains non-finite values.",
            call. = FALSE
        )
    }

    output_dir <- dirname(output_file)
    if (
        !dir.exists(output_dir) &&
            !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    ) {
        stop("Could not create output directory: ", output_dir, call. = FALSE)
    }
    utils::write.csv(output, output_file, row.names = FALSE, na = "")
    message("Wrote ", nrow(output), " NUTS3 x year estimates to ", output_file)
}
