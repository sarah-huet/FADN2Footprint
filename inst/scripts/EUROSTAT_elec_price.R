#' EUROSTAT electricity prices (€/kWh, annual mean from semester data)
#'
#' Country-level electricity price time series derived from an EUROSTAT export of
#' dataset \code{NRG_PC_205}. Values are extracted from a custom Excel spreadsheet
#' bundled with the package and processed for use in **FADN2Footprint** (e.g.,
#' energy cost proxies and footprint accounting steps requiring electricity price
#' assumptions).
#'
#' The source table provides prices per semester (S1, S2). This dataset aggregates
#' semester values into an annual mean electricity price per country and year.
#'
#' Units are **euros per kilowatt-hour** (\(€ / kWh\)).
#'
#' @docType data
#' @usage data(EUROSTAT_elec_price)
#'
#' @format
#' A tibble/data frame with one row per country-year combination and 3 columns:
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{euro_kWh}}{Annual mean electricity price in \(€ / kWh\) (numeric), computed as the mean of S1 and S2 when available.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read range \code{A12:AK56} from \code{Sheet 7} of the custom EUROSTAT export.
#'   \item Drop non-data header rows (e.g. \code{"GEO (Labels)"}).
#'   \item Convert price columns to numeric (suppressing coercion warnings).
#'   \item Map country labels to ISO3 codes using \code{data_extra$country_names}.
#'   \item Pivot semester columns (e.g. \code{YYYY-S1}, \code{YYYY-S2}) to long format.
#'   \item Compute annual means by country and year.
#' }
#'
#' @source
#' EUROSTAT dataset \emph{Electricity prices} \code{NRG_PC_205}
#' (processed via a custom Excel export stored in the package at
#' \code{inst/ext_data/EUROSTAT_prices/EUROSTAT_nrg_pc_205__custom_17738899_spreadsheet.xlsx}).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_elec_price)
#' str(EUROSTAT_elec_price)
#' head(EUROSTAT_elec_price)
"EUROSTAT_elec_price"

library(readxl)
library(dplyr)

# File path
tmp_file_path <- "inst/ext_data/EUROSTAT_prices/EUROSTAT_nrg_pc_205__custom_17738899_spreadsheet.xlsx"

# read sheet
tmp_sheet_raw <- read_excel(tmp_file_path, sheet = "Sheet 7", range = "A12:AK56", col_names = T)

# wrangle
tmp_data <- tmp_sheet_raw |>
  # remove empty row
  dplyr::filter(TIME != "GEO (Labels)") |>
  # Mutate variables as numerical
  dplyr::mutate(dplyr::across(
    .cols = -TIME,  # Apply to all columns except TIME
    .fns = ~ suppressWarnings(as.numeric(.))
  )) |>
  # add country ISO codes
  dplyr::left_join(
    FADN2Footprint::data_extra$country_names |>
      dplyr::select(country_name,Country_ISO_3166_1_A3) |>
      tidyr::separate_longer_delim(country_name,";") |>
      dplyr::filter(country_name %in% tmp_sheet_raw$TIME),
    by = dplyr::join_by(TIME == country_name)
  ) |>
  dplyr::filter(!is.na(Country_ISO_3166_1_A3)) |>
  dplyr::select(-TIME) |>
  # group by year and country
  tidyr::pivot_longer(
    cols = -Country_ISO_3166_1_A3,
    names_to = "semester",
    values_to = "euro_kWh"
  ) |>
  mutate(
    YEAR = as.character(gsub("-S1|-S2","",semester))
  ) |>
  summarise(
    euro_kWh = mean(euro_kWh,na.rm = T),
    .by = c(Country_ISO_3166_1_A3,YEAR)
    )

# Empty tibble
EUROSTAT_elec_price <- tmp_data

# export data
usethis::use_data(EUROSTAT_elec_price, overwrite = T)


rm(list = names(.GlobalEnv)[grepl("tmp",names(.GlobalEnv))])
