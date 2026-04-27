#' EUROSTAT natural gas prices (€/GJ, annual mean from semester data)
#'
#' Country-level natural gas price time series derived from EUROSTAT dataset
#' \code{NRG_PC_203}. Values are extracted from a custom EUROSTAT Excel export
#' and processed for use in **FADN2Footprint** (e.g., energy cost proxies and
#' footprint accounting steps requiring gas price assumptions).
#'
#' The source table provides prices per semester (S1, S2). This dataset aggregates
#' semester values into an annual mean price per country and year.
#'
#' Units are **euros per gigajoule** (\(€ / \mathrm{GJ}\)), reported in **gross
#' calorific value (GCV)** terms, as indicated in the extraction script.
#'
#' @docType data
#' @usage data(EUROSTAT_gaz_price)
#'
#' @format
#' A tibble/data frame with one row per country-year combination and 3 columns:
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{euro_GJ}}{Annual mean gas price in \(€ / \mathrm{GJ}\) (numeric), computed as the mean of S1 and S2 when available.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read range \code{A12:AK50} from \code{Sheet 16} of the custom EUROSTAT export.
#'   \item Drop non-data header rows (e.g. \code{"GEO (Labels)"}).
#'   \item Convert price columns to numeric (suppressing coercion warnings).
#'   \item Map country labels to ISO3 codes using \code{data_extra$country_names}.
#'   \item Pivot semester columns (e.g. \code{YYYY-S1}, \code{YYYY-S2}) to long format.
#'   \item Compute annual means by country and year.
#' }
#'
#' @source
#' EUROSTAT dataset \emph{Natural gas prices} \code{NRG_PC_203}. \doi{10.2908/NRG_PC_203}
#' (processed via a custom Excel export stored in the package).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_gaz_price)
#' str(EUROSTAT_gaz_price)
#' head(EUROSTAT_gaz_price)
"EUROSTAT_gaz_price"

library(readxl)
library(dplyr)

# File path
tmp_file_path <- "inst/ext_data/EUROSTAT_prices/EUROSTAT_nrg_pc_203__custom_17739505_spreadsheet.xlsx"

# read sheet
tmp_sheet_raw <- readxl::read_excel(tmp_file_path, sheet = "Sheet 16", range = "A12:AK50", col_names = T)
# values in Gigajoule (gross calorific value - GCV)

# wrangle
tmp_data <- tmp_sheet_raw %>%
  # remove empty row
  dplyr::filter(TIME != "GEO (Labels)") %>%
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
  # group by year
  tidyr::pivot_longer(
    cols = -Country_ISO_3166_1_A3,
    names_to = "semester",
    values_to = "euro_GJ"
  ) |>
  dplyr::mutate(
    YEAR = as.character(gsub("-S1|-S2","",semester))
  ) |>
  dplyr::summarise(
    euro_GJ = mean(euro_GJ,na.rm = T),
    .by = c(Country_ISO_3166_1_A3,YEAR)
  )

# Empty tibble
EUROSTAT_gaz_price <- tmp_data

# export data
usethis::use_data(EUROSTAT_gaz_price, overwrite = T)


rm(list = names(.GlobalEnv)[grepl("tmp",names(.GlobalEnv))])
