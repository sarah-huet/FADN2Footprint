#' EUROSTAT input prices for feed and other agricultural inputs (€/tonne)
#'
#' Harmonised input price time series extracted from a custom EUROSTAT spreadsheet
#' and transformed to support **FADN2Footprint** costing and allocation steps (e.g.,
#' feed cost proxies or price-based allocation keys).
#'
#' The dataset is built by reading selected sheets from an EUROSTAT Excel export,
#' extracting yearly prices (2014–2024) by country, mapping country labels to ISO3
#' codes using \code{data_extra$country_names}, and standardising units from
#' \(€ / 100\ \mathrm{kg}\) to \(€ / \mathrm{tonne}\).
#'
#' Missing values are imputed by the (country) mean over available years within the
#' processed table.
#'
#' @docType data
#' @usage data(EUROSTAT_input_price)
#'
#' @format
#' A tibble/data frame where each row is a (country, input) observation with one
#' column per year.
#'
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{2014}...\code{2024}}{Annual price in \emph{euros per tonne} (numeric).}
#'   \item{\code{EUROSTAT_feedstuff}}{Input product name (sanitised, underscores) extracted from the sheet title cell.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read the input/product label from cell \code{C7} of each selected sheet.
#'   \item Read country-by-year prices from range \code{A9:W37}.
#'   \item Keep \code{TIME} plus year columns 2014–2024; drop header rows.
#'   \item Convert ":" to \code{NA} and coerce to numeric; multiply by 10 to convert
#'   from \(€ / 100\ \mathrm{kg}\) to \(€ / \mathrm{tonne}\).
#'   \item Join to \code{data_extra$country_names} to obtain ISO3 country codes.
#'   \item Impute remaining missing numeric values by the mean (within each numeric column).
#' }
#'
#' @source
#' EUROSTAT custom Excel export stored in the package at
#' \code{inst/ext_data/EUROSTAT_prices/EUROSTAT_apri_ap_ina__custom_16862102_spreadsheet.xlsx}
#' (multiple sheets).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_input_price)
#' str(EUROSTAT_input_price)
#' # Example: subset one input and inspect the time series
#' subset(EUROSTAT_input_price, EUROSTAT_feedstuff == unique(EUROSTAT_feedstuff)[1])
"EUROSTAT_input_price"


library(readxl)
library(dplyr)

# File path
tmp_file_path <- "inst/ext_data/EUROSTAT_prices/EUROSTAT_apri_ap_ina__custom_16862102_spreadsheet.xlsx"

# Specify the sheet names to read
tmp_sheets_to_read <- c(
  "Sheet 28","Sheet 29",
  "Sheet 30","Sheet 31",
  "Sheet 32","Sheet 33",
  "Sheet 34","Sheet 35",
  "Sheet 36","Sheet 39",
  "Sheet 40","Sheet 41",
  "Sheet 42")

# Empty tibble
EUROSTAT_input_price <- tibble()

# Loop to process each sheet
for (tmp_sheet in tmp_sheets_to_read) {

  # Read input product name
  tmp_feedstuff <- readxl::read_excel(tmp_file_path, sheet = tmp_sheet, range = "C7", col_names = FALSE) |>
    pull(1)

  # Read input product prices
  tmp_sheet_raw <- readxl::read_excel(tmp_file_path, sheet = tmp_sheet, range = "A9:W37",col_names = T)

  tmp_data <- tmp_sheet_raw |>
    # Keep columns with data
    dplyr::select(TIME,as.character(2014:2024)) |>
    # remove empty row
    dplyr::filter(TIME != "GEO (Labels)") |>
    # Mutate variables as numerical
    dplyr::mutate(dplyr::across(
      .cols = -TIME,  # Apply to all columns except TIME
      .fns = ~ as.numeric(gsub(":", "", .))*10 # change from character to numeric and convert from € / 100kg to € / tonnes
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
    # Replace NAs by the year average
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),  # Only apply to numeric columns
      ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
    )) |>
    # Add the feed stuff name as a new column
    dplyr::mutate(EUROSTAT_feedstuff = gsub(" ","_",gsub("Feedingstuffs: | - prices per 100 kg","",tmp_feedstuff)))

  EUROSTAT_input_price <- EUROSTAT_input_price |>
    rbind(tmp_data)

}

# export data
usethis::use_data(EUROSTAT_input_price, overwrite = T)


rm(list = names(.GlobalEnv)[grepl("tmp",names(.GlobalEnv))])
