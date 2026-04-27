#' Additional databases
#'
#' Additional databases used in the Footprint estimation
#'
#' @docType data
#' @usage data(EF_electricity)
#'
#' @format A list of 16 data frames
#'
#' @source Bastos, Joana; Monforti-Ferrario, Fabio; Melica, Giulia (2024):
#' GHG Emission Factors for Electricity Consumption. European Commission,
#' Joint Research Centre (JRC) [Dataset] PID: \url{http://data.europa.eu/89h/919df040-0252-4e4e-ad82-c054896e1641}
#' @usage data(electricity_factors)
#'
#' @keywords datasets
"EF_electricity"


library(readxl)
library(dplyr)

# File path
tmp_file_path <- "inst/ext_data/EF_parameters/CoM-Emission-factors-for-national-electricity-2024.xlsx"

# read sheet
tmp_sheet_raw <- read_excel("inst/ext_data/EF_parameters/CoM-Emission-factors-for-national-electricity-2024.xlsx",
                            sheet = "Table1_EU_IPCC_CO2", skip = 1)

# add country names from FADN
tmp_data <- tmp_sheet_raw |>
  dplyr::left_join(
    FADN2Footprint::data_extra$country_names,
    by = dplyr::join_by('...2' == country_name)
  ) |>
  dplyr::select(Country_ISO_3166_1_A3 ,
                dplyr::matches(as.character(seq(1990,2021)))) |>
  dplyr::filter(!is.na(Country_ISO_3166_1_A3 )) |>
  dplyr::mutate(dplyr::across(-Country_ISO_3166_1_A3 , function(x) gsub("-",NA,x))) |>
  dplyr::mutate(dplyr::across(-Country_ISO_3166_1_A3 , as.numeric)) |>
  tidyr::pivot_longer(cols = -Country_ISO_3166_1_A3 ,
                      names_to = "YEAR",
                      values_to = "EF_elec") |>
  dplyr::mutate(YEAR = as.character(YEAR))

# Tibble
EF_electricity <- tmp_data

# export data
usethis::use_data(EF_electricity, overwrite = T)


rm(list = names(.GlobalEnv)[grepl("tmp",names(.GlobalEnv))])
