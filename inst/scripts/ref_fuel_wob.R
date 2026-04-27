#' Weekly Oil Bulletin fuel prices (without taxes), by country and year
#'
#' Reference fuel price time series derived from the European Commission
#' **Weekly Oil Bulletin (WOB)** historical files, processed for use in
#' **FADN2Footprint** (e.g., cost proxies for energy use and related footprint
#' accounting).
#'
#' The dataset provides annual average prices **without taxes** (WOT) for:
#' \itemize{
#'   \item automotive gas oil (road diesel),
#'   \item heating gas oil.
#' }
#'
#' Coverage is **2004–2022** (as constructed by the data-creation script):
#' \itemize{
#'   \item 2005–2022: extracted from \code{Oil_Bulletin_Prices_History.xlsx}
#'   (sheets \code{wotax_ctr_road} and \code{wotax_ctr_heat}).
#'   \item 2004: reconstructed from one Excel file per country (weekly observations),
#'   aggregated to annual means and converted to \(€ / L\).
#' }
#'
#' Country identifiers from WOB (\code{country_eu}, e.g. \code{"EL"} for Greece) are
#' mapped to the package's FADN country codes (\code{COUNTRY}) using
#' \code{data_extra$country_names}. Special cases are handled (\code{GR -> EL},
#' \code{GB -> UK}) to match Eurostat/WOB conventions in older files.
#'
#' @docType data
#' @usage data(ref_fuel_wob)
#'
#' @format
#' A data frame with one row per country-year and the following columns:
#' \describe{
#'   \item{\code{COUNTRY}}{Country code as used in FADN inputs (\code{country_FADN}).}
#'   \item{\code{country_eu}}{EU/WOB country code (e.g. \code{FR}, \code{DE}, \code{EL}).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{PFUEL_ROAD}}{Automotive gas oil price without tax, in \(€ / L\) (annual mean).}
#'   \item{\code{PFUEL_HEAT}}{Heating gas oil price without tax, in \(€ / L\) (annual mean).}
#' }
#'
#' @details
#' Data-processing notes (as implemented in the script):
#' \enumerate{
#'   \item Zeros in the 2005+ history tables are treated as missing values (\code{NA}).
#'   \item The 2005–2022 tables are reshaped from wide (years as columns) to long
#'   (one row per country-year).
#'   \item For 2004, weekly observations are filtered to year 2004, averaged by
#'   country, and converted from \(€ / 1000\ L\) to \(€ / L\)
#'   via division by 1000.
#'   \item The final dataset is the row-bind of the 2004 and 2005–2022 blocks,
#'   sorted by \code{COUNTRY} and \code{YEAR}.
#' }
#'
#' @source
#' European Commission, DG Energy, Weekly Oil Bulletin (WOB), historical price files
#' (without taxes), as downloaded on **2022-11-04** and stored locally in the
#' project raw-data folders (see script paths \code{data_raw/ref_fuel/...}).
#'
#' @keywords datasets
#'
#' @examples
#' data(ref_fuel_wob)
#' str(ref_fuel_wob)
#' head(ref_fuel_wob)
"ref_fuel_wob"



# libraries -----------------------------------------------------------------
library(tidyverse)
library(readxl)
#library(farmsty)

# 2005 - 2022 -------------------------------------------------------------
# Automotive gas oil (EUR/L)
ob_wot_road = read_excel("data_raw/ref_fuel/Oil_Bulletin_Prices_History.xlsx",
                         sheet = "wotax_ctr_road",
                         range = "A64:S92")

ob_wot_road[ob_wot_road == 0] <- NA

ob_wot_road = ob_wot_road %>%
  pivot_longer(cols = names(.)[names(.) != "COUNTRY"],
               names_to = "YEAR",
               values_to = "PFUEL_ROAD")

# Heating gas oil (EUR/L)
ob_wot_heat = read_excel("data_raw/ref_fuel/Oil_Bulletin_Prices_History.xlsx",
                         sheet = "wotax_ctr_heat",
                         range = "A64:S92")

ob_wot_heat[ob_wot_heat == 0] <- NA

ob_wot_heat = ob_wot_heat %>%
  pivot_longer(cols = names(.)[names(.) != "COUNTRY"],
               names_to = "YEAR",
               values_to = "PFUEL_HEAT")

# Join

ref_fuel_wob = left_join(ob_wot_road, ob_wot_heat, by = c("COUNTRY", "YEAR")) %>%
  rename(country_eu = COUNTRY) %>%
  mutate(country_eu = if_else(country_eu == "GR", "EL", country_eu)) %>%
  left_join(data_extra$country_names[,c("country_eu","country_FADN")], by = "country_eu") %>%
  rename(COUNTRY = country_FADN) %>%
  select(COUNTRY, country_eu, YEAR, PFUEL_ROAD, PFUEL_HEAT)

rm(ob_wot_road, ob_wot_heat)


# 2004 --------------------------------------------------------------------
# we have data for each country in a separate file

# list file paths
files <- list.files("data_raw/ref_fuel/wob", pattern="*.xls", full.names=TRUE)
# read all files into a list
ldf <- lapply(files, read_excel)
# select usefull columns
ldf <- lapply(ldf, function(x) dplyr::select(x, all_of(c("Country_ID", "price_date", "Euro_Price", "DIESEL HT", "HGASOIL HT"))))
# bind into a single table
wob_old <- do.call("rbind", ldf)

# clean country code
wob_old$Country_ID[wob_old$Country_ID == "GR"] <- "EL"
wob_old$Country_ID[wob_old$Country_ID == "GB"] <- "UK"

# final calculations
wob_old <- wob_old %>%
  # keep only 2004
  filter(grepl("2004", price_date)) %>%
  mutate(YEAR = "2004") %>%
  # add country code
  rename(country_eu = Country_ID) %>%
  left_join(data_extra$country_names[,c("country_eu","country_FADN")], by = "country_eu") %>%
  rename(COUNTRY = country_FADN) %>%
  # compute mean price for each country (NB: we only kep year 2004)
  group_by(COUNTRY) %>%
  mutate(PFUEL_HEAT = mean(`HGASOIL HT`, na.rm = T),
         PFUEL_ROAD = mean(`DIESEL HT`, na.rm = T)) %>%
  # conversion to EUR/L
  mutate(PFUEL_HEAT = PFUEL_HEAT/1000,
         PFUEL_ROAD = PFUEL_ROAD/1000) %>%
  ungroup() %>%
  # order cols and clean table
  select(COUNTRY, country_eu, YEAR, PFUEL_ROAD, PFUEL_HEAT) %>%
  # remove rows that appear more than once
  distinct()

rm(ldf, files)

# join to ref_fuel_wob
ref_fuel_wob = bind_rows(ref_fuel_wob, wob_old) %>%
  arrange(COUNTRY, YEAR)

# export data
usethis::use_data(ref_fuel_wob, overwrite = T)
