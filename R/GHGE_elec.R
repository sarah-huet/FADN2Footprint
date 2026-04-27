#' Estimate GHG Emissions from On-Farm Electricity Consumption
#'
#' @description
#' Calculates the greenhouse gas (GHG) emissions associated with on-farm
#' electricity consumption, using country- and year-specific emission factors
#' and EUROSTAT electricity prices to convert expenditure data (value) to
#' physical quantities (kWh).
#'
#' Since FADN reports electricity as an expenditure value (\code{IELE_V},
#' in national currency) rather than a physical quantity, electricity consumption
#' in kWh is back-calculated using EUROSTAT national average electricity prices
#' (€ kWh\eqn{^{-1}}, source: EUROSTAT
#' \href{https://doi.org/10.2908/NRG_PC_205}{NRG_PC_205}).
#'
#' @details
#' ## Electricity Quantity Estimation
#' Electricity consumption is estimated as:
#' \deqn{elec\_kWh = \frac{elec\_V}{euro\_kWh}}
#' where:
#' \itemize{
#'   \item \eqn{elec\_V} = electricity expenditure from FADN (\code{IELE_V},
#'     in euros)
#'   \item \eqn{euro\_kWh} = national average electricity price (€ kWh\eqn{^{-1}}),
#'     from EUROSTAT table \code{NRG_PC_205}, matched by country ISO code
#'     (\code{Country_ISO_3166_1_A3}) and survey year (\code{YEAR})
#' }
#' When country- and year-specific electricity prices are missing, the
#' cross-country mean price is used as a fallback (TODO: replace with
#' EU-27 average price).
#'
#' ## GHG Emission Estimation
#' Total GHG emissions from electricity use are estimated as:
#' \deqn{ghg\_elec\_kgCO2e = elec\_kWh \times EF_{elec}}
#' where \eqn{EF_{elec}} is the country- and year-specific electricity emission
#' factor (kg CO2-eq kWh\eqn{^{-1}} = t CO2 MWh\eqn{^{-1}}), sourced from
#' \code{\link{EF_electricity}}.
#'
#' When country- and year-specific emission factors are missing, the
#' cross-country mean emission factor is substituted.
#'
#' @note
#' \itemize{
#'   \item Allocation of electricity emissions to specific farm activities
#'     (crops or livestock) is not performed within this function; it is
#'     handled downstream in \code{\link{f_GHGE_crops}} and
#'     \code{\link{f_GHGE_livestock}} using economic allocation keys.
#'   \item The FADN variable \code{IELE_V} represents total farm electricity
#'     expenditure and does not distinguish between end-uses.
#'   \item When EUROSTAT prices are unavailable for a given country-year
#'     combination, missing values are currently replaced by the cross-country
#'     mean; replacement with the EU-27 average price is planned (TODO).
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#'
#' @return A \code{\link[tibble]{tibble}} containing one row per farm
#' observation, with the following columns:
#' \describe{
#'   \item{...}{Traceability identifier columns as defined in
#'     \code{object@traceability$id_cols} (e.g., farm ID, year, NUTS2 region).}
#'   \item{elec_V}{\code{numeric}. Electricity expenditure from FADN (€).}
#'   \item{Country_ISO_3166_1_A3}{\code{character}. Country ISO 3166-1 alpha-3
#'     code.}
#'   \item{euro_kWh}{\code{numeric}. National average electricity price
#'     (€ kWh\eqn{^{-1}}), from EUROSTAT \code{NRG_PC_205}.}
#'   \item{elec_kWh}{\code{numeric}. Estimated electricity consumption (kWh
#'     yr\eqn{^{-1}}).}
#'   \item{EF_elec}{\code{numeric}. Country- and year-specific electricity
#'     emission factor (kg CO2-eq kWh\eqn{^{-1}}).}
#'   \item{ghg_elec_kgCO2e}{\code{numeric}. Total GHG emissions from electricity
#'     consumption (kg CO2-eq yr\eqn{^{-1}}).}
#' }
#'
#' @references
#' EUROSTAT (2024). \emph{Electricity prices for non-household consumers —
#' bi-annual data (from 2007 onwards)}. Table \code{NRG_PC_205}.
#' \url{https://doi.org/10.2908/NRG_PC_205}
#'
#' @seealso
#' \code{\link{EF_electricity}}, \code{\link{EUROSTAT_elec_price}},
#' \code{\link{GHGE_fuels}}, \code{\link{f_GHGE_crops}},
#' \code{\link{f_GHGE_livestock}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr select left_join mutate across starts_with if_else
#'   join_by distinct
#'
#' @concept footprint-ghge
#' @export


GHGE_elec <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Emission factor ----
  EF_elec <- EF_electricity # EF for CO2 expressed in tCO2/MWh = kg CO2 / kWh

  # Activity data ----
  data_elec <- object@input |>
    # convert electricity value in consumption in kWh
    dplyr::select(dplyr::all_of(object@traceability$id_cols),elec_V) |>
    # add country ISO codes
    dplyr::left_join(
      data_extra$country_names |>
        dplyr::select(country_FADN,Country_ISO_3166_1_A3) |>
        dplyr::distinct(),
      by = dplyr::join_by(COUNTRY == country_FADN)
    ) |>
    # add EUROSTAT data
    dplyr::left_join(
      EUROSTAT_elec_price,
      by = join_by(Country_ISO_3166_1_A3, YEAR)
      # TODO: check for replacing NAs with price from "European Union - 27 countries (from 2020)"
    ) |>
    ## replace NAs by EF mean
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("EF"),
        .fns = ~ dplyr::if_else(is.na(.), mean(., na.rm = TRUE), .)
      )
    ) |>
    # estimate elec quantity in kWh
    dplyr::mutate(
      elec_kWh = (elec_V/ euro_kWh)
    )

  # Estimate emissions ----

  GHGE_elec <- data_elec |>
    dplyr::left_join(EF_elec, by = c('Country_ISO_3166_1_A3','YEAR')) |>
    # kg CO2 with an economic allocation to the crop
    dplyr::mutate(ghg_elec_kgCO2e = elec_kWh * EF_elec)

  return(GHGE_elec)


}
