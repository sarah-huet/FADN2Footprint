#' Calculate GHG Emissions from On-Farm Fuel Combustion
#'
#' @description
#' Estimates greenhouse gas (GHG) emissions from the combustion of liquid fuels
#' used on the farm, covering diesel (machinery and field operations) and
#' heating fuels (buildings and facilities). Emission factors are country- and
#' year-specific, sourced from UNFCCC national inventory submissions, following
#' IPCC stationary and mobile combustion methodology.
#'
#' @details
#' ## Activity Data
#' Fuel consumption is retrieved from \code{object@input} and expressed in
#' both litres and megajoules (MJ). The conversion from litres to MJ is
#' assumed to be performed upstream (see \code{\link{infer_practices}}):
#' \itemize{
#'   \item \code{diesel_l}, \code{diesel_MJ}: diesel consumption for farm
#'     machinery and field operations.
#'   \item \code{heating_fuels_l}, \code{heating_fuels_MJ}: heating fuel
#'     consumption for farm buildings and livestock facilities.
#' }
#'
#' ## Emission Factors
#' Country- and year-specific CO2 emission factors (\eqn{EF_{fuel,CO2}},
#' t CO2 TJ\eqn{^{-1}}) are sourced from UNFCCC submissions
#' (\code{\link{UNFCCC_data}}\code{$EF_fuel}), matched by
#' \code{Country_ISO_3166_1_A3} and \code{YEAR}. Cross-country means are
#' used as fallback when country- or year-specific values are missing.
#'
#' Emission factors are converted to kg CO2 MJ\eqn{^{-1}} for calculation:
#' \deqn{EF_{fuel,CO2} \; [\text{kg CO}_2 \text{ MJ}^{-1}] =
#'   EF_{fuel,CO2} \; [\text{t CO}_2 \text{ TJ}^{-1}] \times 10^{-3}}
#'
#' ## Emission Calculation
#' \deqn{ghg_{diesel} = Q_{diesel,MJ} \times EF_{fuel,CO2}}
#' \deqn{ghg_{heat} = Q_{heat,MJ} \times EF_{fuel,CO2}}
#' \deqn{ghg_{fuels} = ghg_{diesel} + ghg_{heat}}
#'
#' @note
#' \itemize{
#'   \item A single average emission factor is applied to both diesel and
#'     heating fuels. Differentiated factors by fuel type (e.g., gas oil vs.
#'     natural gas) may be implemented in future versions.
#'   \item Only CO2 emissions from fuel combustion are currently estimated.
#'     CH4 and N2O emissions from mobile combustion represent a minor
#'     contribution and are not yet included.
#'   \item Indirect (upstream) emissions from fuel production are not included
#'     here; this function covers direct (scope 1) combustion emissions only.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm observation,
#' containing:
#' \describe{
#'   \item{...}{Traceability identifier columns
#'     (\code{object@traceability$id_cols}).}
#'   \item{diesel_l}{\code{numeric}. Diesel consumption (litres yr\eqn{^{-1}}).}
#'   \item{heating_fuels_l}{\code{numeric}. Heating fuel consumption
#'     (litres yr\eqn{^{-1}}).}
#'   \item{diesel_MJ}{\code{numeric}. Diesel consumption
#'     (MJ yr\eqn{^{-1}}).}
#'   \item{heating_fuels_MJ}{\code{numeric}. Heating fuel consumption
#'     (MJ yr\eqn{^{-1}}).}
#'   \item{EF_fuel_CO2}{\code{numeric}. CO2 emission factor for fuel
#'     combustion (t CO2 TJ\eqn{^{-1}}), from UNFCCC submissions.}
#'   \item{EF_fuel_CO2_kgCO2MJ}{\code{numeric}. CO2 emission factor
#'     converted to kg CO2 MJ\eqn{^{-1}}.}
#'   \item{ghg_diesel_kgCO2e}{\code{numeric}. GHG emissions from diesel
#'     combustion (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{ghg_heat_fuel_kgCO2e}{\code{numeric}. GHG emissions from heating
#'     fuel combustion (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{ghg_all_fuels_kgCO2e}{\code{numeric}. Total GHG emissions from
#'     all fuel combustion (kg CO2-eq yr\eqn{^{-1}}).}
#' }
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories, Volume 2: Energy, Chapter 2: Stationary Combustion} and
#' \emph{Chapter 3: Mobile Combustion}. Eggleston H.S., Buendia L., Miwa K.,
#' Ngara T. and Tanabe K. (eds). IGES, Japan.
#'
#' UNFCCC (2023). \emph{National Inventory Submissions 2023}.
#' \url{https://unfccc.int/ghg-inventories-annex-i-parties/2023}
#'
#' @seealso
#' \code{\link{GHGE_elec}}, \code{\link{f_GHGE_crops}},
#' \code{\link{UNFCCC_data}}, \code{\link{infer_practices}},
#' \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr select left_join mutate across starts_with if_else
#'   all_of distinct join_by
#'
#' @concept footprint-ghge
#' @export


GHGE_fuels <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Activity data ----
  data_fuels <- object@input |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  diesel_l,heating_fuels_l,
                  diesel_MJ,heating_fuels_MJ
    ) |>
    # add country ISO codes
    dplyr::left_join(
      data_extra$country_names |>
        dplyr::select(country_FADN,Country_ISO_3166_1_A3) |>
        dplyr::distinct(),
      by = dplyr::join_by(COUNTRY == country_FADN)
    )

  # Estimate emissions ----

  GHGE_fuels <- data_fuels |>
    # add EF
    dplyr::left_join(
      UNFCCC_data$EF_fuel,
      by = c('Country_ISO_3166_1_A3','YEAR')
    ) |>
    ## replace NAs by EF mean
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("EF"),
        .fns = ~ dplyr::if_else(is.na(.), mean(., na.rm = TRUE), .)
      )
    ) |>
    # convert EF from t CO2 TJ-1 to kg CO2 MJ-1
    dplyr::mutate(
      EF_fuel_CO2_kgCO2MJ = EF_fuel_CO2 * 10^-3
    ) |>
    # estimate CO2 emissions
    dplyr::mutate(
      ghg_diesel_kgCO2e = diesel_MJ * EF_fuel_CO2_kgCO2MJ,
      ghg_heat_fuel_kgCO2e = heating_fuels_MJ * EF_fuel_CO2_kgCO2MJ,
      ghg_all_fuels_kgCO2e = ghg_diesel_kgCO2e + ghg_heat_fuel_kgCO2e
    )

  return(GHGE_fuels)


}
