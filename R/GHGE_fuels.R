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
#' @importFrom dplyr select left_join mutate across starts_with if_else all_of distinct join_by
#'
#' @concept footprint-ghge
#' @export


f_GHGE_fuels <- function(object,
                       account_pseudoherd = FALSE,
                       ...){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  id_cols = object@traceability$id_cols

  # Activity data ----
  data_fuels <- object@input |>
    dplyr::select(dplyr::all_of(id_cols),
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

  # For diesel specifically, we estimate the quantity used for ploughing
  ## Estimate tillage in L/ha
  tmp_tillage = f_tillage(object)
  ## Estimate L used for tillage per crop
  tmp_diesel_tillage <- tmp_tillage |>
    # add crop area
    dplyr::left_join(
      object@crop |>
        dplyr::select(dplyr::all_of(id_cols),
                      FADN_code_letter,
                      area_ha
        ),
      by = c(id_cols, 'FADN_code_letter')
    ) |>
    dplyr::mutate(
      diesel_tillage_l = tillage * area_ha
    )

  # Estimate total emissions ----

  GHGE_fuels <- data_fuels |>
    # add diesel use for tillage
    dplyr::left_join(
      tmp_diesel_tillage |>
        dplyr::summarise(
          diesel_tillage_tot_l = sum(diesel_tillage_l, na.rm = TRUE),
          .by = dplyr::all_of(id_cols)
        ),
      by = id_cols
    ) |>
    dplyr::mutate(
      diesel_tillage_tot_MJ = dplyr::coalesce(diesel_tillage_tot_l * (diesel_MJ / diesel_l), 0),
      diesel_remain_MJ = diesel_MJ - diesel_tillage_tot_MJ
    ) |>
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
      farm_ghg_diesel_kgCO2e = diesel_MJ * EF_fuel_CO2_kgCO2MJ,
      farm_ghg_diesel_tillage_kgCO2e = diesel_tillage_tot_MJ * EF_fuel_CO2_kgCO2MJ,
      farm_ghg_diesel_remain_kgCO2e = diesel_remain_MJ * EF_fuel_CO2_kgCO2MJ,

      farm_ghg_heat_fuel_kgCO2e = heating_fuels_MJ * EF_fuel_CO2_kgCO2MJ,
      farm_ghg_all_fuels_kgCO2e = farm_ghg_diesel_kgCO2e + farm_ghg_heat_fuel_kgCO2e
    )

  # Allocate emissions ----

  # We use an economic allocation to allocate farm level emissions to outputs
  #For diesel specifically, we first allocate emissions related to ploughing to crops than economically allocate the remaining emissions to all outputs

  ## Output economic allocation ratio
  tmp_econ_alloc = f_output_econ_alloc(object, account_pseudoherd = account_pseudoherd)

  tmp_GHGE_fuels_alloc = tmp_econ_alloc$all_outputs |>
    dplyr::filter(econ_alloc_ratio_farm >0) |>
    # add GHGE
    dplyr::left_join(
      GHGE_fuels,
      by = id_cols
    ) |>
    # add tillage activity
    dplyr::left_join(
      tmp_diesel_tillage,
      by = c(id_cols, 'FADN_code_letter')
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to output
    dplyr::summarise(

      ghg_diesel_tillage_kgCO2e_output = sum(farm_ghg_diesel_kgCO2e * (diesel_tillage_l / diesel_l), na.rm = TRUE),
      ghg_diesel_remain_kgCO2e_output = sum(farm_ghg_diesel_remain_kgCO2e * econ_alloc_ratio_farm, na.rm = TRUE),

      ghg_heat_fuel_kgCO2e_output = sum(farm_ghg_heat_fuel_kgCO2e * econ_alloc_ratio_farm, na.rm = TRUE),

      .by = c(dplyr::all_of(id_cols), activity, species, output, FADN_code_letter, FADN_code_letter_output)
    )

  return(list(total_GHGE_fuels = GHGE_fuels,
              alloc_GHGE_fuels = tmp_GHGE_fuels_alloc))


}
