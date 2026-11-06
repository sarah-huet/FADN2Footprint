.estimate_electricity_activity <- function(data_elec, electricity_prices,
                                           emission_factors) {
  mean_finite_or_stop <- function(values, label) {
    values <- values[is.finite(values)]
    if (length(values) == 0L) {
      stop(
        paste0("No finite reference values are available for ", label, "."),
        call. = FALSE
      )
    }
    mean(values)
  }

  price_mean <- mean_finite_or_stop(
    electricity_prices$euro_kWh,
    "EUROSTAT electricity prices"
  )
  ef_mean <- mean_finite_or_stop(
    emission_factors$EF_elec,
    "electricity emission factors"
  )
  ef_mean_by_year <- emission_factors |>
    dplyr::group_by(YEAR) |>
    dplyr::summarise(
      EF_elec_year_mean = {
        values <- EF_elec[is.finite(EF_elec)]
        if (length(values) == 0L) NA_real_ else mean(values)
      },
      .groups = "drop"
    )

  data_elec |>
    dplyr::left_join(
      electricity_prices,
      by = c("Country_ISO_3166_1_A3", "YEAR")
    ) |>
    dplyr::mutate(
      euro_kWh = dplyr::coalesce(euro_kWh, price_mean),
      elec_kWh = elec_V / euro_kWh
    ) |>
    dplyr::left_join(
      emission_factors,
      by = c("Country_ISO_3166_1_A3", "YEAR")
    ) |>
    dplyr::left_join(ef_mean_by_year, by = "YEAR") |>
    dplyr::mutate(
      EF_elec = dplyr::coalesce(EF_elec, EF_elec_year_mean, ef_mean),
      farm_ghg_elec_kgCO2e = elec_kWh * EF_elec
    ) |>
    dplyr::select(-EF_elec_year_mean)
}

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
#' When a country- and year-specific electricity price is missing, the mean
#' of available EUROSTAT country-year prices is used as a fallback.
#'
#' ## GHG Emission Estimation
#' Total GHG emissions from electricity use are estimated as:
#' \deqn{ghg\_elec\_kgCO2e = elec\_kWh \times EF_{elec}}
#' where \eqn{EF_{elec}} is the country- and year-specific electricity emission
#' factor (kg CO2-eq kWh\eqn{^{-1}} = t CO2 MWh\eqn{^{-1}}), sourced from
#' \code{\link{EF_electricity}}.
#'
#' When a country- and year-specific emission factor is missing, the mean
#' across available European factors for that year is used. If no factors are
#' available for that year, the mean across all available country-year factors
#' is used.
#'
#' @note
#' \itemize{
#'   \item Allocation of electricity emissions to specific farm activities
#'     (crops or livestock) is not performed within this function; it is
#'     handled downstream in \code{\link{f_GHGE_crops}} and
#'     \code{\link{f_GHGE_livestock}} using economic allocation keys.
#'   \item The FADN variable \code{IELE_V} represents total farm electricity
#'     expenditure and does not distinguish between end-uses.
#'   \item If no finite price or emission-factor reference values are available,
#'     the function stops with an error rather than returning emissions with
#'     missing fallback inputs.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param account_pseudoherd Logical, default FALSE. When TRUE, pseudo-herd
#'   animals (inferred from feed flows rather than directly observed) are
#'   included in the emission calculation. Currently reserved for future
#'   implementation.
#' @param ... Additional arguments.
#'
#' @return A **list** with two tibble elements:
#' \describe{
#'   \item{total_GHGE_electricity}{\code{tibble}. One row per farm (identified by the
#'     traceability columns). Columns include:\cr
#'     \code{elec_V} (electricity expenditure, €),\cr
#'     \code{euro_kWh} (national electricity price, € kWh⁻¹),\cr
#'     \code{elec_kWh} (estimated electricity consumption, kWh yr⁻¹),\cr
#'     \code{EF_elec} (electricity emission factor, kg CO₂‑eq kWh⁻¹),\cr
#'     \code{farm_ghg_elec_kgCO2e} (total electricity‑related GHG, kg CO₂‑eq yr⁻¹).}
#'   \item{alloc_GHGE_electricity}{\code{tibble}. Same traceability identifiers plus
#'     output‑level variables (e.g., `activity`, `species`, `output`,
#'     `FADN_code_letter`, `FADN_code_letter_output`) and the column
#'     \code{ghg_elec_kgCO2e_output}, which is the electricity‑related GHG
#'     allocated to each output (kg CO₂‑eq yr⁻¹).}
#' }
#'
#' @references
#' EUROSTAT (2024). \emph{Electricity prices for non-household consumers —
#' bi-annual data (from 2007 onwards)}. Table \code{NRG_PC_205}.
#' \url{https://doi.org/10.2908/NRG_PC_205}
#'
#' @seealso
#' \code{\link{EF_electricity}}, \code{\link{EUROSTAT_elec_price}},
#' \code{\link{f_GHGE_fuels()}}, \code{\link{f_GHGE_crops()}},
#' \code{\link{f_GHGE_herd()}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr select left_join mutate coalesce join_by distinct
#'
#' @concept footprint-ghge
#' @export
f_GHGE_elec <- function(object,
                      account_pseudoherd = FALSE,
                      ...){

  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  id_cols = object@traceability$id_cols

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
    dplyr::select(dplyr::all_of(c(
      object@traceability$id_cols,
      "elec_V",
      "Country_ISO_3166_1_A3"
    )))

  # Estimate total emissions ----

  GHGE_elec <- .estimate_electricity_activity(
    data_elec,
    EUROSTAT_elec_price,
    EF_electricity
  )

  # Allocate emissions ----

  # We use an economic allocation to allocate farm level emissions to outputs

  ## Output economic allocation ratio
  tmp_econ_alloc = f_output_econ_alloc(object, account_pseudoherd = account_pseudoherd)

  ## Allocate
  ### select economic allocation across outputs
  tmp_GHGE_elec_alloc = tmp_econ_alloc$all_outputs |>
    # add GHGE
    dplyr::left_join(
      GHGE_elec,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to output
    dplyr::summarise(
      ghg_elec_kgCO2e_output = sum(farm_ghg_elec_kgCO2e * econ_alloc_ratio_farm, na.rm = TRUE),
      .by = c(dplyr::all_of(id_cols), activity, species, output, FADN_code_letter, FADN_code_letter_output)
    )


  return(list(total_GHGE_electricity = GHGE_elec,
              alloc_GHGE_electricity = tmp_GHGE_elec_alloc))


}
