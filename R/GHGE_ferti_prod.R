#' Calculate GHG Emissions from Mineral Fertiliser Production
#'
#' @description
#' Estimates the upstream greenhouse gas (GHG) emissions associated with the
#' production of mineral (synthetic) nitrogen fertilisers applied on the farm,
#' following a life cycle assessment (LCA) approach (scope 3). Emission factors
#' are sourced from the JRC (Edwards et al., 2019).
#'
#' @details
#' ## Emission Factor
#' A single average emission factor for nitrogen fertiliser supply is applied:
#' \deqn{EF_{ferti,prod} = 4{,}571.9 \; \text{g CO}_2\text{-eq kg}^{-1}
#'   \text{N} = 4.572 \; \text{kg CO}_2\text{-eq kg}^{-1} \text{N}}
#' sourced from JRC Table 47 (Edwards et al., 2019), which covers the full
#' supply chain of N fertiliser production including energy use and process
#' emissions.
#'
#' ## Calculation
#' \deqn{ghg_{ferti,prod} = N_{ferti,min} \times EF_{ferti,prod}}
#' where \eqn{N_{ferti,min}} (kg N yr\eqn{^{-1}}) is the mineral nitrogen
#' applied per crop and farm, estimated by \code{\link{f_n_ferti}}.
#'
#' @note
#' \itemize{
#'   \item The emission factor is an average across all mineral N fertiliser
#'     types. It may overestimate emissions for urea-based fertilisers and
#'     underestimate them for ammonium nitrate (AN), but is consistent with
#'     the review of Walling & Vaneeckhaute (2020), accounting for recent
#'     innovations in Europe reducing the carbon content of AN fertilisers.
#'   \item Only mineral N fertilisers (\eqn{F_{SN}}) are considered here.
#'     Emissions from organic fertiliser production are attributed to the
#'     livestock system of origin and are not double-counted.
#'   \item This function covers fertiliser production emissions only. Field-
#'     level N2O emissions from fertiliser application are estimated
#'     separately in \code{\link{GHGE_n2o_msoils}}.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm observation ×
#' crop category, containing:
#' \describe{
#'   \item{...}{Traceability identifier columns
#'     (\code{object@traceability$id_cols}).}
#'   \item{FADN_code_letter}{\code{character}. FADN crop category code.}
#'   \item{ghg_ferti_prod_kgCO2e}{\code{numeric}. GHG emissions from mineral
#'     fertiliser production (kg CO2-eq yr\eqn{^{-1}}).}
#' }
#'
#' @references
#' Edwards, R., Padella, M., Giuntoli, J., Koeble, R., O'Connell, A.,
#' Bulgheroni, C. and Marelli, L. (2019). \emph{Definition of Input Data to
#' Assess GHG Default Emissions from Biofuels in EU Legislation, Version 1d}.
#' JRC Science for Policy Report. Publications Office of the European Union,
#' Luxembourg. \doi{10.2760/69179}
#'
#' Walling, E. and Vaneeckhaute, C. (2020). Greenhouse gas emissions from
#' inorganic and organic fertilizer production and use: A review of emission
#' factors and their variability. \emph{Journal of Environmental Management},
#' 276, 111211. \doi{10.1016/j.jenvman.2020.111211}
#'
#' @seealso
#' \code{\link{f_n_ferti}}, \code{\link{GHGE_n2o_msoils}},
#' \code{\link{f_GHGE_crops}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr mutate select all_of
#'
#' @concept footprint-ghge
#' @export


GHGE_ferti_prod <- function(object,
                            overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  tmp_ferti_prod = f_n_ferti(object, overwrite = overwrite)

  # EF of fertiliser production
  # Source: Edwards et al., 2019. Definition of input data to assess GHG default emissions from biofuels in EU legislation: version 1d - 2019. JRC, Luxembourg. https://doi.org/10.2760/69179
## « Table 47 Emission factors for fossil fuels, fertilizers and chemicals »
  ### « N fertilizer Supply [g/kg] 4 571.9 »
  ef_ferti_prod <- 4571.9 *10^-3 # conversion to kg CO2e / kg N
  # NB: it is possible that it is overvalued for urea fertilisers and undervalued for ammonium nitrate fertilisers
  # But overall it is consistent with the review of Walling & Vaneeckhaute (2020) considering the recent innovation in Europe to reduce carbone content of AN fertilisers.

  GHGE_ferti_prod <- tmp_ferti_prod |>
    dplyr::mutate(
      ghg_ferti_prod_kgCO2e = N_ferti_min * area_ha * ef_ferti_prod
    ) |>
    dplyr::select(
      dplyr::all_of(object@traceability$id_cols),
      FADN_code_letter,ghg_ferti_prod_kgCO2e)

  return(GHGE_ferti_prod)


}
