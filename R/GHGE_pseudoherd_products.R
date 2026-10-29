#' Estimate Greenhouse Gas Emissions for Pseudoherd Outputs
#'
#' @description
#' Computes greenhouse gas emissions (GHG, kg CO\eqn{_2}e) allocated to
#' livestock outputs (milk, meat, eggs) for each farm x year x livestock
#' category combination, accounting for both on-farm animals and the
#' off-farm animals implied by purchased feed / young stock (the
#' "pseudoherd"). Mirrors \code{\link{f_GHGE_herd_output}}, but carries
#' three parallel boundaries throughout the allocation chain:
#' \code{on_farm}, \code{off_farm} and \code{pseudoherd} (on_farm +
#' off_farm).
#'
#' @details
#' ## Step 1 - Pseudoherd activities and outputs
#' Off-farm animals are distributed across activities (milk / meat / eggs)
#' using \code{\link{f_pseudoherd_animals}}, which already reports, per
#' farm x year x livestock category: \code{Qobs} (on-farm heads),
#' \code{Qeq} (pseudoherd heads, on-farm + off-farm) and \code{Qofffarm}
#' (off-farm heads), together with their activity-level splits
#' (\code{Qobs_milk}, \code{Qobs_meat}, \code{Qeq_milk}, \code{Qeq_meat}).
#' Head counts are converted to livestock units (LU) using
#' \code{data_extra$livestock$livestock_unit_coef}, and summed per
#' farm x activity x boundary.
#'
#' ## Step 2 - Herd, electricity and fuel impacts
#' Direct herd emissions (enteric fermentation, manure management) and
#' feed emissions are retrieved from \code{\link{f_GHGE_pseudoherd}},
#' which itself distinguishes on-farm and off-farm animals impacts
#' (columns suffixed \code{_livcat}, \code{_livcat_offfarm} and
#' \code{_livcat_pseudoherd}). Electricity (\code{\link{f_GHGE_elec}}) and
#' fuel (\code{\link{f_GHGE_fuels}}) impacts are also computed but not yet
#' merged (as in \code{\link{f_GHGE_herd_output}}), since no off-farm
#' analog currently exists for on-site energy use.
#'
#' ## Step 3 - Allocation of impacts to activities
#' For each boundary, impacts are prorated across activities using the LU
#' share of that boundary's animals in that activity:
#' \code{Qobs_activity_LU / Qobs_LU} for \code{on_farm},
#' \code{Qofffarm_activity_LU / Qofffarm_LU} for \code{off_farm}, and
#' \code{Qeq_activity_LU / Qeq_LU} for \code{pseudoherd}.
#'
#' ## Step 4 - Allocation of activity impacts to outputs
#' Economic allocation ratios are retrieved via
#' \code{\link{f_output_econ_alloc}} (optionally including pseudo-herd
#' outputs when \code{account_pseudoherd = TRUE}), and applied to each
#' boundary's activity-level impact.
#'
#' ## Step 5 - Aggregation per output
#' \code{meat_cull_cow} is recoded as \code{meat_beef}. CO2e, area and
#' production variables are summed per farm x year x activity x output x
#' species x boundary, and expressed per hectare (farm / pseudo-farm feed
#' area) and per tonne of product.
#'
#' ## Step 6 - Per product type tables
#' Results are split into \code{GHGE_milk} (\code{output == "milk"}),
#' \code{GHGE_meat} (\code{activity == "meat"}) and \code{GHGE_eggs}
#' (\code{activity == "eggs"}), each carrying the \code{boundary} column
#' (\code{on_farm}, \code{off_farm}, \code{pseudoherd}).
#'
#' @param object A valid \code{FADN2Footprint} S4 object.
#' @param overwrite Logical (default FALSE). If FALSE and cached results
#'   exist, the cached object is returned.
#' @param account_pseudoherd Logical (default TRUE, unlike
#'   \code{\link{f_GHGE_herd_output}}). Passed to
#'   \code{\link{f_output_econ_alloc}} to include off-farm animal outputs
#'   in the economic allocation ratios.
#' @param ... Additional arguments passed to internal helper functions.
#'
#' @return A named \code{list} with three elements (\code{GHGE_milk},
#'   \code{GHGE_meat}, \code{GHGE_eggs}), each a
#'   \code{\link[tibble]{tibble}} with one row per farm x year x activity x
#'   output x species x boundary, where \code{boundary} distinguishes
#'   \code{"on_farm"}, \code{"off_farm"} and \code{"pseudoherd"} impacts.
#'
#' @seealso
#' \code{\link{f_GHGE_herd_output}}, \code{\link{f_GHGE_pseudoherd}},
#' \code{\link{f_pseudoherd_animals}}, \code{\link{f_herd_activities}},
#' \code{\link{f_output_econ_alloc}}, \code{\link{new_FADN2Footprint}}
#'
#' @concept footprint-ghge
#' @export
#' @import dplyr
#' @import tidyr
#' @import stringr

# Steps:
## 1. Estimate pseudoherd activities and outputs (on-farm, off-farm, pseudoherd)
## 2. Estimate herd and feed impact (on-farm, off-farm, pseudoherd)
## 3. Allocate impact to activities, per boundary
## 4. Allocate activity impact to outputs, per boundary
## 5. Aggregate per output
## 6. Split per product type

f_GHGE_pseudoherd_output <- function(object,
                                     overwrite = FALSE,
                                     account_pseudoherd = TRUE,
                                     ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_pseudoherd_milk) && !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_pseudoherd_milk.")
    return(object@footprints$GHGE$GHGE_pseudoherd_milk)
  }
  if (!is.null(object@footprints$GHGE$GHGE_pseudoherd_meat) && !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_pseudoherd_meat.")
    return(object@footprints$GHGE$GHGE_pseudoherd_meat)
  }
  if (!is.null(object@footprints$GHGE$GHGE_pseudoherd_eggs) && !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_pseudoherd_eggs.")
    return(object@footprints$GHGE$GHGE_pseudoherd_eggs)
  }

  id_cols = object@traceability$id_cols

  ## Cattle ----
  GHGE_pseudoherd_output_cattle <- f_GHGE_pseudoherd_output_cattle(object)

  ## swine ----
  GHGE_pseudoherd_output_swine <- f_GHGE_pseudoherd_output_swine(object)

  ## poultry ----
  GHGE_pseudoherd_output_poultry <- f_GHGE_pseudoherd_output_poultry(object)


  # 5. Per product type tables ------------------------------------------------------------------------------

  milk_impact <- GHGE_pseudoherd_output_cattle |>
    dplyr::filter(output == "milk")

  meat_impact <- Reduce(f = bind_rows,
                        x = list(GHGE_pseudoherd_output_cattle |>
                                   dplyr::filter(grepl("meat", output)),
                                 GHGE_pseudoherd_output_swine,
                                 GHGE_pseudoherd_output_poultry |>
                                   dplyr::filter(grepl("meat", output))
                        ))

  #eggs_impact <- GHGE_pseudoherd_output_poultry |>
  #  dplyr::filter(grepl("eggs", output))



  return(list(
    GHGE_milk = milk_impact,
    GHGE_meat = meat_impact,
    GHGE_eggs = NULL
  ))
}
