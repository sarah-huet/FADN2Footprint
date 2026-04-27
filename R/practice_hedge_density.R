#' Function to estimate hedge density based on FADN and LPIS data
#' `f_hedge_density` Create a tibble with the following variables:  'farm_id', 'land_use_type', 'hedge_density'
#'
#' @param object a FADN2Footprint object
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @returns
#' A tibble with:
#' - hedge_density: average of hedge linear meter per hectare
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_hedge_density(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_hedge_density <- function(object,
                            overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$crops$hedge_density)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$hedge_density.")
    return(object@practices$crops$hedge_density)  # use cached value
  }

  if (is.null(object@landscape_metrics)) {
    warning("No landscape metrics available, returning NULL hedge density")
    hedge_density <- NULL
  } else {

    hedge_density <- object@crop |>
      # add landscape metrics
      dplyr::left_join(
        object@landscape_metrics |>
          dplyr::select(tidyselect::all_of(object@traceability$id_cols),
                 area_tot,parcelle_tot,
                 densite_ln_haie_tot,densitelnhaie_pprlandes,densitelnhaie_arbocultvigne,densitelnhaie_culture),
        by = object@traceability$id_cols) |>
      # estimate parameter
      dplyr::mutate(
        # retrieve hedge density depending on land use type
        hedge_density = case_when(
          land_use_type == "arable" & species != "fruits" ~ densitelnhaie_culture,
          land_use_type == "arable" & species == "fruits" ~ densitelnhaie_arbocultvigne,
          land_use_type == "grassland" ~ densitelnhaie_pprlandes
        )) |>
      # case when there is no value for the corresponding land use type => use farm average
      ## we considered that FADN data are more exhaustive than LPIS data regarding crops as farmers might not declare crops in LPIS for which they will not have any subsidies while such crop would be registered in their accountancy
      dplyr::mutate(
        hedge_density = ifelse(is.na(hedge_density),densite_ln_haie_tot,hedge_density)) |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),land_use_type,FADN_code_letter,hedge_density)

  }

  #object@practices$crops$hedge_density <- hedge_density

  return(hedge_density)

}

utils::globalVariables(c('id_cols', 'PACAGE',
                         'area_tot','parcelle_tot',
                         'densite_ln_haie_tot','densitelnhaie_pprlandes','densitelnhaie_arbocultvigne','densitelnhaie_culture'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

