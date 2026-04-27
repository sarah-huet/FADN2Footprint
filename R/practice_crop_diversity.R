#' Function to estimate crop diversity based on FADN data
#' `crop_diversity` Create a tibble with the following variables:  'farm_id', 'land_use_type', 'Shannon', 'crop_nb_LU'
#'
#' @param object a FADN2Footprint object
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @returns
#' A tibble with:
#' - crop_nb_pLU: number of crop per land use type (i.e., arable or grassland)
#' - Shannon: the Shannon index per land use type
#' - R_Simpson: the reciprocal Simpson index per land use type
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' crop_diversity(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_crop_diversity <- function(object,
                             overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$crops$crop_diversity) && !overwrite) {
    message("Using cached values stored in object@practices$crops$crop_diversity.")
    return(object@practices$crops$crop_diversity)  # use cached value
  }

  # we estimate two diversity indices, Shannon index and Reciprocal Simpson index, for crop spatial diversity

  # Diversity ---------------------------------------------------------------------------------------------------------------------------------
  crop_diversity <- object@crop |>
    #filter(area_ha >0) |> # filter in FADN2Footprint_constructor
    #dplyr::group_by(across(all_of(object@traceability$id_cols)),land_use_type) |>
    dplyr::summarise(
      # Crop number per land use type
      crop_nb_pLU = length(unique(FADN_code_letter)),
      # Shannon index
      Shannon = - sum((area_ha / sum(area_ha,na.rm = T))*log((area_ha / sum(area_ha,na.rm = T))),na.rm = T),
      # Reciprocal Simpson
      R_Simpson = 1/sum(area_ha^2 / sum(area_ha,na.rm = T)^2,na.rm = T),
      .by = c(object@traceability$id_cols,land_use_type)) |>
    # here we select the Shannon index
    dplyr::mutate(
      crop_diversity = Shannon
    ) |>
    # only for arable land
    dplyr::filter(land_use_type == "arable")

  #object@practices$crops$crop_diversity <- crop_diversity

  return(crop_diversity)

}

utils::globalVariables(c('ID','land_use_type','FADN_code_letter','area_ha'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

