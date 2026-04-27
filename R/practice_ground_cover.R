#' Function to estimate ground cover based on FADN, LPIS and SENTINEL-2 data
#' `ground_cover` Create a tibble with the following variables:  'farm_id', 'land_use_type', 'ground_cover'
#'
#' @param object a FADN2Footprint object
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @returns
#' A tibble with:
#' - ground_cover: average of hedge linear meter per hectare
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' ground_cover(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_ground_cover <- function(object,
                           overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$crops$ground_cover)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$ground_cover.")
    return(object@practices$crops$ground_cover)  # use cached value
  }

  if (is.null(object@landscape_metrics)) {
    warning("No landscape metrics available, returning NULL ground cover")
    ground_cover <- NULL
  } else {

    ground_cover <- object@crop |>
      # add landscape metrics
      dplyr::left_join(
        object@landscape_metrics |>
          dplyr::select(tidyselect::all_of(object@traceability$id_cols),
                 area_tot,parcelle_tot,
                 moy_pacage_cvegetale,moy_cvegetale_pprlandes,moy_cvegetale_arbocultvigne,moy_cvegetale_culture),
        by = object@traceability$id_cols) |>
        # estimate parameter
      dplyr::mutate(
        # retrieve hedge density depending on land use type
        ground_cover = case_when(
          land_use_type == "arable" & species != "fruits" ~ moy_cvegetale_culture,
          land_use_type == "arable" & species == "fruits" ~ moy_cvegetale_arbocultvigne,
          land_use_type == "grassland" ~ moy_cvegetale_pprlandes
        )) |>
      # case when there is no value for the corresponding land use type => use farm average
      ## we considered that FADN data are more exhaustive than LPIS data regarding crops as farmers might not declare crops in LPIS for which they will not have any subsidies while such crop would be registered in their accountancy
      dplyr::mutate(
        ground_cover = ifelse(is.na(ground_cover),moy_pacage_cvegetale,ground_cover)) |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),land_use_type,FADN_code_letter,ground_cover) |>
      # only for arable land
      dplyr::filter(land_use_type == "arable") |>
      # for ground cover, the BV contribution function is decreasing
      # thus we take the inverse of the number of covered days as the number of day of uncovered soil
      # 2020 had 366 days
      dplyr::mutate(ground_cover = 366 - ground_cover)

  }

  #object@practices$crops$ground_cover <- ground_cover


  return(ground_cover)

}

utils::globalVariables(c('id', 'PACAGE',
                         'area_tot','parcelle_tot','moy_pacage_cvegetale','moy_cvegetale_pprlandes','moy_cvegetale_arbocultvigne','moy_cvegetale_culture'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

