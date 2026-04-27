#' Function to estimate hedge density based on FADN and LPIS data
#' `mean_field_size` Create a tibble with the following variables:  'farm_id', 'land_use_type', 'mean_field_size'
#'
#' @param object a FADN2Footprint object
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @returns
#' A tibble with:
#' - mean_field_size: average of hedge linear meter per hectare
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' mean_field_size(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_mean_field_size <- function(object,
                              overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$crops$mean_field_size)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$mean_field_size.")
    return(object@practices$crops$mean_field_size)  # use cached value
  }

  if (is.null(object@landscape_metrics)) {
    warning("No landscape metrics available, returning NULL mean field size")
    mean_field_size <- NULL
  } else {

    mean_field_size <- object@crop |>
      # add landscape metrics
      dplyr::left_join(
        object@landscape_metrics |>
          dplyr::select(tidyselect::all_of(object@traceability$id_cols),
                 area_tot,parcelle_tot,
                 moy_parcelle_tot,moy_parcelle_pprlandes,moy_parcelle_arbocultvigne,moy_parcelle_culture),
        by = object@traceability$id_cols) |>
      # estimate parameter
      dplyr::mutate(
        # retrieve hedge density depending on land use type
        mean_field_size = case_when(
          land_use_type == "arable" & species != "fruits" ~ moy_parcelle_culture,
          land_use_type == "arable" & species == "fruits" ~ moy_parcelle_arbocultvigne,
          land_use_type == "grassland" ~ moy_parcelle_pprlandes
        )) |>
      # case when there is no value for the corresponding land use type => use farm average
      ## we considered that FADN data are more exhaustive than LPIS data regarding crops as farmers might not declare crops in LPIS for which they will not have any subsidies while such crop would be registered in their accountancy
      dplyr::mutate(
        mean_field_size = ifelse(is.na(mean_field_size),moy_parcelle_tot,mean_field_size)) |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),land_use_type,FADN_code_letter,mean_field_size)

  }

  #object@practices$crops$mean_field_size <- mean_field_size

  return(mean_field_size)

}

utils::globalVariables(c('id_cols', 'PACAGE',
                         'area_tot','parcelle_tot',
                         'moy_parcelle_tot','moy_parcelle_pprlandes','moy_parcelle_arbocultvigne','moy_parcelle_culture'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

