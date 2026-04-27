#' Function to estimate the share of fertilizer based on either FADN or RICA data
#' `share_Nmin_ferti` Create a tibble with 4 variables: 'farm_id', 'land_use_type', 'crop', 'A.4.3'
#'
#' @param object a FADN2Footprint object
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @returns a tibble
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' share_Nmin_ferti(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#'
#'
#'

f_share_Nmin_ferti <- function(object,
                               overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$share_Nmin_ferti)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$share_Nmin_ferti.")
    return(object@practices$crops$share_Nmin_ferti)  # use cached value
  }

  N_ferti <- f_n_ferti(object)

  share_Nmin_ferti <- N_ferti |>
    dplyr::mutate(
      share_Nmin_ferti = case_when(
        N_ferti_min == 0 ~ 0,
        .default = N_ferti_min / N_ferti
      )
    ) |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),ORGANIC,FADN_code_letter,share_Nmin_ferti)

  #object@practices$crops$share_Nmin_ferti <- share_Nmin_ferti

  return(share_Nmin_ferti)

}

utils::globalVariables(c('share_Nmin_ferti', 'N_ferti_min', 'N_ferti'))
# this is to avoid a note in check package (the issue is from the use of dplyr)
