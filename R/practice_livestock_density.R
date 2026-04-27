#' Function to estimate the farm livestock density based on FADN data
#' `livestock_density` Create a tibble with the following variables:  'farm_id', 'crop', 'TFI_ha'
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
#' livestock_density(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_livestock_density <- function(object,
                                overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$livestock_density)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$livestock_density.")
    return(object@practices$crops$livestock_density)  # use cached value
  }

  # see EUROSTAT glossary: grazing livestock density

  # 1. Estimate Main Forage Area ------------------------------------------------------------------------------

  tmp_forage = unique(na.omit(data_extra$crops$FADN_code_letter[data_extra$crops$feed_type == "feed_rough"]))
  tmp_grazing_livestock = c("cattle","sheep","goats","equidae")
  #tmp_grazing_livestock = "cattle"

   MFA <- object@crop |>
    # filter forage
     dplyr::filter(FADN_code_letter %in% tmp_forage) |>
    # sum up areas
     dplyr::summarise(
      MFA_ha = sum(area_ha,na.rm = T),
      .by = object@traceability$id_cols
    )

   # 2. Estimate grazing livestock units ------------------------------------------------------------------------------

  grazing_LU <- object@herd |>
     dplyr::filter(species %in% tmp_grazing_livestock) |>
    ## summarize livestock_density per farm
    dplyr::summarise(
      grazing_LU = sum(Qobs*livestock_unit_coef,na.rm = T),
      .by = c(object@traceability$id_cols)
    )

  # 3. Estimate livestock density ------------------------------------------------------------------------------

   livestock_density <- object@crop |>
     # livestock density is applied only to grasslands
     dplyr::filter(land_use_type == "grassland") |>
     dplyr::select(tidyselect::all_of(object@traceability$id_cols),land_use_type,FADN_code_letter) |>
     # add main forage area
     dplyr::left_join(
       MFA,
       by = object@traceability$id_cols
     ) |>
     # add grazing livestock units
     dplyr::left_join(
       grazing_LU,
       by = object@traceability$id_cols
     ) |>
     ## if NA in grazing LU = zero grazing LU
     tidyr::replace_na(list(grazing_LU = 0)) |>
     # estimate grazing livestock density
     dplyr::mutate(
       livestock_density = grazing_LU / MFA_ha
     )

  return(livestock_density)

}


utils::globalVariables(c('TFI_ha', 'CONSOPEST', 'TFI', 'TFI_th', 'TFI_crop_name', 'ifttref', 'TFI_th', 'sum_TFI_th'))
# this is to avoid a note in check package (the issue is from the use of dplyr)














