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

f_livestock_density <- function(object, overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$livestock_density) && !overwrite) {
    message(
      "Using cached values stored in object@practices$crops$livestock_density."
    )
    return(object@practices$crops$livestock_density) # use cached value
  }

  id_cols <- object@traceability$id_cols

  # see EUROSTAT glossary: grazing livestock density

  # 1. Estimate Main Forage Area ------------------------------------------------------------------------------

  tmp_forage <- unique(na.omit(data_extra$crops$FADN_code_letter[
    data_extra$crops$feed_type == "feed_rough"
  ]))
  tmp_grazing_livestock <- c("cattle", "sheep", "goats", "equidae")
  #tmp_grazing_livestock = "cattle"

  MFA <- object@crop |>
    # filter forage
    dplyr::filter(FADN_code_letter %in% tmp_forage) |>
    # sum up areas
    dplyr::summarise(
      MFA_ha = sum(area_ha, na.rm = T),
      .by = id_cols
    )

  # 2. Estimate grazing livestock units ------------------------------------------------------------------------------

  grazing_LU <- object@herd |>
    dplyr::filter(species %in% tmp_grazing_livestock) |>
    ## summarize livestock_density per farm
    dplyr::summarise(
      grazing_LU = sum(Qobs * livestock_unit_coef, na.rm = T),
      .by = c(id_cols)
    )

  # 3. Estimate livestock density ------------------------------------------------------------------------------

  livestock_density <- object@farm |>
    dplyr::select(dplyr::all_of(id_cols)) |>
    # add main forage area
    dplyr::left_join(
      MFA,
      by = id_cols
    ) |>
    # add grazing livestock units
    dplyr::left_join(
      grazing_LU,
      by = id_cols
    ) |>
    # estimate grazing livestock density for farms with grazing livestock and main forage area
    dplyr::mutate(
      livestock_density = dplyr::case_when(
        MFA_ha > 0 & grazing_LU > 0 ~ grazing_LU / MFA_ha,
        # ToDo: handle cases where either MFA_ha or grazing_LU is zero or absent, which may indicate missing data or invalid practices
        ## specifically: how to handle farms with grazing livestock but no main forage area, or vice versa
        TRUE ~ 0
      )
    )

  return(livestock_density)
}
