#' Function to estimate Plant Protection Agents based on FADN data
#' `plant_prot` Create a tibble with the following variables:  'farm_id', 'crop', 'TFI_ha'
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
#' plant_prot(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @export

#' @import dplyr
#' @import tidyr
#'
#'
#'

f_plant_prot <- function(object,
                         overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$pesticides)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$pesticides.")
    return(object@practices$crops$pesticides)  # use cached value
  }

  # Retrieve reference values ----------------------------------------------------------------------------------------------
  # source EPK 2017 (kg ha-1) and others (see raw file)
  EPK_TFI_avrg <- data_extra$IFT_ref |>
     # we will use ift.hbcref = average TFI without biological control agents
    dplyr::select(lib_ift,ifthbcref)

  # assign averages by crop codes (60 different crops)
  TFI_ref <- data_extra$crops |>
    dplyr::select(FADN_code_letter,TFI_crop_name) |>
    # select crop code and name
    dplyr::filter(FADN_code_letter %in% unique(object@crop$FADN_code_letter)) |>
    # add pesticide use data from EPK
    separate_longer_delim(TFI_crop_name,";") |>
    left_join(EPK_TFI_avrg,
              by = join_by(TFI_crop_name == lib_ift),
              relationship = "many-to-many" # each crop has both an organic and a conventional value
    ) |>
    # add average TFI
    filter(!is.na(TFI_crop_name)) |>
    group_by(FADN_code_letter) |>
    summarise(
      TFI = mean(ifthbcref,na.rm = T)
    )

  # add missing FADN crop codes
  # For crops without specific reference average, we used values of same species crops or, when no value for the crop species, the overall average
  EPK_missing <- data_extra$crops |>
    dplyr::select(FADN_code_letter,TFI_crop_name) |>
    filter(is.na(TFI_crop_name)) |>
    # add species
    left_join(
      data_extra$crops |>
        dplyr::select(FADN_code_letter,species),
      by = join_by(FADN_code_letter)
    ) |>
    # add reference value by species
    left_join(
      TFI_ref |>
        left_join(
          data_extra$crops |>
            dplyr::select(FADN_code_letter,species),
          by = join_by(FADN_code_letter)
        ) |>
        group_by(species) |>
        summarise(
          TFI = mean(TFI,na.rm = T),
          .groups = "drop"),
      by = join_by(species),
      relationship = "many-to-many"
    ) |>
    # for species without reference value, we assign overall average
    mutate(
      TFI = ifelse(is.na(TFI), mean(TFI,na.rm = T), TFI)
      )

  TFI_ref <- rbind(TFI_ref,EPK_missing |> dplyr::select(colnames(TFI_ref)))


  # Join data ------------------------------------------------------------------------------------------------------------------

  input_data <- object@crop |>
    # crop data
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,land_use_type,area_ha) |>
    #filter(!is.na(area_ha)) |>
    # input data
    left_join(
      object@input |>
        dplyr::select(all_of(object@traceability$id_cols),pesticides_v),
      by = object@traceability$id_cols
    ) |>
    # organic certification
    left_join(
      object@farm |>
        dplyr::select(all_of(object@traceability$id_cols),ORGANIC),
      by = object@traceability$id_cols
    ) |>
    # add reference TFI
    dplyr::left_join(TFI_ref,
                     by = join_by(FADN_code_letter))

  # Estimate parameter ---------------------------------------------------------------------------------------------------------
  pesticides <- input_data |>
      # calculate theoretical TFI for crop area
    dplyr::mutate(
      TFI_th = TFI * area_ha
    ) |>
    # calculate sum of theoretical TFI for each farm
    dplyr::group_by(across(all_of(object@traceability$id_cols))) |>
    dplyr::mutate(
      sum_TFI_th = sum(TFI_th,na.rm = T)
    ) |> ungroup() |>
    # calculate parameter for each crop in each farm
    dplyr::mutate(
      value_TFI_ha = dplyr::case_when(
        # we consider that no plant protection agents are applied to grasslands
        land_use_type == "grassland" ~ 0,
        # for organic farms
        ## we considered that the plant protection agents used in organic farming are 3.82 less impacting for biodiversity than the plant protection agents used in conventional farming (see method)
        ORGANIC == T ~ ((pesticides_v/3.82) * (TFI_th/sum_TFI_th)) / area_ha,
        # ORGANIC == T ~ ((pesticides_v/1) * (TFI_th/sum_TFI_th)) / area_ha,
        # for conventional farms
        .default = (pesticides_v * (TFI_th/sum_TFI_th)) / area_ha
      )
    )

  ## Output -------------------------------------------------------------------------------------------------------------
  pesticides = pesticides |>
    dplyr::select(all_of(object@traceability$id_cols),ORGANIC,FADN_code_letter,land_use_type,value_TFI_ha) |>
    rename(pesticides = value_TFI_ha) |>
    # only for arable land
    filter(land_use_type == "arable")

  #object@practices$crops$pesticides <- pesticides

  return(pesticides)

}


utils::globalVariables(c('TFI_ha', 'pesticides_v', 'TFI', 'TFI_th', 'TFI_crop_name', 'ifttref', 'TFI_th', 'sum_TFI_th'))
# this is to avoid a note in check package (the issue is from the use of dplyr)














