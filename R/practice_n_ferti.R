#' Calculate Nitrogen Fertilization for Crops and Grasslands
#'
#' This function estimates the nitrogen (N) fertilization applied to crops and
#' grasslands based on FADN and farming practices surveys (EPK).
#' It considers both mineral and organic N fertilization,
#' taking into account organic farming practices and livestock presence on farms.
#'
#' @param object An object of class `FADN2Footprint`. This object should contain
#'   farm data, crop data, input data, and farm-level organic certification status.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A data frame containing estimated nitrogen fertilization values for
#'   each crop on each farm. The columns include:
#'   \itemize{
#'     \item `id_cols`: Farm identifier(s) as defined in `object@traceability$id_cols`.
#'     \item `ORGANIC`: A logical value indicating if the farm is certified organic (TRUE) or not (FALSE).
#'     \item `FADN_code_letter`: The FADN crop code.
#'     \item `area_ha`: The area in hectares for the specific crop.
#'     \item `N_ferti`: Total nitrogen fertilization in tons of N per hectare (kg N ha-1).
#'     \item `N_ferti_min`: Mineral nitrogen fertilization in tons of N per hectare (kg N ha-1).
#'     \item `N_ferti_org`: Organic nitrogen fertilization in tons of N per hectare (kg N ha-1).
#'   }
#'
#' @details
#' The function performs the following key steps:
#' \enumerate{
#'   \item **Data Validation**: Checks if the input `object` is of class `FADN2Footprint`.
#'   \item **EPK Averages**: Retrieves average N fertilization (mineral and organic)
#'     from the EPK (French Farming Practices Surveys; stored in \code{data_extra$PKGC_N_ferti}
#'     and \code{data_extra$PKGC_N_ferti_org_thresholds}) 2017 dataset,
#'     assigning these averages to crops based on FADN codes. It accounts for
#'     organic certification, setting mineral N to zero for organic farms.
#'   \item **EPK Organic Thresholds**: Fetches thresholds for organic N fertilization
#'     from EPK data, which are used to cap N application on organic farms based on
#'     livestock.
#'   \item **Reference Value Combination**: Combines EPK averages and thresholds to
#'     create a reference table (`EPK_ref`) for N fertilization. It handles missing
#'     crop-specific values by using species averages or overall national averages.
#'   \item **Livestock N Excretion**: Calls a helper function `f_n_excr` to estimate
#'     the total N excreted from livestock on each farm.
#'   \item **Data Joining**: Merges farm, crop, input, organic certification,
#'     EPK reference values, and livestock N excretion data.
#'   \item **N Fertilization Estimation**: Calculates theoretical N fertilization
#'     based on EPK averages and farm area (stored in \code{data_extra$PKGC_N_ferti}). It then estimates actual N fertilization
#'     (`N_ferti`) considering:
#'     \itemize{
#'       \item **Mineral N**: Applied based on theoretical values, but set to 0 for organic farms.
#'       \item **Organic N**: Determined by livestock N excretion. If livestock is present,
#'         the amount of N excreted is used, capped by estimated EPK N thresholds
#'         (minimum and maximum; stored in \code{data_extra$PKGC_N_ferti_org_thresholds}). If no livestock is present, the EPK average organic N
#'         is used.
#'     }
#'   \item **Output Formatting**: Selects and returns the relevant N fertilization
#'     variables. Messages are printed to the console to inform about the number of
#'     farms falling into different livestock categories (no livestock, not enough,
#'     too much, sufficient).
#' }
#'
#' @references
#' EPK: Ministère De L’Agriculture - SSP, 2019. Pratiques culturales sur les grandes cultures - 2017. https://doi.org/10.34724/CASD.56.3033.V1
#'
#' @concept practice-crop
#' @export
#' @importFrom dplyr select mutate left_join summarise filter distinct case_when ungroup bind_rows
#' @importFrom tidyr separate_longer_delim
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#'
#'

f_n_ferti <- function(object,
                      overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$N_ferti)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$N_ferti.")
    return(object@practices$crops$N_ferti)  # use cached value
  }

  # here we considered that fertilizers are applied both on crops and grasslands

  # EPK averages --------------------------------------------------------------------------------------------------------------------------------------
  EPK_Nferti <- data_extra$PKGC_N_ferti

  # assign averages by crop codes (60 different crops)
  EPK_avrg <- data_extra$crops |>
    dplyr::select(FADN_code_letter,land_use_type,EPK_ferti_crop_name) |>
    # select crop code and name
    dplyr::filter(FADN_code_letter %in% unique(object@crop$FADN_code_letter)) |>
    # add fertilization data from EPK 2017
    tidyr::separate_longer_delim(EPK_ferti_crop_name,";") |>
    dplyr::left_join(EPK_Nferti,
              by = join_by(EPK_ferti_crop_name),
              relationship = "many-to-many" # each crop has both an organic and a conventional value
    ) |>
    # average by crop and organic certification
    dplyr::filter(!is.na(EPK_ferti_crop_name)) |>
    dplyr::summarise(
      EPK_Nmin = mean(N_min, na.rm = T),
      EPK_Norg = mean(N_org, na.rm = T),
      .by = c(FADN_code_letter,ORGANIC)
    ) |>
    # add zero for organic mineral nitrogen fertilization
    dplyr::mutate(
      EPK_Nmin = ifelse(ORGANIC == 1, 0, EPK_Nmin)
    )

  # Estimate N org threshold for farms with livestock ---------------------------------------------------------------------------------------------------------------------
  #source("~/BiodivLabel/scripts/EPK_data_Norg_threshold.R", encoding = 'UTF-8')
  ## execute script when EPK 2017 data available
  EPK_Norg <- data_extra$PKGC_N_ferti_org_thresholds

  # assign threshold by crop codes (60 different crops)
  EPK_thrhld <- data_extra$crops |>
    dplyr::select(FADN_code_letter,land_use_type,EPK_ferti_crop_name) |>
    # select crop code and name
    dplyr::filter(FADN_code_letter %in% unique(object@crop$FADN_code_letter)) |>
    # add fertilization data from EPK 2017
    tidyr::separate_longer_delim(EPK_ferti_crop_name,";") |>
    dplyr::left_join(EPK_Norg,
              by = join_by(EPK_ferti_crop_name),
              relationship = "many-to-many" # each crop has both an organic and a conventional value
    ) |>
    # average by crop and organic certification
    dplyr::filter(!is.na(EPK_ferti_crop_name)) |>
    dplyr::summarise(
      Norg_thrhld_min = min(Norg_thrhld_min, na.rm = T),
      Norg_thrhld_max = max(Norg_thrhld_max, na.rm = T),
      .by = c(FADN_code_letter,ORGANIC)
    )

  # Combine EPK reference values ---------------------------------------------------------------------------------------------------------------------

  # select threshold as the minimum or maximum between average standard value CI 95% limit
  EPK_ref <- dplyr::left_join(EPK_avrg,EPK_thrhld,
                       by = join_by(FADN_code_letter, ORGANIC)) |>
    dplyr::mutate(
      Norg_thrhld_min = pmin(EPK_Norg, Norg_thrhld_min, na.rm = T),
      Norg_thrhld_max = pmax(EPK_Norg, Norg_thrhld_max, na.rm = T),
    ) |>
    # encode organic farming
    dplyr::mutate(
      ORGANIC = ifelse(ORGANIC == 1,"T","F")
    )


  # add missing FADN crop codes
  # For crops without specific reference average, we used values of same species crops or, when no value for the crop species, the overall average
  EPK_missing <- data_extra$crops |>
    dplyr::select(FADN_code_letter,land_use_type,EPK_ferti_crop_name) |>
    dplyr::filter(is.na(EPK_ferti_crop_name)) |>
    # add organic for each
    dplyr::mutate(ORGANIC = "T;F") |>
    tidyr::separate_longer_delim(ORGANIC,";") |>
    # add species
    dplyr::left_join(
      data_extra$crops |>
        dplyr::select(FADN_code_letter,species),
      by = join_by(FADN_code_letter)
    ) |>
    # add reference value by species
    dplyr::left_join(
      EPK_ref |>
        dplyr::left_join(
          data_extra$crops |>
            dplyr::select(FADN_code_letter,species),
          by = join_by(FADN_code_letter)
        ) |>
        dplyr::summarise(
          EPK_Nmin = mean(EPK_Nmin,na.rm = T),
          EPK_Norg = mean(EPK_Norg,na.rm = T),
          Norg_thrhld_min = min(Norg_thrhld_min, na.rm = T),
          Norg_thrhld_max = max(Norg_thrhld_max, na.rm = T),
          .by = c(species,ORGANIC)),
      by = join_by(species,ORGANIC),
      relationship = "many-to-many"
    ) |>
    # for species without reference value, we assign overall average by organic certification
    group_by(ORGANIC) |>
    dplyr::mutate(
      EPK_Nmin = ifelse(is.na(EPK_Nmin), mean(EPK_Nmin,na.rm = T), EPK_Nmin),
      EPK_Norg = ifelse(is.na(EPK_Norg), mean(EPK_Norg,na.rm = T), EPK_Norg),
      Norg_thrhld_min = ifelse(is.na(Norg_thrhld_min), mean(Norg_thrhld_min,na.rm = T), Norg_thrhld_min),
      Norg_thrhld_max = ifelse(is.na(Norg_thrhld_max), mean(Norg_thrhld_max,na.rm = T), Norg_thrhld_max)
    ) |> ungroup()

  EPK_ref <- rbind(EPK_ref,EPK_missing |> dplyr::select(colnames(EPK_ref))) |>
    dplyr::mutate(ORGANIC = as.logical(ORGANIC))

  # Estimate N excretion from livestock --------------------------------------------------------------------------------------------------------------------------------------

  N_excr <- f_n_excr(object, overwrite = overwrite)
  N_excr = Reduce(f = bind_rows, x = N_excr)
  # Total amount of N excreted is in kg N yr-1

  # Join data --------------------------------------------------------------------------------------------------------------------------------------

  input_data <- object@crop |>
    # crop data
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),FADN_code_letter,area_ha) |>
    #filter(!is.na(area_ha)) |>
    # input data
    dplyr::left_join(
      object@input |>
        dplyr::select(tidyselect::all_of(object@traceability$id_cols),
                      N_min_ferti_Q),
      by = object@traceability$id_cols) |>
    # Convert Quantity of N used in mineral fertilizers from tons to kg
    dplyr::mutate(N_min_ferti_Q = N_min_ferti_Q * 10^3) |>
    # organic certification
    dplyr::left_join(
      object@farm |>
        dplyr::select(tidyselect::all_of(object@traceability$id_cols),
                      ORGANIC),
      by = object@traceability$id_cols) |>
    # add N averages and thresholds
    dplyr::left_join(
      EPK_ref,
      by = join_by(FADN_code_letter, ORGANIC)
    ) |>
    # add N excreted from livestock
    dplyr::left_join(
      N_excr |>
        dplyr::summarise(Nex_kgN_farm = sum(Nex_kgN_livcat_y,na.rm = T),
                  .by = object@traceability$id_cols),
      by = object@traceability$id_cols)

  # Estimate parameter -----------------------------------------------------------------------------------------------------------------------------
  N_ferti <- input_data |>
    # here we considered that fertilizers are applied both on crops and grasslands
    # calculate theoretical fertilization
    dplyr::mutate(
      Nmin_th = EPK_Nmin * area_ha,
      Norg_th = EPK_Norg * area_ha
    ) |>
    # calculate sum of theoretical fertilization for each farm
    dplyr::mutate(
      sum_Nmin_th = sum(Nmin_th, na.rm = T),
      sum_Norg_th = sum(Norg_th, na.rm = T),
      .by = object@traceability$id_cols
    ) |>
    # calculate N fertilization for each crop in each farm
    dplyr::mutate(

      ## mineral N fertilizer
      ## still zero for organic farms
      N_ferti_min = case_when(
        ORGANIC == T ~ 0,
        N_min_ferti_Q == 0 ~ 0,
        .default = (N_min_ferti_Q * (Nmin_th/sum_Nmin_th)) / area_ha
      ),

      ## organic N fertilizer
      ### estimate if there is N excretion from livestock in the farm
      livestock = case_when(
        Nex_kgN_farm >0 ~ T,
        .default = F),
      ### estimate if enough livestock
      livestock_enough = case_when(
        Nex_kgN_farm >0 & ((Nex_kgN_farm * (Norg_th/sum_Norg_th)) / area_ha) > Norg_thrhld_min  ~ T,
        .default = F
      ),
      ### estimate if too much livestock
      livestock_toomuch = case_when(
        Nex_kgN_farm >0 & ((Nex_kgN_farm * (Norg_th/sum_Norg_th)) / area_ha) > Norg_thrhld_max  ~ T,
        .default = F
      ),

      ### estimate N org
      N_ferti_org = case_when(
        ### if no livestock
        livestock == F ~ EPK_Norg,
        ### if livestock: N excreted from livestock
        livestock == T & livestock_enough == T & livestock_toomuch == F ~ (Nex_kgN_farm * (Norg_th/sum_Norg_th)) / area_ha,
        ### if not enough livestock: organic fertilizer minimum threshold
        livestock == T & livestock_enough == F & livestock_toomuch == F ~ Norg_thrhld_min,
        ### if too much livestock: organic fertilizer maximum threshold
        livestock == T & livestock_enough == T & livestock_toomuch == T ~ Norg_thrhld_max
      ),
      # Total nitrogen fertilization, t N ha-1
      N_ferti = N_ferti_min + N_ferti_org
    ) |> ungroup()

  message(nrow(N_ferti |> dplyr::filter(livestock == F) |> dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |> distinct()),
          " observations have no livestock. They receive the average national value as a standard value (see Methods)."
  )
  message(nrow(N_ferti |> dplyr::filter(livestock == T & livestock_enough == F) |> dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |> distinct()),
          " observations have not enough N excretion from their livestock. They receive the minimum organic N threshold as a standard value (see Methods)."
  )

  message(nrow(N_ferti |> dplyr::filter(livestock == T & livestock_toomuch == T) |> dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |> distinct()),
          " observations have too much N excretion from their livestock. They receive the maximum organic N threshold as a standard value (see Methods)."
  )

  message(nrow(N_ferti |> dplyr::filter(livestock == T & livestock_enough == T & livestock_toomuch == F) |> dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |> distinct()),
          " observations have enough and not too much N excretion from their livestock. They spread the total N excreted from their livestock on-farm (see Methods)."
  )

  # Output ------------------------------------------------------------------------------------------------------------------------------------------------------
  N_ferti = N_ferti |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),ORGANIC,
                  FADN_code_letter,area_ha,N_min_ferti_Q,
                  N_ferti,N_ferti_min,N_ferti_org)

  message("\nNitrogen fertilization variables are in ton of N ha-1.")

  #object@practices$crops$N_ferti <- N_ferti

  return(N_ferti)

}



#tmp_check = N_ferti |>
#  summarise(raw_data = unique(N_min_ferti_Q),
#            model_output = sum(N_ferti_min*area_ha,na.rm = T),
#            .by = object@traceability$id_cols)
#plot(tmp_check$raw_data,tmp_check$model_output)




