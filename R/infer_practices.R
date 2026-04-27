#' Function to estimate agricultural practices based on FADN data
#' `infer_practices` Create a tibble with 4 variables: 'farm_id', 'land_use_type', 'crop', 'A.4.3'
#'
#' @param object a FADN2Footprint object
#' @returns a tibble
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' infer_practices(object = fadn_fict_obj)
#'
#' @concept practice-crop
#' @concept practice-herding
#' @export

#' @import dplyr
#'
#'
#'

infer_practices <- function(object,
                            overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # 1. Compute each practice ----
  # 2. Define validation rule for each practice
  ## Farms with non valid practice will be discarded from the analysed data set
  # 3. Update object slot

  ### CROPS ----
  crop_diversity = f_crop_diversity(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = ifelse(!is.na(crop_diversity) & crop_diversity >= 0,TRUE,FALSE))
  object@practices$crops$crop_diversity <- crop_diversity

  N_ferti = f_n_ferti(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = dplyr::case_when(
      # farms without fertilization estimation or negative one
      is.na(N_ferti) ~ FALSE,
      N_ferti < 0 ~ FALSE,
      # farms that are organic but have purchased mineral fertiliser
      ORGANIC == TRUE & N_min_ferti_Q > 0 ~ FALSE,
      .default = TRUE))
  object@practices$crops$N_ferti <- N_ferti

  share_Nmin_ferti <- f_share_Nmin_ferti(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = ifelse(!is.na(share_Nmin_ferti) & share_Nmin_ferti >= 0 & share_Nmin_ferti <= 1,TRUE,FALSE))
  object@practices$crops$share_Nmin_ferti <- share_Nmin_ferti

  pesticides <- f_plant_prot(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = ifelse(!is.na(pesticides) & pesticides >= 0,TRUE,FALSE))
  object@practices$crops$pesticides <- pesticides

  tillage <- f_tillage(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = ifelse(!is.na(tillage) & tillage >= 0,TRUE,FALSE))
  ## In the f_tillage function, we set as NA tillage value for farms without any diesel registered as it is unlikely
  object@practices$crops$tillage <- tillage

  livestock_density <- f_livestock_density(object, overwrite = overwrite) |>
    dplyr::mutate(valid_practice = ifelse(!is.na(livestock_density) & livestock_density >= 0,TRUE,FALSE))
  object@practices$crops$livestock_density <- livestock_density

  if (!is.null(object@landscape_metrics)) {
    hedge_density <- f_hedge_density(object, overwrite = overwrite) |>
      dplyr::mutate(valid_practice = ifelse(!is.na(hedge_density) & hedge_density >= 0,TRUE,FALSE))
    object@practices$crops$hedge_density <- hedge_density

    mean_field_size <- f_mean_field_size(object, overwrite = overwrite) |>
      dplyr::mutate(valid_practice = ifelse(!is.na(mean_field_size) & mean_field_size >= 0,TRUE,FALSE))
    object@practices$crops$mean_field_size <- mean_field_size

    ground_cover <- f_ground_cover(object, overwrite = overwrite) |>
      dplyr::mutate(valid_practice = ifelse(!is.na(ground_cover) & ground_cover >= 0 & ground_cover <= 366,TRUE,FALSE))
    # we set the maximum at 366 to account for leap years such as 2020
    object@practices$crops$ground_cover <- ground_cover
  }

  ### HERDING ----

  #### General ----

  herding_practices <- f_herding_practices(object, overwrite = overwrite)
  object@practices$herding$general <- herding_practices

  #### Feed ----

  herd_feed <- f_herd_feed(object, overwrite = overwrite)
  object@practices$herding$feed <- herd_feed

  # Remove farms with aberrant gross energy values for their main livestock category
  ## GE thresholds
  tmp_mean_GE <- FADN_averages$GE_MJ_anim_day |>
    # define thresholds
    dplyr::mutate(

      threshold_up = mean_GE_MJ_anim_day + 3*sd_GE_MJ_anim_day,

      threshold_down = dplyr::case_when(
        FADN_code_letter == "LCOWDAIR" ~  0.25*mean_GE_MJ_anim_day,
        .default = 0.10*mean_GE_MJ_anim_day
      )
    )

  ## main livestock category
  tmp_max_cat <- object@herd |>
    # estimate LU
    dplyr::mutate(
      LU = Qobs * livestock_unit_coef
    ) |>
    # filter max LU
    dplyr::slice_max(order_by = LU, with_ties = FALSE, by = object@traceability$id_cols) |>
    # select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter) |>
    # add thresholds
    dplyr::left_join(
      tmp_mean_GE,
      by = c('FADN_code_letter', 'COUNTRY')
    )

  ## retrieve farms with aberrant values
  farms_to_remove <- herd_feed$feed_intake$total |>
    # add thresholds and filter for the main livestock category
    dplyr::inner_join(
      tmp_max_cat,
      by = c(object@traceability$id_cols,'FADN_code_letter')
    ) |>
    # filter farms above & below thresholds
    dplyr::mutate(
      valid_practice = dplyr::case_when(
        # between thresholds
        GE_MJ_anim / 365 >= threshold_down & GE_MJ_anim / 365 <= threshold_up ~ TRUE,
        # if NAs
        is.na(GE_MJ_anim) | is.na(threshold_down) | is.na(threshold_up) ~ FALSE,
        .default = FALSE
      )
    ) |>
    dplyr::filter(valid_practice == FALSE) |>
    # add removing justification
    dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |>
    dplyr::mutate(
      problem = "aberrant_feed"
    )

  #### N excretion ----

  Nexcr <- f_n_excr(object, overwrite = overwrite)
  object@practices$herding$N_excretion <- Nexcr

  #### Rearing parameters ----

  rear_param_cattle <- f_herd_rearing_param_cattle(object)
  object@practices$herding$rearing_parameters$cattle <- rear_param_cattle |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  dplyr::matches("rt_|t_1st|offspring"))

  rear_param_swine <- f_herd_rearing_param_swine(object)
  object@practices$herding$rearing_parameters$swine <- rear_param_swine |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  dplyr::matches("rt_|t_1st|offspring"))

  rear_param_poultry <- f_herd_rearing_param_poultry(object)
  object@practices$herding$rearing_parameters$poultry <- rear_param_poultry |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  dplyr::matches("rt_|t_1st|offspring"))

  #### Activity ----

  herd_activity <- f_herd_activities(object, overwrite = overwrite)
  object@practices$herding$activities <- herd_activity


  # 2. Retrieve farms with non valid practices ----

  if (!is.null(object@traceability$discarded_farms)&& !overwrite) {

    message("Using cached values stored in object@traceability$discarded_farms")
    discarded_farms = object@traceability$discarded_farms  # use cached value
  } else {

  ## Farms with non valid practice will be discarded from the analysed data set
  ## Farms for which we could not estimate any practices will be discarded
  discarded_farms <- tibble::tibble()
  # or even better, pre-define with id_cols

  ### CROPS
  for (i in names(object@practices$crops)[!sapply(object@practices$crops, is.null)]) {

    tmp <- object@practices$crops[[i]] |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),valid_practice) |>
      dplyr::filter(valid_practice == FALSE) |>
      dplyr::distinct() |>
      # add justification
      dplyr::mutate(problem = paste0("invalid_",i))

    discarded_farms <- discarded_farms  |>
      # add to other farms
      dplyr::bind_rows(tmp) |>
      dplyr::distinct()
  }
  ### HERDING
  if (exists("farms_to_remove") && nrow(farms_to_remove) > 0) {
    discarded_farms <- dplyr::bind_rows(discarded_farms, farms_to_remove)
  }

  # 5. Remove discarded farms from object

  if (nrow(discarded_farms) >0) {
    discarded_farms_nb <- discarded_farms |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |>
      dplyr::distinct()

    message(nrow(discarded_farms_nb),
            " out of ",
            nrow(object@farm),
            " (",
            round(nrow(discarded_farms_nb)/nrow(object@farm)*100,0),
            "%)",
            " observations with non valid practices are discarded from the S4 object slots (see method).")

    object <- m_remove_farms(object, farms_to_remove = discarded_farms)

  }

  }

  # Return updated object

  return(object)

}

