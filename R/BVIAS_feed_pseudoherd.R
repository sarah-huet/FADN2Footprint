#' Function to estimate economic allocation ratios between herd outputs
#' `f_pseudoherd_feed_impact` Estimate herd outputs
#'
#' @param object a FADN2Footprint object
#' @returns
#' A list with, for each livestock category:
#' - Q_obs: the number of animals observed in the farm (in number of heads)
#' - Q_obs_pseudo: the number of animals of the pseudo-herd (in number of heads)
#'
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_pseudoherd_feed_impact(object = fadn_fict_obj)
#'
#' @concept footprint-biodiv
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_BVIAS_feed_pseudoherd <- function(object) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Steps:
  ## 1. Estimate off-farm animals output
  ## 2. Estimate economic allocation between outputs or activities

  ## For off-farm animals (if considering pseudo-farm), estimate feed and associated crop practices and impact ----

    # 2.2. Add NUTS2 average of feed and associated practices and impact ----

    if (exists("avrg_feed")==F) {
      source("~/GitHub/FADN2Footprint/R/extra_data/avrg_feed.R")
    }

  pseudoherd <- f_pseudoherd(object)

    pseudoherd_feed_impact <- pseudoherd |>
      # add farm NUTS2 and Organic certification
      dplyr::left_join(
        object@farm |>
          select(all_of(object@traceability$id_cols),NUTS2,ORGANIC),
        by = object@traceability$id_cols
      ) |>
      # add NUTS2 average feed
      left_join(
        avrg_feed$NUTS2,
        by = join_by(FADN_code_letter, NUTS2, ORGANIC)
      ) |>
      # add overall average feed
      left_join(
        avrg_feed$overall,
        by = join_by(FADN_code_letter, ORGANIC)
      ) |>
      # replace NAs in NUTS2 average by overall average
      mutate(
        A.3.1 = ifelse(is.na(A.3.1_NUTS2),A.3.1_all,A.3.1_NUTS2),
        A.3.3 = ifelse(is.na(A.3.3_NUTS2),A.3.3_all,A.3.3_NUTS2),
        A.4.3 = ifelse(is.na(A.4.3_NUTS2),A.4.3_all,A.4.3_NUTS2),
        A.4.5 = ifelse(is.na(A.4.5_NUTS2),A.4.5_all,A.4.5_NUTS2),
        A.5.1 = ifelse(is.na(A.5.1_NUTS2),A.5.1_all,A.5.1_NUTS2),
        BVI_ha = ifelse(is.na(BVI_ha_NUTS2),BVI_ha_all,BVI_ha_NUTS2),
        BVI_t = ifelse(is.na(BVI_t_NUTS2),BVI_t_all,BVI_t_NUTS2),
        DM_t_anim = ifelse(is.na(DM_t_anim_NUTS2),DM_t_anim_all,DM_t_anim_NUTS2),
        area_ha_anim = ifelse(is.na(area_ha_anim_NUTS2),area_ha_anim_all,area_ha_anim_NUTS2)
      ) |>
      select(-matches("_NUTS2|_all")) |>
      #      select(-matches("Qobs")) |>
      pivot_longer(cols = matches("Q_off_farm"),
                   names_to = "output",
                   values_to = "Q_off_farm") |>
      filter(Q_off_farm >0) |>
      # detail activity
      ## add species
      left_join(
        data_extra$livestock |>
          select(FADN_code_letter,species),
        by = join_by(FADN_code_letter)
      ) |>
      ## explicit activity
      mutate(
        activity = case_when(
          str_detect(output,"milk") ~ "milk",
          FADN_code_letter == "LCOWDAIR" ~ "milk",
          str_detect(output,"meat") ~ "meat",
          str_detect(output,"eggs") ~ "eggs",
          str_detect(output,"wool") ~ "wool",
          # TODO: check for wool, honey, etc
          .default = "farm"
        )
      ) |>
      # add livestock unit coefficients and convert Qobs in LU
      left_join(
        data_extra$livestock |>
          select(FADN_code_letter,livestock_unit_coef),
        by = join_by(FADN_code_letter)
      ) |>
      mutate(Q_off_farm_LU = Q_off_farm * livestock_unit_coef) |>
      # estimate total feed quantities
      mutate(
        feed_origin = "off_farm_animals",
        DM_t_crop = DM_t_anim*Q_off_farm,
        area_ha = area_ha_anim*Q_off_farm
      )


  return(pseudoherd_feed_impact)




}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

