#' Function to estimate theoretical rations of feed by livestock category
#' `f_feed_theo_ration` Estimate the theoretical herd ration of the farm per livestock category and per crop using the total amount of dry matter per livestock category in AROPAJ and the total dry matter per crop in Sailley.
#'
#' @param my_object a FADN2Footprint object
#' @returns a list with the following dataframes:
#' - AROPAJ_ref_ration: reference total amount of feed per livestock category (in ton of dry matter per livestock unit per year)
#' - Sailley_ref_feed_allocation: reference total amount of feed per crop and herd orientation (e.g., milk, meat) (in ton of dry matter per year)
#' - th_feed_ration: the theoretical ration of each farm herd (for each crop, in total ton of dry matter per year and in ton of dry matter per livestock unit per year)
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_feed_theo_ration(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#' @keywords internal

#' @import dplyr
#' @import tidyr


f_feed_theo_ration <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # AROPAJ total dry matter per livestock category -------------------------------------------------------------------------------------------------------
  ## AROPAJ
  AROPAJ <- data_extra$AROPAJ_France

  ## assign average ratio by livestock code
  AROPAJ_ref_ration <- data_extra$livestock |>
    dplyr::select(FADN_code_letter,AROPAJ) |>
    tidyr::separate_longer_delim(AROPAJ,";") |>
    # add AROPAJ values in ton of Dry Matter per livestock unit per year
    dplyr::left_join(AROPAJ,
                     by = c('AROPAJ')) |>
    # estimate average ration per FADN livestock categories
    dplyr::summarise(
      AROPAJ_DMI_t_LU_y = mean(DMI_t_LU_y,na.rm = T), # in tDM/LU/year
      .by = FADN_code_letter
    )


  # Sailley total dry matter per crop -------------------------------------------------------------------------------------------------------
  ## From Sailley et al. 2021, INRAe Prod. Anim.
  # TODO: how to generalize this to all FADN countries?
  sailley_livestock = intersect(colnames(data_extra$Sailley_2021_feed_flows),
                                unique(data_extra$livestock$Sailley_livestock))

  Sailley_ref_feed_allocation <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(Sailley_feed,FADN_code_feed,dplyr::all_of(sailley_livestock)) |>
    tidyr::pivot_longer(cols = !c(Sailley_feed,FADN_code_feed), names_to = "Sailley_livestock", values_to = "raw_flow") |>
    # estimate relative weights for each feed sub category (see data_extra$Sailley_2021_feed_flows)
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::select(Sailley_feed,dplyr::all_of(sailley_livestock)) |>
        tidyr::pivot_longer(cols = !Sailley_feed, names_to = "Sailley_livestock", values_to = "raw_flow") |>
        tidyr::pivot_wider(id_cols = Sailley_livestock, names_from = Sailley_feed, values_from = raw_flow) |>
        dplyr::mutate(
          Total_matieres_premieres_concentrees = 0,
          Grains = 0,
          Cereales = Cereales - (Dont_ble_tendre + Dont_mais_grain + Dont_orge),
          Coproduits = 0,
          Tourteaux = Tourteaux - Dont_tourteau_de_soja,
          Total_Fourrages = 0
        ) |>
        tidyr::pivot_longer(cols = !Sailley_livestock,names_to = "Sailley_feed",values_to = "rel_weight"),
      by = c('Sailley_feed', 'Sailley_livestock')
    ) |>
    # estimate relative weight percent per feed category
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::select(Sailley_feed,dplyr::all_of(sailley_livestock)) |>
        dplyr::filter(Sailley_feed %in% c("Total_matieres_premieres_concentrees","Total_Fourrages")) |>
        tidyr::pivot_longer(cols = !Sailley_feed, names_to = "Sailley_livestock", values_to = "raw_flow") |>
        tidyr::pivot_wider(id_cols = Sailley_livestock, names_from = Sailley_feed, values_from = raw_flow),
      by = c('Sailley_livestock')
    ) |>
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::select(Sailley_feed,feed_type),
      by = c('Sailley_feed')
    ) |>
    dplyr::mutate(
      rel_weight_p100 = case_when(
        feed_type == "feed_concent" ~ rel_weight / Total_matieres_premieres_concentrees,
        feed_type == "feed_rough" ~ rel_weight / Total_Fourrages
      )
    ) |>
    # filter feed >= 10% to keep only main feed type
    #filter(rel_weight_p100 >= 0.1) |>
    # filter feed >= 5% to keep only main feed type
    #filter(rel_weight_p100 >= 0.05) |>
    # recalculate overall feed percent
    dplyr::mutate(relweight_feed = sum(rel_weight,na.rm = T), # in tDM
                  ref_p100_feed = rel_weight / relweight_feed, # in %
                  .by = Sailley_livestock
    )
  # Estimate farm theoretical ration -------------------------------------------------------------------------------------------------------
  ## Steps:
  ## 1. Estimate total feed DM per livestock category by multiplying AROPAJ ref by nb of livestock units => th_total_DMI_t_livcat_y
  ## 2. Estimate ton of DM per crop per livestock category using the proportion based on Sailley et al., 2021 => th_DMI_t_livcat_y

  th_feed_ration <- object@herd |>
    dplyr::select(object@traceability$id_cols,FADN_code_letter,species,livestock_unit_coef,
                  tidyselect::matches(c("Qobs|Qeq"))) |>
    # convert Qobs in livestock units
    # number of livestock units
    ## either for observed number of animals or number of animals at the pseudofarm equilibrium
    (\(x) {
      dplyr::mutate(x,
                    Qobs_LU = if ("Qeq" %in% names(x)) Qeq * livestock_unit_coef else Qobs * livestock_unit_coef
      )
    })() |>
    # add theoretical total dry matter per livestock category
    dplyr::left_join(
      AROPAJ_ref_ration,
      by = c('FADN_code_letter')) |>
    dplyr::mutate(
      th_total_DMI_t_livcat_y = Qobs_LU * AROPAJ_DMI_t_LU_y # in tDM
    ) |>
    # add theoretical feed allocation
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,Sailley_livestock),
      by = c('FADN_code_letter')) |>
    dplyr::left_join(
      Sailley_ref_feed_allocation |>
        dplyr::select(Sailley_livestock,Sailley_feed,feed_type,ref_p100_feed),
      by = c('Sailley_livestock'),
      relationship = "many-to-many") |>
    # estimate theoretical ration per livestock category per crop
    dplyr::mutate(
      th_DMI_t_livcat_y = th_total_DMI_t_livcat_y * ref_p100_feed#, # in tDM
      #th_DMI_t_LU_y = th_DMI_t_livcat_y / Qobs_LU # in tDM/LU
    )

  # Reference feed value ------------------------------------------------------

  feed_value_ref <- f_feed_value(object)

  # Estimate theoretical ration value ------------------------------------------------------
  ## Match feed stuff values by year, country, and feed stuff
  ## Steps:
  ## 1. Estimate total feed value per livestock category by multiplying EUROSTAT ref by theoretical ton of DM per crop per livestock category  => th_crop_value
  ## 2. Sum all crops from all livestock category by feed type (i.e., concentrates or rough) for each farm => th_feed_type_value
  ### we estimate total value per feed type as one purchases value is available in the FADN for each feed type
  ## 3. Estimate the share of the feed type value for each crop in each livestock category  => th_crop_value_p100_feed_type

  # Match feed stuff values
  th_feed_value <- th_feed_ration |>
    # add country
    dplyr::left_join(
      object@farm |>
        dplyr::select(all_of(object@traceability$id_cols),
                      Country_ISO_3166_1_A3 ,YEAR),
      by = object@traceability$id_cols#[!object@traceability$id_cols %in% c(names(match_counts_country), names(match_counts_year))]
    ) |>
    # add feed stuff value
    dplyr::left_join(
      feed_value_ref$feed_price_country_year |>
        # average by Sailley crop, country and year
        dplyr::summarise(euros_t_cy = mean(euros_t, na.rm = T), # in euros/ton
                         .by = c(Sailley_feed, Country_ISO_3166_1_A3 ,YEAR)),
      by = c('Sailley_feed', 'YEAR', 'Country_ISO_3166_1_A3' )
    ) |>
    ## for missing values, add year averages
    dplyr::left_join(feed_value_ref$feed_price_year |>
                       # average by Sailley crop and year
                       dplyr::summarise(euros_t_y = mean(euros_t,na.rm = T), # in euros/ton
                                        .by = c(Sailley_feed,YEAR)),
                     by = c('Sailley_feed', 'YEAR')
    ) |>
    dplyr::mutate(euros_t = ifelse(is.na(euros_t_cy), euros_t_y, euros_t_cy)) |>
    dplyr::select(-c(euros_t_cy,euros_t_y)) |>
    # change grazing species
    dplyr::mutate(
      species = case_when(
        species %in% c("cattle","sheep","goats","horse") ~ "grazing",
        .default = species
      )
    )|>
    # Estimate theoretical feed value per crop (in euros)
    dplyr::mutate(th_crop_value = th_DMI_t_livcat_y * euros_t) |>
    # Estimate total theoretical value per feed type (in euros)
    dplyr::mutate(th_feed_type_value = sum(th_crop_value,na.rm = T),
                  .by = c(object@traceability$id_cols,feed_type,species)) |>
    # Calculate share of feed value
    dplyr::mutate(
      th_crop_value_p100_feed_type = th_crop_value / th_feed_type_value
    )


  return(list(AROPAJ_ref_ration = AROPAJ_ref_ration,
              Sailley_ref_feed_allocation = Sailley_ref_feed_allocation,
              feed_value = feed_value_ref,
              th_feed_ration = th_feed_ration,
              th_feed_value = th_feed_value))

}
