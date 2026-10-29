#' Compute herding practices indicators for dairy cattle from a FADN2Footprint object
#'
#' @description
#' f_herding_practices computes a set of herd- and feed-related practice indicators
#' (yields, livestock density, shares of grassland and protein crops, feed autonomy,
#' shares of concentrates and soybean meal, maize per cow, main forage area, etc.)
#' for dairy cattle from a prepared FADN2Footprint object. The function attempts to
#' return a cached value if the object already contains computed herding practices
#' at object@practices$herding$general.
#'
#' @details
#' The function:
#'
#' - validates that the supplied object inherits from the S4 class "FADN2Footprint";
#'
#' - if a cached result is present at `object@practices$herding$general` it is returned;
#'
#' - otherwise it calls internal helper functions (notably f_herd_feed) and uses
#'   object slots (e.g. @herd, @farm, @output, @traceability) and additional data
#'   prepared in the object to compute per-farm indicators for dairy cows
#'   (FADN code letter "LCOWDAIR");
#'
#' - the computation aggregates feed origin and type into areas and dry-matter
#'   quantities, computes derived ratios while guarding against division-by-zero
#'   (returns NA where denominators are zero), and returns one row per traceable
#'   farm identifier.
#'
#' The indicators produced include (non-exhaustive):
#'
#' - yield_l_pha_ps: milk yield (L per ha pseudofarm)
#'
#' - yield_l_pha: milk yield (L per ha farm / produced feed area)
#'
#' - yield_l_panim: milk yield (L per dairy cow)
#'
#' - nb_cow_pha_ps, nb_cow_pha, nb_cow_pMFA: dairy cows per ha (pseudofarm, farm, MFA)
#'
#' - MFA_pcow, MFA_pha_ps, MFA_pha: main forage area per cow / per ha
#'
#' - ha_perm_grassland_pha_ps, ha_perm_grassland_pha: permanent grassland share
#'
#' - ha_temp_grassland_pha_ps, ha_temp_grassland_pha: temporary grassland share
#'
#' - protein_crop_ha_pha_ps, protein_crop_ha_pha: legumes / protein crops share
#'
#' - feed_autonomy: produced feed / total feed (t DM)
#'
#' - t_DM_fod_maize: t DM of forage maize per cow
#'
#' - share_soybean: share of purchased soybean meal (t / total feed t)
#'
#' - share_concent: share of concentrates (t / total feed t)
#'
#' Note: this function expects that the input object contains the required
#' intermediate tables and helper functions (for example f_herd_feed), as well as
#' the traceability id columns indicated in `object@traceability$id_cols`.
#'
#' @param object A S4 object of class "FADN2Footprint". The object must contain
#'   slots used by the function such as @herd, @farm, @output, @traceability and
#'   may contain cached results at `object@practices$herding$general`.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble / data.frame with one row per traceable farm identifier (as
#'   defined by `object@traceability$id_cols)`. Columns include the computed herd
#'   practice indicators listed in Details along with the traceability columns and
#'   some original production quantities used in the calculations (e.g. prod_t_milk).
#'
#' @examples
#' \dontrun{
#' # Assume `f` is a pre-built FADN2Footprint object prepared by the package workflow
#' # and that helper functions (f_herd_feed, etc.) are available.
#' res <- f_herding_practices(f)
#' head(res)
#' }
#'
#' @seealso f_herd_feed
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr filter summarise mutate select rename left_join inner_join distinct pull %>%
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom purrr reduce
#' @aliases f_herding_practices

f_herding_practices <- function(object,
                                overwrite = FALSE
                                ){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$herding$general)&& !overwrite) {
    message("Using cached values stored in object@practices$herding$general.")
    return(object@practices$herding$general)  # use cached value
  }

  id_cols = object@traceability$id_cols

  # Feed ----

  herd_feed <- f_herd_feed(object, overwrite = overwrite)

  # Yield (L of milk / ha pseudofarm)
  # yield_l_pha_ps
  # Yield (L of milk / ha farm)
  # yield_l_pha
  # Yield (L of milk / dairy cow)
  # yield_l_panim


  # Livestock density (dairy cow / ha pseudofarm)
  # nb_cow_pha_ps
  # Livestock density (dairy cow / ha farm)
  # nb_cow_pha
  # Livestock density (dairy cow / ha MFA)
  # nb_cow_pMFA

  # Main Forage Area (MFA / dairy cow)
  # MFA_pcow
  # Share of Main Forage Area (ha MFA / ha pseudofarm)
  # MFA_pha_ps
  # Share of Main Forage Area (ha MFA / ha farm)
  # MFA_pha

  # Share of permanent grassland (ha permanent grassland / ha pseudofarm)
  # ha_perm_grassland_pha_ps
  # Share of permanent grassland (ha permanent grassland / ha farm)
  # ha_perm_grassland_pha

  # Share of temporary grassland (ha temporary grassland / ha pseudofarm)
  # ha_temp_grassland_pha_ps
  # Share of temporary grassland (ha temporary grassland / ha farm)
  # ha_temp_grassland_pha

  # Share of legumes (ha legumes / ha pseudofarm)
  # protein_crop_ha_pha_ps
  # Share of legumes (ha legumes / ha farm)
  # protein_crop_ha_pha

  # Feed autonomy (t produced feed / t total feed)
  # feed_autonomy

  # Forage maize (t of forage maize produced / dairy cow)
  # t_DM_fod_maize
  # Share of purchased soybean meal (t soybean meal / t total feed)
  # share_soybean
  # Share of concentrate (t concentrate / t total feed)
  # share_concent



  # areas for feed ----
  tmp_areas <- herd_feed$feed_intake$detail |>
    # sum up areas
    dplyr::summarise(
      area_ha = sum(DM_t_livcat / yield,na.rm = T),
      .by = c(dplyr::all_of(id_cols), feed_origin, FADN_code_letter)
      #.by = c(dplyr::all_of(id_cols), feed_origin, species)
      #.by = c(dplyr::all_of(id_cols), feed_origin)
    ) |>
    tidyr::pivot_wider(id_cols = c(dplyr::all_of(id_cols), FADN_code_letter),
    #tidyr::pivot_wider(id_cols = c(dplyr::all_of(id_cols), species),
    #tidyr::pivot_wider(id_cols = c(dplyr::all_of(id_cols)),
                       values_from = area_ha,
                       names_from = feed_origin,
                       names_prefix = "area_")

  # Permanent grassland ----
  tmp_perm_grass <- herd_feed$feed_intake$detail |>
    # filter permanent grasslands
    dplyr::filter(FADN_code_feed %in% c("CGRSXRG", "CRG")) |>
    # sum up areas
    dplyr::summarise(
      area_ha_perm_grass =  sum(DM_t_livcat / yield, na.rm = T),
      .by = c(dplyr::all_of(id_cols), FADN_code_letter))
      #.by = c(dplyr::all_of(id_cols), species))
      #.by = c(dplyr::all_of(id_cols)))

  # temporary grassland ----
  tmp_temp_grass <- herd_feed$feed_intake$detail |>
    # filter temporary grasslands
    dplyr::filter(FADN_code_feed == "CGRSTMP") |>
    # sum up areas
    dplyr::summarise(
      area_ha_temp_grass =  sum(DM_t_livcat / yield, na.rm = T),
      .by = c(dplyr::all_of(id_cols), FADN_code_letter))
      #.by = c(dplyr::all_of(id_cols), species))
      #.by = c(dplyr::all_of(id_cols))

  # dairy cow population ----
  tmp_dairy_cow_pop <- object@herd |>
    # filter dairy cows
    dplyr::filter(FADN_code_letter == "LCOWDAIR" & Qobs >0) |>
    # sum up livestock units
    dplyr::summarise(
      cow_pop = sum(Qobs*livestock_unit_coef,na.rm = T),
      .by = c(dplyr::all_of(id_cols), FADN_code_letter))
      #.by = c(dplyr::all_of(id_cols), species))
      #.by = c(dplyr::all_of(id_cols)))

  # population ----
  tmp_pop <- object@herd |>
    # sum up livestock units
    dplyr::summarise(
      Qobs_LU = sum(Qobs*livestock_unit_coef,na.rm = T),
      .by = c(dplyr::all_of(id_cols), FADN_code_letter))
      #.by = c(dplyr::all_of(id_cols), species))
  #.by = c(dplyr::all_of(id_cols)))

  # main forage area ----
  # surface fourragère see instruction de collecte de 311 - 371
  tmp_MFA <- herd_feed$feed_intake$detail |>
    # filter forage
    dplyr::filter(feed_type == "feed_rough" & feed_origin != "feed_purchased") |>
    # estimate main forage area for dairy cows
    dplyr::summarise(
      MFA_ha = sum(DM_t_livcat / yield,na.rm = T),
      #.by = c(dplyr::all_of(id_cols))
      .by = c(dplyr::all_of(id_cols), FADN_code_letter)
    )

  # share of protein crops ----

  tmp_protein_crops_code <- data_extra$crops |>
    dplyr::filter(species == "legumes") |>
    dplyr::pull(FADN_code_letter)

  tmp_protein_crops <- herd_feed$feed_intake$detail |>
    # filter protein crops
    dplyr::filter(FADN_code_feed %in% tmp_protein_crops_code & feed_origin != "feed_purchased") |>
    # estimate main forage area for dairy cows
    dplyr::summarise(
      protein_crops_ha = sum(DM_t_livcat / yield, na.rm = T),
      .by = c(dplyr::all_of(id_cols),  FADN_code_letter))
      #.by = c(dplyr::all_of(id_cols),  species))
      #.by = c(dplyr::all_of(id_cols)))

  # t of DM ----

  tmp_sum_t_DM_feed_origin <- herd_feed$feed_intake$detail |>
    # sum up t of feed
    dplyr::summarise(
      feed_t = sum(DM_t_livcat, na.rm = T),
      .by = c(dplyr::all_of(id_cols), feed_origin, FADN_code_letter)) |>
      #.by = c(dplyr::all_of(id_cols), feed_origin, species)) |>
      #.by = c(dplyr::all_of(id_cols), feed_origin)) |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(id_cols), FADN_code_letter),
      #id_cols = c(dplyr::all_of(id_cols), species),
      #id_cols = c(dplyr::all_of(id_cols)),
      values_from = feed_t,
      names_from = feed_origin,
      names_prefix = "t_DM_",values_fill = 0) |>
    dplyr::mutate(
      total_t_DM = t_DM_feed_produced + t_DM_feed_purchased
    )

  # sum t of maize ----

  tmp_t_maize <- herd_feed$feed_intake$detail |>
    # filter maize produced
    dplyr::filter(FADN_code_feed == "CFODMZ" & feed_origin == "feed_produced") |>
    # sum up t of maize
    dplyr::summarise(t_DM_fod_maize = sum(DM_t_livcat, na.rm = T),
                     .by = c(dplyr::all_of(id_cols), FADN_code_letter))
                     #.by = c(dplyr::all_of(id_cols)))

  # sum t of soybean meal ----

  tmp_t_soy <- herd_feed$feed_intake$detail |>
    # filter soybean meal purchased
    dplyr::filter(Sailley_feed == "Dont_tourteau_de_soja" & feed_origin == "feed_purchased") |>
    # sum up t of soybean meal
    dplyr::summarise(t_DM_soy = sum(DM_t_livcat, na.rm = T),
                     .by = c(dplyr::all_of(id_cols), FADN_code_letter))
                     #.by = c(dplyr::all_of(id_cols), species))
                     #.by = c(dplyr::all_of(id_cols)))

  # share of purchased concentrates t ----

  tmp_sum_t_DM_feed_type_purchased <- herd_feed$feed_intake$detail |>
    # filter purchased feed
    dplyr::filter(feed_origin == "feed_purchased") |>
    # sum up t of feed
    dplyr::summarise(
      feed_t = sum(DM_t_livcat, na.rm = T),
      .by = c(dplyr::all_of(id_cols), feed_type, FADN_code_letter)) |>
      #.by = c(dplyr::all_of(id_cols), feed_type, species)) |>
      #.by = c(dplyr::all_of(id_cols), feed_type)) |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(id_cols), FADN_code_letter),
      #id_cols = c(dplyr::all_of(id_cols), species),
      #id_cols = c(dplyr::all_of(id_cols)),
      values_from = feed_t,
      names_from = feed_type,
      names_prefix = "t_DM_purchased_", values_fill = 0)

  # share of all concentrates t ----

  tmp_sum_t_DM_feed_type <- herd_feed$feed_intake$detail |>
    # sum up t of feed
    dplyr::summarise(
      feed_t = sum(DM_t_livcat, na.rm = T),
      .by = c(dplyr::all_of(id_cols), feed_type, FADN_code_letter)) |>
      #.by = c(dplyr::all_of(id_cols), feed_type, species)) |>
      #.by = c(dplyr::all_of(id_cols), feed_type)) |>
    tidyr::pivot_wider(
      id_cols = c(dplyr::all_of(id_cols), FADN_code_letter),
      #id_cols = c(dplyr::all_of(id_cols), species),
      #id_cols = c(dplyr::all_of(id_cols)),
      values_from = feed_t,
      names_from = feed_type,
      names_prefix = "t_DM_", values_fill = 0)

  # Compile herding practices ----
  # Base "besides_feed" rows: distinct id rows from feed_det
  base_rows <- herd_feed$feed_intake$detail |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter) |>
    #dplyr::select(dplyr::all_of(id_cols), species) |>
    #dplyr::select(dplyr::all_of(id_cols)) |>
    dplyr::distinct() |>
    # add milk production data
    dplyr::left_join(
      object@output$other_herd_products |>
        dplyr::filter(output == "milk" & species == "cattle") |>
        dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, prod_t) |>
        #dplyr::select(dplyr::all_of(id_cols), prod_t) |>
        dplyr::rename(prod_t_milk = prod_t),
      by = c(id_cols, 'FADN_code_letter')) |>
      #by = c(id_cols)) |>
    # add farm characteristic data
    dplyr::left_join(
      object@farm |>
        dplyr::select(dplyr::all_of(id_cols), TF14, SYS02),
      by = id_cols)

  # collect intermediate tables and left_join them in one go
  join_list <- list(
    tmp_areas,
    tmp_perm_grass,
    tmp_temp_grass,
    tmp_dairy_cow_pop,
    tmp_pop,
    tmp_MFA,
    tmp_protein_crops,
    tmp_sum_t_DM_feed_origin,
    tmp_t_maize,
    tmp_t_soy,
    tmp_sum_t_DM_feed_type_purchased,
    tmp_sum_t_DM_feed_type
  )

  # Use reduce to left_join all pieces (if purrr not available, use a loop - see alternative below)
  #combined <- reduce(join_list, ~ dplyr::left_join(.x, .y, by = id_cols))

  combined <- base_rows
  for (i in seq_along(join_list)) {
    combined <- dplyr::left_join(combined,
                          join_list[[i]],
                          by = c(id_cols, 'FADN_code_letter'))
                          #by = (id_cols))
  }


  # Replace NAs for columns where NA means 0
  combined <- combined %>%
    replace_na(list(
      area_feed_produced = 0, area_feed_purchased = 0,
      area_ha_perm_grass = 0,
      area_ha_temp_grass = 0,
      cow_pop = 0,
      Qobs_LU = 0,
      MFA_ha = 0,
      protein_crops_ha = 0,
      t_DM_feed_produced = 0, t_DM_feed_purchased = 0,
      t_DM_fod_maize = 0,
      t_DM_soy = 0,
      t_DM_purchased_feed_rough = 0, t_DM_purchased_feed_concent = 0,
      t_DM_feed_rough = 0, t_DM_feed_concent = 0,
      total_t_DM = 0
    ))

  tmp_practice_data <- combined %>%
    # Compute all derived indicators (use coalesce to avoid div by NA)
    mutate(
      pseudofarm_area = area_feed_produced + area_feed_purchased,

      # Yield (L of milk / ha pseudofarm)
      # yield_l_pha_ps
      yield_l_pha_ps = prod_t_milk / pseudofarm_area,
      # Yield (L of milk / ha farm)
      # yield_l_pha
      yield_l_pha = ifelse(area_feed_produced == 0, NA_real_, prod_t_milk / area_feed_produced),
      # Yield (L of milk / dairy cow)
      # yield_l_panim
      yield_l_milk_panim = ifelse(FADN_code_letter == "LCOWDAIR" & Qobs_LU >0, prod_t_milk / Qobs_LU, NA_real_),

      # Livestock density (dairy cow / ha pseudofarm)
      # nb_cow_pha_ps
      nb_LU_pha_ps = ifelse(pseudofarm_area == 0, NA_real_, Qobs_LU / pseudofarm_area),
      # Livestock density (dairy cow / ha farm)
      # nb_cow_pha
      nb_LU_pha = ifelse(area_feed_produced == 0, NA_real_, Qobs_LU / area_feed_produced),
      # Livestock density (dairy cow / ha MFA)
      # nb_cow_pMFA
      nb_LU_pMFA = ifelse(MFA_ha == 0, NA_real_, Qobs_LU / MFA_ha),

      # Main Forage Area (MFA / dairy cow)
      # MFA_pLU
      MFA_pLU = ifelse(Qobs_LU == 0, NA_real_, MFA_ha / Qobs_LU),
      # Share of Main Forage Area (ha MFA / ha pseudofarm)
      # MFA_pha_ps
      MFA_pha_ps = ifelse(pseudofarm_area == 0, NA_real_, MFA_ha / pseudofarm_area),
      # Share of Main Forage Area (ha MFA / ha farm)
      # MFA_pha
      MFA_pha = ifelse(area_feed_produced == 0, NA_real_, MFA_ha / area_feed_produced),

      # Share of permanent grassland (ha permanent grassland / ha pseudofarm)
      # ha_perm_grassland_pha_ps
      ha_perm_grassland_pha_ps = ifelse(pseudofarm_area == 0, NA_real_, area_ha_perm_grass / pseudofarm_area),
      # Share of permanent grassland (ha permanent grassland / ha farm)
      # ha_perm_grassland_pha
      ha_perm_grassland_pha = ifelse(area_feed_produced == 0, NA_real_, area_ha_perm_grass / area_feed_produced),

      # Share of temporary grassland (ha temporary grassland / ha pseudofarm)
      # ha_temp_grassland_pha_ps
      ha_temp_grassland_pha_ps = ifelse(pseudofarm_area == 0, NA_real_, area_ha_temp_grass / pseudofarm_area),
      # Share of temporary grassland (ha temporary grassland / ha farm)
      # ha_temp_grassland_pha
      ha_temp_grassland_pha = ifelse(area_feed_produced == 0, NA_real_, area_ha_temp_grass / area_feed_produced),

      # Share of legumes (ha legumes / ha pseudofarm)
      # protein_crop_ha_pha_ps
      protein_crop_ha_pha_ps = ifelse(pseudofarm_area == 0, NA_real_, protein_crops_ha / pseudofarm_area),
      # Share of legumes (ha legumes / ha farm)
      # protein_crop_ha_pha
      protein_crop_ha_pha = ifelse(area_feed_produced == 0, NA_real_, protein_crops_ha / area_feed_produced),

      # Feed autonomy (t produced feed / t total feed)
      # feed_autonomy
      feed_autonomy = ifelse(total_t_DM == 0, NA_real_, t_DM_feed_produced / total_t_DM),

      # Forage maize (t of forage maize produced / dairy cow)
      # t_DM_fod_maize
      # Share of purchased soybean meal (t soybean meal / t total feed)
      # share_soybean
      share_soybean = ifelse(total_t_DM == 0, NA_real_, t_DM_soy / total_t_DM),
      # Share of concentrate (t concentrate / t total feed)
      # share_concent
      share_concent = ifelse(total_t_DM == 0, NA_real_, t_DM_feed_concent / total_t_DM)
    )

  # one row per farm
  #length(unique(paste0(tmp_practice_data$ID,tmp_practice_data$YEAR))) == nrow(tmp_practice_data)



  # output ----

  return(tmp_practice_data)

  #rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

}
