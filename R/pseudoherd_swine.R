#' Estimate pseudo-herd for swine based on rearing stage balancing
#'
#' @description
#' f_pseudoherd_swine estimates a "pseudo-herd" of swine animals for the meat
#' production workshop by reconciling the number of animals observed at each
#' rearing stage (piglets, fattening pigs, breeding sows) with the flows
#' implied by rearing parameters (turnover rates and litter/offspring size).
#' This allows the number of animals recorded in one rearing stage to be
#' consistently propagated to the others, producing a balanced herd at
#' equilibrium.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Model farm rearing process:**
#' f_herd_rearing_param_swine is called to derive rearing parameters
#' (turnover rates rt_LPIGLET, rt_LPIGOTH, rt_LPIGFAT, rt_LSOWBRE, and
#' offspring_LSOWBRE) and observed animal counts (LPIGLET_Qobs,
#' LPIGOTH_Qobs, LPIGFAT_Qobs, LSOWBRE_Qobs) for each farm. Unlike cattle,
#' swine has no mixed livestock categories to disaggregate.
#'
#' **2. Estimate pseudo-herd:**
#' - *2.1 Differentiate farm workshops*: all swine categories are treated as
#'   belonging to a single meat production workshop (no milk workshop, as
#'   for cattle).
#' - *2.2 Balance number of animals in each workshop*:
#'   - *Aggregation by rearing stage*: observed counts and turnover rates are
#'     aggregated into three rearing stages — juveniles (piglets), fattening
#'     (other + fattening pigs, weighted average turnover rate), and
#'     breeders (sows), together with the offspring rate per sow.
#'   - *Herd at equilibrium*: for each farm, the rearing stage with the
#'     largest implied herd size (Q_max) is identified by comparing observed
#'     counts to the counts that would be implied by the other stages via
#'     the turnover rates and offspring rate. The equilibrium quantities
#'     (Qeq_j, Qeq_f, Qeq_b) for juveniles, fattening and breeders are then
#'     derived consistently from the stage with the maximum implied herd
#'     size.
#'   - *Balanced number of animals*: the difference between equilibrium and
#'     observed quantities at each stage is distributed across the
#'     corresponding FADN livestock categories (LPIGLET, LPIGFAT, LPIGOTH,
#'     LSOWBRE), added back to the observed counts, and returned in long
#'     format with one row per farm × FADN_code_letter.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing 'object@traceability$id_cols' and all data
#'   required by f_herd_rearing_param_swine.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{rearing_param}{A tibble with one row per farm containing the
#'     traceability id columns and rearing parameters (turnover rates,
#'     time to first stage, and offspring rate) used in the balancing
#'     process.}
#'   \item{pseudoherd}{A tibble in long format with one row per farm ×
#'     FADN_code_letter (LPIGLET, LPIGFAT, LPIGOTH, LSOWBRE), containing the
#'     column Qeq_meat: the balanced (pseudo-herd) number of animals for the
#'     meat production workshop.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' pseudoherd_swine <- f_pseudoherd_swine(f)
#' head(pseudoherd_swine$pseudoherd)
#' head(pseudoherd_swine$rearing_param)
#' }
#'
#' @seealso f_herd_rearing_param_swine, f_pseudoherd_cattle,
#'   f_pseudoherd_poultry, f_pseudoherd_animals
#'
#' @concept practice-pseudoherd
#' @export
#' @import dplyr tidyr tidyselect


f_pseudoherd_swine <- function(object,
                               overwrite = FALSE
) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  id_cols = object@traceability$id_cols

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Differentiate farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_swine <- f_herd_rearing_param_swine(object)

  # View(herd_rearing_param_swine |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  # 2. On-farm herd activities ---------------------------------------------------------------------------------

  herd_activities = f_herd_activities(object) |>
    dplyr::filter(species == "swine")

  # 3. Estimate pseudoherd ---------------------------------------------------------------------------------

  ## 2.1. Differentiate farm workshops ----

  ## all swine is in meat workshop
  herd_swine_meat <-  herd_activities |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs_meat) |>
    # pivot table
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_Qobs_meat",
      values_from = Qobs_meat,
      values_fill = 0
    )

  ## 2.2. Balance number of animals in each workshop ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_swine_meat_aggr <- herd_swine_meat |>
    # add rearing parameters
    dplyr::left_join(herd_rearing_param_swine,
                     by = id_cols) |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = LPIGLET_Qobs,
      rt_j = rt_LPIGLET,
      # fattening
      Qobs_f =  LPIGFAT_Qobs,
      rt_f = rt_LPIGFAT,
      # breeders
      Qobs_b = LSOWBRE_Qobs + LPIGOTH_Qobs,
      # LPIGOTH are probably male breeders
      offspring = offspring_LSOWBRE
    )

  # Remove NAs
  herd_swine_meat_aggr <- herd_swine_meat_aggr |>
    # add SYSO2
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), SYS02),
                     by = c(id_cols))


  tmp_avrg_rearing_param = h_average_practices(data = herd_swine_meat_aggr,
                                               target_vars = c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring"),
                                               primary_grp = c('YEAR', 'COUNTRY', 'NUTS2'),
                                               secondary_grp = c( 'COUNTRY'),
                                               weight_var = 'SYS02')|>
    dplyr::rename_with(~ paste0("avrg_", .x),
                       .cols = c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring"))


  ## add fallback averages
  herd_swine_meat_aggr <- herd_swine_meat_aggr |>
    dplyr::left_join(tmp_avrg_rearing_param,
                     by = c('YEAR', 'COUNTRY', 'NUTS2')) |>
    # replace NAs with fallback
    dplyr::mutate(
      Qobs_j = ifelse(is.na(Qobs_j), avrg_Qobs_j, Qobs_j),
      rt_j = ifelse(is.na(rt_j), avrg_rt_j, rt_j),
      Qobs_f = ifelse(is.na(Qobs_f), avrg_Qobs_f, Qobs_f),
      rt_f = ifelse(is.na(rt_f), avrg_rt_f, rt_f),
      Qobs_b = ifelse(is.na(Qobs_b), avrg_Qobs_b, Qobs_b),
      offspring = ifelse(is.na(offspring), avrg_offspring, offspring)
    ) |>
    # remove fallback variables
    dplyr::select(-dplyr::matches("^avrg_"))


  # View(herd_swine_meat_aggr |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----

  herd_swine_meat_eq <- herd_swine_meat_aggr |>
    # select the livestock category from which the pseudo-herd at equilibrium will be estimated as the pseudoherd with the highest numbers of animals
    dplyr::mutate(
      Q_max = case_when(
        ## Qobs_j >= ^Q_j|f estimated from fattening & >= ^Q_j|b estimated from breeders
        Qobs_j >= ifelse(Qobs_f>0,(rt_j*(Qobs_f/rt_f)),0) & Qobs_j >= ifelse(Qobs_b>0,(rt_j*Qobs_b*offspring),0) ~ "juveniles",
        ## Qobs_f >= ^Q_f|j estimated from juveniles & >= ^Q_f|b estimated from breeders
        Qobs_f >= ifelse(Qobs_j>0,(rt_f*(Qobs_j/rt_j)),0) & Qobs_f >= ifelse(Qobs_b>0,(rt_f*Qobs_b*offspring),0) ~ "fattening",
        ## Qobs_b >= ^Q_b estimated from juveniles & >= ^Q_b estimated from fattening
        Qobs_b >= ifelse(Qobs_j>0,(Qobs_j/rt_j/offspring),0) & Qobs_b >= ifelse(Qobs_f>0,(Qobs_f/rt_f/offspring),0) ~ "breeders"
      )
    ) |>
    dplyr::mutate(
      Qeq_j_meat = case_when(
        Q_max == "juveniles" ~ Qobs_j,
        Q_max == "fattening" ~ (rt_j*(Qobs_f/rt_f)),
        Q_max == "breeders" ~ (rt_j*Qobs_b*offspring)
      ),
      Qeq_f_meat = case_when(
        Q_max == "juveniles" ~ (rt_f*(Qobs_j/rt_j)),
        Q_max == "fattening" ~ Qobs_f,
        Q_max == "breeders" ~ (rt_f*Qobs_b*offspring)
      ),
      Qeq_b_meat = case_when(
        Q_max == "juveniles" ~ (Qobs_j/rt_j/offspring),
        Q_max == "fattening" ~ (Qobs_f/rt_f/offspring),
        Q_max == "breeders" ~ Qobs_b
      )
    ) |>
    # check that Qeq >= Qobs
    dplyr::mutate(
      Qeq_j_meat = pmax(Qeq_j_meat, Qobs_j, na.rm = TRUE),
      Qeq_f_meat = pmax(Qeq_f_meat, Qobs_f, na.rm = TRUE),
      Qeq_b_meat = pmax(Qeq_b_meat, Qobs_b, na.rm = TRUE)
    )


  # View(herd_swine_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_swine_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  # We allocate animal equilibrium number across livestock categories
  # according to the share of animals in each category observed at the NUTS2 level
  # underlying hypothesis: NUTS2 animal numbers are at equilibrium

  # define which categories belong to which rearing stage
  cat_juveniles <- c("LPIGLET")
  cat_fattening <- c("LPIGFAT")
  cat_breeders  <- c("LSOWBRE", "LPIGOTH")

  # estimate shares
  share_Qobs <- herd_activities |>
    # add NUTS2 and SYS02
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
                     by = id_cols) |>
    # sum all animals per category in each rearing stage at the NUTS2 level
    dplyr::summarise(
      Qobs_meat_NUTS2_cat = sum(Qobs_meat, na.rm = T),
      .by = c(COUNTRY, NUTS2, FADN_code_letter)
    ) |>
    # sum all animals per category in each rearing stage at the country level
    dplyr::mutate(
      Qobs_meat_COUNTRY_cat = sum(Qobs_meat_NUTS2_cat, na.rm = T),
      .by = c(COUNTRY, FADN_code_letter)
    ) |>
    # add rearing stage
    dplyr::mutate(
      stage = dplyr::case_when(
        FADN_code_letter %in% cat_juveniles ~ "j",
        FADN_code_letter %in% cat_fattening  ~ "f",
        FADN_code_letter %in% cat_breeders   ~ "b",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(stage)) |>
    # sum all animals at the NUTS2 level (per stage)
    dplyr::mutate(
      Qobs_meat_NUTS2_stage = sum(Qobs_meat_NUTS2_cat, na.rm = T),
      .by = c(NUTS2, stage)
    ) |>
    # sum all animals at the country level (per stage)
    dplyr::mutate(
      Qobs_meat_COUNTRY_stage = sum(Qobs_meat_NUTS2_cat, na.rm = T),
      .by = c(COUNTRY, stage)
    ) |>
    # estimate share of animal per category at the NUTS2 level
    dplyr::mutate(
      share_NUTS2 = Qobs_meat_NUTS2_cat / Qobs_meat_NUTS2_stage,
      share_COUNTRY = Qobs_meat_COUNTRY_cat / Qobs_meat_COUNTRY_stage
    )

  # allocate animals
  pseudoherd_swine_meat <- herd_swine_meat_eq |>
    # add shares
    #dplyr::left_join(share_Qobs |>
    #                   dplyr::select(FADN_code_letter, NUTS2, share_COUNTRY) |>
    #                   tidyr::pivot_wider(names_from = FADN_code_letter, values_from = share_COUNTRY,
    #                                      names_prefix = "share_"),
    #                 by = c('NUTS2')) |>
    dplyr::left_join(share_Qobs |>
                       dplyr::select(FADN_code_letter, COUNTRY, share_COUNTRY) |>
                       dplyr::distinct() |>
                       tidyr::pivot_wider(names_from = FADN_code_letter, values_from = share_COUNTRY,
                                          names_prefix = "share_"),
                     by = c('COUNTRY')) |>
    # balance number of animals for the meat workshop
    dplyr::mutate(

      # --- juveniles (single category, no weighting needed) ---
      LPIGLET_Qeq_meat = Qeq_j_meat,

      # --- fattening: weight residual by NUTS2-level category share ---
      LPIGFAT_Qeq_meat = Qeq_f_meat,

      # --- breeders (single category, no weighting needed) ---
      LSOWBRE_Qeq_meat =  LSOWBRE_Qobs_meat +
        (Qeq_b_meat - Qobs_b) * share_LSOWBRE,
      LPIGOTH_Qeq_meat = LPIGOTH_Qobs_meat +
        (Qeq_b_meat - Qobs_b) * share_LPIGOTH
    ) |>
    # select columns
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qeq_meat")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = dplyr::matches("Qeq_meat"),
      names_to = "FADN_code_letter",
      values_to = "Qeq_meat"
    ) |>
    dplyr::mutate(
      FADN_code_letter = gsub("_Qeq_meat","",FADN_code_letter)
    ) |>
    # round values
    dplyr::mutate(
      Qeq_meat = round(Qeq_meat, 2)
    )

  # Output ----

  # all possible combination of livestock category in each farm
  full_grid <- herd_activities |>
    dplyr::distinct(dplyr::across(dplyr::all_of(id_cols))) |>
    tidyr::expand_grid(FADN_code_letter = unique(herd_activities$FADN_code_letter))

  pseudoherd_swine <- list(
    # rearing parameters
    rearing_param = herd_rearing_param_swine |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
    # pseudo herd
    pseudoherd = full_grid |>
      dplyr::left_join(herd_activities, by = c(id_cols, 'FADN_code_letter')) |>
      dplyr::left_join(pseudoherd_swine_meat, by = c(id_cols, 'FADN_code_letter')) |>
      dplyr::mutate(species = "swine")
  )


  return(pseudoherd_swine)

}

