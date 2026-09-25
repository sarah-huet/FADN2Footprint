#' Estimate off-farm animals of the cattle pseudoherd
#'
#' @description
#' f_pseudoherd_cattle models the on-farm cattle rearing process (milk and
#' meat activities) and estimates the pseudoherd, i.e. the equilibrium
#' number of animals (Qeq) needed at each rearing stage (juveniles,
#' fattening, breeders) to sustain observed on-farm production, for both the
#' milk and meat activities.
#'
#' @details
#' The function proceeds in the following steps:
#'
#' **1. Model farm rearing process:** rearing parameters (turnover rates,
#' time to first calving, offspring rates) are obtained from
#' f_herd_rearing_param_cattle.
#'
#' **2. On-farm herd activities:** observed animal numbers per livestock
#' category and activity (milk, meat) are obtained from f_herd_activities,
#' filtered on cattle.
#'
#' **3. Estimate pseudoherd:**
#' - **Milk**: the number of animals involved in the milk activity is taken
#'   directly from observed dairy cows (LCOWDAIR). The renewal categories
#'   (LHEIFBRE, LBOV1_2F breeders share, LBOV1) required to sustain the
#'   dairy herd are then derived from the dairy cow turnover rate and the
#'   respective category turnover rates.
#' - **Meat**: on-farm animals are aggregated into three rearing stages —
#'   juveniles (LBOV1), fattening (LBOV1_2M, LBOV2, LHEIFFAT, and the
#'   fattening share of LBOV1_2F) and breeders (LHEIFBRE, LCOWOTH, LBOV1_2F
#'   breeders share, plus LCOWDAIR as a source of offspring). Missing
#'   turnover rates and offspring rates are replaced by NUTS2/country
#'   average values (via h_average_practices). The rearing stage with the
#'   highest observed-to-equilibrium animal ratio (Q_max) is used as the
#'   reference to compute the equilibrium number of animals at each stage
#'   (Qeq_j_meat, Qeq_f_meat, Qeq_b_meat), accounting for direct sales of
#'   juveniles (LBOV1_SSN). These stage-level equilibrium numbers are then
#'   redistributed across individual livestock categories in proportion to
#'   their observed country-level share within each stage (share_Qobs),
#'   with mixed categories (LBOV1_2F) split between their fattening and
#'   breeding shares.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow. The function expects the object to provide:
#'   - object@traceability$id_cols (vector of id column names used for joins),
#'   - object@farm (for SYS02, NUTS2, COUNTRY),
#'   - helper function f_herd_rearing_param_cattle to supply rearing
#'     parameter tables,
#'   - helper function f_herd_activities to supply observed herd activities.
#' @param overwrite Logical (default FALSE). Currently unused within the
#'   function body but reserved for future caching behavior consistent with
#'   other package functions.
#'
#' @returns
#' A list with:
#' \describe{
#'   \item{rearing_param}{A tibble of rearing parameters (turnover rates,
#'     time to first calving, offspring rates) per farm.}
#'   \item{pseudoherd}{A tibble with one row per farm × livestock category,
#'     containing observed animal numbers for milk and meat activities
#'     (Qobs_milk, Qobs_meat) and estimated pseudoherd equilibrium numbers
#'     (Qeq_milk, Qeq_meat), together with species = "cattle".}
#' }
#'
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_pseudoherd_cattle(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr


f_pseudoherd_cattle <- function(object,
                                overwrite = FALSE
                                ) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  id_cols = object@traceability$id_cols

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Restrain herds to farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_cattle <- f_herd_rearing_param_cattle(object)

  # View(herd_rearing_param_cattle |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))

  # 2. On-farm herd activities ---------------------------------------------------------------------------------

  herd_activities = f_herd_activities(object) |>
    dplyr::filter(species == "cattle")

  # 3. Estimate pseudo herd ---------------------------------------------------------------------------------
  # we first estimate animals involved in the dairy herd with Equation @eq-restrain_herd_act. We consider all additional cattle as part of the meat activity


  ## MILK ----
  herd_cattle_milk <- herd_activities |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs_milk) |>
    # pivot table
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_Qobs_milk",
      values_from = Qobs_milk,
      values_fill = 0
    )


  pseudoherd_cattle_milk_wide <- herd_cattle_milk |>
    # add rearing parameters
    dplyr::left_join(herd_rearing_param_cattle,
                     by = id_cols) |>
    # estimate observed quantities and times for each production process step
    # first estimate how many animals are needed to renew the dairy cows
    dplyr::mutate(

      # breeders
      LCOWDAIR_Qeq_milk = ifelse(LCOWDAIR_Qobs_milk >0, LCOWDAIR_Qobs_milk, 0),

      ## we estimate other breeders category based on the dairy cows, to ensure dairy cow renewal
      LHEIFBRE_Qeq_milk = ifelse(LCOWDAIR_Qobs_milk > 0,
                                 rt_LHEIFBRE * (LCOWDAIR_Qeq_milk/rt_LCOWDAIR),
                                 0),
      LBOV1_2F_Qeq_milk = ifelse(LCOWDAIR_Qobs_milk >0,
                                 (rt_LBOV1_2F_breeders * (LCOWDAIR_Qobs_milk/rt_LCOWDAIR)),
                                 0),
      LBOV1_2F_breeders_Qeq_milk = LBOV1_2F_Qeq_milk,
      LBOV1_2F_fattening_Qeq_milk = 0,

      #breeders_milk_Qobs = (rt_LHEIFBRE * (LCOWDAIR_Qobs/rt_LCOWDAIR)) + (rt_LBOV1_2F_breeders * (LCOWDAIR_Qobs/rt_LCOWDAIR)),
      #LHEIFBRE_milk_Qobs = breeders_milk_Qobs * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),
      #LBOV1_2F_milk_Qobs = breeders_milk_Qobs * (LBOV1_2F_breeders_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),

      #breeders_milk_Qobs = (((rt_LBOV1_2F_breeders)*LBOV1_2F_breeders_Qobs + (rt_LHEIFBRE)*LHEIFBRE_Qobs)/ (LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs))* (LCOWDAIR_Qobs/rt_LCOWDAIR),
      #LHEIFBRE_milk_Qobs = breeders_milk_Qobs * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),
      #LBOV1_2F_milk_Qobs = breeders_milk_Qobs * (LBOV1_2F_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),

      #LHEIFBRE_milk_Qobs = LHEIFBRE_Qobs * ((LCOWDAIR_Qobs/rt_LCOWDAIR)/ ((LCOWDAIR_Qobs/rt_LCOWDAIR) + (LCOWOTH_Qobs/rt_LCOWOTH))),
      #LBOV1_2F_milk_Qobs = LBOV1_2F_breeders_Qobs * ((LCOWDAIR_Qobs/rt_LCOWDAIR)/ ((LCOWDAIR_Qobs/rt_LCOWDAIR) + (LCOWOTH_Qobs/rt_LCOWOTH))),

      # juveniles
      LBOV1_Qeq_milk = ifelse(LCOWDAIR_Qobs_milk >0,
                              rt_LBOV1 * (LBOV1_2F_breeders_Qeq_milk/rt_LBOV1_2F_breeders),
                              0)
      #LBOV1_milk_Qobs = LBOV1_Qobs * ((LBOV1_2F_milk_Qobs/rt_LBOV1_2F_breeders) / ((LBOV1_2F_Qobs/rt_LBOV1_2F) + (LBOV1_2M_Qobs/rt_LBOV1_2M)))

    )

  pseudoherd_cattle_milk <- pseudoherd_cattle_milk_wide |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),dplyr::matches("Qeq")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = dplyr::matches("Qeq"),
      names_to = "FADN_code_letter",
      values_to = "Qeq_milk"
    ) |>
    dplyr::mutate(
      FADN_code_letter = gsub("_Qeq_milk","",FADN_code_letter),
      Qeq_milk = round(Qeq_milk, 2)
    )

  ## MEAT ----
  # We deduct animals of the milk workshop from the on-farm animals to estimate on-farms animals involve in the meat workshop. Then we estimate number of animal at equilibrium for this workshop.

  ### 3.1. Restrain herds to farm workshops ----

  # Qmeat = Qobs - pmin(Qobs,Qeq_milk)

  herd_cattle_meat <- herd_activities |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs_meat) |>
    # pivot table
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_Qobs_meat",
      values_from = Qobs_meat,
      values_fill = 0
    )


  # View(herd_cattle_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))

  ### 3.2. Balance number of animals in each workshop ----
  # as the main livestock category in the meat workshop can be different depending on the farm orientation,
  # we estimate animal numebrs from the herd at equilibrium with highest numer of animals

  ### 3.2.1. Aggregate by rearing stage ----

  herd_cattle_meat_aggr <- herd_rearing_param_cattle |>
    # add Qobs meat
    dplyr::left_join(herd_cattle_meat,
                     by = id_cols) |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = LBOV1_Qobs_meat,
      rt_j = rt_LBOV1,
      # fattening
      Qobs_f = LBOV1_2M_Qobs_meat + LBOV2_Qobs_meat + LBOV1_2F_fattening_Qobs_meat + LHEIFFAT_Qobs_meat,
      ## LBOV1_2M & LBOV1_2F have at least 1 y.o., LBOV2 & LHEIFFAT have at least 2 y.o.
      rt_f = ((1+rt_LBOV1_2M)*LBOV1_2M_Qobs_meat +
                (2+rt_LBOV2)*LBOV2_Qobs_meat +
                (1+rt_LBOV1_2F_fattening)*LBOV1_2F_fattening_Qobs_meat +
                (2+rt_LHEIFFAT)*LHEIFFAT_Qobs_meat) / Qobs_f,
      # breeders
      Qobs_b = LBOV1_2F_breeders_Qobs_meat  + LHEIFBRE_Qobs_meat + LCOWOTH_Qobs_meat + LCOWDAIR_Qobs,
      offspring = offspring_b
    )

  # View(herd_cattle_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))
  # View(herd_cattle_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))


  # Remove NAs
  herd_cattle_meat_aggr <- herd_cattle_meat_aggr |>
    # add SYSO2
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), SYS02),
                     by = c(id_cols))


  tmp_avrg_rearing_param = h_average_practices(data = herd_cattle_meat_aggr,
                                               target_vars = c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring"),
                                               primary_grp = c('YEAR', 'COUNTRY', 'NUTS2'),
                                               secondary_grp = c( 'COUNTRY'),
                                               weight_var = 'SYS02')|>
    dplyr::rename_with(~ paste0("avrg_", .x),
                       .cols = c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring"))

  ## add fallback averages
  herd_cattle_meat_aggr <- herd_cattle_meat_aggr |>
    dplyr::left_join(tmp_avrg_rearing_param,
                     by = c('YEAR', 'COUNTRY', 'NUTS2')) |>
    # replace NAs with fallback
    dplyr::mutate(
      #Qobs_j = ifelse(is.na(Qobs_j), avrg_Qobs_j, Qobs_j),
      rt_j = ifelse(is.na(rt_j), avrg_rt_j, rt_j),
      #Qobs_f = ifelse(is.na(Qobs_f), avrg_Qobs_f, Qobs_f),
      rt_f = ifelse(is.na(rt_f), avrg_rt_f, rt_f),
      #Qobs_b = ifelse(is.na(Qobs_b), avrg_Qobs_b, Qobs_b),
      offspring = ifelse(is.na(offspring), avrg_offspring, offspring)
    ) |>
    # remove fallback variables
    dplyr::select(-dplyr::matches("^avrg_"))



  ### 3.2.2. Estimate herd at equilibrium ----

  # we remove calves sold for slaughter and dairy cows
  herd_cattle_meat_eq1 <- herd_cattle_meat_aggr |>
    # select the livestock category from which the pseudo-herd at equilibrium will be estimated as the pseudoherd with the highest numbers of animals
    dplyr::mutate(
      Q_max = case_when(
        ## Qobs_j >= ^Q_j estimated from fattening & >= ^Q_j estimated from breeders
        Qobs_j >= ifelse(Qobs_f>0, (rt_j*(Qobs_f/rt_f)) * (LBOV1_SN/LBOV1_SRN), 0) & Qobs_j >= ifelse(Qobs_b>0, (rt_j*Qobs_b*offspring), 0) ~ "juveniles",
        ## Qobs_f >= ^Q_f estimated from juveniles & >= ^Q_f estimated from breeders
        Qobs_f >= ifelse(Qobs_j>0, (rt_f*(Qobs_j/rt_j)) * (LBOV1_SRN/LBOV1_SN), 0) & Qobs_f >= ifelse(Qobs_b>0, (rt_f*Qobs_b*offspring) * (LBOV1_SRN/LBOV1_SN), 0) ~ "fattening",
        ## Qobs_b >= ^Q_b estimated from juveniles & >= ^Q_b estimated from fattening
        Qobs_b >= ifelse(Qobs_j>0, (Qobs_j/rt_j/offspring) - LCOWDAIR_Qobs, 0) & Qobs_b >= ifelse(Qobs_f>0, (Qobs_f/rt_f/offspring) * (LBOV1_SN/LBOV1_SRN), 0) ~ "breeders",
        .default = ifelse(LCOWDAIR_Qobs >0 | LCOWOTH_Qobs >0, "breeders", NA)
      )
    ) |>
    dplyr::mutate(
      Qeq_j_meat = dplyr::case_when(
        Q_max == "juveniles" ~ Qobs_j,
        Q_max == "fattening" ~ (rt_j*(Qobs_f/rt_f)) * (LBOV1_SN/LBOV1_SRN),
        Q_max == "breeders" ~ (rt_j*Qobs_b*offspring)
      ),
      Qeq_f_meat = dplyr::case_when(
        Q_max == "juveniles" ~ (rt_f*(Qobs_j/rt_j)) * (LBOV1_SRN/LBOV1_SN),
        Q_max == "fattening" ~ Qobs_f,
        Q_max == "breeders" ~ (rt_f*Qobs_b*offspring) * (LBOV1_SRN/LBOV1_SN),
      ),
      Qeq_b_meat = dplyr::case_when(
        Q_max == "juveniles" ~ (Qobs_j/rt_j/offspring) - LCOWDAIR_Qobs,
        Q_max == "fattening" ~ (Qobs_f/rt_f/offspring) * (LBOV1_SN/LBOV1_SRN),
        Q_max == "breeders" ~ Qobs_b
      )
    ) |>
    # check that Qeq >= Qobs
    dplyr::mutate(
      Qeq_j_meat = pmax(Qeq_j_meat, Qobs_j, na.rm = TRUE),
      Qeq_f_meat = pmax(Qeq_f_meat, Qobs_f, na.rm = TRUE),
      Qeq_b_meat = pmax(Qeq_b_meat, Qobs_b, na.rm = TRUE)
    )

  herd_cattle_meat_eq2 <- herd_cattle_meat_aggr |>
    # select the livestock category from which the pseudo-herd at equilibrium will be estimated as the pseudoherd with the highest numbers of animals
    dplyr::mutate(
      Q_max = case_when(
        ## Qobs_j >= ^Q_j estimated from fattening & >= ^Q_j estimated from breeders
        Qobs_j >= ifelse(Qobs_f>0, (rt_j*(Qobs_f/rt_f)), 0) & Qobs_j >= ifelse(Qobs_b>0, (rt_j*Qobs_b*offspring), 0) ~ "juveniles",
        ## Qobs_f >= ^Q_f estimated from juveniles & >= ^Q_f estimated from breeders
        Qobs_f >= ifelse(Qobs_j>0, (rt_f*(Qobs_j/rt_j)), 0) & Qobs_f >= ifelse(Qobs_b>0, (rt_f*Qobs_b*offspring), 0) ~ "fattening",
        ## Qobs_b >= ^Q_b estimated from juveniles & >= ^Q_b estimated from fattening
        Qobs_b >= ifelse(Qobs_j>0, (Qobs_j/rt_j/offspring), 0) & Qobs_b >= ifelse(Qobs_f>0, (Qobs_f/rt_f/offspring), 0) ~ "breeders",
        .default = ifelse(LCOWDAIR_Qobs >0 | LCOWOTH_Qobs >0, "breeders", NA)
      )
    ) |>
    dplyr::mutate(
      Qeq_j_meat = dplyr::case_when(
        Q_max == "juveniles" ~ Qobs_j,
        Q_max == "fattening" ~ (rt_j*(Qobs_f/rt_f)),
        Q_max == "breeders" ~ (rt_j*Qobs_b*offspring)
      ),
      Qeq_f_meat = dplyr::case_when(
        Q_max == "juveniles" ~ (rt_f*(Qobs_j/rt_j)),
        Q_max == "fattening" ~ Qobs_f,
        Q_max == "breeders" ~ (rt_f*Qobs_b*offspring),
      ),
      Qeq_b_meat = dplyr::case_when(
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

  herd_cattle_meat_eq = herd_cattle_meat_eq1

  # View(herd_cattle_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 3.2.3. Estimate balanced number of animals ----

  # We allocate animal equilibrium number across livestock categories
  # according to the share of animals in each category observed at the NUTS2 level
  # underlying hypothesis: NUTS2 animal numbers are at equilibrium

  # define which categories belong to which rearing stage
  cat_juveniles <- c("LBOV1")
  cat_fattening <- c("LBOV1_2M", "LBOV2", "LHEIFFAT", "LBOV1_2F_fattening")
  cat_breeders  <- c("LHEIFBRE", "LCOWOTH", "LBOV1_2F_breeders")

  # estimate shares
  share_Qobs <- herd_activities |>
    # add NUTS2 and SYS02
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
                     by = id_cols) |>
    # sum all animals per category in each rearing stage at the NUTS2 level
    #dplyr::summarise(
    #  Qobs_cat = sum(Qobs_meat, na.rm = T),
    #  .by = c(COUNTRY, NUTS2, FADN_code_letter)
    #) |>
    # sum all animals per category in each rearing stage at the country level
    dplyr::summarise(
      Qobs_cat = sum(Qobs * SYS02, na.rm = T),
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
    #dplyr::mutate(
    #  Qobs_meat_NUTS2_stage = sum(Qobs_cat, na.rm = T),
    #  .by = c(NUTS2, stage)
    #) |>
    # sum all animals at the country level (per stage)
    dplyr::mutate(
      Qobs_stage = sum(Qobs_cat, na.rm = T),
      .by = c(COUNTRY, stage)
    ) |>
    # estimate share of animal per category at the NUTS2 level
    dplyr::mutate(
      #share_NUTS2 = Qobs_cat / Qobs_meat_NUTS2_stage,
      share_COUNTRY = Qobs_cat / Qobs_stage
    )

  # allocate animals
  pseudoherd_cattle_meat <- herd_cattle_meat_eq |>
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
      LBOV1_Qeq_meat = (Qeq_j_meat - Qobs_j) + LBOV1_Qobs_meat,

      # --- fattening: weight residual by NUTS2-level category share ---
      LBOV1_2M_Qeq_meat = LBOV1_2M_Qobs_meat +
        (Qeq_f_meat - Qobs_f) * share_LBOV1_2M,

      LBOV2_Qeq_meat = LBOV2_Qobs_meat +
        (Qeq_f_meat - Qobs_f) * share_LBOV2,

      LHEIFFAT_Qeq_meat = LHEIFFAT_Qobs_meat +
        (Qeq_f_meat - Qobs_f) * share_LHEIFFAT,

      # fattening share of mixed category LBOV1_2F
      LBOV1_2F_fattening_Qeq_meat = LBOV1_2F_fattening_Qobs_meat +
        (Qeq_f_meat - Qobs_f) * share_LBOV1_2F_fattening,

      # --- breeders: weight residual by NUTS2-level category share ---
      LHEIFBRE_Qeq_meat = LHEIFBRE_Qobs_meat +
        (Qeq_b_meat - Qobs_b) * share_LHEIFBRE,

      LCOWOTH_Qeq_meat = LCOWOTH_Qobs_meat +
        (Qeq_b_meat - Qobs_b) * share_LCOWOTH,

      # breeding share of mixed category LBOV1_2F
      LBOV1_2F_breeders_Qeq_meat = LBOV1_2F_breeders_Qobs_meat +
        (Qeq_b_meat - Qobs_b) * share_LBOV1_2F_breeders,

      # --- mixed categories: sum of fattening share + breeding share ---
      LBOV1_2F_Qeq_meat = LBOV1_2F_Qobs_meat +
        (Qeq_f_meat - Qobs_f) * share_LBOV1_2F_fattening +
        (Qeq_b_meat - Qobs_b) * share_LBOV1_2F_breeders

    ) |>
    # select columns
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qeq_meat")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = dplyr::matches("_Qeq_meat"),
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

  pseudoherd_cattle <- list(
    # rearing parameters
    rearing_param = herd_rearing_param_cattle |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
    # pseudo herd
    pseudoherd = full_grid |>
      dplyr::left_join(herd_activities, by = c(id_cols, 'FADN_code_letter')) |>
      dplyr::left_join(pseudoherd_cattle_milk, by = c(id_cols, 'FADN_code_letter')) |>
      dplyr::left_join(pseudoherd_cattle_meat, by = c(id_cols, 'FADN_code_letter')) |>
      dplyr::mutate(species = "cattle")
    )


return(pseudoherd_cattle)

}

