#' Function to estimate off-farm animals of the cattle herd based on FADN data
#' `f_pseudoherd_cattle` Estimate off-farm animals of the cattle herd
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
#' f_pseudoherd_cattle(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_cattle <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Restrain herds to farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_cattle <- f_herd_rearing_param_cattle(object)

  # View(herd_rearing_param_cattle |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))

  # 2. Estimate pseudo herd ---------------------------------------------------------------------------------


  ## MILK ----

  ### 2.1. Restrain herds to farm workshops ----

  herd_cattle_milk <- herd_rearing_param_cattle |>
    # estimate observed quantities and times for each production process step
    # first estimate how many animals are needed to renew the dairy cows
    # TODO: check formula to estimate Qobs to renew dairy cows
    dplyr::mutate(

      # breeders
      LHEIFBRE_Qeq_milk = rt_LHEIFBRE * (LCOWDAIR_Qobs/rt_LCOWDAIR),
      LBOV1_2F_Qeq_milk = rt_LBOV1_2F_breeders * (LHEIFBRE_Qeq_milk/rt_LHEIFBRE),

      #breeders_milk_Qobs = (rt_LHEIFBRE * (LCOWDAIR_Qobs/rt_LCOWDAIR)) + (rt_LBOV1_2F_breeders * (LCOWDAIR_Qobs/rt_LCOWDAIR)),
      #LHEIFBRE_milk_Qobs = breeders_milk_Qobs * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),
      #LBOV1_2F_milk_Qobs = breeders_milk_Qobs * (LBOV1_2F_breeders_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),

      #breeders_milk_Qobs = (((rt_LBOV1_2F_breeders)*LBOV1_2F_breeders_Qobs + (rt_LHEIFBRE)*LHEIFBRE_Qobs)/ (LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs))* (LCOWDAIR_Qobs/rt_LCOWDAIR),
      #LHEIFBRE_milk_Qobs = breeders_milk_Qobs * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),
      #LBOV1_2F_milk_Qobs = breeders_milk_Qobs * (LBOV1_2F_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs)),

      #LHEIFBRE_milk_Qobs = LHEIFBRE_Qobs * ((LCOWDAIR_Qobs/rt_LCOWDAIR)/ ((LCOWDAIR_Qobs/rt_LCOWDAIR) + (LCOWOTH_Qobs/rt_LCOWOTH))),
      #LBOV1_2F_milk_Qobs = LBOV1_2F_breeders_Qobs * ((LCOWDAIR_Qobs/rt_LCOWDAIR)/ ((LCOWDAIR_Qobs/rt_LCOWDAIR) + (LCOWOTH_Qobs/rt_LCOWOTH))),

      # juveniles
      LBOV1_Qeq_milk = rt_LBOV1 * (LBOV1_2F_Qeq_milk/rt_LBOV1_2F_breeders)
      #LBOV1_milk_Qobs = LBOV1_Qobs * ((LBOV1_2F_milk_Qobs/rt_LBOV1_2F_breeders) / ((LBOV1_2F_Qobs/rt_LBOV1_2F) + (LBOV1_2M_Qobs/rt_LBOV1_2M)))

    )

  # View(herd_cattle_milk |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_milk |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))
  #plot(herd_cattle_milk$LHEIFBRE_Qobs,herd_cattle_milk$LHEIFBRE_Qeq_milk) + abline(1,1)
  #plot(herd_cattle_milk$LBOV1_2F_breeders_Qobs,herd_cattle_milk$LBOV1_2F_Qeq_milk) + abline(1,1)

  ### 2.2. Balance number of animals ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_cattle_milk_aggr <- herd_cattle_milk |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = pmin(LBOV1_Qeq_milk,LBOV1_Qobs),
      rt_j = rt_LBOV1,
      # fattening
      Qobs_f = 0,
      rt_f = ((1+rt_LBOV1_2M)*LBOV1_2M_Qobs +
                (2+rt_LBOV2)*LBOV2_Qobs +
                (1+rt_LBOV1_2F_fattening)*LBOV1_2F_fattening_Qobs +
                (2+rt_LHEIFFAT)*LHEIFFAT_Qobs) /
        (LBOV1_2M_Qobs + LBOV2_Qobs + LBOV1_2F_fattening_Qobs + LHEIFFAT_Qobs),
      # breeders
      Qobs_b = pmin(LBOV1_2F_Qeq_milk,LBOV1_2F_breeders_Qobs) + pmin(LHEIFBRE_Qeq_milk,LHEIFBRE_Qobs) + LCOWDAIR_Qobs,
      offspring = offspring_b
    )

  # View(herd_cattle_milk_aggr |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  ### Replace NAs with reference values for aggregate parameters ----

  herd_cattle_milk_clean <- herd_cattle_milk_aggr
  for (var in c("Qobs_j","rt_j","rt_f","Qobs_b","offspring")) {

    v <- rlang::sym(var)

    # Join and replace
    herd_cattle_milk_clean <- herd_cattle_milk_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median,threshold_down,threshold_up)|>
          dplyr::rename(median_all = median)
      ) |>
      dplyr::mutate(ref_val = case_when(
        !is.finite(median_NUTS2) ~ median_all,
        .default = median_NUTS2
      )) |>
      # replace
      dplyr::mutate(!!v := case_when(
        !is.finite(!!v) ~ ref_val,
        .default = !!v
      )) |>
      dplyr::mutate(!!v := case_when(
        (!!v < threshold_down) ~ threshold_down,
        (!!v > threshold_up) ~ threshold_up,
        .default = !!v
      )) |>
      dplyr::select(-c(median_NUTS2,median_all,ref_val,threshold_down,threshold_up))
  }

  # View(herd_cattle_milk_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----

  herd_cattle_dairy_meat_eq <- herd_cattle_milk_clean |>
    # select the livestock category from which the pseudo-herd at equilibrium will be estimated as the pseudoherd with the highest numbers of animals
    dplyr::mutate(
      Q_max = "dairy"
    ) |>
    dplyr::mutate(
      Qeq_j_dairy_meat = (rt_j*Qobs_b*offspring) - LBOV1_Qeq_milk,
      Qeq_f_dairy_meat = (rt_f*Qobs_b*offspring),
      Qeq_b_dairy_meat = 0
    )


  # View(herd_cattle_milk_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_milk_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  pseudoherd_cattle_milk <- herd_cattle_dairy_meat_eq |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("Qeq_milk|LCOWDAIR_Qobs")) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(object@traceability$id_cols),
      names_to = "FADN_code_letter",
      values_to = "Qeq_milk"
    ) |>
    dplyr::mutate(
      FADN_code_letter = gsub("_Qeq|_Qobs|_milk","",FADN_code_letter)
    )

  ## MEAT ----
  # We deduct animals of the milk workshop from the on-farm animals to estimate on-farms animals involve in the meat workshop. Then we estimate number of animal at equilibrium for this workshop.

  ### 2.1. Restrain herds to farm workshops ----

  herd_cattle_meat <- herd_cattle_dairy_meat_eq |> # herd_rearing_param_cattle |>
    # estimate additional animals from the milk workshop
    dplyr::mutate(

      # LHEIFBRE
      LHEIFBRE_Qobs_meat = pmax(LHEIFBRE_Qobs - LHEIFBRE_Qeq_milk, 0), # pmax to have zero if negative result
      # LBOV1_2F_breeders
      LBOV1_2F_Qobs_meat = pmax(LBOV1_2F_breeders_Qobs - LBOV1_2F_Qeq_milk, 0),

      # LBOV1
      LBOV1_Qobs_meat = pmax(LBOV1_Qobs - LBOV1_Qeq_milk, 0)

    )

  # View(herd_cattle_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))

  ### 2.2. Balance number of animals in each workshop ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_cattle_meat_aggr <- herd_cattle_meat |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = LBOV1_Qobs_meat,
      rt_j = rt_LBOV1,
      # fattening
      Qobs_f = LBOV1_2M_Qobs + LBOV2_Qobs + LBOV1_2F_fattening_Qobs + LHEIFFAT_Qobs,
      ## LBOV1_2M & LBOV1_2F have at least 1 y.o., LBOV2 & LHEIFFAT have at least 2 y.o.
      rt_f = ((1+rt_LBOV1_2M)*LBOV1_2M_Qobs +
                (2+rt_LBOV2)*LBOV2_Qobs +
                (1+rt_LBOV1_2F_fattening)*LBOV1_2F_fattening_Qobs +
                (2+rt_LHEIFFAT)*LHEIFFAT_Qobs) / Qobs_f,
      # breeders
      Qobs_b = LBOV1_2F_Qobs_meat + LHEIFBRE_Qobs_meat + LCOWOTH_Qobs,
      offspring = offspring_b
    )

  # View(herd_cattle_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))
  # View(herd_cattle_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### Replace NAs with reference values for aggregate parameters ----


  herd_cattle_meat_clean <- herd_cattle_meat_aggr
  for (var in c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring")) {

    v <- rlang::sym(var)

    # Join and replace
    herd_cattle_meat_clean <- herd_cattle_meat_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median,threshold_down,threshold_up)|>
          dplyr::rename(median_all = median)
      ) |>
      dplyr::mutate(ref_val = case_when(
        !is.finite(median_NUTS2) ~ median_all,
        .default = median_NUTS2
      )) |>
      # replace
      dplyr::mutate(!!v := case_when(
        !is.finite(!!v) ~ ref_val,
        .default = !!v
      )) |>
      dplyr::mutate(!!v := case_when(
        (!!v < threshold_down) ~ threshold_down,
        (!!v > threshold_up) ~ threshold_up,
        .default = !!v
      )) |>
      dplyr::select(-c(median_NUTS2,median_all,ref_val,threshold_down,threshold_up))
  }

  # View(herd_cattle_meat_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----

  herd_cattle_meat_eq <- herd_cattle_meat_clean |>
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
      Qeq_j_meat0 = case_when(
        Q_max == "juveniles" ~ Qobs_j,
        Q_max == "fattening" ~ (rt_j*(Qobs_f/rt_f)),
        Q_max == "breeders" ~ (rt_j*Qobs_b*offspring)
      ),
      Qeq_f_meat0 = case_when(
        Q_max == "juveniles" ~ (rt_f*(Qobs_j/rt_j)),
        Q_max == "fattening" ~ Qobs_f,
        Q_max == "breeders" ~ (rt_f*Qobs_b*offspring)
      ),
      Qeq_b_meat0 = case_when(
        Q_max == "juveniles" ~ (Qobs_j/rt_j/offspring),
        Q_max == "fattening" ~ (Qobs_f/rt_f/offspring),
        Q_max == "breeders" ~ Qobs_b
      )
    ) |>
    # add additional animals from the milk workshop
    dplyr::mutate(
      Qeq_j_meat = Qeq_j_meat0 + Qeq_j_dairy_meat,
      Qeq_f_meat = Qeq_f_meat0 + Qeq_f_dairy_meat,
      Qeq_b_meat = Qeq_b_meat0 + Qeq_b_dairy_meat
    ) |>
    # add or remove calves sold for rearing
    dplyr::mutate(
      Qeq_j_meat = case_when(
        Q_max == "fattening" ~ Qeq_j_meat + LBOV1_SSN,
        .default = Qeq_j_meat
      ),
      Qeq_f_meat = case_when(
        Q_max %in% c("juveniles","breeders") ~ Qeq_f_meat - LBOV1_SSN,
        .default = Qeq_f_meat
      ),
      Qeq_b_meat = case_when(
        Q_max == "fattening" ~ Qeq_b_meat + LBOV1_SSN,
        .default = Qeq_b_meat
      )
    )


  # View(herd_cattle_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_cattle_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  pseudoherd_cattle_meat <- herd_cattle_meat_eq |>
    # balance number of animals for the meat workshop
    dplyr::mutate(

      # juveniles
        LBOV1_Qeq_meat = ((Qeq_j_meat - Qobs_j)/1) + LBOV1_Qobs_meat,

        # fattening
        LBOV1_2M_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LBOV1_2M_Qobs,
        LBOV2_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LBOV2_Qobs,
        LHEIFFAT_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LHEIFFAT_Qobs,

        # mixed categories
        LBOV1_2F_Qeq_meat = (((Qeq_f_meat - Qobs_f)/4) + LBOV1_2F_fattening_Qobs) +
          (((Qeq_b_meat - Qobs_b)/3) + LBOV1_2F_Qobs_meat),

        # breeders
        LHEIFBRE_Qeq_meat = ((Qeq_b_meat - Qobs_b)/3) + LHEIFBRE_Qobs_meat,
        LCOWOTH_Qeq_meat  = ((Qeq_b_meat - Qobs_b)/3) + LCOWOTH_Qobs
    ) |>
    # select columns
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), matches("Qeq_meat")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = colnames(.)[grepl("_Qeq_meat",colnames(.))],
      names_to = "FADN_code_letter",
      values_to = "Qeq_meat"
    ) |>
    dplyr::mutate(
      FADN_code_letter = gsub("_Qeq_meat","",FADN_code_letter)
    )


  # Output ----

  pseudofarm_herd_cattle <- list(
  # rearing parameters
    rearing_param = herd_rearing_param_cattle |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
  # milk pseudo herd
  pseudoherd_milk = pseudoherd_cattle_milk,
  # meat pseudo herd
  pseudoherd_meat = pseudoherd_cattle_meat
  )

  return(pseudofarm_herd_cattle)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

