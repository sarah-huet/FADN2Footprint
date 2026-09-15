#' Function to estimate off-farm animals of the sheep herd based on FADN data
#' `f_pseudoherd_sheep` Estimate off-farm animals of the sheep herd
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
#' f_pseudoherd_sheep(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_sheep <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Restrain herds to farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_sheep <- f_herd_rearing_param_sheep(object)

  # View(herd_rearing_param_sheep |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))

  # 2. Estimate pseudo herd ---------------------------------------------------------------------------------


  ## MILK ----

  ### 2.1. Restrain herds to farm workshops ----

  herd_sheep_milk <- herd_rearing_param_sheep |>
    # estimate observed quantities and times for each production process step
    # first estimate how many animals are needed to renew the dairy ewes
    # TODO: check formula to estimate Qobs to renew dairy ewes
    dplyr::mutate(

      # breeders
      LSHEIBR_Qeq_milk = rt_LSHEIBR * (LSHEDA_Qobs/rt_LSHEDA),
      LSHE1_2F_Qeq_milk = rt_LSHE1_2F_breeders * (LSHEIBR_Qeq_milk/rt_LSHEIBR),

      # juveniles
      LSHE1_Qeq_milk = rt_LSHE1 * (LSHE1_2F_Qeq_milk/rt_LSHE1_2F_breeders)

    )

  # View(herd_sheep_milk |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_milk |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))

  ### 2.2. Balance number of animals ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_sheep_milk_aggr <- herd_sheep_milk |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = pmin(LSHE1_Qeq_milk,LSHE1_Qobs),
      rt_j = rt_LSHE1,
      # fattening
      Qobs_f = 0,
      rt_f = ((1+rt_LSHE1_2M)*LSHE1_2M_Qobs +
                (2+rt_LSHE2)*LSHE2_Qobs +
                (1+rt_LSHE1_2F_fattening)*LSHE1_2F_fattening_Qobs +
                (2+rt_LSHEIFAT)*LSHEIFAT_Qobs) /
        (LSHE1_2M_Qobs + LSHE2_Qobs + LSHE1_2F_fattening_Qobs + LSHEIFAT_Qobs),
      # breeders
      Qobs_b = pmin(LSHE1_2F_Qeq_milk,LSHE1_2F_breeders_Qobs) + pmin(LSHEIBR_Qeq_milk,LSHEIBR_Qobs) + LSHEDA_Qobs,
      offspring = offspring_b
    )

  # View(herd_sheep_milk_aggr |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  ### Replace NAs with reference values for aggregate parameters ----

  herd_sheep_milk_clean <- herd_sheep_milk_aggr
  for (var in c("Qobs_j","rt_j","rt_f","Qobs_b","offspring")) {

    v <- rlang::sym(var)

    # Join and replace
    herd_sheep_milk_clean <- herd_sheep_milk_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$sheep |>
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

  # View(herd_sheep_milk_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----

  herd_sheep_dairy_meat_eq <- herd_sheep_milk_clean |>
    # select the livestock category from which the pseudo-herd at equilibrium will be estimated as the pseudoherd with the highest numbers of animals
    dplyr::mutate(
      Q_max = "dairy"
    ) |>
    dplyr::mutate(
      Qeq_j_dairy_meat = (rt_j*Qobs_b*offspring) - LSHE1_Qeq_milk,
      Qeq_f_dairy_meat = (rt_f*Qobs_b*offspring),
      Qeq_b_dairy_meat = 0
    )


  # View(herd_sheep_milk_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_milk_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  pseudoherd_sheep_milk <- herd_sheep_dairy_meat_eq |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("Qeq_milk|LSHEDA_Qobs")) |>
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

  herd_sheep_meat <- herd_sheep_dairy_meat_eq |> # herd_rearing_param_sheep |>
    # estimate additional animals from the milk workshop
    dplyr::mutate(

      # LSHEIBR
      LSHEIBR_Qobs_meat = pmax(LSHEIBR_Qobs - LSHEIBR_Qeq_milk, 0), # pmax to have zero if negative result
      # LSHE1_2F_breeders
      LSHE1_2F_Qobs_meat = pmax(LSHE1_2F_breeders_Qobs - LSHE1_2F_Qeq_milk, 0),

      # LSHE1
      LSHE1_Qobs_meat = pmax(LSHE1_Qobs - LSHE1_Qeq_milk, 0)

    )

  # View(herd_sheep_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))

  ### 2.2. Balance number of animals in each workshop ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_sheep_meat_aggr <- herd_sheep_meat |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = LSHE1_Qobs_meat,
      rt_j = rt_LSHE1,
      # fattening
      Qobs_f = LSHE1_2M_Qobs + LSHE2_Qobs + LSHE1_2F_fattening_Qobs + LSHEIFAT_Qobs,
      ## LSHE1_2M & LSHE1_2F have at least 1 y.o., LSHE2 & LSHEIFAT have at least 2 y.o.
      rt_f = ((1+rt_LSHE1_2M)*LSHE1_2M_Qobs +
                (2+rt_LSHE2)*LSHE2_Qobs +
                (1+rt_LSHE1_2F_fattening)*LSHE1_2F_fattening_Qobs +
                (2+rt_LSHEIFAT)*LSHEIFAT_Qobs) / Qobs_f,
      # breeders
      Qobs_b = LSHE1_2F_Qobs_meat + LSHEIBR_Qobs_meat + LSHEOTH_Qobs,
      offspring = offspring_b
    )

  # View(herd_sheep_meat |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2)) |> dplyr::filter(value >0))
  # View(herd_sheep_meat |> summarise(across(everything(), ~sum(is.finite(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### Replace NAs with reference values for aggregate parameters ----


  herd_sheep_meat_clean <- herd_sheep_meat_aggr
  for (var in c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring")) {

    v <- rlang::sym(var)

    # Join and replace
    herd_sheep_meat_clean <- herd_sheep_meat_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$sheep |>
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

  # View(herd_sheep_meat_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----

  herd_sheep_meat_eq <- herd_sheep_meat_clean |>
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
    # add or remove lambs sold for rearing
    dplyr::mutate(
      Qeq_j_meat = case_when(
        Q_max == "fattening" ~ Qeq_j_meat + LSHE1_SSN,
        .default = Qeq_j_meat
      ),
      Qeq_f_meat = case_when(
        Q_max %in% c("juveniles","breeders") ~ Qeq_f_meat - LSHE1_SSN,
        .default = Qeq_f_meat
      ),
      Qeq_b_meat = case_when(
        Q_max == "fattening" ~ Qeq_b_meat + LSHE1_SSN,
        .default = Qeq_b_meat
      )
    )


  # View(herd_sheep_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  pseudoherd_sheep_meat <- herd_sheep_meat_eq |>
    # balance number of animals for the meat workshop
    dplyr::mutate(

      # juveniles
        LSHE1_Qeq_meat = ((Qeq_j_meat - Qobs_j)/1) + LSHE1_Qobs_meat,

        # fattening
        LSHE1_2M_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LSHE1_2M_Qobs,
        LSHE2_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LSHE2_Qobs,
        LSHEIFAT_Qeq_meat = ((Qeq_f_meat - Qobs_f)/4) + LSHEIFAT_Qobs,

        # mixed categories
        LSHE1_2F_Qeq_meat = (((Qeq_f_meat - Qobs_f)/4) + LSHE1_2F_fattening_Qobs) +
          (((Qeq_b_meat - Qobs_b)/3) + LSHE1_2F_Qobs_meat),

        # breeders
        LSHEIBR_Qeq_meat = ((Qeq_b_meat - Qobs_b)/3) + LSHEIBR_Qobs_meat,
        LSHEOTH_Qeq_meat  = ((Qeq_b_meat - Qobs_b)/3) + LSHEOTH_Qobs
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

  pseudofarm_herd_sheep <- list(
  # rearing parameters
    rearing_param = herd_rearing_param_sheep |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
  # milk pseudo herd
  pseudoherd_milk = pseudoherd_sheep_milk,
  # meat pseudo herd
  pseudoherd_meat = pseudoherd_sheep_meat
  )

  return(pseudofarm_herd_sheep)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)
