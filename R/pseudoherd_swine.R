#' Function to estimate off-farm animals of the swine herd based on FADN data
#' `f_pseudoherd_swine` Estimate off-farm animals of the swine herd
#'
#' @param object a FADN2Footprint object
#' @returns
#' A tibble with, for each livestock category:
#' - Q_obs: the number of animals observed in the farm (in number of heads)
#' - Q_obs_pseudo: the number of animals of the pseudo-herd (in number of heads)
#'
#' @examples
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_pseudoherd_swine(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_swine <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Differentiate farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_swine <- f_herd_rearing_param_swine(object)

  # View(herd_rearing_param_swine |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ## 2.3. MIXED CATEGORIES
  # no mixed categories in swine

  # 2. Estimate pseudoherd ---------------------------------------------------------------------------------

  ## 2.1. Differentiate farm workshops ----

  ## all swine is in meat workshop
  herd_swine_meat <- herd_rearing_param_swine

  ## 2.2. Balance number of animals in each workshop ----

  ### 2.2.1. Aggregate by rearing stage ----

  herd_swine_meat_aggr <- herd_swine_meat |>
    # estimate observed quantities and times for each production process step
    dplyr::mutate(
      # juveniles
      Qobs_j = LPIGLET_Qobs,
      rt_j = rt_LPIGLET,
      # fattening
      Qobs_f = LPIGOTH_Qobs + LPIGFAT_Qobs,
      rt_f = (rt_LPIGOTH*LPIGOTH_Qobs + rt_LPIGFAT*LPIGFAT_Qobs) / Qobs_f,
      # breeders
      Qobs_b = LSOWBRE_Qobs,
      offspring = offspring_LSOWBRE
    )


  # View(herd_swine_meat_aggr |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  # Replace NAs with reference values

  herd_swine_meat_clean <- herd_swine_meat_aggr
  for (var in c("Qobs_j","rt_j","Qobs_f","rt_f","Qobs_b","offspring")) {

    v <- rlang::sym(var)

    # Join and replace
    herd_swine_meat_clean <- herd_swine_meat_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$swine |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$swine |>
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

  # View(herd_swine_meat_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_swine_meat_clean |> summarise(across(colnames(.)[grepl("Qobs|rt_|t_1st|offspring",colnames(.))], ~quantile(.x))) |> t(.))

  ### 2.2.2. Estimate herd at equilibrium ----


  herd_swine_meat_eq <- herd_swine_meat_clean |>
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
      Qeq_j = case_when(
        Q_max == "juveniles" ~ Qobs_j,
        Q_max == "fattening" ~ (rt_j*(Qobs_f/rt_f)),
        Q_max == "breeders" ~ (rt_j*Qobs_b*offspring)
      ),
      Qeq_f = case_when(
        Q_max == "juveniles" ~ (rt_f*(Qobs_j/rt_j)),
        Q_max == "fattening" ~ Qobs_f,
        Q_max == "breeders" ~ (rt_f*Qobs_b*offspring)
      ),
      Qeq_b = case_when(
        Q_max == "juveniles" ~ (Qobs_j/rt_j/offspring),
        Q_max == "fattening" ~ (Qobs_f/rt_f/offspring),
        Q_max == "breeders" ~ Qobs_b
      )
    )

  # View(herd_swine_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_swine_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----

  pseudoherd_swine_meat <- herd_swine_meat_eq |>
    # balance number of animals for the meat workshop
    dplyr::mutate(

      # juveniles
      LPIGLET_Qeq_meat = ((Qeq_j - Qobs_j)/1) + LPIGLET_Qobs,

      # fattening
      LPIGFAT_Qeq_meat = ((Qeq_f - Qobs_f)/2) + LPIGFAT_Qobs,
      LPIGOTH_Qeq_meat = ((Qeq_f - Qobs_f)/2) + LPIGOTH_Qobs,

      # breeders
      LSOWBRE_Qeq_meat = ((Qeq_b - Qobs_b)/1) + LSOWBRE_Qobs
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

  pseudofarm_herd_swine <- list(
    # rearing parameters
    rearing_param = herd_swine_meat_clean |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
    # meat pseudo herd
    pseudoherd_meat = pseudoherd_swine_meat
  )


  return(pseudofarm_herd_swine)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

