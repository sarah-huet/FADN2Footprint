#' Function to estimate off-farm animals of the sheep herd based on FADN data
#' `f_pseudoherd_sheep` Estimate off-farm animals of the sheep herd
#'
#' @param object a FADN2Footprint object
#' @param overwrite logical, force recomputation
#' @returns
#' A list with rearing parameters and the pseudo herd (in number of heads)
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

f_pseudoherd_sheep <- function(object,
                               overwrite = FALSE) {
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

  herd_rearing_param_sheep <- f_herd_rearing_param_sheep(object)

  # 2. Estimate pseudoherd ---------------------------------------------------------------------------------

  ## 2.1. Differentiate farm workshops ----

  ## all sheep is in meat workshop
  ## (milk activity is handled separately in f_herd_activities via
  ## the PMLKSHEP production variable)
  herd_sheep_meat <- herd_rearing_param_sheep

  ## 2.2. Balance number of animals in each workshop ----

  ### 2.2.1. Aggregate by rearing stage ----
  ## Sheep only have two stages: others (juveniles/fattening) and breeders.
  ## There is no separate "juveniles" stage as with swine (LPIGLET vs LPIGFAT):
  ## LSHEPOTH already aggregates juveniles + fattening sheep.

  herd_sheep_meat_aggr <- herd_sheep_meat |>
    dplyr::mutate(
      # others (juveniles / fattening)
      Qobs_o = LSHEPOTH_Qobs,
      rt_o = rt_LSHEPOTH,
      # breeders
      Qobs_b = LEWEBRE_Qobs,
      rt_b = rt_LEWEBRE,
      offspring = offspring_LEWEBRE
    )

  # Remove NAs
  tmp_avrg_rearing_param <- h_average_practices(data = herd_sheep_meat_aggr,
                                                target_vars = c("Qobs_o","rt_o","Qobs_b","rt_b","offspring"),
                                                primary_grp = c('YEAR', 'COUNTRY', 'NUTS2'),
                                                secondary_grp = c('COUNTRY'),
                                                weight_var = NULL) |>
    dplyr::rename_with(~ paste0("avrg_", .x),
                       .cols = c("Qobs_o","rt_o","Qobs_b","rt_b","offspring"))

  ## add fallback averages
  herd_sheep_meat_aggr <- herd_sheep_meat_aggr |>
    dplyr::left_join(tmp_avrg_rearing_param,
                     by = c('YEAR', 'COUNTRY', 'NUTS2')) |>
    # replace NAs with fallback
    dplyr::mutate(
      Qobs_o = ifelse(is.na(Qobs_o), avrg_Qobs_o, Qobs_o),
      rt_o = ifelse(is.na(rt_o), avrg_rt_o, rt_o),
      Qobs_b = ifelse(is.na(Qobs_b), avrg_Qobs_b, Qobs_b),
      rt_b = ifelse(is.na(rt_b), avrg_rt_b, rt_b),
      offspring = ifelse(is.na(offspring), avrg_offspring, offspring)
    ) |>
    # remove fallback variables
    dplyr::select(-dplyr::matches("^avrg_"))

  # View(herd_sheep_meat_aggr |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))

  ### 2.2.2. Estimate herd at equilibrium ----
  ## Since there are only two stages (others vs breeders), the pseudo-herd
  ## at equilibrium is estimated as the pair whose numbers are consistent
  ## with each other via the offspring/residence-time relationship.

  herd_sheep_meat_eq <- herd_sheep_meat_aggr |>
    dplyr::mutate(
      Q_max = case_when(
        ## Qobs_o >= ^Q_o|b estimated from breeders
        Qobs_o >= ifelse(Qobs_b>0,(rt_o*Qobs_b*offspring),0) ~ "others",
        ## otherwise, breeders is the limiting category
        .default = "breeders"
      )
    ) |>
    dplyr::mutate(
      Qeq_o = case_when(
        Q_max == "others" ~ Qobs_o,
        Q_max == "breeders" ~ (rt_o*Qobs_b*offspring)
      ),
      Qeq_b = case_when(
        Q_max == "others" ~ (Qobs_o/rt_o/offspring),
        Q_max == "breeders" ~ Qobs_b
      )
    )

  # View(herd_sheep_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 2.2.3. Estimate balanced number of animals ----
  ## Since each rearing stage maps directly to a single FADN category
  ## (LSHEPOTH for others, LEWEBRE for breeders), no NUTS2-level share
  ## weighting is needed (unlike swine's breeders stage, split across
  ## LSOWBRE and LPIGOTH).

  pseudoherd_sheep_meat <- herd_sheep_meat_eq |>
    dplyr::mutate(
      LSHEPOTH_Qeq_meat = Qeq_o,
      LEWEBRE_Qeq_meat = Qeq_b
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

  pseudofarm_herd_sheep <- list(
    # rearing parameters
    rearing_param = herd_sheep_meat_aggr |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
    # meat pseudo herd
    pseudoherd = pseudoherd_sheep_meat
  )


  return(pseudofarm_herd_sheep)

}
