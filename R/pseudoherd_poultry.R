#' Function to estimate off-farm animals of the poultry herd based on FADN data
#' `f_pseudoherd_poultry` Estimate off-farm animals of the poultry herd
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
#' f_pseudoherd_poultry(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_poultry <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate pseudo herd
  ### 2.1. Differentiate farm workshops
  ### 2.2. Balance number of animals in each workshop

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_poultry <- f_herd_rearing_param_poultry(object)

  # 2. Estimate pseudoherd ---------------------------------------------------------------------------------

  ## EGGS ----
  ### 2.1. Differentiate farm workshops ----

  # we consider that only laying hens are involved in the egg workshop
  herd_poultry_eggs <- herd_rearing_param_poultry |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("LHENSLAY"))

  ## 2.2 Balance number of animals in each workshop ----

  # For poultry, we consider breeders of broilers and juveniles of laying hens as negligible, hence equilibrium quantities equaling observed quantities.
  pseudoherd_poultry_eggs <- herd_poultry_eggs |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qobs")) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(object@traceability$id_cols),
      names_to = "FADN_code_letter",
      values_to = "Qeq_eggs"
    ) |>
    mutate(
      FADN_code_letter = gsub("_Qobs","",FADN_code_letter)
    )

  ### MEAT ----

  ### 3.1. Differentiate farm workshops ----
  # we consider that only laying hens are not involved in the meat workshop
  herd_poultry_meat <- herd_rearing_param_poultry |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), !dplyr::matches("LHENSLAY"))

  ## 3.2. Balance number of animals in each workshop ----

  # For poultry, we consider breeders of broilers and juveniles of laying hens as negligible, hence equilibrium quantities equaling observed quantities.
  pseudoherd_poultry_meat <- herd_poultry_meat |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qobs")) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(object@traceability$id_cols),
      names_to = "FADN_code_letter",
      values_to = "Qeq_meat"
    ) |>
    mutate(
      FADN_code_letter = gsub("_Qobs","",FADN_code_letter)
    )

  # Output ----

  pseudoherd_poultry <- list(
    # rearing parameters
    rearing_param = herd_rearing_param_poultry |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),dplyr::matches("rt_|t_1st|offspring")),
    # eggs pseudo herd
    pseudoherd_eggs = pseudoherd_poultry_eggs,
    # meat pseudo herd
    pseudoherd_meat = pseudoherd_poultry_meat
  )

  return(pseudoherd_poultry)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

