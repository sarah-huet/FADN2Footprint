#' Estimate pseudo-herd for poultry based on egg and meat workshops
#'
#' @description
#' f_pseudoherd_poultry estimates a "pseudo-herd" of poultry animals for the
#' egg and meat production workshops. Unlike cattle and swine, poultry
#' rearing stages other than laying hens (broiler breeders, laying hen
#' juveniles) are considered negligible, so equilibrium quantities are set
#' equal to observed quantities for all categories.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Model farm rearing process:**
#' f_herd_rearing_param_poultry is called to derive rearing parameters
#' (turnover rates, time to first stage, offspring rate) and observed animal
#' counts for each poultry FADN livestock category on the farm.
#'
#' **2. Estimate pseudo-herd:**
#' - **Eggs workshop**:
#'   - *2.1 Differentiate farm workshops*: only laying hens (LHENSLAY) are
#'     considered part of the egg production workshop.
#'   - *2.2 Balance number of animals*: since juveniles and breeders
#'     contributing to laying hens are considered negligible, the
#'     equilibrium quantity (Qeq_eggs) is set equal to the observed quantity
#'     (Qobs) for LHENSLAY, without further balancing across rearing stages.
#' - **Meat workshop**:
#'   - *3.1 Differentiate farm workshops*: all poultry categories except
#'     laying hens (e.g. broilers) are considered part of the meat
#'     production workshop.
#'   - *3.2 Balance number of animals*: as for eggs, breeder and juvenile
#'     stages are considered negligible, so the equilibrium quantity
#'     (Qeq_meat) is set equal to the observed quantity (Qobs) for each
#'     relevant FADN livestock category.
#'
#' Both pseudo-herd tables are pivoted to long format (one row per farm ×
#' FADN_code_letter) and combined into a single output table.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing object@traceability$id_cols and all data
#'   required by f_herd_rearing_param_poultry.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{rearing_param}{A tibble with one row per farm containing the
#'     traceability id columns and rearing parameters (turnover rates,
#'     time to first stage, and offspring rate) for poultry categories.}
#'   \item{pseudoherd}{A tibble in long format combining egg and meat
#'     workshops, with one row per farm × FADN_code_letter, containing
#'     columns Qeq_eggs (for laying hens) and Qeq_meat (for other poultry
#'     categories, e.g. broilers).}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' pseudoherd_poultry <- f_pseudoherd_poultry(f)
#' head(pseudoherd_poultry$pseudoherd)
#' head(pseudoherd_poultry$rearing_param)
#' }
#'
#' @seealso f_herd_rearing_param_poultry, f_pseudoherd_cattle,
#'   f_pseudoherd_swine, f_pseudoherd_animals
#'
#' @concept practice-pseudoherd
#' @export
#' @import dplyr tidyr tidyselect


f_pseudoherd_poultry <- function(object){
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

  herd_rearing_param_poultry <- f_herd_rearing_param_poultry(object)

  # 2. On-farm herd activities ---------------------------------------------------------------------------------

  herd_activities = f_herd_activities(object) |>
    dplyr::filter(species == "poultry")

  # 3. Estimate pseudoherd ---------------------------------------------------------------------------------

  ## EGGS ----
  ### 2.1. Differentiate farm workshops ----

  # we consider that only laying hens are involved in the egg workshop
  herd_poultry_eggs <- herd_activities |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs_eggs) |>
    # pivot table
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_Qobs_eggs",
      values_from = Qobs_eggs,
      values_fill = 0
    )

  ## 2.2 Balance number of animals in each workshop ----

  # For poultry, we consider breeders of broilers and juveniles of laying hens as negligible, hence equilibrium quantities equaling observed quantities.
  pseudoherd_poultry_eggs <- herd_poultry_eggs |>
    # add rearing parameters
    dplyr::left_join(herd_rearing_param_poultry,
                     by = id_cols)  |>
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
  herd_poultry_meat <- herd_activities |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs_meat) |>
    # pivot table
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_Qobs_meat",
      values_from = Qobs_meat,
      values_fill = 0
    )

  ## 3.2. Balance number of animals in each workshop ----

  # For poultry, we consider breeders of broilers and juveniles of laying hens as negligible, hence equilibrium quantities equaling observed quantities.
  pseudoherd_poultry_meat <- herd_poultry_meat |>
    # add rearing parameters
    dplyr::left_join(herd_rearing_param_poultry,
                     by = id_cols)  |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qobs")) |>
    tidyr::pivot_longer(
      cols = dplyr::matches("Qobs"),
      names_to = "FADN_code_letter",
      values_to = "Qeq_meat"
    ) |>
    mutate(
      FADN_code_letter = gsub("_Qobs","",FADN_code_letter)
    ) |>
    # round values
    dplyr::mutate(
      Qeq_meat = round(Qeq_meat, 2)
    )

  # Output ----

  pseudoherd_poultry <- list(
    # rearing parameters
    rearing_param = herd_rearing_param_poultry |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),dplyr::matches("rt_|t_1st|offspring")),
    # eggs pseudo herd
    pseudoherd = dplyr::bind_rows(pseudoherd_poultry_eggs,
                                  pseudoherd_poultry_meat)
  )

  return(pseudoherd_poultry)

}

