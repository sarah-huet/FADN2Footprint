#' Determine herd participation in farm activities (milk, meat, eggs, etc.)
#'
#' @description
#' f_herd_activities computes which part of each herd is involved in the
#' different production activities on the farm (e.g. milk, meat, eggs). It
#' combines rearing-parameter estimates for multiple species (cattle, swine,
#' poultry) with herd records (sheep, goats, horses) to produce a
#' traceable per‑farm / per‑FADN animal‑category table of observed animal
#' quantities participating in each activity.
#'
#' @details
#' The function:
#' - validates that `object` inherits from the S4 class "FADN2Footprint";
#' - returns cached results stored in object@practices$herding$activities when
#'   present and `overwrite = FALSE`;
#' - for cattle, uses f_herd_rearing_param_cattle(object) and a set of
#'   rearing/transfer rate variables (rt_*) to estimate the observed number of
#'   animals involved in the milk production chain (Qobs_milk) and the remainder
#'   allocated to meat production (Qobs_meat). The code performs intermediate
#'   calculations to allocate breeders, juveniles and other rearing classes to
#'   the milk or meat activity. Several internal formulas include safety checks
#'   (e.g. pmin/pmax or conditional zeroing) to avoid negative or impossible
#'   allocations.
#' - for swine and poultry, calls f_herd_rearing_param_swine and
#'   f_herd_rearing_param_poultry, reshapes the rearing-parameter outputs into
#'   long format and assigns activity roles (meat, eggs) where relevant;
#' - for sheep, goats and horses, extracts existing Qobs columns from
#'   object@herd and keeps them for activity allocation (no extra rearing
#'   parameter computation is performed here);
#' - finally, the results for all species are row-bound, filtered to keep only
#'   positive observed animal counts, and annotated with species information
#'   using data_extra$livestock to allow downstream joins and aggregation.
#'
#' Important notes and assumptions:
#' - The cattle allocation logic is relatively complex and depends on rearing
#'   parameters provided by f_herd_rearing_param_cattle (rt_* variables,
#'   breeder and juvenile Qobs columns). Users should check those inputs and
#'   accompanying TODOs in the implementation for edge cases (e.g. farms with
#'   missing intermediate classes).
#' - The function does not modify the input object; it returns a tibble that
#'   can be stored back into the object's cache slot (object@practices$herding$activities)
#'   by the caller if desired.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the package
#'   workflow. The function expects the object to provide:
#'   - object@traceability$id_cols (vector of id column names used for joins),
#'   - object@herd (for sheep, goats, horses),
#'   - helper functions f_herd_rearing_param_cattle, f_herd_rearing_param_swine,
#'     f_herd_rearing_param_poultry to supply rearing parameter tables,
#'   - package data table data_extra$livestock for species lookup.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble/data.frame with one or more rows per traceable farm and
#'   FADN animal category (FADN_code_letter). Typical columns include:
#'   - traceability id columns (as in object@traceability$id_cols),
#'   - FADN_code_letter (animal category code),
#'   - Qobs: observed number of animals (baseline from rearing params / herd),
#'   - Qobs_milk, Qobs_meat, Qobs_eggs (when applicable): observed numbers of
#'     animals participating in each activity (zeros when not applicable),
#'   - species: species label joined from data_extra$livestock.
#'
#' Rows with Qobs <= 0 are filtered out.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' herd_act <- f_herd_activities(f)
#' # Inspect cattle milk vs meat allocation
#' subset(herd_act, species == "cattle")
#'
#' # Force recomputation even if cached
#' herd_act2 <- f_herd_activities(f, overwrite = TRUE)
#' }
#'
#' @seealso f_herd_rearing_param_cattle, f_herd_rearing_param_swine,
#'   f_herd_rearing_param_poultry, data_extra$livestock
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr select filter left_join matches all_of mutate case_when coalesce
#' @importFrom tidyr pivot_longer pivot_wider


f_herd_activities <- function(object,
                              overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$herding$activities)&& !overwrite) {
    message("Using cached values stored in object@practices$herding$activities.")
    return(object@practices$herding$activities)  # use cached value
  }

  # On-farm herd ----

  ## CATTLE ----
  herd_cattle <- f_herd_rearing_param_cattle(object) |>
    # estimate observed quantities and times for each production process step
    # first estimate how many animals are needed to renew the dairy cows, in farms that have dairy cows
    # TODO: check formula to estimate Qobs to renew dairy cows, especially when there is no LHEIFBRE in the farm, how to estimate LBOV1_2F?
      dplyr::mutate(

        # breeders
        LCOWDAIR_Qobs_milk = ifelse(LCOWDAIR_Qobs >0, LCOWDAIR_Qobs, 0),

        #F_LHEIFBRE_2_LCOWDAIR = (LCOWDAIR_Fin - LCOWDAIR_PN) * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs) ),
        #F_LBOV1_2F_breeders_2_LCOWDAIR = (LCOWDAIR_Fin - LCOWDAIR_PN) * (LBOV1_2F_breeders_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs) ),

        #LCOWDAIR_upward_Qobs_milk = (( rt_LBOV1_2F_breeders*LBOV1_2F_breeders_Qobs + ((1+rt_LHEIFBRE)*LHEIFBRE_Qobs) ) / ( LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs )) * ((LCOWDAIR_Fin - LCOWDAIR_PN)/rt_LCOWDAIR),
        LCOWDAIR_upward_Qobs_milk = ifelse(LCOWDAIR_Qobs >0,
                                           (( rt_LBOV1_2F_breeders*LBOV1_2F_breeders_Qobs + ((1+rt_LHEIFBRE)*LHEIFBRE_Qobs) ) / ( LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs )) * (LCOWDAIR_Qobs/rt_LCOWDAIR),
                                           0),
        LHEIFBRE_Qobs_milk = ifelse(LCOWDAIR_Qobs >0,
          LCOWDAIR_upward_Qobs_milk * (LHEIFBRE_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs) ),
                                    0),

        LBOV1_2F_Qobs_milk = ifelse(LCOWDAIR_Qobs >0,
          LCOWDAIR_upward_Qobs_milk * (LBOV1_2F_breeders_Qobs / (LHEIFBRE_Qobs + LBOV1_2F_breeders_Qobs) ),
                                    0),

        LBOV1_Qobs_milk = ifelse(LCOWDAIR_Qobs >0,
          rt_LBOV1 * (LBOV1_2F_Qobs_milk/rt_LBOV1_2F_breeders),
                                 0)


        #LHEIFBRE_Qobs_milk = rt_LHEIFBRE * (LCOWDAIR_Qobs/rt_LCOWDAIR),
        #LHEIFBRE_Qobs_milk = pmin(rt_LHEIFBRE * (LCOWDAIR_Qobs/rt_LCOWDAIR),LHEIFBRE_Qobs),

        #LBOV1_2F_Qobs_milk = rt_LBOV1_2F_breeders * (LHEIFBRE_Qobs_milk/rt_LHEIFBRE),
        #LBOV1_2F_Qobs_milk = pmin(rt_LBOV1_2F_breeders * (LHEIFBRE_Qobs_milk/rt_LHEIFBRE),LBOV1_2F_breeders_Qobs),

        # juveniles
        #LBOV1_Qobs_milk = rt_LBOV1 * (LBOV1_2F_Qobs_milk/rt_LBOV1_2F_breeders)
        #LBOV1_Qobs_milk = pmin(rt_LBOV1 * (LBOV1_2F_Qobs_milk/rt_LBOV1_2F_breeders), LBOV1_Qobs)

      )  |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), dplyr::matches("Qobs"), -dplyr::matches("fattening|breeders")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = -dplyr::all_of(object@traceability$id_cols),
      names_to = c("FADN_code_letter", "variable"),
      names_pattern = "(.+)_(Qobs(?:_milk)?)"  # captures 'LHEIFFAT' and 'Qobs' or 'Qobs_milk'
    ) |>
    tidyr::pivot_wider(
      names_from = variable,
      values_from = value
    ) |>
    # estimate Qobs
    dplyr::mutate(
      ## for milk: check that estimated Qobs are <= Qobs
      Qobs_milk = pmin(Qobs,Qobs_milk),
      ## for meat: animals not involved in the milk process are considered part of the meat activity
      Qobs_meat = Qobs - dplyr::coalesce(Qobs_milk,0)
    )


  ## SWINE ----
  herd_swine <- f_herd_rearing_param_swine(object) |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), dplyr::matches("Qobs")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = -dplyr::all_of(object@traceability$id_cols),
      names_to = "FADN_code_letter",
      names_pattern = "(.+)_",
      values_to = "Qobs"
    ) |>
    # MEAT
    dplyr::mutate(
      Qobs_meat = Qobs
    )

  ## POULTRY ----
  herd_poultry <- f_herd_rearing_param_poultry(object) |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), dplyr::matches("Qobs")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = -dplyr::all_of(object@traceability$id_cols),
      names_to = "FADN_code_letter",
      names_pattern = "(.+)_",
      values_to = "Qobs"
    ) |>
    # restrain to activity
    dplyr::mutate(
      # eggs
      Qobs_eggs = dplyr::case_when(
        FADN_code_letter == "LHENSLAY" ~ Qobs,
        .default = 0
      ),
      # meat
      Qobs_meat = dplyr::case_when(
        FADN_code_letter != "LHENSLAY" ~ Qobs,
        .default = 0
      )
    )

  ## SHEEP ----
  herd_sheep <- object@herd |>
    dplyr::filter(species == "sheep") |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), FADN_code_letter, dplyr::matches("Qobs"))

  ## GOATS ----
  herd_goats <- object@herd |>
    dplyr::filter(species == "goats") |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), FADN_code_letter, dplyr::matches("Qobs"))

  ## HORSES ----
  herd_horse <- object@herd |>
    dplyr::filter(species == "horse") |>
    # dplyr::select columns
    dplyr::select(dplyr::all_of(object@traceability$id_cols), FADN_code_letter, dplyr::matches("Qobs"))

  # combine tables ----
  herd <- Reduce(
    dplyr::bind_rows,
    list(
      herd_cattle,
      herd_swine,
      herd_poultry,
      herd_sheep,
      herd_goats,
      herd_horse
    ))|>
    dplyr::filter(Qobs >0) |>
    # add species
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,species),
      by = c('FADN_code_letter')
    )


  return(herd)
}
