#' Function to estimate off-farm animals of the cattle herd based on FADN data
#' `f_herd_rearing_param_cattle` Estimate off-farm animals of the cattle herd
#'
#' @param object a FADN2Footprint object
#' @returns
#' A list with, for each livestock category:
#' - Q_obs: the number of animals observed in the farm (in number of heads)
#' - Q_obs_pseudo: the number of animals of the pseudo-herd (in number of heads)
#'
#' @examples
#' data("mock_data", package = "FADN2Footprint")
#' fadn_fict <- mock_data$fadn_fict[
#'   which(as.numeric(mock_data$fadn_fict$YEAR) >= 2017)[1], ,
#'   drop = FALSE
#' ]
#' fadn_fict_obj <- data_4FADN2Footprint(fadn_fict)
#' f_herd_rearing_param_cattle(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_herd_rearing_param_cattle <- function(object) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Retrieve observed herd structure
  ### Observed animal stock prefers AN and falls back to the ALU-derived count
  ## 2. Modeling farm rearing process
  ### 2.1. Estimate animal flows between livestock categories
  ### 2.2. Estimate rearing parameters
  ### 2.3. Estimate values for mixed categories
  ### 2.4. Replace outliers with reference values

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_cattle <- object@herd |>
    dplyr::filter(
      species == "cattle"
    )

  # if no cattle, create a tibble with zeros for cattle variables
  if (nrow(herd_cattle) == 0) {
    # stop("The input data frame has no poultry.")
    # return(tibble())

    herd_cattle <- object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols)) |>
      dplyr::distinct() |>
      dplyr::left_join(
        herd_cattle,
        by = object@traceability$id_cols
      )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_cattle_process_init <- h_herd_cattle_process_init(
    herd_cattle = herd_cattle,
    id_cols = object@traceability$id_cols
  )

  # remove columns with only zeros or NAs
  # select(where(~ !is.numeric(.) || is.na(sum(., na.rm = TRUE)) || sum(., na.rm = TRUE) != 0))


  # how many NAs per columns => only in residence time columns
  # View(herd_cattle_process_init |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## replace outliers per percentiles ----
  # Clean the output of the first function

  herd_cattle_process_clean1 <- herd_cattle_process_init |>
    # add NUTS2 and SYS02
    dplyr::left_join(
      object@farm |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols), NUTS2, SYS02),
      by = object@traceability$id_cols
    )

  for (var in colnames(herd_cattle_process_clean1)[grepl("rt_|t_1st|offspring", colnames(herd_cattle_process_clean1))]) {
    v <- rlang::sym(var)

    # Join and replace
    herd_cattle_process_clean1 <- herd_cattle_process_clean1 |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2, median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2"
      ) |>
      # join overall medians and thresholds
      (function(.) {
        ovrll_tbl <- reference_rearing_param$ref_overall$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median, threshold_down, threshold_up) |>
          dplyr::rename(median_all = median)

        if (nrow(ovrll_tbl) > 0) {
          cbind(., ovrll_tbl)
        } else {
          cbind(., tibble(
            median_all = NA,
            threshold_down = NA,
            threshold_up = NA
          ))
        }
      })() |>
      dplyr::mutate(ref_val = dplyr::case_when(
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
      dplyr::select(-c(median_NUTS2, median_all, ref_val, threshold_down, threshold_up))
  }


  ## 2.3. MIXED CATEGORIES ----

  herd_cattle_process <- h_herd_cattle_process_mixed_categories(
    herd_cattle_process_clean1
  )

  # View(herd_cattle_process |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## replace outliers per percentiles ----

  # Clean the output of the second function
  herd_cattle_process_clean2 <- herd_cattle_process

  tmp_var <- setdiff(
    colnames(herd_cattle_process),
    colnames(herd_cattle_process_clean1)
  )[grepl("rt_|t_1st|offspring", setdiff(
    colnames(herd_cattle_process),
    colnames(herd_cattle_process_clean1)
  ))]
  for (var in tmp_var) {
    # var = tmp_var[5]

    v <- rlang::sym(var)
    # Join and replace
    herd_cattle_process_clean2 <- herd_cattle_process_clean2 |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2, median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2"
      ) |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median, threshold_down, threshold_up) |>
          dplyr::rename(median_all = median)
      ) |>
      dplyr::mutate(ref_val = dplyr::case_when(
        !is.finite(median_NUTS2) ~ median_all,
        .default = median_NUTS2
      )) |>
      # replace
      dplyr::mutate(!!v := dplyr::case_when(
        !is.finite(!!v) ~ ref_val,
        .default = !!v
      )) |>
      dplyr::mutate(!!v := dplyr::case_when(
        (!!v < threshold_down) ~ threshold_down,
        (!!v > threshold_up) ~ threshold_up,
        .default = !!v
      )) |>
      dplyr::select(-c(median_NUTS2, median_all, ref_val, threshold_down, threshold_up))
  }


  # Output ----

  herd_rearing_param_cattle <- herd_cattle_process_clean2


  return(herd_rearing_param_cattle)
}

#' Estimate flows and residence times for cattle livestock categories
#'
#' @param herd_cattle A cattle herd table with annual animal counts and flows.
#'   Counts and flows are in heads.
#' @param id_cols Names of the columns uniquely identifying each farm record.
#' @return A tibble with one row per farm record, flow estimates in heads, and
#'   residence-time estimates in years.
#' @noRd
h_herd_cattle_process_init <- function(herd_cattle, id_cols) {
  herd_cattle |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs, ON, CN, PN, SN, SSN, SRN) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(id_cols),
      names_from = FADN_code_letter,
      values_from = c(Qobs, ON, CN, PN, SN, SSN, SRN),
      names_glue = "{FADN_code_letter}_{.value}"
    ) |>
    # replace NAs by zeros
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    # add missing livestock categories to prevent errors
    cbind(
      expand.grid(
        code = setdiff(
          data_extra$livestock |>
            dplyr::filter(species == "cattle") |>
            dplyr::pull(FADN_code_letter),
          unique(herd_cattle$FADN_code_letter)
        ),
        suffix = c("Qobs", "ON", "CN", "PN", "SN", "SSN", "SRN")
      ) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        dplyr::mutate(value = 0) |>
        tidyr::pivot_wider(names_from = name, values_from = value)
    ) |>
    ## Flows between livestock categories
    ## See Annex 1 of COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK,
    ## 2009. Typology Handbook of agricultural holdings and the standard output
    ## (SO) coefficient calculation. (No. RI/CC 1500 rev. 3), Brussels.
    ## See the figure in the package vignette.
    dplyr::mutate(
      # Flow in LCOWDAIR
      LCOWDAIR_Fout = LCOWDAIR_SN,
      ## LCOWDAIR_Fin = LCOWDAIR_PN + LCOWDAIR_CN - LCOWDAIR_ON + LCOWDAIR_SN, # no ON and CN variables for LCOWDAIR
      LCOWDAIR_Fin = LCOWDAIR_CN - LCOWDAIR_ON + LCOWDAIR_Fout,
      # Flow in LCOWOTH
      LCOWOTH_Fout = LCOWOTH_SN,
      LCOWOTH_Fin = LCOWOTH_CN - LCOWOTH_ON + LCOWOTH_Fout,
      # Flow in LBOV2
      LBOV2_Fout = LBOV2_SN,
      LBOV2_Fin = LBOV2_CN - LBOV2_ON + LBOV2_Fout,
      # Flow in LHEIFFAT
      LHEIFFAT_Fout = LHEIFFAT_SN,
      LHEIFFAT_Fin = LHEIFFAT_CN - LHEIFFAT_ON + LHEIFFAT_Fout,
      # Flow in LHEIFBRE
      LHEIFBRE_Fout = LHEIFBRE_SN + pmax(0, LCOWDAIR_Fin - LCOWDAIR_PN) + pmax(0, LCOWOTH_Fin - LCOWOTH_PN),
      LHEIFBRE_Fin = LHEIFBRE_CN - LHEIFBRE_ON + LHEIFBRE_Fout,
      # Flow in LBOV1_2F
      LBOV1_2F_Fout = LBOV1_2F_SN + pmax(0, LHEIFFAT_Fin - LHEIFFAT_PN) + pmax(0, LHEIFBRE_Fin - LHEIFBRE_PN),
      LBOV1_2F_Fin = LBOV1_2F_CN - LBOV1_2F_ON + LBOV1_2F_Fout,
      # Flow in LBOV1_2M
      LBOV1_2M_Fout = LBOV1_2M_SN + pmax(0, LBOV2_Fin - LBOV2_PN),
      LBOV1_2M_Fin = LBOV1_2M_CN - LBOV1_2M_ON + LBOV1_2M_Fout,
      # Flow in LBOV1
      LBOV1_Fout = LBOV1_SN + pmax(0, LBOV1_2F_Fin - LBOV1_2F_PN) + pmax(0, LBOV1_2M_Fin - LBOV1_2M_PN),
      LBOV1_Fin = LBOV1_CN - LBOV1_ON + LBOV1_Fout
    ) |>
    # replace flow values below zero by zeros
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("Fin|Fout"),
        ~ pmax(.x, 0, na.rm = TRUE)
      )
    ) |>
    ## Rearing parameters
    dplyr::mutate(
      rt_LCOWDAIR = LCOWDAIR_Qobs / ((LCOWDAIR_Fin + LCOWDAIR_Fout) / 2),
      rt_LCOWOTH = LCOWOTH_Qobs / ((LCOWOTH_Fin + LCOWOTH_Fout) / 2),
      rt_LBOV2 = LBOV2_Qobs / ((LBOV2_Fin + LBOV2_Fout) / 2),
      rt_LHEIFFAT = LHEIFFAT_Qobs / ((LHEIFFAT_Fin + LHEIFFAT_Fout) / 2),
      rt_LBOV1_2M = LBOV1_2M_Qobs / ((LBOV1_2M_Fin + LBOV1_2M_Fout) / 2),
      rt_LHEIFBRE = LHEIFBRE_Qobs / ((LHEIFBRE_Fin + LHEIFBRE_Fout) / 2),
      rt_LBOV1_2F = LBOV1_2F_Qobs / ((LBOV1_2F_Fin + LBOV1_2F_Fout) / 2),
      rt_LBOV1 = LBOV1_Qobs / ((LBOV1_Fin + LBOV1_Fout) / 2),
      t_1st_calve_heiffers = ((2 + rt_LHEIFBRE) * LHEIFBRE_Qobs) / LHEIFBRE_Qobs, ## LHEIFBRE have at least 2 y.o.
      offspring_cows = (LBOV1_Fin - LBOV1_PN) / (LCOWDAIR_Qobs + LCOWOTH_Qobs)
    ) |>
    # replace Inf per NAs
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("rt_|t_1st|offspring"),
        ~ ifelse(!is.finite(.x), NA_real_, .x)
      )
    )
}

#' Estimate flows and residence times for mixed cattle categories
#'
#' @param herd_cattle_process_clean1 Cattle process table with base flow and
#'   residence-time estimates. Counts and flows are in heads.
#' @return The input table with mixed-category flow estimates in heads and
#'   residence-time estimates in years.
#' @noRd
h_herd_cattle_process_mixed_categories <- function(herd_cattle_process_clean1) {
  herd_cattle_process_clean1 |>
    # Split LBOV1_2F into fattening and breeding categories
    dplyr::mutate(
      ## Total number of animals in downward rearing stages
      # LBOV1_2F_total_downward =
      #  LHEIFFAT_Qobs / rt_LHEIFFAT
      #+ LHEIFBRE_Qobs / rt_LHEIFBRE
      #+ LCOWDAIR_Qobs / rt_LCOWDAIR
      #+ LCOWOTH_Qobs / rt_LCOWOTH,
      ### Total number of animals in downward fattening rearing stages
      # LBOV1_2F_total_downward_fattening =
      #  LHEIFFAT_Qobs / rt_LHEIFFAT,
      ### Total number of animals in downward breeders rearing stages
      # LBOV1_2F_total_downward_breeders =
      #  LHEIFBRE_Qobs / rt_LHEIFBRE
      #+ LCOWDAIR_Qobs / rt_LCOWDAIR
      #+ LCOWOTH_Qobs / rt_LCOWOTH,
      LBOV1_2F_total_downward = LBOV1_2F_Fout,
      LBOV1_2F_total_downward_fattening = LBOV1_2F_SSN + (LHEIFFAT_Fin - LHEIFFAT_PN),
      LBOV1_2F_total_downward_breeders = LBOV1_2F_Fout - LBOV1_2F_total_downward_fattening,
      ## proportion of fattening
      LBOV1_2F_fattening_prop = ifelse(LBOV1_2F_total_downward > 0, LBOV1_2F_total_downward_fattening / LBOV1_2F_total_downward, 0),
      ## proportion of breeding
      LBOV1_2F_breeders_prop = ifelse(LBOV1_2F_total_downward > 0, LBOV1_2F_total_downward_breeders / LBOV1_2F_total_downward, 0),
      ## Observed number of animals
      LBOV1_2F_fattening_Qobs = LBOV1_2F_Qobs * LBOV1_2F_fattening_prop,
      LBOV1_2F_breeders_Qobs = LBOV1_2F_Qobs * LBOV1_2F_breeders_prop,
      ## Outflow
      LBOV1_2F_fattening_Fout = LBOV1_2F_Fout * LBOV1_2F_fattening_prop,
      LBOV1_2F_breeders_Fout = LBOV1_2F_Fout * LBOV1_2F_breeders_prop,
      ## Inflow
      LBOV1_2F_fattening_Fin = LBOV1_2F_Fin * LBOV1_2F_fattening_prop,
      LBOV1_2F_breeders_Fin = LBOV1_2F_Fin * LBOV1_2F_breeders_prop,
      ## residence time
      rt_LBOV1_2F_fattening = LBOV1_2F_fattening_Qobs / ((LBOV1_2F_fattening_Fin + LBOV1_2F_fattening_Fout) / 2),
      rt_LBOV1_2F_breeders = LBOV1_2F_breeders_Qobs / ((LBOV1_2F_breeders_Fin + LBOV1_2F_breeders_Fout) / 2)
    ) |>
    # rearing parameter including mixed categories
    ## LBOV1_2F have at least 1 y.o., LHEIFBRE have at least 2 y.o.
    dplyr::mutate(
      t_1st_calve = ((1 + rt_LBOV1_2F_breeders) * LBOV1_2F_breeders_Qobs + ((2 + rt_LHEIFBRE) * LHEIFBRE_Qobs)) / (LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs),
      offspring_b = (LBOV1_Fin - LBOV1_PN) / (LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs + LCOWDAIR_Qobs + LCOWOTH_Qobs)
    ) |>
    # Split LBOV1_2F fattening category into rearing and slaughter
    dplyr::mutate(
      ## proportion of calves for slaughter
      LBOV1_2F_fattening_slaughter_prop = ifelse(LBOV1_2F_SSN > 0 & LBOV1_2F_fattening_Fout > 0, LBOV1_2F_SSN / LBOV1_2F_fattening_Fout, 0),
      ## proportion of calves for rearing
      LBOV1_2F_fattening_rearing_prop = 1 - LBOV1_2F_fattening_slaughter_prop,
      ## we consider that all calves are either reared or slaughtered, so the sum of the two proportions equals 1
      ## if no sales is reported, we consider that all calves are reared, so the rearing proportion equals 1 and the slaughter proportion equals 0
      ## Observed number of animals
      LBOV1_2F_fattening_rearing_Qobs = LBOV1_2F_fattening_Qobs * LBOV1_2F_fattening_rearing_prop,
      LBOV1_2F_fattening_slaughter_Qobs = LBOV1_2F_fattening_Qobs * LBOV1_2F_fattening_slaughter_prop,
      ## Outflow
      LBOV1_2F_fattening_rearing_Fout = LBOV1_2F_fattening_Fout * LBOV1_2F_fattening_rearing_prop,
      LBOV1_2F_fattening_slaughter_Fout = LBOV1_2F_fattening_Fout * LBOV1_2F_fattening_slaughter_prop,
      ## Inflow
      LBOV1_2F_fattening_rearing_Fin = LBOV1_2F_fattening_Fin * LBOV1_2F_fattening_rearing_prop,
      LBOV1_2F_fattening_slaughter_Fin = LBOV1_2F_fattening_Fin * LBOV1_2F_fattening_slaughter_prop,
      ## residence time
      rt_LBOV1_2F_fattening_rearing = LBOV1_2F_fattening_rearing_Qobs / ((LBOV1_2F_fattening_rearing_Fin + LBOV1_2F_fattening_rearing_Fout) / 2),
      rt_LBOV1_2F_fattening_slaughter = LBOV1_2F_fattening_slaughter_Qobs / ((LBOV1_2F_fattening_slaughter_Fin + LBOV1_2F_fattening_slaughter_Fout) / 2)
    ) |>
    # Split LBOV1_2M into rearing and slaughter categories
    dplyr::mutate(
      ## proportion of calves for slaughter
      LBOV1_2M_slaughter_prop = ifelse(LBOV1_2M_SSN > 0 & LBOV1_2M_Fout > 0, LBOV1_2M_SSN / LBOV1_2M_Fout, 0),
      ## proportion of calves for rearing
      LBOV1_2M_rearing_prop = 1 - LBOV1_2M_slaughter_prop,
      ## we consider that all calves are either reared or slaughtered, so the sum of the two proportions equals 1
      ## if no sales is reported, we consider that all calves are reared, so the rearing proportion equals 1 and the slaughter proportion equals 0
      ## Observed number of animals
      LBOV1_2M_rearing_Qobs = LBOV1_2M_Qobs * LBOV1_2M_rearing_prop,
      LBOV1_2M_slaughter_Qobs = LBOV1_2M_Qobs * LBOV1_2M_slaughter_prop,
      ## Outflow
      LBOV1_2M_rearing_Fout = LBOV1_2M_Fout * LBOV1_2M_rearing_prop,
      LBOV1_2M_slaughter_Fout = LBOV1_2M_Fout * LBOV1_2M_slaughter_prop,
      ## Inflow
      LBOV1_2M_rearing_Fin = LBOV1_2M_Fin * LBOV1_2M_rearing_prop,
      LBOV1_2M_slaughter_Fin = LBOV1_2M_Fin * LBOV1_2M_slaughter_prop,
      ## residence time
      rt_LBOV1_2M_rearing = LBOV1_2M_rearing_Qobs / ((LBOV1_2M_rearing_Fin + LBOV1_2M_rearing_Fout) / 2),
      rt_LBOV1_2M_slaughter = LBOV1_2M_slaughter_Qobs / ((LBOV1_2M_slaughter_Fin + LBOV1_2M_slaughter_Fout) / 2)
    ) |>
    # Split LBOV1 into rearing and slaughter categories
    dplyr::mutate(
      ## proportion of calves for slaughter
      LBOV1_slaughter_prop = ifelse(LBOV1_SSN > 0 & LBOV1_Fout > 0, LBOV1_SSN / LBOV1_Fout, 0),
      ## proportion of calves for rearing
      LBOV1_rearing_prop = 1 - LBOV1_slaughter_prop,
      ## we consider that all calves are either reared or slaughtered, so the sum of the two proportions equals 1
      ## if no sales is reported, we consider that all calves are reared, so the rearing proportion equals 1 and the slaughter proportion equals 0
      ## Observed number of animals
      LBOV1_rearing_Qobs = LBOV1_Qobs * LBOV1_rearing_prop,
      LBOV1_slaughter_Qobs = LBOV1_Qobs * LBOV1_slaughter_prop,
      ## Outflow
      LBOV1_rearing_Fout = LBOV1_Fout * LBOV1_rearing_prop,
      LBOV1_slaughter_Fout = LBOV1_Fout * LBOV1_slaughter_prop,
      ## Inflow
      LBOV1_rearing_Fin = LBOV1_Fin * LBOV1_rearing_prop,
      LBOV1_slaughter_Fin = LBOV1_Fin * LBOV1_slaughter_prop,
      ## residence time
      rt_LBOV1_rearing = LBOV1_rearing_Qobs / ((LBOV1_rearing_Fin + LBOV1_rearing_Fout) / 2),
      rt_LBOV1_slaughter = LBOV1_slaughter_Qobs / ((LBOV1_slaughter_Fin + LBOV1_slaughter_Fout) / 2)
    ) |>
    # replace Inf per NAs
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("rt_|t_1st|offspring"),
        ~ ifelse(!is.finite(.x), NA_real_, .x)
      )
    )
}
