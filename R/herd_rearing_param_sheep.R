#' Function to estimate off-farm animals of the sheep herd based on FADN data
#' `f_herd_rearing_param_sheep` Estimate off-farm animals of the sheep herd
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
#' f_herd_rearing_param_sheep(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_herd_rearing_param_sheep <- function(object){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Retrieve observed herd structure
  ### We considered the observed animal stock as the mean between average, opening, and closing variables
  ## 2. Modeling farm rearing process
  ### 2.1. Estimate animal flows between livestock categories
  ### 2.2. Estimate rearing parameters
  ### 2.3. Estimate values for mixed categories
  ### 2.4. Replace outliers with reference values

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_sheep <- object@herd |>
    dplyr::filter(
      species == "sheep"
    )

  # if no sheep, create a tibble with zeros for sheep variables
  if (nrow(herd_sheep) == 0) {
    #stop("The input data frame has no poultry.")
    #return(tibble())

    herd_sheep <- object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols)) |>
      dplyr::distinct() |>
      dplyr::left_join(
        herd_sheep,
        by = object@traceability$id_cols
      )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_sheep_process_init <- herd_sheep |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,Qobs,ON,CN,PN,SN,SSN) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(object@traceability$id_cols),
      names_from = FADN_code_letter,
      values_from = c(Qobs,ON,CN,PN,SN,SSN),
      names_glue = "{FADN_code_letter}_{.value}"
    ) |>
    # replace NAs by zeros
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~tidyr::replace_na(., 0))) |>
    # add missing livestock categories to prevent errors
    cbind(
      expand.grid(code = setdiff(data_extra$livestock |>
                                   dplyr::filter(species == "sheep") |>
                                   dplyr::pull(FADN_code_letter),
                                 unique(herd_sheep$FADN_code_letter)),
                  suffix = c("Qobs","ON","CN","PN","SN","SSN")) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        dplyr::mutate(value = 0) |>
        tidyr::pivot_wider(names_from = name,values_from = value)
    ) |>

    ## 2.1. FLOWS ----
  ## see diagram in Annex 1 of COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK, 2009. Typology Handbook of agricultural holdings and the standard output (SO) coefficient calculation. (No. RI/CC 1500 rev. 3), COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK. European Commission, Brussels.
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LEWEBRE
    LEWEBRE_Fout = LEWEBRE_SN,
    ## LEWEBRE_Fin = LEWEBRE_PN + LEWEBRE_CN - LEWEBRE_ON + LEWEBRE_SN, # no ON and CN variables for LEWEBRE
    LEWEBRE_Fin = LEWEBRE_PN + LEWEBRE_Fout,

    # Flow in LSHEPOTH
    LSHEPOTH_Fout = LSHEPOTH_SN + (LEWEBRE_Fin - LEWEBRE_PN),
    LSHEPOTH_Fin = LSHEPOTH_PN + LSHEPOTH_CN - LSHEPOTH_ON + LSHEPOTH_Fout
    ) |>
    # replace flow values below zero by zeros
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("Fin|Fout"),
        ~ pmax(.x, 0, na.rm = TRUE)
      )
    ) |>

    ## 2.2. REARING PARAMETERS ----
  dplyr::mutate(
    rt_LEWEBRE = LEWEBRE_Qobs / ((LEWEBRE_Fin+LEWEBRE_Fout)/2),

    rt_LSHEPOTH = LSHEPOTH_Qobs / ((LSHEPOTH_Fin+LSHEPOTH_Fout)/2)
    ) #|>
  # remove columns with only zeros or NAs
  #select(where(~ !is.numeric(.) || is.na(sum(., na.rm = TRUE)) || sum(., na.rm = TRUE) != 0))



  # how many NAs per columns => only in residence time columns
  # View(herd_sheep_process_init |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## Replace outliers with reference values ----

  herd_sheep_process_clean1 <- herd_sheep_process_init |>
    # add NUTS2
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(object@traceability$id_cols),NUTS2),
                     by = object@traceability$id_cols)

  for (var in colnames(herd_sheep_process_clean1)[grepl("rt_|t_1st|offspring",colnames(herd_sheep_process_clean1))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_sheep_process_clean1 <- herd_sheep_process_clean1 |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      (function(.) {
        ovrll_tbl <- reference_rearing_param$ref_overall$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median, threshold_down, threshold_up) |>
          dplyr::rename(median_all = median)

        if (nrow(ovrll_tbl) > 0) {
          cbind(., ovrll_tbl)
        } else {
          cbind(., tibble(median_all = NA,
                          threshold_down = NA,
                          threshold_up = NA))
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
      dplyr::select(-c(median_NUTS2,median_all,ref_val,threshold_down,threshold_up))
  }

  ## 2.3. MIXED CATEGORIES ----

  herd_sheep_process <- herd_sheep_process_clean1 |>
    dplyr::mutate(
      ## Total number of animals in downward rearing stages
      LSHEPOTH_total_downward = coalesce(LEWEBRE_Qobs/rt_LEWEBRE,0) + LSHEPOTH_SN,
  ## Total number of animals in downward fattening rearing stages
  LSHEPOTH_total_downward_fattening = LSHEPOTH_SN,
  ## Total number of animals in downward breeders rearing stages
  LSHEPOTH_total_downward_breeders = coalesce(LEWEBRE_Qobs/rt_LEWEBRE,0),
  ## proportion of fattening
  LSHEPOTH_fattening_prop = coalesce(LSHEPOTH_total_downward_fattening / LSHEPOTH_total_downward,0),
  ## proportion of breeding
  LSHEPOTH_breeders_prop = coalesce(LSHEPOTH_total_downward_breeders / LSHEPOTH_total_downward,0),

  ## Observed number of animals
  LSHEPOTH_fattening_Qobs = LSHEPOTH_Qobs * LSHEPOTH_fattening_prop,
  LSHEPOTH_breeders_Qobs = LSHEPOTH_Qobs * LSHEPOTH_breeders_prop,

  ## Outflow
  LSHEPOTH_fattening_Fout = LSHEPOTH_Fout * LSHEPOTH_fattening_prop,
  LSHEPOTH_breeders_Fout = LSHEPOTH_Fout * LSHEPOTH_breeders_prop,
  ## Inflow
  LSHEPOTH_fattening_Fin = LSHEPOTH_Fin * LSHEPOTH_fattening_prop,
  LSHEPOTH_breeders_Fin = LSHEPOTH_Fin * LSHEPOTH_breeders_prop,

  ## residence time
  rt_LSHEPOTH_fattening = LSHEPOTH_fattening_Qobs / ((LSHEPOTH_fattening_Fin+LSHEPOTH_fattening_Fout)/2),
  rt_LSHEPOTH_breeders = LSHEPOTH_breeders_Qobs  / ((LSHEPOTH_breeders_Fin+LSHEPOTH_breeders_Fout)/2)
) |>
    # rearing parameter including mixed categories
    ## LSHEPOTH have at least 1 y.o., LHEIFBRE have at least 2 y.o.
    dplyr::mutate(
      offspring_b = (LSHEPOTH_Fin-LSHEPOTH_PN) / LEWEBRE_Qobs
      ) |>
    ungroup()

  # View(herd_sheep_process |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## Replace outliers with reference values for mixed categories ----

  herd_sheep_process_clean2 <- herd_sheep_process

  for (var in setdiff(colnames(herd_sheep_process),
                      colnames(herd_sheep_process_clean1))[grepl("rt_|t_1st|offspring",setdiff(colnames(herd_sheep_process),
                                                                                                colnames(herd_sheep_process_clean1)))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_sheep_process_clean2 <- herd_sheep_process_clean2 |>
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
      dplyr::select(-c(median_NUTS2,median_all,ref_val,threshold_down,threshold_up))
  }

  # View(herd_sheep_process_clean2 |> summarise(across(everything(), ~sum(is.finite(.x)))) |> pivot_longer(cols = everything()))

  # Output ----

  herd_rearing_param_sheep <- herd_sheep_process_clean2


  return(herd_rearing_param_sheep)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

