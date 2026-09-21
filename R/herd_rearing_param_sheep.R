#' Function to estimate rearing parameters of the sheep herd based on FADN data
#' `f_herd_rearing_param_sheep` Estimate rearing parameters of the sheep herd
#'
#' @param object a FADN2Footprint object
#' @returns
#' A tibble with, for each farm, the observed quantities, flows, and rearing
#' parameters for the two sheep livestock categories:
#' - LEWEBRE: breeding ewes
#' - LSHEPOTH: other sheep (juveniles / fattening)
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

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_sheep <- object@herd |>
    dplyr::filter(
      species == "sheep"
    )

  # if no sheep, create a tibble with zeros for sheep variables
  if (nrow(herd_sheep) == 0) {
    herd_sheep <- object@farm |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |>
      dplyr::distinct() |>
      dplyr::left_join(
        herd_sheep,
        by = object@traceability$id_cols
      )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_sheep_process_init <- herd_sheep |>
    dplyr::select(tidyselect::all_of(object@traceability$id_cols),FADN_code_letter,Qobs,ON,CN,PN,SN) |>
    tidyr::pivot_wider(
      id_cols = tidyselect::all_of(object@traceability$id_cols),
      names_from = FADN_code_letter,
      values_from = c(Qobs,ON,CN,PN,SN),
      names_glue = "{FADN_code_letter}_{.value}"
    ) |>
    # replace NAs by zeros
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0))) |>
    # add missing livestock categories to prevent errors
    cbind(
      expand.grid(code = setdiff(data_extra$livestock |>
                                   dplyr::filter(species == "sheep") |>
                                   pull(FADN_code_letter),
                                 unique(herd_sheep$FADN_code_letter)),
                  suffix = c("Qobs","ON","CN","PN","SN")) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        dplyr::mutate(value = 0) |>
        tidyr::pivot_wider(names_from = name,values_from = value)
    ) |>

    ## 2.1. FLOWS ----
  ## see diagram in Annex 1 of COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK, 2009. Typology Handbook of agricultural holdings and the standard output (SO) coefficient calculation. (No. RI/CC 1500 rev. 3), COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK. European Commission, Brussels.
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LEWEBRE (breeding ewes)
    LEWEBRE_Fout = LEWEBRE_SN,
    LEWEBRE_Fin = LEWEBRE_PN + LEWEBRE_CN - LEWEBRE_ON + LEWEBRE_Fout,
    # Flow in LSHEPOTH (other sheep: juveniles / fattening)
    LSHEPOTH_Fout = LSHEPOTH_SN,
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
    rt_LSHEPOTH = LSHEPOTH_Qobs / ((LSHEPOTH_Fin+LSHEPOTH_Fout)/2),
    offspring_LEWEBRE = (LSHEPOTH_Fin-LSHEPOTH_PN) / LEWEBRE_Qobs
  ) |>
    # replace Inf per NAs
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("rt_|t_1st|offspring"),
        ~ ifelse(!is.finite(.x), NA_real_, .x)
      ))

  # how many NAs per columns => only in residence time columns
  # View(herd_sheep_process_init |> summarise(dplyr::across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## replace outliers per percentiles ----
  herd_sheep_process_clean <- herd_sheep_process_init |>
    # add NUTS2
    dplyr::left_join(object@farm |>
                       dplyr::select(tidyselect::all_of(object@traceability$id_cols), NUTS2),
                     by = object@traceability$id_cols)

  for (var in colnames(herd_sheep_process_clean)[grepl("rt_|offspring",colnames(herd_sheep_process_clean))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_sheep_process_clean <- herd_sheep_process_clean |>
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

  ## Replace NAs with fallback averages ----

  # identify target variables dynamically
  target_vars_sheep <- colnames(herd_sheep_process_clean)[grepl("rt_|offspring", colnames(herd_sheep_process_clean))]

  tmp_avrg_rearing_param <- h_average_practices(data = herd_sheep_process_clean,
                                                target_vars = target_vars_sheep,
                                                primary_grp = c('YEAR', 'COUNTRY', 'NUTS2'),
                                                secondary_grp = c('COUNTRY'),
                                                weight_var = NULL) |>
    dplyr::rename_with(~ paste0("avrg_", .x),
                       .cols = dplyr::all_of(target_vars_sheep))

  ## add fallback averages
  herd_sheep_process_clean <- herd_sheep_process_clean |>
    dplyr::left_join(tmp_avrg_rearing_param,
                     by = c('YEAR', 'COUNTRY', 'NUTS2')) |>
    # replace NAs with fallback, for each target variable
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(target_vars_sheep),
        ~ ifelse(is.na(.x), get(paste0("avrg_", dplyr::cur_column())), .x)
      )
    ) |>
    # remove fallback variables
    dplyr::select(-dplyr::matches("^avrg_"))

  # View(herd_sheep_process_clean |> summarise(dplyr::across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## 2.3. MIXED CATEGORIES
  # no mixed categories in sheep (only LEWEBRE and LSHEPOTH)

  # Output ----

  herd_rearing_param_sheep <- herd_sheep_process_clean


  return(herd_rearing_param_sheep)

}
