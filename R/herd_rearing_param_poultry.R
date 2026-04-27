#' Function to estimate off-farm animals of the poultry herd based on FADN data
#' `f_herd_rearing_param_poultry` Estimate off-farm animals of the poultry herd
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
#' f_herd_rearing_param_poultry(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_herd_rearing_param_poultry <- function(object){
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

  herd_poultry <- object@herd |>
    filter(
      species == "poultry"
    )

  # if no poultry, create a tibble with zeros for poultry variables
  if (nrow(herd_poultry) == 0) {
    #stop("The input data frame has no poultry.")
    #return(tibble())

    herd_poultry <- object@farm |>
      dplyr::select(all_of(object@traceability$id_cols)) |>
      distinct() |>
      left_join(
      herd_poultry,
      by = object@traceability$id_cols
    )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_poultry_process_init <- herd_poultry |>
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,Qobs,ON,CN,PN,SN) |>
    pivot_wider(
      id_cols = all_of(object@traceability$id_cols),
      names_from = FADN_code_letter,
      values_from = c(Qobs,ON,CN,PN,SN),
      names_glue = "{FADN_code_letter}_{.value}"
    ) |>
    # replace NAs by zeros
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    # add missing livestock categories to prevent errors
    cbind(
      expand.grid(code = setdiff(data_extra$livestock |> filter(species == "poultry") |> pull(FADN_code_letter),unique(herd_poultry$FADN_code_letter)),
                  suffix = c("Qobs","ON","CN","PN","SN")) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        mutate(value = 0) |>
        pivot_wider(names_from = name,values_from = value)
    ) |>

    ## 2.1. FLOWS ----
  # Flows
  ## See Figure in package vignette
  mutate(
    # Flow in LHENSLAY
    LHENSLAY_Fout = LHENSLAY_SN,
    LHENSLAY_Fin = LHENSLAY_PN + LHENSLAY_CN - LHENSLAY_ON + LHENSLAY_Fout,
    # Flow in LPLTRBROYL
    LPLTRBROYL_Fout = LPLTRBROYL_SN,
    LPLTRBROYL_Fin = LPLTRBROYL_PN + LPLTRBROYL_CN - LPLTRBROYL_ON + LPLTRBROYL_Fout,
    # Flow in LPLTROTH
    LPLTROTH_Fout = LPLTROTH_SN,
    LPLTROTH_Fin = LPLTROTH_PN + LPLTROTH_CN - LPLTROTH_ON + LPLTROTH_Fout
  ) |>
    # replace flow values below zero by zeros
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("Fin|Fout"),
        ~ pmax(.x, 0, na.rm = TRUE)
      )
    ) |>

    ## 2.2. REARING PARAMETERS ----
  mutate(
    rt_LHENSLAY = LHENSLAY_Qobs / ((LHENSLAY_Fin+LHENSLAY_Fout)/2),
    rt_LPLTRBROYL = LPLTRBROYL_Qobs / ((LPLTRBROYL_Fin+LPLTRBROYL_Fout)/2),
    rt_LPLTROTH = LPLTROTH_Qobs / ((LPLTROTH_Fin+LPLTROTH_Fout)/2)
  ) # |>
    # remove columns with only zeros or NAs
    #dplyr::select(where(~ !is.numeric(.) || is.na(sum(., na.rm = TRUE)) || sum(., na.rm = TRUE) != 0))

  # how many NAs per columns => only in residence time columns
  #View(tmp_df |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  # Replace outliers with reference values

  herd_poultry_process_clean <- herd_poultry_process_init |>
    # add NUTS2
    left_join(object@farm |>
                dplyr::select(all_of(object@traceability$id_cols),NUTS2),
              by = object@traceability$id_cols)
  for (var in colnames(herd_poultry_process_clean)[grepl("rt_",colnames(herd_poultry_process_clean))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_poultry_process_clean <- herd_poultry_process_clean |>
      # Join NUTS2 medians
      left_join(
        reference_rearing_param$ref_per_NUTS2$poultry |>
          filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      (function(.) {
        ovrll_tbl <- reference_rearing_param$ref_overall$poultry |>
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
      mutate(ref_val = case_when(
        !is.finite(median_NUTS2) ~ median_all,
        .default = median_NUTS2
      )) |>
      # replace
      mutate(!!v := case_when(
        !is.finite(!!v) ~ ref_val,
        .default = !!v
      )) |>
      mutate(!!v := case_when(
        (!!v < threshold_down) ~ threshold_down,
        (!!v > threshold_up) ~ threshold_up,
        .default = !!v
      )) |>
      dplyr::select(-c(median_NUTS2,median_all,ref_val,threshold_down,threshold_up))
  }

  # View(herd_poultry_process_clean |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## 2.3. MIXED CATEGORIES
  # no mixed categories in poultry


  # Output ----

  herd_rearing_param_poultry <- herd_poultry_process_clean

  return(herd_rearing_param_poultry)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

