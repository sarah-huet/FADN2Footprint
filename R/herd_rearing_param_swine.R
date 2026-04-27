#' Function to estimate off-farm animals of the swine herd based on FADN data
#' `f_herd_rearing_param_swine` Estimate off-farm animals of the swine herd
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
#' f_herd_rearing_param_swine(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_herd_rearing_param_swine <- function(object){
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
  ## 3. Estimate pseudo herd
  ### 3.1. Differentiate farm workshops
  ### 3.2. Balance number of animals in each workshop

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_swine <- object@herd |>
    dplyr::filter(
      species == "swine"
    )

  # if no swine, create a tibble with zeros for swine variables
  if (nrow(herd_swine) == 0) {
    #stop("The input data frame has no poultry.")
    #return(tibble())

    herd_swine <- object@farm |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols)) |>
      dplyr::distinct() |>
      dplyr::left_join(
        herd_swine,
        by = object@traceability$id_cols
      )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_swine_process_init <- herd_swine |>
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
                                   dplyr::filter(species == "swine") |>
                                   pull(FADN_code_letter),
                                 unique(herd_swine$FADN_code_letter)),
                  suffix = c("Qobs","ON","CN","PN","SN")) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        dplyr::mutate(value = 0) |>
        tidyr::pivot_wider(names_from = name,values_from = value)
    ) |>

    ## 2.1. FLOWS ----
  # Flows
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LSOWBRE
    LSOWBRE_Fout = LSOWBRE_SN,
    LSOWBRE_Fin = LSOWBRE_PN + LSOWBRE_CN - LSOWBRE_ON + LSOWBRE_Fout,
    # Flow in LPIGOTH
    LPIGOTH_Fout = LPIGOTH_SN,
    LPIGOTH_Fin = LPIGOTH_PN + LPIGOTH_CN - LPIGOTH_ON + LPIGOTH_Fout,
    # Flow in LPIGFAT
    LPIGFAT_Fout = LPIGFAT_SN,
    LPIGFAT_Fin = LPIGFAT_PN + LPIGFAT_CN - LPIGFAT_ON + LPIGFAT_Fout,
    # Flow in LPIGLET
    LPIGLET_Fout = LPIGLET_SN + (LPIGFAT_Fin-LPIGFAT_PN) + (LSOWBRE_Fin-LSOWBRE_PN) + (LPIGOTH_Fin-LPIGOTH_PN),
    LPIGLET_Fin = LPIGLET_PN + LPIGLET_CN - LPIGLET_ON + LPIGLET_Fout
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
    rt_LSOWBRE = LSOWBRE_Qobs / ((LSOWBRE_Fin+LSOWBRE_Fout)/2),
    rt_LPIGOTH = LPIGOTH_Qobs / ((LPIGOTH_Fin+LPIGOTH_Fout)/2),
    rt_LPIGFAT = LPIGFAT_Qobs / ((LPIGFAT_Fin+LPIGFAT_Fout)/2),
    rt_LPIGLET = LPIGLET_Qobs / ((LPIGLET_Fin + LPIGLET_Fout)/2),
    offspring_LSOWBRE = (LPIGLET_Fin-LPIGLET_PN) / LSOWBRE_Qobs
  ) # |>
  # remove columns with only zeros or NAs
  #dplyr::select(where(~ !is.numeric(.) || is.na(sum(., na.rm = TRUE)) || sum(., na.rm = TRUE) != 0))

  # how many NAs per columns => only in residence time columns
  #View(tmp_df |> summarise(dplyr::across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## Replace outliers with reference values ----

  herd_swine_process_clean <- herd_swine_process_init |>
    # add NUTS2
    dplyr::left_join(object@farm |>
                       dplyr::select(tidyselect::all_of(object@traceability$id_cols),NUTS2),
                     by = object@traceability$id_cols)
  for (var in colnames(herd_swine_process_clean)[grepl("rt_|offspring",colnames(herd_swine_process_clean))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_swine_process_clean <- herd_swine_process_clean |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$swine |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      (function(.) {
        ovrll_tbl <- reference_rearing_param$ref_overall$swine |>
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

  # View(herd_swine_process_clean |> summarise(dplyr::across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## 2.3. MIXED CATEGORIES
  # no mixed categories in swine

  # Output ----

  herd_rearing_param_swine <- herd_swine_process_clean


  return(herd_rearing_param_swine)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

