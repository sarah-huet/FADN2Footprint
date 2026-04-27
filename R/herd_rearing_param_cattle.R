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
#' data(fadn_fict)
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_herd_rearing_param_cattle(object = fadn_fict_obj)
#'
#' @concept practice-herding
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_herd_rearing_param_cattle <- function(object){
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

  herd_cattle <- object@herd |>
    dplyr::filter(
      species == "cattle"
    )

  # if no cattle, create a tibble with zeros for cattle variables
  if (nrow(herd_cattle) == 0) {
    #stop("The input data frame has no poultry.")
    #return(tibble())

    herd_cattle <- object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols)) |>
      dplyr::distinct() |>
      dplyr::left_join(
        herd_cattle,
        by = object@traceability$id_cols
      )
  }

  # 2. Modeling farm rearing process ---------------------------------------------------------------------------------

  herd_cattle_process_init <- herd_cattle |>
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
                                   dplyr::filter(species == "cattle") |>
                                   dplyr::pull(FADN_code_letter),
                                 unique(herd_cattle$FADN_code_letter)),
                  suffix = c("Qobs","ON","CN","PN","SN","SSN")) |>
        transmute(name = paste(code, suffix, sep = "_")) |>
        dplyr::mutate(value = 0) |>
        tidyr::pivot_wider(names_from = name,values_from = value)
    ) |>

    ## 2.1. FLOWS ----
  ## see diagram in Annex 1 of COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK, 2009. Typology Handbook of agricultural holdings and the standard output (SO) coefficient calculation. (No. RI/CC 1500 rev. 3), COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK. European Commission, Brussels.
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LCOWDAIR
    LCOWDAIR_Fout = LCOWDAIR_SN,
    ## LCOWDAIR_Fin = LCOWDAIR_PN + LCOWDAIR_CN - LCOWDAIR_ON + LCOWDAIR_SN, # no ON and CN variables for LCOWDAIR
    LCOWDAIR_Fin = LCOWDAIR_PN + LCOWDAIR_Fout,
    # Flow in LCOWOTH
    LCOWOTH_Fout = LCOWOTH_SN,
    LCOWOTH_Fin = LCOWOTH_PN + LCOWOTH_CN - LCOWOTH_ON + LCOWOTH_Fout,
    # Flow in LBOV2
    LBOV2_Fout = LBOV2_SN,
    LBOV2_Fin = LBOV2_PN + LBOV2_CN - LBOV2_ON + LBOV2_Fout,
    # Flow in LHEIFFAT
    LHEIFFAT_Fout = LHEIFFAT_SN,
    LHEIFFAT_Fin = LHEIFFAT_PN + LHEIFFAT_CN - LHEIFFAT_ON + LHEIFFAT_Fout,

    # Flow in LHEIFBRE
    LHEIFBRE_Fout = LHEIFBRE_SN + (LCOWDAIR_Fin-LCOWDAIR_PN) + (LCOWOTH_Fin - LCOWOTH_PN),
    LHEIFBRE_Fin = LHEIFBRE_PN + LHEIFBRE_CN - LHEIFBRE_ON + LHEIFBRE_Fout,

    # Flow in LBOV1_2F
    LBOV1_2F_Fout = LBOV1_2F_SN + (LHEIFFAT_Fin-LHEIFFAT_PN) + (LHEIFBRE_Fin-LHEIFBRE_PN),
    LBOV1_2F_Fin = LBOV1_2F_PN + LBOV1_2F_CN - LBOV1_2F_ON + LBOV1_2F_Fout,

    # Flow in LBOV1_2M
    LBOV1_2M_Fout = LBOV1_2M_SN + (LBOV2_Fin-LBOV2_PN),
    LBOV1_2M_Fin = LBOV1_2M_PN + LBOV1_2M_CN - LBOV1_2M_ON + LBOV1_2M_Fout,

    # Flow in LBOV1
    LBOV1_Fout = LBOV1_SN + (LBOV1_2F_Fin-LBOV1_2F_PN) + (LBOV1_2M_Fin-LBOV1_2M_PN),
    LBOV1_Fin = LBOV1_PN + LBOV1_CN - LBOV1_ON + LBOV1_Fout
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
    rt_LCOWDAIR = LCOWDAIR_Qobs / ((LCOWDAIR_Fin+LCOWDAIR_Fout)/2),
    rt_LCOWOTH = LCOWOTH_Qobs / ((LCOWOTH_Fin+LCOWOTH_Fout)/2),
    rt_LBOV2 = LBOV2_Qobs / ((LBOV2_Fin+LBOV2_Fout)/2),
    rt_LHEIFFAT = LHEIFFAT_Qobs / ((LHEIFFAT_Fin+LHEIFFAT_Fout)/2),

    rt_LBOV1_2M = LBOV1_2M_Qobs / ((LBOV1_2M_Fin+LBOV1_2M_Fout)/2),
    rt_LHEIFBRE = LHEIFBRE_Qobs / ((LHEIFBRE_Fin+LHEIFBRE_Fout)/2),

    rt_LBOV1_2F = LBOV1_2F_Qobs / ((LBOV1_2F_Fin+LBOV1_2F_Fout)/2),

    rt_LBOV1 = LBOV1_Qobs / ((LBOV1_Fin + LBOV1_Fout)/2),

    t_1st_calve_heiffers = ((2+rt_LHEIFBRE)*LHEIFBRE_Qobs) / LHEIFBRE_Qobs, ## LHEIFBRE have at least 2 y.o.
    offspring_cows = (LBOV1_Fin-LBOV1_PN) / ( LCOWDAIR_Qobs + LCOWOTH_Qobs )
  ) #|>
  # remove columns with only zeros or NAs
  #select(where(~ !is.numeric(.) || is.na(sum(., na.rm = TRUE)) || sum(., na.rm = TRUE) != 0))



  # how many NAs per columns => only in residence time columns
  # View(herd_cattle_process_init |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## Replace outliers with reference values ----

  herd_cattle_process_clean1 <- herd_cattle_process_init |>
    # add NUTS2
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(object@traceability$id_cols),NUTS2),
                     by = object@traceability$id_cols)

  for (var in colnames(herd_cattle_process_clean1)[grepl("rt_|t_1st|offspring",colnames(herd_cattle_process_clean1))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_cattle_process_clean1 <- herd_cattle_process_clean1 |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      (function(.) {
        ovrll_tbl <- reference_rearing_param$ref_overall$cattle |>
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

  herd_cattle_process <- herd_cattle_process_clean1 |>
    dplyr::mutate(

      # LBOV1_2F
      ## Total number of animals in downward rearing stages
      LBOV1_2F_total_downward =
        coalesce(LHEIFFAT_Qobs/rt_LHEIFFAT,0)
      + coalesce(LHEIFBRE_Qobs/rt_LHEIFBRE,0)
      +  coalesce(LCOWDAIR_Qobs/rt_LCOWDAIR,0)
      +  coalesce(LCOWOTH_Qobs/rt_LCOWOTH,0),
      ## Total number of animals in downward fattening rearing stages
      LBOV1_2F_total_downward_fattening =
        coalesce(LHEIFFAT_Qobs/rt_LHEIFFAT,0),
      ## Total number of animals in downward breeders rearing stages
      LBOV1_2F_total_downward_breeders =
        coalesce(LHEIFBRE_Qobs/rt_LHEIFBRE,0)
      +  coalesce(LCOWDAIR_Qobs/rt_LCOWDAIR,0)
      +  coalesce(LCOWOTH_Qobs/rt_LCOWOTH,0),
      ## proportion of fattening
      LBOV1_2F_fattening_prop = coalesce(LBOV1_2F_total_downward_fattening / LBOV1_2F_total_downward,0),
      ## proportion of breeding
      LBOV1_2F_breeders_prop = coalesce(LBOV1_2F_total_downward_breeders / LBOV1_2F_total_downward,0),

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
      rt_LBOV1_2F_fattening = LBOV1_2F_fattening_Qobs / ((LBOV1_2F_fattening_Fin+LBOV1_2F_fattening_Fout)/2),
      rt_LBOV1_2F_breeders = LBOV1_2F_breeders_Qobs  / ((LBOV1_2F_breeders_Fin+LBOV1_2F_breeders_Fout)/2)
    ) |>
    # rearing parameter including mixed categories
    ## LBOV1_2F have at least 1 y.o., LHEIFBRE have at least 2 y.o.
    dplyr::mutate(
      t_1st_calve = ( (1+rt_LBOV1_2F_breeders)*LBOV1_2F_breeders_Qobs + ((2+rt_LHEIFBRE)*LHEIFBRE_Qobs) ) / ( LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs ),
      offspring_b = (LBOV1_Fin-LBOV1_PN) / ( LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs + LCOWDAIR_Qobs + LCOWOTH_Qobs )
    ) |>
    ungroup()

  # View(herd_cattle_process |> summarise(across(everything(), ~sum(is.na(.x)))) |> pivot_longer(cols = everything()))

  ## Replace outliers with reference values for mixed categories ----

  herd_cattle_process_clean2 <- herd_cattle_process

  for (var in setdiff(colnames(herd_cattle_process),
                      colnames(herd_cattle_process_clean1))[grepl("rt_|t_1st|offspring",setdiff(colnames(herd_cattle_process),
                                                                                                colnames(herd_cattle_process_clean1)))]) {

    v <- rlang::sym(var)

    # Join and replace
    herd_cattle_process_clean2 <- herd_cattle_process_clean2 |>
      # Join NUTS2 medians
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$cattle |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2,median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2") |>
      # join overall medians and thresholds
      cbind(
        reference_rearing_param$ref_overall$cattle |>
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

  # View(herd_cattle_process_clean2 |> summarise(across(everything(), ~sum(is.finite(.x)))) |> pivot_longer(cols = everything()))

  # Output ----

  herd_rearing_param_cattle <- herd_cattle_process_clean2


  return(herd_rearing_param_cattle)

}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

