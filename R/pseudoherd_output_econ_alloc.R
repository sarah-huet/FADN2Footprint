#' Function to estimate economic allocation ratios between herd outputs
#' `f_herd_output_econ_alloc` Estimate herd outputs
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
#' f_herd_output_econ_alloc(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_output_econ_alloc <- function(object) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Steps:
  ## 1. Estimate off-farm animals output
  ## 2. Estimate economic allocation between outputs or activities

  # 1. Estimate off-farm animals output ------------------------------------------------------------------------------

  if (exists("avrg_output")==F) {
    source("data_raw/data_processing/avrg_herd_output.R")
  }

  ## Note that only off-farm animals of the meat activity produce output (i.e., meat)
  ## Off-farm animals from the milk activity serve to diary cow renewal

  pseudoherd <- f_pseudoherd(object)

  pseudoherd_output <- pseudoherd |>
    # select off-farm animals on the meat activity
    select(all_of(object@traceability$id_cols), FADN_code_letter, Q_off_farm_meat) |>
    filter(Q_off_farm_meat >0) |>
    # detail output and activity
    ## add species
    left_join(
      data_extra$livestock |>
        select(FADN_code_letter,species),
      by = join_by(FADN_code_letter)) |>
    mutate(
      ## explicit activity
      animals = "off-farm",
      activity = "meat",
      # name output
      output = case_when(
        FADN_code_letter == "LBOV1" ~ "meat_veal",
        #FADN_code_letter == "LCOWDAIR" ~ "meat_cull_cow", # no off-farm dairy cows
        species == "cattle" ~ "meat_beef",
        species == "swine" ~ "meat_pork",
        species == "poultry" ~ "meat_chicken",
        .default = "meat"
      )) |>
    # add farm NUTS2 and Organic certification
    left_join(
      object@farm |>
        select(all_of(object@traceability$id_cols),NUTS2,ORGANIC),
      by = object@traceability$id_cols
    ) |>
    # add NUTS2 average output
    left_join(
      avrg_output$NUTS2 |>
        filter( grepl("meat",output)),
      by = join_by(FADN_code_letter, species, NUTS2, ORGANIC,activity,output)
    ) |>
    # add overall average output
    left_join(
      avrg_output$overall |>
        filter( grepl("meat",output)),
      by = join_by(FADN_code_letter, species, ORGANIC,activity,output)
    ) |>
    # replace NAs in NUTS2 average by overall average
    mutate(
      #      prod_t = ifelse(is.na(prod_t_NUTS2),prod_t_all,prod_t_NUTS2),
      #      sales_t = ifelse(is.na(sales_t_NUTS2),sales_t_all,sales_t_NUTS2),
      #      sales_e = ifelse(is.na(sales_e_NUTS2),sales_e_all,sales_e_NUTS2),
      #      output_e = ifelse(is.na(output_e_NUTS2),output_e_all,output_e_NUTS2),
      #      yield = ifelse(is.na(yield_NUTS2),yield_all,yield_NUTS2),
      #      prod_nb = ifelse(is.na(prod_nb_NUTS2),prod_nb_all,prod_nb_NUTS2),
      #      sales_nb = ifelse(is.na(sales_nb_NUTS2),sales_nb_all,sales_nb_NUTS2),
      yield_t_sales_nb = ifelse(is.na(yield_t_sales_nb_NUTS2),yield_t_sales_nb_all,yield_t_sales_nb_NUTS2),
      prop_sales_Qobs = ifelse(is.na(prop_sales_Qobs_NUTS2),prop_sales_Qobs_all,prop_sales_Qobs_NUTS2),
      yield_t_Qobs = ifelse(is.na(yield_t_Qobs_NUTS2),yield_t_Qobs_all,yield_t_Qobs_NUTS2),
      yield_e_Qobs = ifelse(is.na(yield_e_Qobs_NUTS2),yield_e_Qobs_all,yield_e_Qobs_NUTS2)
    ) |>
    select(-matches("_NUTS2|_all")) |>
    # estimate outputs
    ## add live weight
    left_join(
      data_extra$livestock |>
        select(FADN_code_letter,live_weight_kg),
      by = join_by(FADN_code_letter)
    ) |>
    ## estimate outputs
    mutate(
      # estimate sales quantity and value
      sales_t = yield_t_Qobs * Q_off_farm_meat,
      prod_t = sales_t,
      sales_e = yield_e_Qobs * Q_off_farm_meat,
      # estimate number of off-farm animals sold for slaughter
      sales_nb = prod_t / (live_weight_kg*10^-3),
    )

  # unique(pseudoherd_output$FADN_code_letter[is.na(pseudoherd_output$prod_t)])
  ## still NAs for LHEIFBRE as they serve to suckler cow renewal

  # 2. Estimate economic allocation between outputs or activities ------------------------------------------------------------------------------

  # ECONOMIC ALLOCATION RATIO between herd outputs
  pseudoherd_output_econ_alloc <- pseudoherd_output |>
    # sum up sales per species
    group_by(across(all_of(object@traceability$id_cols)),species) |>
    mutate(
      sum_sales_e_species = sum(sales_e,na.rm = T)
    ) |> ungroup() |>
    # sum up sales per activity
    group_by(across(all_of(object@traceability$id_cols)),activity) |>
    mutate(
      sum_sales_e_activity = sum(sales_e,na.rm = T)
    ) |> ungroup() |>
    # sum up
    group_by(across(all_of(object@traceability$id_cols)), species, activity, output,sum_sales_e_species,sum_sales_e_activity) |>
    summarise(
      sales_e = sum(sales_e,na.rm = T),
      .groups = "drop"
    ) |>
    # calculate economic allocation ratio
    mutate(
      econ_alloc_ratio = case_when(
        activity == "milk" ~ sales_e / sum_sales_e_activity,
        activity == "meat" ~ sales_e / sum_sales_e_activity,
        species == "sheep" ~ sales_e / sum_sales_e_species,
        .default = 1
      )

    )


  return(list(outputs = pseudoherd_output,
              econ_alloc_ratio = pseudoherd_output_econ_alloc))




}

utils::globalVariables(c('land_use_type', 'sales_t', 'sales_kg','GE_MJ_kg', 'QVENT3'))
# this is to avoid a note in check package (the issue is from the use of dplyr)

