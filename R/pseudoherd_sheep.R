#' Function to estimate off-farm animals of the sheep herd based on FADN data
#' `f_pseudoherd_sheep` Estimate off-farm animals of the sheep herd
#'
#' @param object a FADN2Footprint object
#' @param overwrite logical, force recomputation
#' @returns
#' A list with rearing parameters and a pseudo-herd tibble containing
#' equilibrium animal numbers for milk and meat activities (in heads).
#'
#' @examples
#' data("mock_data", package = "FADN2Footprint")
#' fadn_fict <- mock_data$fadn_fict[
#'   which(as.numeric(mock_data$fadn_fict$YEAR) >= 2017)[1], ,
#'   drop = FALSE
#' ]
#' fadn_fict_obj = data_4FADN2Footprint(fadn_fict)
#' f_pseudoherd_sheep(object = fadn_fict_obj)
#'
#' @concept practice-pseudoherd
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import stringr

f_pseudoherd_sheep <- function(object,
                               overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  id_cols = object@traceability$id_cols

  ## Steps:
  ## 1. Model farm rearing process
  ## 2. Estimate animals needed for the milk activity
  ## 3. Balance the remaining herd for the meat activity

  # 1. Model farm rearing process ---------------------------------------------------------------------------------

  herd_rearing_param_sheep <- f_herd_rearing_param_sheep(object)

  # 2. On-farm herd activities ---------------------------------------------------------------------------------

  herd_activities <- f_herd_activities(object) |>
       dplyr::filter(species == "sheep")

  # 3. Allocate dairy ewes and their replacement animals to milk -------------

  ## MILK ----

  herd_sheep_milk <- herd_activities |>
    dplyr::select(
      tidyselect::all_of(id_cols),
      FADN_code_letter,
      Qobs_milk
    ) |>
    tidyr::pivot_wider(
      names_from = FADN_code_letter,
      names_glue = "{FADN_code_letter}_{.value}",
      values_from = Qobs_milk,
      values_fill = 0
    )

  for (category in c("LEWEBRE", "LSHEPOTH")) {
    column <- paste0(category, "_Qobs_milk")
    if (!column %in% names(herd_sheep_milk)) {
      herd_sheep_milk[[column]] <- numeric(nrow(herd_sheep_milk))
    }
  }

  pseudoherd_sheep_milk <- herd_sheep_milk |>
       # add rearing parameters
       dplyr::left_join(herd_rearing_param_sheep, by = id_cols) |>
       # estimate observed quantities and times for each production process step
       # first estimate how many animals are needed to renew the dairy ewes
       dplyr::mutate(
            LEWEBRE_Qeq_milk = ifelse(LEWEBRE_Qobs_milk > 0, LEWEBRE_Qobs_milk, 0),

            ## we estimate other sheep category based on the dairy ewes, to ensure dairy ewe renewal
            LSHEPOTH_Qeq_milk = ifelse(
                 LEWEBRE_Qobs_milk > 0,
                 pmax(LSHEPOTH_Qobs_milk, rt_LSHEPOTH * (LEWEBRE_Qeq_milk / rt_LEWEBRE)),
                 0
            )
       ) |>
       dplyr::select(
            tidyselect::all_of(id_cols),
            dplyr::matches("Qeq_milk")
       ) |>
       tidyr::pivot_longer(
            cols = dplyr::matches("Qeq_milk"),
            names_to = "FADN_code_letter",
            values_to = "Qeq_milk"
       ) |>
       dplyr::mutate(
            FADN_code_letter = gsub("_Qeq_milk", "", FADN_code_letter)
       )

  ## MEAT ----

  herd_sheep_meat_aggr <- herd_rearing_param_sheep |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_Qobs_milk$"),
        ~ dplyr::coalesce(.x, 0)
      )
    ) |>
    dplyr::mutate(
      # others (juveniles / fattening)
      Qobs_o = LSHEPOTH_Qobs,
      rt_o = rt_LSHEPOTH,
      # breeders
      Qobs_b = LEWEBRE_Qobs,
      rt_b = rt_LEWEBRE,
      offspring = offspring_LEWEBRE
    )

  ### 3.1. Aggregate the remaining herd by rearing stage ----

  ### Replace missing aggregate parameters with reference values ----

  for (var in c("rt_o", "rt_b", "offspring")) {
    v <- rlang::sym(var)

    herd_sheep_meat_aggr <- herd_sheep_meat_aggr |>
      dplyr::left_join(
        reference_rearing_param$ref_per_NUTS2$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(NUTS2, median) |>
          dplyr::rename(median_NUTS2 = median),
        by = "NUTS2"
      ) |>
      (function(.) {
        overall_tbl <- reference_rearing_param$ref_overall$sheep |>
          dplyr::filter(rearing_param == var) |>
          dplyr::select(median, threshold_down, threshold_up) |>
          dplyr::rename(median_all = median)

        if (nrow(overall_tbl) > 0) {
          cbind(., overall_tbl)
        } else {
          cbind(., tibble::tibble(
            median_all = NA_real_,
            threshold_down = NA_real_,
            threshold_up = NA_real_
          ))
        }
      })() |>
      dplyr::mutate(ref_val = dplyr::case_when(
        !is.finite(median_NUTS2) ~ median_all,
        .default = median_NUTS2
      )) |>
      dplyr::mutate(!!v := dplyr::case_when(
        !is.finite(!!v) ~ ref_val,
        .default = !!v
      )) |>
      dplyr::mutate(!!v := dplyr::case_when(
        !!v < threshold_down ~ threshold_down,
        !!v > threshold_up ~ threshold_up,
        .default = !!v
      )) |>
      dplyr::select(-c(
        median_NUTS2, median_all, ref_val, threshold_down, threshold_up
      ))
  }

  ## Sheep have two stages: LSHEPOTH (juveniles/fattening) and LEWEBRE
  ## (breeders). Reserve the LSHEPOTH animals needed to renew dairy ewes.

  ### 3.2. Estimate meat herd at equilibrium ----
  ## Since there are only two stages (others vs breeders), the pseudo-herd
  ## at equilibrium is estimated as the pair whose numbers are consistent
  ## with each other via the offspring/residence-time relationship.

  herd_sheep_meat_eq <- herd_sheep_meat_aggr |>
       # add herd cattle milk for estimation
       dplyr::left_join(
            herd_sheep_milk |>
                 dplyr::select(
                      dplyr::all_of(id_cols),
                      LSHEPOTH_Qobs_milk,
                      LEWEBRE_Qobs_milk
                 ),
            by = id_cols
       ) |>
       # juveniles sold for slaughter never reach the fattening stage
       # when estimating the juveniles or breeders from fattening, "add" juveniles sold for slaughter
       # When estimating the fattening from juveniles or breeders, "remove" juveniles sold for slaughter
       dplyr::mutate(
            remove_juv = ifelse(
                 LSHEPOTH_SSN > 0 & LSHEPOTH_Fout > 0,
                 (LSHEPOTH_Fout - LSHEPOTH_SSN) / LSHEPOTH_Fout,
                 0
            )
       ) |>
    dplyr::mutate(
      Q_max = case_when(
        ## Qobs_o >= ^Q_o|b estimated from breeders
        Qobs_o >= ifelse(Qobs_b>0,
                         (rt_o*Qobs_b*offspring)* remove_juv,
                         0) ~ "others",
        ## otherwise, breeders is the limiting category
        .default = "breeders"
      )
    ) |>
    dplyr::mutate(
      Qeq_o = case_when(
        Q_max == "others" ~ Qobs_o,
        Q_max == "breeders" ~ (rt_o * Qobs_b * offspring * remove_juv)
      ),
      Qeq_b = case_when(
        Q_max == "others" ~ (Qobs_o/rt_o) * (1/offspring) - LEWEBRE_Qobs_milk,
        Q_max == "breeders" ~ Qobs_b
      )
    ) |>
       # check that Qeq >= Qobs
       dplyr::mutate(
            Qeq_o = pmax(Qeq_o, Qobs_o, na.rm = TRUE),
            Qeq_b = pmax(Qeq_b, Qobs_b, na.rm = TRUE)
       )

  # View(herd_sheep_meat_eq |> summarise(across(everything(), ~sum(is.na(.x)))) |> tidyr::pivot_longer(cols = everything()))
  # View(herd_sheep_meat_eq |> tidyr::pivot_longer(cols = -c(ID,YEAR,NUTS2,Q_max)) |> dplyr::filter(value >0))

  ### 3.3. Estimate balanced number of animals ----
  ## Since each rearing stage maps directly to a single FADN category
  ## (LSHEPOTH for others, LEWEBRE for breeders), no NUTS2-level share
  ## weighting is needed (unlike swine's breeders stage, split across
  ## LSOWBRE and LPIGOTH).

  pseudoherd_sheep_meat <- herd_sheep_meat_eq |>
    dplyr::mutate(
      LSHEPOTH_Qeq_meat = Qeq_o,
      LEWEBRE_Qeq_meat = Qeq_b
    ) |>
    # select columns
    dplyr::select(tidyselect::all_of(object@traceability$id_cols), dplyr::matches("Qeq_meat")) |>
    # pivot table
    tidyr::pivot_longer(
      cols = dplyr::matches("Qeq_meat"),
      names_to = "FADN_code_letter",
      values_to = "Qeq_meat"
    ) |>
    dplyr::mutate(
      FADN_code_letter = gsub("_Qeq_meat","",FADN_code_letter)
    ) |>
    # round values
    dplyr::mutate(
      Qeq_meat = round(Qeq_meat, 2)
    )



  # Output ----

  pseudoherd_sheep <- list(
    # rearing parameters
    rearing_param = herd_sheep_meat_aggr |>
      dplyr::select(tidyselect::all_of(object@traceability$id_cols),matches("rt_|t_1st|offspring")),
    # milk and meat pseudo herd
    pseudoherd = pseudoherd_sheep_milk |>
      dplyr::left_join(
        pseudoherd_sheep_meat,
        by = c(id_cols, "FADN_code_letter")
      ) |>
      dplyr::mutate(species = "sheep")
  )


  return(pseudoherd_sheep)

}
