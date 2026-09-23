#' Calculate GHG emission intensity of poultry outputs (eggs and meat)
#'
#' @description
#' f_GHGE_herd_output_poultry allocates the greenhouse gas emissions of poultry
#' herd activities to their respective co-products (eggs, meat, living animals)
#' using economic allocation, and computes emission intensities per hectare
#' (farm and pseudofarm) and per tonne of product.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Allocate activity emissions to co-products:**
#' - **Eggs**: the eggs activity produces only one product.
#'  The number of animals involved in the eggs activity is
#'   derived from f_herd_activities (share_eggs_act = Qobs_eggs / Qobs), and
#'   used to weight the total herd emissions (from f_GHGE_herd).
#' - **Meat**: the meat activity produces only one product.
#' Similarly, the share of animals involved in the meat activity
#' (share_meat_act = Qobs_meat / Qobs) is used to weight herd emissions.
#'
#' **2. Calculate emission intensity:**
#' Eggs and meat production volumes (prod_t) are retrieved from
#' `object@output$other_herd_products` and `object@output$meat` respectively,
#' and joined to the allocated emissions. Emission intensities are then
#' computed for all GHG-related columns (matching "kgCO2e"):
#' \itemize{
#'   \item per hectare of on-farm feed area (excluding pseudofarm-related
#'     columns), suffixed "_per_ha_farm";
#'   \item per hectare of pseudofarm feed area (excluding farm-related
#'     columns), suffixed "_per_ha_pseudofarm";
#'   \item per tonne of product (prod_t), suffixed "_per_t".
#' }
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing `object@traceability$id_cols` and
#'   object@output (living_animals, meat, other_herd_products).
#' @param overwrite Logical. If TRUE, forces recomputation of intermediate
#'   results (f_herd_activities and f_GHGE_herd) rather than reusing
#'   previously cached/stored values. Default is FALSE.
#' @param ... Additional arguments passed to f_herd_activities and
#'   f_GHGE_herd.
#'
#' @return A tibble with one row per farm × output (eggs, meat) for poultry,
#' containing:
#' \describe{
#'   \item{Allocated GHG emissions}{Columns matching "kgCO2e" and "area_ha",
#'     allocated to each co-product via economic allocation.}
#'   \item{prod_t}{Production volume (tonnes) of the corresponding output.}
#'   \item{Emission intensities}{Additional columns suffixed
#'     "_per_ha_farm", "_per_ha_pseudofarm" and "_per_t", expressing
#'     emissions per hectare (farm or pseudofarm feed area) and per tonne
#'     of product.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' ghge_output_poultry <- f_GHGE_herd_output_poultry(f)
#' head(ghge_output_poultry)
#' }
#'
#' @seealso f_GHGE_herd, f_herd_activities, f_GHGE_herd_output_swine,
#'   f_GHGE_herd_output_poultry
#'
#' @export
#' @import dplyr
#' @import stringr

f_GHGE_herd_output_poultry <- function(object,
                                            overwrite = FALSE,
                                            ...) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }


        id_cols = object@traceability$id_cols

        herd_activities = f_herd_activities(object, overwrite = overwrite)
        GHGE_herd = f_GHGE_herd(object, overwrite =  overwrite)

        # 1. Allocate activity emissions to co-products ---------------------------

        ## eggs ----
        # The eggs activity yield one product: eggs

        # number of animals involved in the eggs activity
        nb_animals_eggs <- herd_activities |>
                dplyr::filter(species == "poultry") |>
                dplyr::mutate(
                        share_eggs_act = Qobs_eggs / Qobs
                ) |>
                dplyr::filter(share_eggs_act >0) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_eggs_act)

        # impact of animals involved in the eggs activity
        GHGE_eggs_activity <- GHGE_herd |>
                # sum activity impact
                dplyr::inner_join(
                        nb_animals_eggs,
                        by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ sum(.x * share_eggs_act, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::mutate(output = "eggs")





        ## Meat ----
        # The meat activity yield three co-product: living animals, veal meat and beef meat
        ## We economically allocate the impact of the meat activity between these co-products
        ## Then we add the cull cow meat impact from the eggs activity

        # number of animals involved in the meat activity
        nb_animals_meat  = herd_activities |>
                dplyr::filter(species == "poultry") |>
                dplyr::mutate(
                        share_meat_act = Qobs_meat / Qobs
                ) |>
                dplyr::filter(share_meat_act >0) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_meat_act)

        # impact of animals involved in the meat activity
        GHGE_meat_activity <- GHGE_herd |>
                dplyr::filter(species == "poultry") |>
                # sum activity impact
                dplyr::inner_join(nb_animals_meat,
                                  by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ sum(.x * share_meat_act, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::mutate(output = "meat_chicken")

        # 2. Calculate Emission intensity ----------------------------------------

        # eggs production
        eggs_prod = object@output$other_herd_products |>
                dplyr::filter(species == "poultry", output == "eggs") |>
                dplyr::select(dplyr::all_of(id_cols), output,
                              prod_t)
        # meat production
        meat_prod = object@output$meat |>
                dplyr::filter(species == "poultry") |>
                dplyr::summarise(
                        prod_t = sum(prod_t, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output))


        # Split output and add production
        ## eggs
        GHGE_eggs = GHGE_eggs_activity |>
                # add production
                dplyr::inner_join(eggs_prod,
                                  by = c(id_cols, 'output'))
        ## meat
        GHGE_meat = GHGE_meat_activity |>
                # add production
                dplyr::inner_join(meat_prod,
                                  by = c(id_cols, 'output'))

        # Intensities

        co2_cols <- names(GHGE_herd)[grepl("kgCO2e", names(GHGE_herd))]

        GHGE_herd_output_poultry <- Reduce(
                f = bind_rows,
                x = list(GHGE_eggs,
                         GHGE_meat))  |>
                # allocate impact per ha and per ton
                dplyr::mutate(
                        # per ha farm
                        dplyr::across(dplyr::all_of(co2_cols[-grep("feed_pseudofarm_|^pseudofarm_ghge_",co2_cols)]),
                                      list(per_ha_farm = ~ .x / feed_farm_area_ha_livcat),
                                      .names = "{str_replace(.col, '_livcat$', '')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        ),
                        # per ha pseudofarm
                        dplyr::across(dplyr::all_of(co2_cols[-grep("feed_farm_|^farm_ghge_",co2_cols)]),
                                      list(per_ha_pseudofarm = ~ .x / feed_pseudofarm_area_ha_livcat),
                                      .names = "{str_replace(.col, '_livcat$', '')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        ),
                        # per t of product
                        dplyr::across(dplyr::all_of(co2_cols),
                                      list(per_t  = ~ .x / prod_t),
                                      .names = "{str_replace(.col, '_livcat$', '')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        )

                )

        return(GHGE_herd_output_poultry)

}
