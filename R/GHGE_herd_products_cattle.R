#' Calculate GHG emission intensity of cattle outputs (milk and meat)
#'
#' @description
#' f_GHGE_herd_output_cattle allocates the greenhouse gas emissions of cattle
#' herd activities to their respective co-products (milk, cull cow meat,
#' veal/beef meat, living animals) using economic allocation, and computes
#' emission intensities per hectare (farm and pseudofarm) and per tonne of
#' product.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Allocate activity emissions to co-products:**
#' - **Milk**: the milk activity produces two co-products — milk and cull
#'   cow meat. The number of animals involved in the milk activity is
#'   derived from f_herd_activities (share_milk_act = Qobs_milk / Qobs), and
#'   used to weight the total herd emissions (from f_GHGE_herd). An economic
#'   allocation ratio (econ_alloc_milk) is computed from sales values of
#'   milk and cull cow meat (`object@output$other_herd_products` and
#'   `object@output$meat`), and applied to split the milk activity's emissions
#'   between the two co-products.
#' - **Meat**: the meat activity produces three co-products — living
#'   animals, veal meat and beef meat. Similarly, the share of animals
#'   involved in the meat activity (share_meat_act = Qobs_meat / Qobs) is
#'   used to weight herd emissions, and an economic allocation ratio
#'   (econ_alloc_meat) computed from `object@output$living_animals` and
#'   `object@output$meat` is applied. The cull cow meat emissions from the
#'   milk activity are subsequently added to the meat co-product emissions.
#'
#' **2. Calculate emission intensity:**
#' Milk and meat production volumes (prod_t) are retrieved from
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
#' @return A tibble with one row per farm × output (milk, meat_cull_cow,
#'   veal/beef meat, etc.) for cattle, containing:
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
#' ghge_output_cattle <- f_GHGE_herd_output_cattle(f)
#' head(ghge_output_cattle)
#' }
#'
#' @seealso f_GHGE_herd, f_herd_activities, f_GHGE_herd_output_swine,
#'   f_GHGE_herd_output_poultry
#'
#' @export
#' @importFrom dplyr filter mutate select summarise inner_join left_join
#'   bind_rows across all_of matches
#' @importFrom stringr str_replace

f_GHGE_herd_output_cattle <- function(object,
                                            overwrite = FALSE,
                                            ...) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }


        id_cols = object@traceability$id_cols

        herd_activities = f_herd_activities(object, overwrite = overwrite)
        GHGE_herd = f_GHGE_herd(object, overwrite =  overwrite)

        # 1. Allocate activity emissions to co-products ---------------------------

        ## Milk ----
        # The milk activity yield two co-products: milk and cull cow meat
        ## We economically allocate the impact of the milk activity between these two co-products

        # number of animals involved in the milk activity
        nb_animals_milk <- herd_activities |>
                dplyr::filter(species == "cattle") |>
                dplyr::mutate(
                        share_milk_act = Qobs_milk / Qobs
                ) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_milk_act)

        # economic allocation ratio between cull cow meat and milk
        econ_alloc_milk = dplyr::bind_rows(
                object@output$meat |>
                        dplyr::filter(output == "meat_cull_cow", species == "cattle"),
                object@output$other_herd_products |>
                        dplyr::filter(output == "milk", species == "cattle")
        )|>
                dplyr::summarise(
                        sales_e_output = sum(sales_e, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output, species)
                ) |>
                dplyr::mutate(
                        sum_SV_species = sum(sales_e_output, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), species)
                ) |>
                dplyr::mutate(
                        econ_ratio = sales_e_output / sum_SV_species
                )

        # impact of animals involved in the milk activity
        # final impact allocated to milk vs cull cow meat
        GHGE_milk_activity <- GHGE_herd |>
                # sum activity impact
                dplyr::inner_join(
                        nb_animals_milk,
                        by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ sum(.x * share_milk_act, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::inner_join(
                        econ_alloc_milk,
                        by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ .x * econ_ratio)
                )

        ## Meat ----
        # The meat activity yield three co-product: living animals, veal meat and beef meat
        ## We economically allocate the impact of the meat activity between these co-products
        ## Then we add the cull cow meat impact from the milk activity

        # number of animals involved in the meat activity
        nb_animals_meat  = herd_activities |>
                dplyr::filter(species == "cattle") |>
                dplyr::mutate(
                        share_meat_act = Qobs_meat / Qobs
                ) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_meat_act)


        # economic allocation ratio between living animals and meat
        econ_alloc_meat <- dplyr::bind_rows(
                object@output$living_animals,
                object@output$meat
        ) |>
                dplyr::filter(species == "cattle") |>
                dplyr::summarise(
                        sales_e_output = sum(sales_e, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output, species)
                ) |>
                dplyr::mutate(
                        sum_SV_species = sum(sales_e_output, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), species)
                ) |>
                dplyr::mutate(
                        econ_ratio = sales_e_output / sum_SV_species
                )

        # impact of animals involved in the meat activity
        GHGE_meat_activity <- GHGE_herd |>
                dplyr::filter(species == "cattle") |>
                # sum activity impact
                dplyr::inner_join(nb_animals_meat,
                                  by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ sum(.x * share_meat_act, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::left_join(econ_alloc_meat,
                                 by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ .x * econ_ratio)
                )

        # 2. Calculate Emission intensity ----------------------------------------

        # milk production
        milk_prod = object@output$other_herd_products |>
                dplyr::filter(species == "cattle", output == "milk") |>
                dplyr::select(dplyr::all_of(id_cols), output,
                              prod_t)
        # meat production
        meat_prod = object@output$meat |>
                dplyr::filter(species == "cattle") |>
                dplyr::summarise(
                        prod_t = sum(prod_t, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output))


        # Split output and add production
        ## milk
        GHGE_milk = GHGE_milk_activity |>
                dplyr::filter(output == "milk") |>
                # add production
                dplyr::inner_join(milk_prod,
                                  by = c(id_cols, 'output'))
        ## meat
        GHGE_meat = GHGE_meat_activity |>
                dplyr::bind_rows(
                        GHGE_milk_activity |>
                                dplyr::filter(output == "meat_cull_cow"))|>
                # add production
                dplyr::inner_join(meat_prod,
                                  by = c(id_cols, 'output'))

        # Intensities

        co2_cols <- names(GHGE_herd)[grepl("kgCO2e", names(GHGE_herd))]

        GHGE_herd_output_cattle <- Reduce(
                f = bind_rows,
                x = list(GHGE_milk,
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

        return(GHGE_herd_output_cattle)

}
