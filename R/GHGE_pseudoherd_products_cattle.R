#' Calculate GHG emission intensity of cattle pseudoherd outputs (milk and meat)
#'
#' @description
#' f_GHGE_pseudoherd_output_cattle allocates the greenhouse gas emissions of
#' the cattle pseudoherd (on-farm and estimated off-farm animals) to their
#' respective co-products (milk, cull cow meat, veal/beef meat, living
#' animals) using economic allocation, and computes emission intensities per
#' hectare (farm and pseudofarm) and per tonne of product.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Allocate activity emissions to co-products:**
#' - **Milk**: the milk activity produces two co-products — milk and cull
#'   cow meat. The share of pseudoherd animals involved in the milk activity
#'   is derived from f_pseudoherd_animals (share_milk_act_Qeq = Qeq_milk /
#'   Qeq) and used to weight the total pseudoherd emissions (from
#'   f_GHGE_pseudoherd, joined on id_cols and FADN_code_letter). Because
#'   off-farm animals are estimated from the renewal of dairy cows, there
#'   are no off-farm dairy cows and therefore no off-farm milk or cull cow
#'   meat production; the economic allocation ratio (econ_alloc_milk)
#'   between milk and cull cow meat is thus computed solely from on-farm
#'   sales values (`object@output$meat` filtered on "meat_cull_cow" and
#'   `object@output$other_herd_products` filtered on "milk"), and applied
#'   (joined on id_cols) to split the milk activity's pseudoherd emissions
#'   between the two co-products.
#' - **Meat**: the meat activity produces three co-products — living
#'   animals, veal meat and beef meat. The share of pseudoherd animals
#'   involved in the meat activity (share_meat_act_Qeq = Qeq_meat / Qeq) is
#'   used to weight pseudoherd emissions. The economic allocation ratio
#'   (econ_alloc_meat) is computed from both on-farm sales
#'   (`object@output$living_animals`, `object@output$meat`) and estimated
#'   off-farm production (from f_pseudoherd_output_off_farm), after
#'   excluding dairy cows (FADN_code_letter == "LCOWDAIR") since their cull
#'   cow meat impact is already accounted for in the milk activity. The cull
#'   cow meat emissions from the milk activity are subsequently added to the
#'   meat co-product emissions.
#'
#' **2. Calculate emission intensity:**
#' Milk production volume (prod_t) is retrieved from on-farm data only
#' (`object@output$other_herd_products`), since there is no off-farm milk
#' production. Meat production volume combines on-farm production
#' (`object@output$meat`) with estimated off-farm production
#' (`f_pseudoherd_output_off_farm`$pseudoherd_output_meat), summed per farm
#' and output. These production volumes are joined (on id_cols and output)
#' to the allocated pseudoherd emissions, and emission intensities are
#' computed for all pseudoherd GHG-related columns (matching "kgCO2e" and
#' "pseudoherd"):
#' \itemize{
#'   \item per hectare of on-farm feed area (excluding pseudofarm-related
#'     columns), suffixed "_per_ha_farm";
#'   \item per hectare of pseudofarm feed area (excluding farm-related
#'     columns), suffixed "_per_ha_pseudofarm";
#'   \item per tonne of product (prod_t), suffixed "_per_t".
#' }
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing `object@traceability$id_cols`,
#'   `object@farm` (for NUTS2), and object@output (living_animals, meat,
#'   other_herd_products).
#' @param overwrite Logical. If TRUE, forces recomputation of intermediate
#'   results (f_pseudoherd_animals, f_GHGE_pseudoherd,
#'   f_pseudoherd_output_off_farm) rather than reusing previously
#'   cached/stored values. Default is FALSE.
#' @param ... Additional arguments passed to f_pseudoherd_animals and
#'   f_GHGE_pseudoherd.
#'
#' @return A tibble with one row per farm × output (milk, meat_cull_cow,
#'   veal/beef meat, etc.) for cattle, containing:
#' \describe{
#'   \item{Allocated GHG emissions}{Pseudoherd-related columns matching
#'     "kgCO2e" and "pseudoherd", allocated to each co-product via economic
#'     allocation.}
#'   \item{prod_t}{Production volume (tonnes) of the corresponding output,
#'     combining on-farm and (for meat) estimated off-farm production.}
#'   \item{Emission intensities}{Additional columns suffixed
#'     "_per_ha_farm", "_per_ha_pseudofarm" and "_per_t", expressing
#'     emissions per hectare (farm or pseudofarm feed area) and per tonne
#'     of product.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' ghge_pseudoherd_output_cattle <- f_GHGE_pseudoherd_output_cattle(f)
#' head(ghge_pseudoherd_output_cattle)
#' }
#'
#' @seealso f_GHGE_pseudoherd, f_pseudoherd_animals,
#'   f_pseudoherd_output_off_farm, f_GHGE_pseudoherd_output_swine,
#'   f_GHGE_pseudoherd_output_poultry
#'
#' @export
#' @concept practice-pseudoherd
#' @concept footprint-ghge
#' @import dplyr
#' @import stringr


f_GHGE_pseudoherd_output_cattle <- function(object,
                                            overwrite = FALSE,
                                            ...) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }


        id_cols = object@traceability$id_cols

        pseudoherd_activities = f_pseudoherd_animals(object, overwrite = overwrite)
        GHGE_pseudoherd = f_GHGE_pseudoherd(object, overwrite =  overwrite)

        # 1. Allocate activity emissions to co-products ---------------------------

        ## Milk ----
        # The milk activity yield two co-products: milk and cull cow meat
        ## We economically allocate the impact of the milk activity between these two co-products

        # number of animals involved in the milk activity
        nb_animals_milk_Qeq <- pseudoherd_activities |>
                dplyr::filter(species == "cattle") |>
                dplyr::mutate(
                        share_milk_act_Qeq = dplyr::coalesce(Qeq_milk / Qeq, 0)
                ) |>
                dplyr::filter(share_milk_act_Qeq >0) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_milk_act_Qeq)

        # economic allocation ratio between cull cow meat and milk => no needed here
                # as we estimated off-farm animals based on the renewal of dairy cows,
                # there is no dairy cows as off-farm animals, hence no production of cull cow meat or milk off-farm
        # we thus allopcate emissions based on the on-farm production
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


        # pseudoherd output for milk is directly estimated from pseudoherd GHGE
        # because there is no off-farm co-products
        # impact of animals involved in the milk activity
        # final impact allocated to milk vs cull cow meat
        GHGE_milk_activity_pseudoherd <- GHGE_pseudoherd |>
                dplyr::filter(species == "cattle") |>
                # sum activity impact
                dplyr::inner_join(
                        nb_animals_milk_Qeq,
                        by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ sum(.x * share_milk_act_Qeq, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                # add NUTS2
                dplyr::left_join(object@farm |>
                                         dplyr::select(dplyr::all_of(id_cols), NUTS2),
                                 by = id_cols)|>
                dplyr::left_join(
                        econ_alloc_milk,
                        by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ .x * econ_ratio)
                )


        ## Meat ----
        # The meat activity yield three co-product: living animals, veal meat and beef meat
        # We sum the GHGE of the whole meat pseudoherd
        ## We economically allocate the impact of the meat activity between these co-products
        # For on-farm animals, the economic allocation is based on observed production
        # For off-farm animals, the economic allocation is based on estimed off-farm production
        ## Then we add the cull cow meat impact from the milk activity


        # then, we add on-farm and off-farm production

        # number of animals involved in the meat activity
        nb_animals_meat_Qeq  = pseudoherd_activities |>
                dplyr::filter(species == "cattle") |>
                dplyr::mutate(
                        share_meat_act_Qeq = dplyr::coalesce(Qeq_meat / Qeq, 0)
                ) |>
                dplyr::filter(share_meat_act_Qeq >0) |>
                dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, share_meat_act_Qeq)

        off_farm_prod = f_pseudoherd_output_off_farm(object, overwrite = overwrite)



        # economic allocation ratio between living animals and meat
        econ_alloc_meat <- dplyr::bind_rows(
                object@output$living_animals,
                object@output$meat,
                off_farm_prod$pseudoherd_output_living_animals,
                off_farm_prod$pseudoherd_output_meat
        ) |>
                dplyr::filter(species == "cattle") |>
                # remove dairy cows as cull cow meat impact has already been estimated in the milk activity
                dplyr::filter(FADN_code_letter != "LCOWDAIR") |>
                # sum sales per output
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



        # impact of off-farm animals involved in the meat activity
        GHGE_meat_activity_pseudoherd <- GHGE_pseudoherd |>
                dplyr::filter(species == "cattle") |>
                # sum activity impact
                dplyr::inner_join(nb_animals_meat_Qeq,
                                  by = c(id_cols, "FADN_code_letter")) |>
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ sum(.x * share_meat_act_Qeq, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::left_join(econ_alloc_meat,
                                 by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ .x * econ_ratio)
                )



        # 2. Calculate Emission intensity ----------------------------------------

        # milk production
        # no off-farm production for milk
        milk_prod = object@output$other_herd_products |>
                dplyr::filter(species == "cattle", output == "milk") |>
                dplyr::select(dplyr::all_of(id_cols), output,
                              prod_t)

        # meat production
        # on-farm production of cull cow neat
        # on-farm and off-farm (NUTS2 average) production of beef and veal meat
        meat_prod = object@output$meat |>
        # add off-farm production for meat
                dplyr::bind_rows(off_farm_prod$pseudoherd_output_meat) |>
                dplyr::filter(species == "cattle") |>
                dplyr::summarise(
                        prod_t = sum(prod_t, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output))

        # Split output and add production
        ## milk
        GHGE_milk = GHGE_milk_activity_pseudoherd |>
                dplyr::filter(output == "milk") |>
                # add production
                dplyr::inner_join(milk_prod,
                                  by = c(id_cols, "output"))
        ## meat
        GHGE_meat = GHGE_meat_activity_pseudoherd |>
                dplyr::bind_rows(
                        GHGE_milk_activity_pseudoherd |>
                                dplyr::filter(output == "meat_cull_cow"))|>
                # add production
                dplyr::inner_join(meat_prod,
                                  by = c(id_cols, "output"))

        # Intensities

        co2_cols <- names(GHGE_pseudoherd)[grepl("kgCO2e", names(GHGE_pseudoherd)) & grepl("pseudoherd", names(GHGE_pseudoherd))]

        GHGE_pseudoherd_output_cattle <- Reduce(
                f = bind_rows,
                x = list(GHGE_milk,
                         GHGE_meat))  |>
                # allocate impact per ha and per ton
                dplyr::mutate(
                        # per ha farm
                        dplyr::across(dplyr::all_of(co2_cols[-grep("feed_pseudofarm_|^pseudofarm_ghge_",co2_cols)]),
                                      list(per_ha_farm = ~ .x / feed_farm_area_ha_livcat_pseudoherd),
                                      .names = "{str_replace(.col, '_livcat_pseudoherd$', '_pseudoherd')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        ),
                        # per ha pseudofarm
                        dplyr::across(dplyr::all_of(co2_cols[-grep("feed_farm_|^farm_ghge_",co2_cols)]),
                                      list(per_ha_pseudofarm = ~ .x / feed_pseudofarm_area_ha_livcat_pseudoherd),
                                      .names = "{str_replace(.col, '_livcat_pseudoherd$', '_pseudoherd')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        ),
                        # per t of product
                        dplyr::across(dplyr::all_of(co2_cols),
                                      list(per_t  = ~ .x / prod_t),
                                      .names = "{str_replace(.col, '_livcat_pseudoherd$', '_pseudoherd')}_{.fn}"  # Remove "_livcat" and append {.fn}
                        )

                )


        return(GHGE_pseudoherd_output_cattle)

}
