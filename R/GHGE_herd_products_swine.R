#' Calculate GHG emission intensity of swine outputs (meat)
#'
#' @description
#' f_GHGE_herd_output_swine allocates the greenhouse gas emissions of swine
#' herd activities to their respective co-products (pork meat, living animals)
#' using economic allocation, and computes emission intensities per hectare
#' (farm and pseudofarm) and per tonne of product.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Allocate activity emissions to co-products:**
#' - **Meat**: the meat activity produces two co-products — living
#'   animals, and pork meat. As all animals are
#'   involved in the meat activity, all swine herd emissions are summed,
#'   and an economic allocation ratio (econ_alloc_meat) computed from
#'   `object@output$living_animals` and `object@output$meat` is applied.
#'
#' **2. Calculate emission intensity:**
#' Meat production volumes (prod_t) are retrieved from `object@output$meat`,
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
#'   `object@output` (living_animals, meat).
#' @param overwrite Logical. If TRUE, forces recomputation of intermediate
#'   results (f_herd_activities and f_GHGE_herd) rather than reusing
#'   previously cached/stored values. Default is FALSE.
#' @param ... Additional arguments passed to f_herd_activities and
#'   f_GHGE_herd.
#'
#' @return A tibble with one row per farm × output (meat) for swine, containing:
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
#' ghge_output_swine <- f_GHGE_herd_output_swine(f)
#' head(ghge_output_swine)
#' }
#'
#' @seealso f_GHGE_herd, f_herd_activities, f_GHGE_herd_output_swine,
#'   f_GHGE_herd_output_poultry
#'
#' @export
#' @importFrom dplyr filter mutate select summarise inner_join left_join
#'   bind_rows across all_of matches
#' @importFrom stringr str_replace

f_GHGE_herd_output_swine <- function(object,
                                            overwrite = FALSE,
                                            ...) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }


        id_cols = object@traceability$id_cols

        herd_activities = f_herd_activities(object, overwrite = overwrite)
        GHGE_herd = f_GHGE_herd(object, overwrite =  overwrite)

        # 1. Allocate activity emissions to co-products ---------------------------

         ## Meat ----
        # The meat activity yield two co-product: living animals, and pork meat
        ## We economically allocate the impact of the meat activity between these co-products


        # economic allocation ratio between living animals and meat
        econ_alloc_meat <- dplyr::bind_rows(
                object@output$living_animals,
                object@output$meat
        ) |>
                dplyr::filter(species == "swine") |>
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
                dplyr::filter(species == "swine") |>
                # sum activity impact
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ sum(.x, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::left_join(econ_alloc_meat,
                                 by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("kgCO2e|area_ha"),
                                      .fns = ~ .x * econ_ratio)
                )

        # 2. Calculate Emission intensity ----------------------------------------

        # meat production
        meat_prod = object@output$meat |>
                dplyr::filter(species == "swine") |>
                dplyr::summarise(
                        prod_t = sum(prod_t, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output))


        # Split output and add production
        ## meat
        GHGE_meat = GHGE_meat_activity |>
                # add production
                dplyr::inner_join(meat_prod,
                                  by = c(id_cols, 'output'))

        # Intensities

        co2_cols <- names(GHGE_herd)[grepl("kgCO2e", names(GHGE_herd))]

        GHGE_herd_output_swine <- GHGE_meat |>
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

        return(GHGE_herd_output_swine)

}
