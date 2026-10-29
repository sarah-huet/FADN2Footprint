#' Calculate GHG emission intensity of swine pseudoherd outputs (meat)
#'
#' @description
#' f_GHGE_pseudoherd_output_swine allocates the greenhouse gas emissions of
#' the swine pseudoherd (on-farm and estimated off-farm animals) to their
#' respective co-products (meat, living
#' animals) using economic allocation, and computes emission intensities per
#' hectare (farm and pseudofarm) and per tonne of product.
#'
#' @details
#' The function proceeds in two main steps:
#'
#' **1. Allocate activity emissions to co-products:**
#' - **Meat**: the meat activity produces two co-products — living
#'   animals and meat. All pseudoherd animals are
#'   involved in the meat activity. The economic allocation ratio
#'   (econ_alloc_meat) is computed from both on-farm sales
#'   (`object@output$living_animals`, `object@output$meat`) and estimated
#'   off-farm production (from f_pseudoherd_output_off_farm).
#'
#' **2. Calculate emission intensity:**
#' Meat production volume combines on-farm production
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
#'   `object@farm` (for NUTS2), and `object@output` (living_animals, meat).
#' @param overwrite Logical. If TRUE, forces recomputation of intermediate
#'   results (f_pseudoherd_animals, f_GHGE_pseudoherd,
#'   f_pseudoherd_output_off_farm) rather than reusing previously
#'   cached/stored values. Default is FALSE.
#' @param ... Additional arguments passed to f_pseudoherd_animals and
#'   f_GHGE_pseudoherd.
#'
#' @return A tibble with one row per farm × output (meat, etc.) for swine, containing:
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
#' ghge_pseudoherd_output_swine <- f_GHGE_pseudoherd_output_swine(f)
#' head(ghge_pseudoherd_output_swine)
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


f_GHGE_pseudoherd_output_swine <- function(object,
                                           overwrite = FALSE,
                                           ...) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }


        id_cols = object@traceability$id_cols

        GHGE_pseudoherd = f_GHGE_pseudoherd(object, overwrite =  overwrite)

        # 1. Allocate activity emissions to co-products ---------------------------

        ## Meat ----
        # The meat activity yield three co-product: living animals and meat
        # We sum the GHGE of the whole meat pseudoherd
        ## We economically allocate the impact of the meat activity between these co-products
        # For on-farm animals, the economic allocation is based on observed production
        # For off-farm animals, the economic allocation is based on estimated off-farm production


        # then, we add on-farm and off-farm production
        off_farm_prod = f_pseudoherd_output_off_farm(object, overwrite = overwrite)

        # economic allocation ratio between living animals and meat
        econ_alloc_meat <- dplyr::bind_rows(
                object@output$living_animals,
                object@output$meat,
                off_farm_prod$pseudoherd_output_living_animals,
                off_farm_prod$pseudoherd_output_meat
        ) |>
                dplyr::filter(species == "swine") |>
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
                dplyr::filter(species == "swine") |>
                # sum activity impact
                # number of animals involved in the meat activity = 100%
                dplyr::summarise(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ sum(.x, na.rm = TRUE)),
                        .by = dplyr::all_of(id_cols)) |>
                # allocate impact to co-products
                dplyr::left_join(econ_alloc_meat,
                                 by = id_cols) |>
                dplyr::mutate(
                        dplyr::across(.cols = dplyr::matches("_pseudoherd"),
                                      .fns = ~ .x * econ_ratio)
                )



        # 2. Calculate Emission intensity ----------------------------------------

        # meat production
        # on-farm production of cull cow neat
        # on-farm and off-farm (NUTS2 average) production of beef and veal meat
        meat_prod = object@output$meat |>
                # add off-farm production for meat
                dplyr::bind_rows(off_farm_prod$pseudoherd_output_meat) |>
                dplyr::filter(species == "swine") |>
                dplyr::summarise(
                        prod_t = sum(prod_t, na.rm = TRUE),
                        .by = c(dplyr::all_of(id_cols), output))

        # Split output and add production
        ## meat
        GHGE_meat = GHGE_meat_activity_pseudoherd |>
                # add production
                dplyr::inner_join(meat_prod,
                                  by = c(id_cols, "output"))

        # Intensities

        co2_cols <- names(GHGE_pseudoherd)[grepl("kgCO2e", names(GHGE_pseudoherd)) & grepl("pseudoherd", names(GHGE_pseudoherd))]

        GHGE_pseudoherd_output_swine <- GHGE_meat |>
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


        return(GHGE_pseudoherd_output_swine)

}
