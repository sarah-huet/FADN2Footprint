#' Compute farm-scale greenhouse gas emissions (GHGE)
#'
#' @description
#' f_GHGE_farm aggregates greenhouse gas emissions at the farm level by
#' combining crop and livestock footprints computed by the package. It
#' produces per‑farm totals for CO2, N2O, CH4 and aggregated CO2‑eq indicators
#' both for the observed farm system (Scope 1&2; "farm") and for the pseudo‑farm that
#' includes emissions embodied in purchased feed and N fertilizer production (Scope 1&2&3; "pseudofarm").
#'
#' @details
#' The function proceeds in three steps:
#'
#' Step 1 – Crop GHGE (f_GHGE_crops):
#' Per-crop emission components are summed across all crops on the farm,
#' including:
#' - direct and indirect soil N2O (crop_N2O_d, crop_N2O_ATD, crop_N2O_L),
#' - diesel combustion (crop_ghg_diesel_crop),
#' - electricity consumption (crop_ghg_elec_crop),
#' - upstream N fertiliser production (crop_ghg_ferti_prod; scope 3).
#' Intensity metrics (per_ha, per_t columns) are excluded from the
#' farm-level summation.
#'
#' Step 2 – Herd and purchased feed GHGE (f_GHGE_herd):
#' Per-animal-category emission components are summed across the herd,
#' including:
#' - enteric CH4 fermentation (herd_CH4_enteric),
#' - manure management CH4 (herd_CH4_MM),
#' - manure management direct N2O (herd_N2O_D_MM),
#' - manure management indirect N2O via grazing (herd_N2O_G_mm) and
#'   leaching/runoff (herd_N2O_L_mm).
#' Intensity metrics (per_ha, per_t, per_anim columns) are excluded.
#' Purchased feed GHGE is derived as the difference between pseudo-farm
#' (all feed including purchased) and on-farm-only feed emissions
#' (herd_purchased_feed_ghge_kgCO2e; scope 3).
#'
#' Step 3 – Farm-scale aggregation:
#' Crop and herd summaries are joined to the farm table and the following
#' aggregate indicators are computed:
#' - farm_total_ghge_kgCO2e: total scope 1 & 2 emissions (on-farm crops +
#'   livestock; note that on-farm feed is already counted in the crop
#'   footprint and is not double-counted).
#' - pseudofarm_total_ghge_kgCO2e: total scope 1, 2 & 3 emissions, adding
#'   upstream fertiliser production and purchased feed emissions.
#' - farm_total_CO2_kgCO2e: scope 1 & 2 CO2 from diesel and electricity.
#' - pseudofarm_total_CO2_kgCO2e: adds scope 3 CO2 from fertiliser
#'   production.
#' - farm_total_N2O_kgCO2e: total N2O from managed soils and manure
#'   management.
#' - farm_total_CH4_kgCO2e: total CH4 from enteric fermentation and
#'   manure management.
#'
#' All NA values in emission components are treated as zero before summation.
#'
#' The function returns cached results stored in
#' `object@footprints$GHGE$GHGE_farm` when present and overwrite = FALSE.
#'
#' @param object An S4 object of class "FADN2Footprint". Required slots used
#'   by the function include at least:
#'   - `object@traceability$id_cols` (character vector of id columns used for joins),
#'   - `object@farm` (farm metadata table),
#'   - underlying helpers `f_GHGE_crops()` and `f_GHGE_herd()` must be available
#'     and compatible with the package naming conventions for emission columns.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @param account_pseudoherd \code{logical}. If \code{TRUE}, off-farm
#'   pseudo-herd outputs estimated from purchased feed are included
#'   alongside on-farm herd outputs in the economic allocation step.
#'   Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to internal helper functions.
#'
#' @return A tibble with one row per farm (identified by
#'   `object@traceability$id_cols`) and columns for each emission component and
#'   aggregate indicator (all in kg CO2-equivalent), including:
#' \describe{
#'   \item{crop}{Per-farm summed crop emission components (prefix crop_).}
#'   \item{herd_}{Per-farm summed herd emission components (prefix herd_).}
#'   \item{herd_purchased_feed_ghge_kgCO2e}{Scope 3 purchased feed emissions.}
#'   \item{farm_total_ghge_kgCO2e}{Total scope 1 & 2 farm GHGE.}
#'   \item{pseudofarm_total_ghge_kgCO2e}{Total scope 1, 2 & 3 GHGE.}
#'   \item{farm_total_CO2_kgCO2e}{Total CO2 scope 1 & 2.}
#'   \item{pseudofarm_total_CO2_kgCO2e}{Total CO2 scope 1, 2 & 3.}
#'   \item{farm_total_N2O_kgCO2e}{Total N2O from soils and manure.}
#'   \item{farm_total_CH4_kgCO2e}{Total CH4 from enteric fermentation and
#'     manure management.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' farm_ghge <- f_GHGE_farm(f)
#' head(farm_ghge)
#'
#' # Include pseudo-herd animals
#' farm_ghge2 <- f_GHGE_farm(f, account_pseudoherd = TRUE)
#'
#' # Force recomputation
#' farm_ghge3 <- f_GHGE_farm(f, overwrite = TRUE)
#' }
#'
#' @seealso f_GHGE_crops, f_GHGE_herd, f_GHGE_herd_output, compute_footprint_ghg
#'
#' @export
#' @concept footprint-ghge
#' @importFrom dplyr select summarise across where matches mutate coalesce
#'   left_join all_of bind_rows
#' @importFrom stringr str_replace




# Steps:
## 1. Estimate crop GHGE
## 2. Estimate herd GHGE
## 3. Sum GHGE at the farm scale


f_GHGE_farm <- function(object,
                        overwrite = FALSE,
                        account_pseudoherd = F, ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_farm)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_farm")
    return(object@footprints$GHGE$GHGE_farm)  # use cached value
  }

  id_cols = object@traceability$id_cols

  # 1. Estimate crop GHGE ------------------------------------------------------------------------------

  crop_GHGE = f_GHGE_crops(object, overwrite =  overwrite)

  crop_GHGE_sum = crop_GHGE |>
    dplyr::select(-dplyr::matches("per_ha$|per_t$")) |>
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),  # Only numeric columns
        list(sum = ~ sum(.x, na.rm = TRUE)),
        .names = "crop_{.col}"
      ),
      .by = id_cols
    )

  # 2. Estimate herd and purchased feed impact ------------------------------------------------------------------------------

  herd_GHGE = f_GHGE_herd(object, overwrite =  overwrite)

  herd_GHGE_sum = herd_GHGE |>
    dplyr::select(-dplyr::matches("per_ha$|per_t$|_per_anim$")) |>
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),  # Only numeric columns
        list(sum = ~ sum(.x, na.rm = TRUE)),
        .names = "herd_{str_replace(.col, '_livcat$', '')}"
      ),
      .by = id_cols
    ) |>
    dplyr::mutate(
      # Purchased feed GHGE = difference between pseudo-farm and on-farm feed emissions
      herd_purchased_feed_ghge_kgCO2e = dplyr::coalesce(herd_feed_pseudofarm_total_ghg_crop_kgCO2e,0) - dplyr::coalesce(herd_feed_farm_total_ghg_crop_kgCO2e,0)
    )

  # 3. Estimate herd energy use -------------------------------------------

  tmp_econ_alloc = f_output_econ_alloc(object)

  ## Heating fuel ----
  ## heating fuel is allocated across all outputs
  tmp_GHGE_fuels = GHGE_fuels(object)
  # select economic allocation across outputs
  tmp_GHGE_fuels_alloc = tmp_econ_alloc$all_outputs |>
    # remove crops
    dplyr::filter(activity != "crop") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_fuels,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::summarise(
      herd_ghg_heat_fuel_kgCO2e = sum(ghg_heat_fuel_kgCO2e * econ_alloc_ratio_farm, na.rm = T),
      .by = id_cols
    )

  ## Electricity ----
  tmp_GHGE_elec = GHGE_elec(object)
  # select economic allocation across outputs
  tmp_GHGE_elec_alloc = tmp_econ_alloc$all_outputs |>
    # remove crops
    dplyr::filter(activity != "crop") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_elec,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::summarise(
      herd_ghg_elec_kgCO2e = sum(ghg_elec_kgCO2e * econ_alloc_ratio_farm, na.rm = T),
      .by = id_cols
    )



  # 4. Sum up GHGE at the farm scale ------------------------------------------------------------------------------


  # TODO: check calculation
  # TODO check diagram



  farm_GHGE <- Reduce(
    x = list(crop_GHGE_sum,
             herd_GHGE_sum,
             tmp_GHGE_fuels_alloc,
             tmp_GHGE_elec_alloc),
    f = function(x,y) dplyr::full_join(x, y,
                                       by = id_cols)
  ) |>
    # replace NAs by zero before summation
    dplyr::mutate(
      dplyr::across(dplyr::matches("_kgCO2e$"), \(x) dplyr::coalesce(x, 0))
    ) |>
    # aggregate GHGE variables at farm scale
    dplyr::mutate(

      # --- Total GHGE ---
      # Scope 1&2: on-farm crop emissions + on-farm livestock emissions + on-farm energy consumption
      farm_total_ghge_kgCO2e = (crop_N2O_d_kgCO2e
                                + crop_N2O_ATD_kgCO2e
                                + crop_N2O_L_kgCO2e
                                + crop_ghg_diesel_crop_kgCO2e
                                + crop_ghg_elec_crop_kgCO2e

                                + herd_CH4_enteric_kgCO2e
                                + herd_CH4_MM_kgCO2e
                                + herd_N2O_D_MM_kgCO2e
                                + herd_N2O_G_mm_kgCO2e
                                + herd_N2O_L_mm_kgCO2e
                                # livestock feed produced on farm is already accounted for in the crop footprint
                                + herd_ghg_heat_fuel_kgCO2e
                                + herd_ghg_elec_kgCO2e
      ),
      # Scope 1&2&3: add upstream purchased feed and N fertilizer production emissions
      pseudofarm_total_ghge_kgCO2e = (farm_total_ghge_kgCO2e
                                      + crop_ghg_ferti_prod_kgCO2e
                                      + herd_purchased_feed_ghge_kgCO2e
                                      ),

      # --- By gas ---
      # CO2: diesel (scope 1) + electricity (scope 2)
      farm_total_CO2_kgCO2e = (crop_ghg_diesel_crop_kgCO2e
                               + crop_ghg_elec_crop_kgCO2e
                               + herd_ghg_heat_fuel_kgCO2e
                               + herd_ghg_elec_kgCO2e
      ),
      # (scope 3)
      pseudofarm_total_CO2_kgCO2e = (farm_total_CO2_kgCO2e
                                     + crop_ghg_ferti_prod_kgCO2e
                                     # share of purchased feed C02
                                     + herd_feed_pseudofarm_ghg_ferti_prod_kgCO2e - herd_feed_farm_ghg_ferti_prod_kgCO2e
                                     + herd_feed_pseudofarm_ghg_diesel_crop_kgCO2e - herd_feed_farm_ghg_diesel_crop_kgCO2e
                                     + herd_feed_pseudofarm_ghg_elec_crop_kgCO2e - herd_feed_farm_ghg_elec_crop_kgCO2e
      ),

      # N2O: managed soils (scope 1) + manure management (scope 1)
      farm_total_N2O_kgCO2e = (crop_N2O_d_kgCO2e
                               + crop_N2O_ATD_kgCO2e
                               + crop_N2O_L_kgCO2e
                               + herd_N2O_D_MM_kgCO2e
                               + herd_N2O_G_mm_kgCO2e
                               + herd_N2O_L_mm_kgCO2e),
      #TODO: add here N2O emitted during N fertilizer production
      pseudofarm_total_N2O_kgCO2e = (farm_total_N2O_kgCO2e
                                     + herd_feed_pseudofarm_N2O_d_kgCO2e - herd_feed_farm_N2O_d_kgCO2e
                                     + herd_feed_pseudofarm_N2O_ATD_kgCO2e - herd_feed_farm_N2O_ATD_kgCO2e
                                     + herd_feed_pseudofarm_N2O_L_kgCO2e - herd_feed_farm_N2O_L_kgCO2e
      ),

      # CH4: enteric fermentation (scope 1) + manure management (scope 1)
      farm_total_CH4_kgCO2e = (herd_CH4_enteric_kgCO2e
                               + herd_CH4_MM_kgCO2e),

      pseudofarm_total_CH4_kgCO2e = (farm_total_CH4_kgCO2e)

    )


  return(
    farm_GHGE
  )

}



