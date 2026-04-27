#' Estimate biodiversity value-impact scores (BVI) for herd outputs (milk, meat, eggs)
#'
#' @description
#' f_BVIAS_herd_output computes biodiversity impact allocations for herd outputs
#' by combining herd output accounting (economic allocation) with feed‑related
#' biodiversity impact estimates (produced by f_BVIAS_feed). The function
#' produces per‑farm activity-level BVI estimates allocated to outputs (e.g.
#' milk) expressed per tonne (BVI_t) and returns results for major herd outputs.
#'
#' @details
#' Workflow implemented by this function:
#' - validate that `object` is an S4 object of class "FADN2Footprint";
#' - use cached results stored in object@footprints$BVIAS$BVI_milk unless
#'   `overwrite = TRUE` (in that case the computation is re-run);
#' - compute herd outputs and economic allocation ratios via
#'   f_herd_output_econ_alloc (or combine on‑farm and pseudoherd outputs when
#'   `account_pseudoherd = TRUE` by calling f_pseudoherd_output_econ_alloc);
#' - compute feed impacts by calling f_BVIAS_feed and aggregate feed impacts at
#'   the farm / activity level (weighted by area required to produce the feed);
#' - join aggregated feed impact to farm output data (for example, milk
#'   production) and allocate the aggregated feed impact to outputs using the
#'   previously computed economic allocation ratios;
#' - return allocated BVI expressed per unit output (BVI_t) together with the
#'   underlying per-farm variables used in the calculation.
#'
#' Notes and assumptions:
#' - The function expects the S4 object to contain or be able to produce the
#'   necessary slots and intermediate data (e.g. object@herd,
#'   object@output$other_herd_products, object@traceability$id_cols). It calls
#'   internal helper functions (f_herd_output_econ_alloc, f_pseudoherd_output_econ_alloc,
#'   f_BVIAS_feed) and therefore those must be available in the package or the
#'   user environment.
#' - When `account_pseudoherd = TRUE`, the function will combine outputs and
#'   economic allocation ratios from on‑farm and pseudoherd estimations, tagging
#'   the on‑farm rows with animals = "on_farm".
#' - The function currently computes and returns results for milk (cow milk),
#'   leaving placeholders (NULL) for meat and eggs; these can be extended in
#'   future versions.
#'
#' @param object An S4 object of class "FADN2Footprint". The object must contain
#'   the traceability id columns referenced in object@traceability$id_cols and
#'   have herd and output information (object@herd and object@output) required
#'   by the helper functions called internally.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @param account_pseudoherd Logical (default FALSE). When FALSE the function
#'   uses economic allocation estimated for the actual herd (f_herd_output_econ_alloc).
#'   When TRUE the function computes and combines both on‑farm and pseudoherd
#'   outputs/allocations (via f_pseudoherd_output_econ_alloc) and merges them so
#'   both sources of animal outputs are accounted for.
#' @param ... Additional arguments (currently ignored) reserved for future use.
#'
#' @return A named list with elements for major herd outputs:
#'   - milk: a tibble/data.frame with one row per traceable farm (as defined in
#'     object@traceability$id_cols) containing production (prod_t), aggregated
#'     feed impact metrics (sum_BVI_ha_feed, BVI_ha), the economic allocation
#'     ratio used, and the allocated biodiversity impact per tonne (BVI_t).
#'   - meat: currently NULL (placeholder).
#'   - eggs: currently NULL (placeholder).
#'
#' @section Caching:
#' If a cached BVI result is present in object@footprints$BVIAS$BVI_milk and
#' `overwrite = FALSE`, the function prints a message and returns the cached
#' object. Use `overwrite = TRUE` to force recomputation.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Compute BVI allocated to milk using default settings
#' res <- f_BVIAS_herd_output(f)
#' head(res$milk)
#'
#' # Recompute and include pseudoherd outputs
#' res2 <- f_BVIAS_herd_output(f, overwrite = TRUE, account_pseudoherd = TRUE)
#' }
#'
#' @seealso f_BVIAS_feed, f_herd_output_econ_alloc, f_pseudoherd_output_econ_alloc
#'
#' @export
#' @concept footprint-biodiv
#' @importFrom dplyr inner_join left_join filter select mutate summarise bind_rows pull across all_of case_when



# Steps:
## 1. Estimate herd outputs (if considering pseudo-farm, estimate associated output)
## 2. Estimate herd feed impact
## 3. Allocate herd impact to outputs

# estimate BVI per ha and per t for herds
f_BVIAS_herd_output <- function(object,
                                overwrite = FALSE,
                                account_pseudoherd = F, ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$BVIAS$BVI_milk) && !overwrite) {
    message("Using cached values stored in object@footprints$BVIAS$BVI_milk")
    return(object@footprints$BVIAS$BVI_milk)  # use cached value
  }

  # 1. Estimate herd outputs ------------------------------------------------------------------------------

  ## output economic allocation ratio
  if (account_pseudoherd == F) {

    herd_output = f_herd_output_econ_alloc(object)

  } else {

    herd_onfarm_output = f_herd_output_econ_alloc(object)
    herd_offfarm_output = f_pseudoherd_output_econ_alloc(object)

    herd_output = list(
      outputs = bind_rows(
        herd_onfarm_output$outputs |>
          dplyr::mutate(animals = "on_farm"),
        herd_offfarm_output$outputs
      ),
      econ_alloc_ratio = bind_rows(
        herd_onfarm_output$econ_alloc_ratio |>
          dplyr::mutate(animals = "on_farm"),
        herd_offfarm_output$econ_alloc_ratio
      )
    )
  }

  # 2. Estimate herd feed impact ------------------------------------------------------------------------------

  herd_feed_impact <- f_BVIAS_feed(object)

  # 3. Aggregate herd feed impact per activity &  Allocate herd impact to outputs ------------------------------------------------------------------------------

  ## 3.1. COW MILK - total feed impact ----

  # Farms producing cow milk

  cow_milk_farms <- dplyr::inner_join(
    # farms with dairy cows
    object@herd |>
      dplyr::filter(FADN_code_letter == "LCOWDAIR" & Qobs >0) |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,Qobs),
    # farms producing cow milk
    object@output$other_herd_products |>
      dplyr::filter(species == "cattle" & output == "milk") |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter_output,prod_t),
    by = object@traceability$id_cols
  ) |>
    dplyr::filter(prod_t >0)
  # 1213 farms
  #length(unique(cow_milk_farms$ID))
  #length(unique(object@herd |> filter(FADN_code_letter == "LCOWDAIR" & Qobs >0) |> pull(ID)))
  #length(unique(RICA_20_raw$ani20$IDENT[RICA_20_raw$ani20$CODE6 == "929" & RICA_20_raw$ani20$EFFEC6 >0]))

  # Activity feed impact
  ## The impact of the feed supplied to the animals involved in this activity equals the total per hectare impact of the area needed to produce the feed
  cow_milk_feed_impact <- herd_feed_impact |>
    dplyr::filter(Qobs_milk >0) |>
    dplyr::summarise(
      BVI_ha = weighted.mean(BVI_ha,area_ha,na.rm = T),
      sum_BVI_ha_feed = sum(BVI_ha*area_ha,na.rm = T),
      .by = c(object@traceability$id_cols)
    )

  #length(unique(cow_milk_feed_impact$ID))

  ## 3.2. COW MILK - impact allocation ----

  cow_milk_impact <- cow_milk_farms |>
    # add total feed impact
    dplyr::left_join(
      cow_milk_feed_impact,
      by = object@traceability$id_cols
    ) |>
    # add economic allocation ratios for milk (to distribute impact among milk and cull cow meat)
    dplyr::left_join(
      herd_output$econ_alloc_ratio |>
        dplyr::filter(species == "cattle" & output == "milk") |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),species,activity,output, econ_alloc_ratio),
      by = object@traceability$id_cols
    ) |>
    # allocate impact to output
    dplyr::mutate(
      BVI_t = (sum_BVI_ha_feed * econ_alloc_ratio) / prod_t
    )

  return(list(
    milk = cow_milk_impact,
    meat = NULL,
    eggs = NULL
  ))

}


