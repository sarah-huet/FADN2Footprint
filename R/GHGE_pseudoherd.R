#' Compute greenhouse gas emissions from pseudoherd livestock and their feed
#'
#' @description
#' f_GHGE_herd estimates greenhouse gas emissions (GHGE) associated with
#' livestock farming, combining direct animal emissions (enteric fermentation
#' and manure management) with the emissions embodied in the feed consumed
#' (both on-farm produced and purchased). Results are expressed in kg
#' CO2-equivalent per livestock category and per animal, under both a farm
#' (scope 1 & 2) and a pseudo-farm (scope 1, 2 & 3) boundary.
#'
#' @details
#' The function proceeds in three steps:
#'
#' **Step 1 – Animal emissions:**
#' Direct emissions are computed per livestock category (FADN_code_letter)
#' by calling:
#' - GHGE_ch4_enteric(object): enteric fermentation CH4 (kg CO2e per
#'   livestock category, CH4_enteric_kgCO2e_livcat),
#' - GHGE_ch4_manure(object): manure management CH4
#'   (CH4_MM_kgCO2e_livcat),
#' - GHGE_n2o_manure(object): manure management N2O, disaggregated into
#'   direct (N2O_D_MM_kgCO2e_livcat), grazing indirect
#'   (N2O_G_mm_kgCO2e_livcat) and leaching/runoff indirect
#'   (N2O_L_mm_kgCO2e_livcat).
#' The three tables are joined by traceability id columns and
#' FADN_code_letter. Heating fuels and electricity for livestock buildings
#' are not included here; they are accounted for per activity in
#' f_GHGE_herd_output.
#'
#' **Step 2 – Feed emissions (f_GHGE_feed):**
#' Feed-related GHGE (kg CO2e per t DM) are retrieved and summed across all
#' feed items per livestock category and feed origin ("feed_produced" vs
#' "feed_purchased"), weighted by dry matter quantity (DM_t_livcat).
#' The results are pivoted wide by feed origin and a pseudo-farm total is
#' constructed as the sum of on-farm and purchased feed emissions:
#' - **feed_farm_***: emissions from on-farm produced feed only (scope 1 & 2
#'   boundary; note that crop footprint already accounts for these).
#' - **feed_pseudofarm_***: total feed emissions including purchased feed
#'   (scope 3 boundary).
#' Column names ending in _per_t are renamed to _livcat after weighting.
#'
#' **Step 3 – Combined herd impact:**
#' Animal and feed emission tables are joined to the herd table
#' (`object@herd`) by traceability id columns and FADN_code_letter.
#' Aggregate indicators are computed:
#' - **farm_ghge_herd_kgCO2e_livcat**: total scope 1 & 2 herd GHGE per
#'   livestock category (on-farm feed + CH4 enteric + CH4 MM + N2O MM).
#' - **pseudofarm_ghge_herd_kgCO2e_livcat**: total scope 1, 2 & 3 herd
#'   GHGE per livestock category (pseudo-farm feed + CH4 + N2O).
#' - **farm_ghge_herd_kgCO2e_per_anim**: farm total divided by observed
#'   animal count (Qobs).
#' - **pseudofarm_ghge_herd_kgCO2e_per_anim**: pseudo-farm total divided
#'   by Qobs.
#'
#' The function returns cached results stored in
#' object@footprints$GHGE$GHGE_herd when present and `overwrite = FALSE`.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow. The object must provide:
#'   - `object@traceability$id_cols`: character vector of farm identifier
#'     column names,
#'   - `object@herd`: herd composition table with FADN_code_letter, species
#'     and Qobs columns,
#'   - all slots required by GHGE_ch4_enteric, GHGE_ch4_manure,
#'     GHGE_n2o_manure and f_GHGE_feed.
#' @param overwrite Logical, default FALSE. If FALSE and
#'   `object@footprints$GHGE$GHGE_herd` is not NULL, the cached table is
#'   returned with a message. Set to TRUE to force recomputation.
#' @param account_pseudoherd Logical, default FALSE. When TRUE, pseudo-herd
#'   animals (inferred from feed flows rather than directly observed) are
#'   included in the emission calculation. Currently reserved for future
#'   implementation.
#' @param ... Additional arguments.
#'
#' @return A tibble with one row per farm × livestock category combination
#'   (identified by `object@traceability$id_cols` and `FADN_code_letter`) and
#'   columns including:
#' \describe{
#'   \item{`species`}{Livestock species label.}
#'   \item{`Qobs`}{Observed animal count.}
#'   \item{`feed_farm_*`}{On-farm feed dry matter, area, and emission components (kg CO2e per livestock category).}
#'   \item{`feed_pseudofarm_*`}{Pseudo-farm (on-farm + purchased) feed dry matter, area, and emission components.}
#'   \item{`CH4_enteric_kgCO2e_livcat`}{Enteric fermentation CH4 emissions.}
#'   \item{`CH4_MM_kgCO2e_livcat`}{Manure management CH4 emissions.}
#'   \item{`N2O_D_MM_kgCO2e_livcat`}{Direct manure management N2O.}
#'   \item{`N2O_G_mm_kgCO2e_livcat`}{Indirect N2O from grazing.}
#'   \item{`N2O_L_mm_kgCO2e_livcat`}{Indirect N2O from leaching/runoff.}
#'   \item{`farm_ghge_herd_kgCO2e_livcat`}{Total scope 1 & 2 herd GHGE.}
#'   \item{`pseudofarm_ghge_herd_kgCO2e_livcat`}{Total scope 1, 2 & 3 GHGE.}
#'   \item{`farm_ghge_herd_kgCO2e_per_anim`}{Scope 1 & 2 GHGE per animal.}
#'   \item{`pseudofarm_ghge_herd_kgCO2e_per_anim`}{Scope 3 GHGE per animal.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' herd_ghge <- f_GHGE_herd(f)
#' head(herd_ghge)
#'
#' # Include pseudo-herd animals
#' herd_ghge2 <- f_GHGE_herd(f, account_pseudoherd = TRUE)
#'
#' # Force recomputation
#' herd_ghge3 <- f_GHGE_herd(f, overwrite = TRUE)
#' }
#'
#' @seealso f_GHGE_ch4_enteric(), f_GHGE_ch4_manure(), f_GHGE_n2o_manure(),
#'   f_GHGE_feed(), f_GHGE_herd_output(), f_GHGE_farm()
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories}, Volume 4: Agriculture, Forestry and Other Land Use.
#' Intergovernmental Panel on Climate Change.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for
#' National Greenhouse Gas Inventories}, Volume 4. IPCC.
#'
#' @concept footprint-ghge
#' @export
#' @importFrom dplyr select left_join summarise across matches mutate rename_with starts_with ends_with all_of pick cur_column
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_replace





# Steps:
## 1. Estimate animals impact
## 2. Estimate feed impact
## 3. Combine animals and feed impact

f_GHGE_pseudoherd <- function(object,
                        overwrite = FALSE,
                        ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_pseudoherd) && !overwrite) {
    message(
      "Using cached values stored in ",
      "object@footprints$GHGE$GHGE_pseudoherd."
    )
    return(object@footprints$GHGE$GHGE_pseudoherd)
  }

  id_cols = object@traceability$id_cols

    ## Compute GHGE per animal ----
  herd_impact = f_GHGE_herd(object, overwrite = overwrite)

  ## estimate pseudoherd ----
  pseudoherd_animals <- f_pseudoherd_animals(object)

  ## estimate emission per animal ----
  herd_impact_anim <- herd_impact |>
    # estimate emissions per animal
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("_livcat$"),
      .fns = ~ .x / Qobs,
      .names = "{stringr::str_remove(.col, '_livcat')}_anim"
    ))|>
    # add NUTS2 and SYS02
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
                     by = id_cols)

  ## estimate averages ----
  herd_impact_avrg <- h_average_practices(data = herd_impact_anim,
                                          target_vars = dplyr::matches("_anim$"),
                                          primary_grp = c('FADN_code_letter', 'species', 'COUNTRY', 'NUTS2'),
                                          secondary_grp = c('FADN_code_letter', 'species', 'COUNTRY'),
                                          weight_var = 'SYS02')
  ## allocate averages to off-farm animals ----
  pseudoherd_offfarm_impact <- pseudoherd_animals |>
    # add NUTS2 and SYS02
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
                     by = id_cols) |>
    dplyr::left_join(herd_impact_avrg,
                     by = c('COUNTRY', 'NUTS2', 'FADN_code_letter', 'species')) |>
    # sum impact per livestock category for off-farm animals
    dplyr::mutate(dplyr::across(
      .cols = dplyr::matches("_anim$"),
      .fns = ~ .x * Qofffarm,
      .names = "{stringr::str_remove(.col, '_anim')}_livcat_offfarm"
    )) |>
    # select
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, species,
                  Qeq, Qofffarm,
                  dplyr::matches("_livcat_offfarm$"))

  ## sum on-farm and off-farm impact ----
  pseudoherd_impact <- dplyr::full_join(
    herd_impact,
    pseudoherd_offfarm_impact,
    by = c(id_cols, 'FADN_code_letter', 'species')
  )
  livcat_vars <- grep("_livcat$", names(pseudoherd_impact), value = TRUE)
  livcat_vars <- livcat_vars[
    paste0(livcat_vars, "_offfarm") %in% names(pseudoherd_impact)
  ]

  for (var in livcat_vars) {
    pseudoherd_impact[[paste0(var, "_pseudoherd")]] <-
      dplyr::coalesce(pseudoherd_impact[[var]], 0) +
      dplyr::coalesce(pseudoherd_impact[[paste0(var, "_offfarm")]], 0)
  }


  return(pseudoherd_impact)

}



