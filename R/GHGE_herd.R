#' Compute greenhouse gas emissions from livestock and their feed
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
#' @param ... Additional arguments passed to downstream helper functions.
#'
#' @return A tibble with one row per farm × livestock category combination
#'   (identified by `object@traceability$id_cols` and `FADN_code_letter`) and
#'   columns including:
#' \describe{
#'   \item{species}{Livestock species label.}
#'   \item{Qobs}{Observed animal count.}
#'   \item{feed_farm_*}{On-farm feed dry matter, area, and emission
#'     components (kg CO2e per livestock category).}
#'   \item{feed_pseudofarm_*}{Pseudo-farm (on-farm + purchased) feed dry
#'     matter, area, and emission components.}
#'   \item{CH4_enteric_kgCO2e_livcat}{Enteric fermentation CH4 emissions.}
#'   \item{CH4_MM_kgCO2e_livcat}{Manure management CH4 emissions.}
#'   \item{N2O_D_MM_kgCO2e_livcat}{Direct manure management N2O.}
#'   \item{N2O_G_mm_kgCO2e_livcat}{Indirect N2O from grazing.}
#'   \item{N2O_L_mm_kgCO2e_livcat}{Indirect N2O from leaching/runoff.}
#'   \item{farm_ghge_herd_kgCO2e_livcat}{Total scope 1 & 2 herd GHGE.}
#'   \item{pseudofarm_ghge_herd_kgCO2e_livcat}{Total scope 1, 2 & 3 GHGE.}
#'   \item{farm_ghge_herd_kgCO2e_per_anim}{Scope 1 & 2 GHGE per animal.}
#'   \item{pseudofarm_ghge_herd_kgCO2e_per_anim}{Scope 3 GHGE per animal.}
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
#' @seealso GHGE_ch4_enteric, GHGE_ch4_manure, GHGE_n2o_manure,
#'   f_GHGE_feed, f_GHGE_herd_output, f_GHGE_farm
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
#' @importFrom dplyr select left_join summarise across matches mutate
#'   rename_with starts_with ends_with all_of pick cur_column
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_replace





# Steps:
## 1. Estimate animals impact
## 2. Estimate feed impact
## 3. Combine animals and feed impact

f_GHGE_herd <- function(object,
                        overwrite = FALSE,
                        account_pseudoherd = F, ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_herd)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_herd")
    return(object@footprints$GHGE$GHGE_herd)  # use cached value
  }

  id_cols = object@traceability$id_cols

  # 1. Estimate animals impact -------------------------------------------------

  ## TODO: if (account_pseudoherd == F) {} else {}

  ### CH4 from enteric fermentation
  ### CH4 from manure management
  ### N2O from manure management
  ### Heating fuels and electricity are accounted for per activity, in`f_GHGE_herd_output`

  tmp_GHGE_ch4_enteric = GHGE_ch4_enteric(object, overwrite = overwrite)

  tmp_GHGE_ch4_manure = GHGE_ch4_manure(object, overwrite = overwrite)

  tmp_GHGE_n2o_manure = GHGE_n2o_manure(object, overwrite = overwrite)

  herd_livestock_GHGE <- Reduce(x = list(tmp_GHGE_ch4_enteric,
                                         tmp_GHGE_ch4_manure,
                                         tmp_GHGE_n2o_manure),
                                f = function(x,y) dplyr::left_join(x, y,
                                                                   by = c(id_cols, 'FADN_code_letter'))) |>
    dplyr::select(dplyr::all_of(id_cols),
                  FADN_code_letter,
                  CH4_enteric_kgCO2e_livcat,
                  CH4_MM_kgCO2e_livcat,
                  N2O_D_MM_kgCO2e_livcat,N2O_G_mm_kgCO2e_livcat,N2O_L_mm_kgCO2e_livcat)

  # 2. Estimate feed impact ----------------------------------------------------

  feed_GHGE <- f_GHGE_feed(object, overwrite = overwrite)

  # sum feed impact per livestock category
  herd_feed_GHGE = feed_GHGE |>
    dplyr::select(dplyr::all_of(id_cols),
                  FADN_code_letter,
                  Sailley_feed,FADN_code_feed,feed_origin,
                  DM_t_livcat,area_ha_livcat,
                  dplyr::matches("per_t$")) |>

    dplyr::summarise(

      # feed emission
      dplyr::across(
        # Select all columns that end with "CO2e"
        dplyr::matches("kgCO2e_per_t$"),
        # Apply two functions to each selected column
        ~ sum(.x * DM_t_livcat, na.rm = T),
        # Name the new columns automatically
        .names = "{.col}"
      ),

      # feed category infos
      DM_t_livcat = sum(DM_t_livcat, na.rm = T),
      area_ha_livcat = sum(area_ha_livcat, na.rm = T),

      .by = c(id_cols, 'FADN_code_letter', 'feed_origin')
    ) |>
    # rename sums
    dplyr::rename_with(
      # Function to apply to the column names: remove "_per_t" at the end ($)
      .fn = ~ gsub("_per_t$", "_livcat", .x),
      # Selection of columns: starts with "sum" AND ends with "kgCO2e_per_t"
      .cols = dplyr::ends_with("kgCO2e_per_t")
    ) |>
    # pivot and distinguish feed origin
    tidyr::pivot_wider(
      id_cols = c(id_cols, FADN_code_letter),
      names_from = feed_origin,
      names_glue = "{feed_origin}_{.value}",
      values_from = c(DM_t_livcat,area_ha_livcat,
                      dplyr::matches("_livcat$")),
      values_fill = 0
    ) |>
    # estimate pseudofarm
    dplyr::mutate(
      dplyr::across(
        # Select columns where names start with "feed_produced_sum_"
        dplyr::starts_with("feed_produced_"),
        # For each produced column, add the corresponding purchased column
        ~ . + dplyr::pick(stringr::str_replace(dplyr::cur_column(), "produced", "purchased"))[[1]],
        # Name the new columns by replacing "produced" with "pseudofarm"
        .names = "{stringr::str_replace(.col, 'produced', 'pseudofarm')}"
      )
    ) |>
    # rename farm variable
    dplyr::rename_with(
      ~ stringr::str_replace(.x, "^feed_produced", "feed_farm"),  # Replace "feed_produced" with "feed_farm"
      dplyr::starts_with("feed_produced")                  # Only apply to columns starting with "feed_produced"
    ) |>
    # select
    dplyr::select(dplyr::all_of(id_cols),
                  FADN_code_letter,
                  dplyr::matches("farm|pseudofarm"))

  # 3. Combine herd and feed impact -------------------------------------------

  herd_impact = Reduce(
    x = list(object@herd |>
               # select variables
               dplyr::select(dplyr::all_of(id_cols),
                             species,FADN_code_letter,
                             Qobs),
             herd_livestock_GHGE,
             herd_feed_GHGE
             ),
    f = function(x,y) dplyr::left_join(x, y,
                                       by = c(id_cols, 'FADN_code_letter'))
  ) |>
    # aggregate impact
    dplyr::mutate(
      farm_ghge_herd_kgCO2e_livcat = (feed_farm_total_ghg_crop_kgCO2e_livcat
                                      + CH4_enteric_kgCO2e_livcat + CH4_MM_kgCO2e_livcat
                                      + N2O_D_MM_kgCO2e_livcat + N2O_G_mm_kgCO2e_livcat + N2O_L_mm_kgCO2e_livcat),
      pseudofarm_ghge_herd_kgCO2e_livcat = (feed_pseudofarm_total_ghg_crop_kgCO2e_livcat
                                            + CH4_enteric_kgCO2e_livcat + CH4_MM_kgCO2e_livcat
                                            + N2O_D_MM_kgCO2e_livcat + N2O_G_mm_kgCO2e_livcat + N2O_L_mm_kgCO2e_livcat),

      farm_ghge_herd_kgCO2e_per_anim = farm_ghge_herd_kgCO2e_livcat / Qobs,
      pseudofarm_ghge_herd_kgCO2e_per_anim = pseudofarm_ghge_herd_kgCO2e_livcat / Qobs
    )

  return(herd_impact)

}



