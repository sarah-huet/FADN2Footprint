#' Estimate tillage intensity from diesel consumption
#'
#' @description
#' f_tillage estimates a tillage intensity indicator for each arable crop
#' activity on each farm, using off-road diesel consumption as a proxy for
#' soil movement. The indicator represents the excess diesel consumption above
#' a no-till baseline, expressed in litres per hectare. Farms with no recorded
#' diesel consumption are treated as unreliable and assigned NA.
#'
#' @details
#' The tillage intensity proxy is based on the following rationale: total
#' off-road diesel consumption reflects the cumulative mechanical work applied
#' to the soil. Subtracting the average diesel consumption observed under
#' direct seeding (no-till) conditions yields an estimate of the additional
#' fuel attributable to tillage operations.
#'
#' The no-till baseline is derived from Chenu (2013) as the average diesel
#' consumption for wheat (60 L/ha), maize (48 L/ha) and a wheat–maize
#' rotation (54 L/ha), giving a baseline of 54 L/ha.
#'
#' This arable reference is applied uniformly to all recorded land areas,
#' including grassland and other non-arable land, to estimate a whole-farm
#' non-tillage baseline. This is an allocation assumption, not a validated
#' grassland-specific reference. Only arable activities receive the residual
#' diesel; non-arable activities are excluded from the output.
#'
#' The estimation procedure per crop activity is:
#' 1. Multiply the no-till reference by the farm's total recorded area across
#'    all land use types to estimate baseline diesel consumption in litres.
#' 2. Subtract this baseline from total farm diesel consumption (diesel_l),
#'    retaining only positive excess consumption.
#' 3. Allocate the excess to arable crops in proportion to their arable area
#'    shares, then divide by crop area. This simplifies to excess consumption
#'    divided by total arable area, in L/ha, for every positive-area arable crop.
#'
#' Three cases are handled explicitly:
#' - **NA or zero diesel**: farms with missing or non-positive diesel_l are
#'   assigned NA (it is considered implausible that a farm has no diesel
#'   consumption, so these records are flagged as unreliable and will be
#'   invalidated by infer_practices).
#' - **At or below the whole-farm baseline**: tillage is set to 0, indicating
#'   no positive excess detected, rather than proof of no-till practice.
#' - **Default**: excess diesel per hectare is returned as the tillage
#'   intensity indicator.
#'
#' Arable activities with missing or non-positive area receive NA. Missing
#' areas are omitted from farm area totals, so incomplete area records can
#' underestimate the baseline. The residual is a proxy, not measured tillage
#' fuel; other operations and machinery efficiency can also affect it.
#'
#' The function returns cached results stored in
#' object@practices$crops$tillage when present and `overwrite = FALSE`.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow. The object must provide:
#'   - object@traceability$id_cols: character vector of farm identifier
#'     column names,
#'   - object@crop: crop activity table with columns FADN_code_letter,
#'     land_use_type and area_ha (hectares), covering all land use types,
#'   - object@input: farm input table with column diesel_l (litres over the
#'     farm reporting period).
#' @param overwrite Logical, default FALSE. If FALSE and
#'   object@practices$crops$tillage is not NULL, the cached table is
#'   returned with a message. Set to TRUE to force recomputation.
#'
#' @return A tibble with one row per farm × arable crop activity combination,
#'   containing the following columns:
#' \describe{
#'   \item{(id_cols)}{Farm identifier columns defined in
#'     object@traceability$id_cols.}
#'   \item{FADN_code_letter}{FADN crop activity code.}
#'   \item{land_use_type}{Land use type (always "arable" in the output).}
#'   \item{tillage}{Tillage intensity indicator in litres of diesel per
#'     arable hectare above the whole-farm baseline per reporting period.
#'     0 indicates no positive excess; NA indicates unreliable diesel data
#'     or missing/non-positive crop area.}
#' }
#'
#' @references
#' Chenu, J. (2013). Référentiel de consommations de carburant pour
#' différents systèmes de travail du sol. Internal technical report.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' tillage_tbl <- f_tillage(f)
#' head(tillage_tbl)
#'
#' # Force recomputation ignoring cache
#' tillage_tbl2 <- f_tillage(f, overwrite = TRUE)
#' }
#'
#' @seealso infer_practices, f_GHGE_crops
#'
#' @concept practice-crop
#' @export
#' @importFrom dplyr select left_join mutate filter case_when all_of
#' @importFrom tidyselect all_of

f_tillage <- function(object, overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$tillage) && !overwrite) {
    message("Using cached values stored in object@practices$crops$tillage.")
    return(object@practices$crops$tillage) # use cached value
  }

  id_cols <- object@traceability$id_cols
  # Chenu (2013): arable no-till reference, assumed here for all land uses.
  avrg_diesel_no_tillage <- mean(c(60, 48, 54))

  # Input data ------------------------------------------------------------------------------------------------------------------------------------------------------

  input_data <- object@crop |>
    # crop data
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter,
      land_use_type,
      area_ha
    ) |>
    # input data
    dplyr::left_join(
      object@input |>
        dplyr::select(tidyselect::all_of(id_cols), diesel_l),
      by = id_cols
    )

  # Estimate parameter -------------------------------------------------------------------------------------------------------------------------------------------------------------

  tillage <- input_data |>
    dplyr::mutate(
      sum_area_ha = sum(area_ha, na.rm = T),
      arable_area_ha = sum(area_ha[land_use_type %in% "arable"], na.rm = TRUE),
      .by = dplyr::all_of(id_cols)
    ) |>
    dplyr::mutate(
      excess_diesel_l = pmax(
        0,
        diesel_l - avrg_diesel_no_tillage * sum_area_ha
      ),
      tillage = dplyr::case_when(
        land_use_type == "grassland" ~ 0,
        is.na(area_ha) | area_ha <= 0 | arable_area_ha <= 0 ~ 0,
        # we discard farms without any diesel registered as it is unlikely
        is.na(diesel_l) | diesel_l <= 0 ~ NA_real_,
        # default estimation
        .default = excess_diesel_l / arable_area_ha
      )
    )

  #mean(tillage$tillage[tillage$FADN_code_letter %in% c("CWHTC", "CMZ")], na.rm = TRUE)

  # Output -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  tillage <- tillage |>
    dplyr::select(
      tidyselect::all_of(object@traceability$id_cols),
      FADN_code_letter,
      land_use_type,
      tillage
    )

  return(tillage)
}
