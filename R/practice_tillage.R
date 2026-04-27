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
#' Only arable land use types are considered; permanent grassland, orchards
#' and other non-arable land uses are excluded from the output.
#'
#' The estimation procedure per crop activity is:
#' 1. The farm's total diesel consumption (diesel_l) is allocated to each
#'    crop in proportion to its share of the farm's total arable area
#'    (area_ha / sum_area_ha within the same land_use_type group).
#' 2. The no-till baseline (avrg_diesel_no_tillage × area_ha) is subtracted
#'    from the allocated diesel.
#' 3. The result is divided by crop area (area_ha) to express the indicator
#'    in L/ha.
#'
#' Three cases are handled explicitly:
#' - **NA or zero diesel**: farms with missing or non-positive diesel_l are
#'   assigned NA (it is considered implausible that a farm has no diesel
#'   consumption, so these records are flagged as unreliable and will be
#'   invalidated by infer_practices).
#' - **Below no-till baseline**: when the allocated diesel is lower than the
#'   no-till reference, tillage is set to 0 (interpreted as direct seeding /
#'   no-till practice).
#' - **Default**: excess diesel per hectare is returned as the tillage
#'   intensity indicator.
#'
#' The function returns cached results stored in
#' object@practices$crops$tillage when present and `overwrite = FALSE`.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow. The object must provide:
#'   - object@traceability$id_cols: character vector of farm identifier
#'     column names,
#'   - object@crop: crop activity table with columns FADN_code_letter,
#'     land_use_type and area_ha,
#'   - object@input: farm input table with column diesel_l.
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
#'     hectare above the no-till baseline. 0 indicates no-till or direct
#'     seeding; NA indicates unreliable diesel data.}
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

f_tillage <-function(object,
                     overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$crops$tillage)&& !overwrite) {
    message("Using cached values stored in object@practices$crops$tillage.")
    return(object@practices$crops$tillage)  # use cached value
  }

  # some preliminary notes:
  # here we choose a proxy of "Soil movement" metric as the total GNR consumption (in liter) minus the mean consumption without tillage per hectare divided by the UAA in crop production
  # we consider that only arable land use receive tillage
  # I calculate the mean off-road diesel used without tillage (i.e., direct seedling) per hectare for wheat, maize and the wheat-maize rotation (Chenu, 2013)
  avrg_diesel_no_tillage = mean(c(60,48,54))

  # Input data ------------------------------------------------------------------------------------------------------------------------------------------------------

  input_data <- object@crop |>
    # crop data
    dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,land_use_type,area_ha) |>
    #filter(!is.na(area_ha)) |>
    # input data
    dplyr::left_join(
      object@input |>
        dplyr::select(tidyselect::all_of(object@traceability$id_cols),diesel_l),
      by = object@traceability$id_cols
    )


  # Estimate parameter -------------------------------------------------------------------------------------------------------------------------------------------------------------

  tillage <- input_data |>
    # estimate total arable area per farm
    mutate(
      sum_area_ha = sum(area_ha,na.rm = T),
      .by = c(object@traceability$id_cols,land_use_type)
) |>
  # estimate parameter
  dplyr::mutate(
    tillage = dplyr::case_when(
      # we discard farms without any diesel registered as it is unlikely
      is.na(diesel_l) | diesel_l <= 0 ~ NA,
      # for farms that consume less off-road diesel than the no-till average, we considered that they practice no-till
      (diesel_l* area_ha/sum_area_ha) - avrg_diesel_no_tillage*area_ha <0 ~ 0,
      # default formula
      .default = ((diesel_l* area_ha/sum_area_ha) - avrg_diesel_no_tillage*area_ha) / area_ha
    )
  )



# Output -----------------------------------------------------------------------------------------------------------------------------------------------------------------------
tillage = tillage |>
  dplyr::select(tidyselect::all_of(object@traceability$id_cols),FADN_code_letter,land_use_type,tillage) |>
  # only for arable land
  dplyr::filter(land_use_type == "arable")

#object@practices$crops$tillage <- tillage


return(tillage)

}




