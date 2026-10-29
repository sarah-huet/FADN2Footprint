#' Compute GHG emission intensity of livestock outputs (milk, meat, eggs)
#'
#' @description
#' f_GHGE_herd_output aggregates the greenhouse gas emission intensities of
#' all livestock outputs — milk, meat, and eggs — across cattle, swine and
#' poultry species, by combining the results of
#' f_GHGE_herd_output_cattle, f_GHGE_herd_output_swine, and
#' f_GHGE_herd_output_poultry into three per-product-type tables.
#'
#' @details
#' The function first checks whether cached results are already stored in
#' `object@footprints$GHGE$GHGE_milk`, `object@footprints$GHGE$GHGE_meat`, or
#' `object@footprints$GHGE$GHGE_eggs`. If any of these are present and
#' overwrite is FALSE, the cached value is returned immediately without
#' recomputation.
#'
#' Otherwise, the function proceeds in two steps:
#'
#' **1. Compute species-level GHG emission outputs:**
#' - f_GHGE_herd_output_cattle for cattle (milk, cull cow meat, veal/beef
#'   meat);
#' - f_GHGE_herd_output_swine for swine (meat);
#' - f_GHGE_herd_output_poultry for poultry (meat and eggs).
#'
#' **2. Build per-product-type tables:**
#' - **Milk**: rows from GHGE_herd_output_cattle where output == "milk".
#' - **Meat**: rows from GHGE_herd_output_cattle where output matches
#'   "meat" (cull cow, veal, beef), combined with all rows from
#'   GHGE_herd_output_swine and the meat-related rows of
#'   GHGE_herd_output_poultry.
#' - **Eggs**: rows from GHGE_herd_output_poultry where output matches
#'   "eggs".
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing `object@traceability$id_cols`,
#'   `object@footprints$GHGE` (for cached results), and all data required by
#'   f_GHGE_herd_output_cattle, f_GHGE_herd_output_swine and
#'   f_GHGE_herd_output_poultry.
#' @param overwrite Logical. If TRUE, forces recomputation of results
#'   instead of reusing previously cached values stored in
#'   `object@footprints$GHGE`. Default is FALSE.
#' @param ... Additional arguments passed to the species-specific
#'   GHG output functions.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{GHGE_milk}{A tibble with GHG emission intensities for milk
#'     production (cattle only).}
#'   \item{GHGE_meat}{A tibble with GHG emission intensities for meat
#'     production, combining cattle (cull cow, veal, beef), swine and
#'     poultry outputs.}
#'   \item{GHGE_eggs}{A tibble with GHG emission intensities for egg
#'     production (poultry only).}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' ghge_output <- f_GHGE_herd_output(f)
#' head(ghge_output$GHGE_milk)
#' head(ghge_output$GHGE_meat)
#' head(ghge_output$GHGE_eggs)
#' }
#'
#' @seealso f_GHGE_herd_output_cattle, f_GHGE_herd_output_swine,
#'   f_GHGE_herd_output_poultry, f_GHGE_herd
#'
#' @concept footprint-ghge
#' @export
#' @importFrom dplyr filter bind_rows
#'


f_GHGE_herd_output <- function(object,
                               overwrite = FALSE,
                               ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_milk)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_milk.")
    return(object@footprints$GHGE$GHGE_milk)  # use cached value
  }
  if (!is.null(object@footprints$GHGE$GHGE_meat)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_meat.")
    return(object@footprints$GHGE$GHGE_meat)  # use cached value
  }
  if (!is.null(object@footprints$GHGE$GHGE_eggs)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_eggs.")
    return(object@footprints$GHGE$GHGE_eggs)  # use cached value
  }

  id_cols = object@traceability$id_cols

  ## Cattle ----

  GHGE_herd_output_cattle <- f_GHGE_herd_output_cattle(object)

  ## swine ----

  GHGE_herd_output_swine <- f_GHGE_herd_output_swine(object)

  ## poultry ----

  GHGE_herd_output_poultry <- f_GHGE_herd_output_poultry(object)


  # 5. Per product type tables ------------------------------------------------------------------------------

  milk_impact <- GHGE_herd_output_cattle |>
    dplyr::filter(output == "milk")

  meat_impact <- Reduce(f = bind_rows,
         x = list(GHGE_herd_output_cattle |>
                dplyr::filter(grepl("meat", output)),
              GHGE_herd_output_swine,
              GHGE_herd_output_poultry |>
                dplyr::filter(grepl("meat", output))
              ))

  eggs_impact <- GHGE_herd_output_poultry |>
    dplyr::filter(grepl("eggs", output))



  return(list(
    GHGE_milk = milk_impact,
    GHGE_meat = meat_impact,
    GHGE_eggs = eggs_impact
  ))

}



