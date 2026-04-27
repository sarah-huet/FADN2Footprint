#' Compute greenhouse gas emissions (GHGE) footprints for crops and herd outputs
#'
#' @description
#' compute_footprint_ghg is a high-level wrapper that computes and caches
#' greenhouse gas emission (GHGE) footprints for cropping activities and for
#' herd outputs (milk, meat, eggs) from a prepared FADN2Footprint object. The
#' function calls lower-level modules that implement the detailed GHGE
#' calculations (currently f_GHGE_crops and f_GHGE_herd_output) and stores
#' results in the object@footprints$GHGE slot.
#'
#' @details
#' The function performs the following steps:
#' - validates that the supplied object inherits from the S4 class
#'   "FADN2Footprint";
#' - if cached GHGE results already exist in object@footprints$GHGE and
#'   overwrite = FALSE, the function returns the cached results (with a
#'   message) and no further computation is performed;
#' - otherwise the function:
#'     1. calls f_GHGE_crops(object) to compute per‑farm / per‑activity crop
#'        greenhouse gas emissions and stores the returned table in
#'        object@footprints$GHGE$GHGE_crops;
#'     2. calls f_GHGE_herd_output(object) to compute GHGE allocated to herd
#'        outputs and stores the milk result in
#'        object@footprints$GHGE$GHGE_milk (the herd function may return a
#'        list with multiple elements for milk/meat/eggs);
#' - finally the updated FADN2Footprint object (with added/updated footprints)
#'   is returned.
#'
#' Note:
#' - The function delegates the domain-specific computations to f_GHGE_crops
#'   and f_GHGE_herd_output; those helper functions must be available and
#'   compatible with the structure of the provided FADN2Footprint object.
#' - The object returned is the original S4 object with updated footprints
#'   stored under object@footprints$GHGE. Users can inspect those slots after
#'   the call to access per‑farm GHGE results.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the package
#'   workflow. The object must contain the slots and intermediate data required
#'   by the helper functions f_GHGE_crops and f_GHGE_herd_output (for example:
#'   object@farm, object@traceability$id_cols, object@practices, object@herd,
#'   object@output, etc.).
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return The input FADN2Footprint S4 object updated with GHGE footprints:
#'   - object@footprints$GHGE$GHGE_crops: table with crop GHGE results (per
#'     traceable farm / activity as produced by f_GHGE_crops),
#'   - object@footprints$GHGE$GHGE_milk: table with GHGE allocated to milk (as
#'     produced by f_GHGE_herd_output).
#'   The returned object can be further processed by downstream functions in
#'   the package or inspected to extract GHGE tables.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Compute and cache GHGE footprints (uses cached values if available)
#' f_updated <- compute_footprint_ghg(f)
#'
#' # Force recomputation even if cached results are present
#' f_updated2 <- compute_footprint_ghg(f, overwrite = TRUE)
#'
#' # Access the stored GHGE tables
#' f_updated@footprints$GHGE$GHGE_crops
#' f_updated@footprints$GHGE$GHGE_milk
#' }
#'
#' @seealso f_GHGE_crops, f_GHGE_herd_output
#'
#' @export
#' @concept footprint-ghge
#' @importFrom methods is


compute_footprint_ghg <- function(object,
                                  overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_crops) && !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_crops.")
    return(object@footprints$GHGE$GHGE_crops)  # use cached value
  }
  if (!is.null(object@footprints$GHGE$GHGE_milk) && !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_milk.")
    return(object@footprints$GHGE$GHGE_milk)  # use cached value
  }

  # CROPS ----------------------------------------------------------------------

  # Compute GHGE for crops
  crop_impact = f_GHGE_crops(object, overwrite = overwrite)
  object@footprints$GHGE$GHGE_crops <- crop_impact

  # HERD -----------------------------------------------------------------------

  ## Feed ----
  # Compute GHGE from produced and purchased feed
  feed_impact = f_GHGE_feed(object, overwrite = overwrite)
  object@footprints$GHGE$GHGE_feed <- feed_impact

  ## Herd ----
  # Compute GHGE per animal
  herd_impact = f_GHGE_herd(object, overwrite = overwrite)
  object@footprints$GHGE$GHGE_herd <- herd_impact

  ## Herd products ----
  # Compute GHGE for milk, meat and eggs
  herd_prod_impact = f_GHGE_herd_output(object, overwrite = overwrite)
  object@footprints$GHGE$GHGE_milk <- herd_prod_impact$GHGE_milk
  object@footprints$GHGE$GHGE_meat <- herd_prod_impact$GHGE_meat


  # FARMS ----------------------------------------------------------------------

  # Compute total GHGE at farm scale
  farm_impact = f_GHGE_farm(object, overwrite = overwrite)
  object@footprints$GHGE$GHGE_farm <- farm_impact


  return(object)
}
