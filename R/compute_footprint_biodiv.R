#' Compute biodiversity (BVIAS) footprints for crops, herd feed and outputs
#'
#' @description
#' compute_footprint_biodiv is a high-level wrapper that computes and caches
#' biodiversity value-impact (BVI) footprints following the BVIAS approach for
#' cropping activities, feed consumed by herds, and herd outputs (e.g. milk).
#' The function loads or validates model parameters (constants and weights),
#' delegates detailed computations to lower-level functions (f_BVIAS_crops,
#' f_BVIAS_feed, f_BVIAS_herd_output), stores results in the object's
#' footprints slot and returns the updated FADN2Footprint object.
#'
#' @details
#' Workflow:
#' - Validate that the supplied object inherits from the S4 class
#'   "FADN2Footprint".
#' - If a cached result is present at object@footprints$BVIAS$BVI_crops and
#'   `overwrite = FALSE`, the function prints a message and returns the cached
#'   value immediately.
#' - Validate `BVIAS_constants` and `BVIAS_weights` (must be data.frames or NULL).
#' - If constants or weights are not supplied, attempt to load them from
#'   object@footprints$BVIAS$model_parameters; if still missing, fall back on
#'   package defaults (data_extra$BVIAS_var_constant and
#'   data_extra$BVIAS_var_weight). The chosen source is indicated with a
#'   message.
#' - Store the chosen constants and weights in
#'   object@footprints$BVIAS$model_parameters.
#' - Call:
#'     * f_BVIAS_crops(object, ...) to compute per‑farm crop biodiversity impacts
#'       and store result in object@footprints$BVIAS$BVI_crops;
#'     * f_BVIAS_feed(object, ...) to compute feed-level impacts and store in
#'       object@footprints$BVIAS$BVI_feed;
#'     * f_BVIAS_herd_output(object, ...) to compute BVI allocated to herd
#'       outputs and store in object@footprints$BVIAS$BVI_milk.
#' - Return the updated FADN2Footprint S4 object with BVIAS outputs attached.
#'
#' Notes and assumptions:
#' - The function delegates domain-specific computations to helper functions
#'   (f_BVIAS_crops, f_BVIAS_feed, f_BVIAS_herd_output) which must be present
#'   and compatible with the structure of the provided object.
#' - The function expects the object to contain required slots and intermediate
#'   data (for example: object@farm, object@traceability$id_cols,
#'   object@practices, object@herd, object@output, etc.). Helper functions may
#'   call infer_practices or other preparatory steps as needed.
#' - If model parameter tables are not provided and not present in the object,
#'   package defaults (data_extra$BVIAS_var_constant and
#'   data_extra$BVIAS_var_weight) are used. A message is printed to indicate the
#'   parameter source.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the package
#'   workflow. The object must contain the traceability id columns referenced in
#'   object@traceability$id_cols and have the herd, farm and output information
#'   required by the helper functions.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @param BVIAS_constants Optional data.frame of model constants. If NULL the
#'   function attempts to use object@footprints$BVIAS$model_parameters$constants
#'   or the package default data_extra$BVIAS_var_constant.
#' @param BVIAS_weights Optional data.frame of model variable weights. If NULL
#'   the function attempts to use object@footprints$BVIAS$model_parameters$weights
#'   or the package default data_extra$BVIAS_var_weight.
#'
#' @return The input FADN2Footprint S4 object updated with biodiversity footprint
#'   outputs stored under object@footprints$BVIAS:
#'   - object@footprints$BVIAS$model_parameters$constants: the constants used,
#'   - object@footprints$BVIAS$model_parameters$weights: the weights used,
#'   - object@footprints$BVIAS$BVI_crops: per‑farm / per‑activity crop BVI table
#'     (as returned by f_BVIAS_crops),
#'   - object@footprints$BVIAS$BVI_feed: per‑feed‑line BVI table (as returned by
#'     f_BVIAS_feed),
#'   - object@footprints$BVIAS$BVI_milk: herd output BVI allocations (as returned
#'     by f_BVIAS_herd_output).
#'
#' @section Caching and messages:
#' If a cached BVI crops table exists and `overwrite = FALSE`, the function will
#' print a message and return the cached object immediately. Messages are also
#' printed when default constants/weights are used or when constants/weights are
#' loaded from the object.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Compute biodiversity footprints using defaults embedded in the object or package
#' f_bvi <- compute_footprint_biodiv(f)
#'
#' # Provide custom constants and weights and force recomputation
#' f_bvi2 <- compute_footprint_biodiv(f,
#'                                    overwrite = TRUE,
#'                                    BVIAS_constants = my_constants_df,
#'                                    BVIAS_weights = my_weights_df)
#'
#' # Inspect stored results
#' f_bvi2@footprints$BVIAS$BVI_crops
#' f_bvi2@footprints$BVIAS$BVI_feed
#' f_bvi2@footprints$BVIAS$BVI_milk
#' }
#'
#' @seealso f_BVIAS_crops, f_BVIAS_feed, f_BVIAS_herd_output
#'
#' @export
#' @concept footprint-biodiv
#' @name compute_footprint_biodiv
#' @importFrom dplyr bind_rows left_join inner_join mutate filter select across all_of


compute_footprint_biodiv <- function(object,
                                     overwrite = FALSE,
                                     BVIAS_constants = NULL,
                                     BVIAS_weights = NULL) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$BVIAS$BVI_crops) && !overwrite) {
    message("Using cached values stored in object@footprints$BVIAS$BVI_crops.")
    return(object@footprints$BVIAS$BVI_crops)  # use cached value
  }
  # TODO: add a message for default constants and weights variables

  # check model constants and weights
  if (!is.null(BVIAS_constants) && !is.data.frame(BVIAS_constants)) {
    stop("BVIAS_constants must be a data.frame or NULL")
  }
  if (!is.null(BVIAS_weights) && !is.data.frame(BVIAS_weights)) {
    stop("BVIAS_weights must be a data.frame or NULL")
  }
  if (is.null(BVIAS_constants)) {
    if (is.null(object@footprints$BVIAS$model_parameters$constants)) {
      BVIAS_constants <- data_extra$BVIAS_var_constant
      message("No BVIAS constants provided. Using default BVIAS constants.")
    } else {
      BVIAS_constants = object@footprints$BVIAS$model_parameters$constants
      message("No BVIAS constants provided. Using BVIAS constants stored in the object.")
    }
  }
  if (is.null(BVIAS_weights)) {
    if (is.null(object@footprints$BVIAS$model_parameters$weights)) {
      BVIAS_weights <- data_extra$BVIAS_var_weight
      message("No BVIAS weights provided. Using default BVIAS weights")
    } else {
      BVIAS_weights = object@footprints$BVIAS$model_parameters$weights
      message("No BVIAS weights provided. Using BVIAS weights stored in the object.")
    }
  }

  object@footprints$BVIAS$model_parameters$constants <- BVIAS_constants
  object@footprints$BVIAS$model_parameters$weights <- BVIAS_weights

  # Compute biodiversity impact

  crop_impact = f_BVIAS_crops(object,
                              overwrite = overwrite,
                              BVIAS_constants = BVIAS_constants,
                              BVIAS_weights = BVIAS_weights)
  object@footprints$BVIAS$BVI_crops <- crop_impact

  feed_impact = f_BVIAS_feed(object,
                             overwrite = overwrite,
                             BVIAS_constants = BVIAS_constants,
                             BVIAS_weights = BVIAS_weights)
  object@footprints$BVIAS$BVI_feed <- feed_impact

  milk_impact = f_BVIAS_herd_output(object,
                                    overwrite = overwrite,
                                    BVIAS_constants = BVIAS_constants,
                                    BVIAS_weights = BVIAS_weights)
  object@footprints$BVIAS$BVI_milk <- milk_impact

  return(object)
}
