#_______________________________________________________________________________________
# Function to create subsampled dataset
# in which each sample has same number of total observations/counts/reads
# Note that the subsampling is random, so some noise is introduced making the
# relative abundances slightly different
#_______________________________________________________________________________________

# Prune farms ----
#' Prune farms from a FADN2Footprint object based on filtering conditions
#'
#' @description
#' This function filters farms in a FADN2Footprint object based on specified conditions,
#' and cascades these filters through all related data slots to maintain consistency.
#'
#' @param object A FADN2Footprint object containing farm data and related information.
#' @param id_cols A character string specifying the name of the column containing unique farm identifiers.
#' @param ... Additional filtering conditions to apply to the farm data. These should be valid
#'   expressions that can be used with dplyr::filter(). For example: `COUNTRY == "FRA"`,
#'   `ORGANIC == TRUE`, etc.
#'
#' @return
#' A FADN2Footprint object with:
#' \itemize{
#'   \item The farm data filtered according to the specified conditions
#'   \item All related data slots (crop, herd, input, etc.) filtered to only include
#'     data corresponding to the remaining farms
#' }
#'
#' @details
#' The function performs a cascading filter operation:
#' 1. First filters the main farm data based on the provided conditions
#' 2. Then filters all other data slots to only include data for the remaining farms
#' 3. Handles both simple data.frame slots and nested list structures
#'
#' @examples
#' # Filter to keep only farms from France
#' pruned_obj <- m_prune_farms(fadn_obj, COUNTRY == "FRA")
#'
#' # Filter to keep only large farms in France
#' pruned_obj <- m_prune_farms(fadn_obj,
#'                          COUNTRY == "FRA", ORGANIC == TRUE)
#'
#' @seealso \code{\link{FADN2Footprint}} for the class definition
#'
#' @concept utils
#' @export
#' @importFrom rlang enquos !!!
#' @importFrom dplyr filter semi_join all_of

# 1. Define the Generic
setGeneric("m_prune_farms", function(object, ...) standardGeneric("m_prune_farms"))

# 2. Define the Method
setMethod("m_prune_farms", "FADN2Footprint", function(object, ...) {

  # --- Step A: Capture the filtering expressions ---
  # This captures "COUNTRY == 'FRA'" etc. as quosures
  filters <- rlang::enquos(...)

  # --- Step B: Filter the main 'farm' slot ---
  # We use `!!!` (splice operator) to inject the captured conditions into filter
  # We operate on a copy to avoid modifying the original object prematurely
  original_farm_data <- object@farm
  filtered_farm_data <- dplyr::filter(original_farm_data, !!!filters)

  # Check if we lost all farms
  if (nrow(filtered_farm_data) == 0) {
    warning("Filtering resulted in 0 farms. Returning empty object.")
  }

  # Get the vector of remaining IDs
  # We assume id_cols is a string name of the column
  id_cols <- object@traceability$id_cols
  if (is.null(id_cols) || length(id_cols) == 0) {
    stop("'object@traceability$id_cols' is not defined in the object.")
  }
  valid_ids <- filtered_farm_data |>
    dplyr::select(dplyr::all_of(id_cols))

  # --- Step C: Helper function for cascading filters ---
  # This function checks if an object is a data.frame with the ID column
  # and filters it. If it's a list, it recurses.
  apply_filter <- function(x) {
    if (is.data.frame(x)) {
        # TODO: Warn if a dataframe doesn't have the ID col?
        return(x |> dplyr::semi_join(valid_ids, by = id_cols))
    } else if (is.list(x)) {
      # Call apply_filter recursively to dive deeper into the list of lists.
      return(lapply(x, apply_filter))
    } else {
      return(x)
    }
  }

  # --- Step D: Update the object slots ---

  # Update farm
  object@farm <- filtered_farm_data

  # Update all other slots using the recursive function
  object@crop <- apply_filter(object@crop)
  object@herd <- apply_filter(object@herd)
  object@input <- apply_filter(object@input)
  object@landscape_metrics <- apply_filter(object@landscape_metrics)

  object@practices <- apply_filter(object@practices)
  object@footprints <- apply_filter(object@footprints)
  object@output <- apply_filter(object@output)

  # Optional: Update traceability to log this operation
  # object@traceability <- c(object@traceability, paste("Pruned farms:", paste(rlang::exprs(...), collapse = "; ")))

  return(object)
})

# Remove farms ----
#' Remove farms from a FADN2Footprint object
#'
#' This function filters out specific farms from all data slots (farm, crop, herd,
#' inputs, practices, footprints, etc.) of a FADN2Footprint object. It automatically
#' handles nested lists and updates the traceability slot to log discarded farms.
#'
#' @param object A \code{FADN2Footprint} object.
#' @param farms_to_remove A \code{data.frame} or \code{tibble} containing at least
#'   the ID columns defined in \code{object@traceability$id_cols}. It may optionally
#'   contain a \code{problem} column describing why the farm is being removed.
#'
#' @return The pruned \code{FADN2Footprint} object.
#'
#' @concept utils
#' @export
#' @importFrom dplyr select reframe anti_join bind_rows distinct all_of
#' @importFrom tidyselect all_of
#' @import methods
#'
#' @examples
#' \dontrun{
#' # Define farms to remove (e.g., outliers)
#' outliers <- data.frame(ID = c(101, 105), YEAR = c(2020, 2020), problem = "Outlier in GHGE")
#'
#' # Remove them
#' obj_clean <- m_remove_farms(my_obj, outliers)
#' }
setGeneric("m_remove_farms", function(object, farms_to_remove) {
  standardGeneric("m_remove_farms")
})

setMethod("m_remove_farms", "FADN2Footprint", function(object, farms_to_remove) {

  # --- 1. Validation ---

  if (!inherits(farms_to_remove, "data.frame")) {
    stop("'farms_to_remove' must be a tibble or a data.frame.")
  }

  id_cols <- object@traceability$id_cols
  if (is.null(id_cols) || length(id_cols) == 0) {
    stop("'object@traceability$id_cols' is not defined in the object.")
  }

  missing_ids <- setdiff(id_cols, colnames(farms_to_remove))
  if (length(missing_ids) > 0) {
    stop("The following ID columns are missing in 'farms_to_remove': ",
         paste(missing_ids, collapse = ", "))
  }

  # --- 2. Prepare Removal Data ---

  # Handle 'problem' column safely (create default if missing to avoid crashes)
  if (!"problem" %in% colnames(farms_to_remove)) {
    farms_to_remove$problem <- "Manual removal"
  }

  # Aggregate problems if there are duplicate rows for the same farm
  # We keep this clean version for the anti_join operations
  farms_to_remove_clean <- farms_to_remove |>
    dplyr::select(tidyselect::all_of(id_cols), problem) |>
    dplyr::reframe(problem = paste(unique(problem), collapse = ";"),
                   .by = tidyselect::all_of(id_cols))

  # --- 3. Recursive Filter Helper ---

  # This function navigates any depth of list/dataframe nesting
  apply_remove <- function(x) {
    if (is.data.frame(x)) {
      # Check if this dataframe contains the ID columns needed for joining
      if (all(id_cols %in% colnames(x))) {
        return(x |> dplyr::anti_join(farms_to_remove_clean, by = id_cols))
      } else {
        return(x) # Return untouched if IDs are missing
      }
    } else if (is.list(x)) {
      # Recursive step: Apply to all elements of the list
      return(lapply(x, apply_remove))
    } else {
      return(x) # Return other types (NULL, vectors) untouched
    }
  }

  # --- 4. Apply to Slots ---

  # Apply to standard dataframes
  object@farm              <- apply_remove(object@farm)
  object@crop              <- apply_remove(object@crop)
  object@herd              <- apply_remove(object@herd)
  object@input             <- apply_remove(object@input)
  object@landscape_metrics <- apply_remove(object@landscape_metrics)

  # Apply to nested lists (Footprints, Practices, Output, etc.)
  object@practices  <- apply_remove(object@practices)
  object@footprints <- apply_remove(object@footprints)
  object@output     <- apply_remove(object@output)

  # --- 5. Update Traceability ---

  if (is.null(object@traceability$discarded_farms)) {
    # Initialize if empty
    object@traceability$discarded_farms <- farms_to_remove_clean
  } else {
    # Append, ensure columns match, and remove duplicates
    object@traceability$discarded_farms <- dplyr::bind_rows(
      object@traceability$discarded_farms,
      farms_to_remove_clean
    ) |>
      dplyr::distinct(dplyr::across(tidyselect::all_of(id_cols)), .keep_all = TRUE)
  }

  return(object)
})

