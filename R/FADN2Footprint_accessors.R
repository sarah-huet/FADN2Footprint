#' Accessors for FADN2Footprint Objects
#'
#' Methods to access internal data slots (farm, crop, herd, input, output)
#' from a \code{\link{FADN2Footprint-class}} object.
#' @include FADN2Footprint_constructor.R
#'
#' @param x An object of class \code{\link{FADN2Footprint-class}}.
#'
#' @return
#' \itemize{
#'   \item \code{farm_data}: A data frame containing farm structural characteristics.
#'   \item \code{crop_data}: A data frame containing cropping data.
#'   \item \code{herd_data}: A data frame containing herd and livestock data.
#'   \item \code{input_data}: A data frame containing input costs and quantities.
#'   \item \code{output_data}: A named list containing data frames for crops, meat, milk, and live animals.
#' }
#'
#' @name accessors
#' @aliases farm_data crop_data herd_data input_data output_data
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_fadn_obj' is an object created by data_4FADN2Footprint()
#'
#' # Access farm characteristics
#' df_farm <- farm_data(my_fadn_obj)
#'
#' # Access crop data
#' df_crop <- crop_data(my_fadn_obj)
#'
#' # Access output data (returns a list)
#' list_out <- output_data(my_fadn_obj)
#' }


# 1. Farm Data ------------------------------------------------------------

#' @rdname accessors
#' @export
setGeneric("farm_data", function(x) standardGeneric("farm_data"))

#' @rdname accessors
#' @export
setMethod("farm_data", "FADN2Footprint", function(x) {
  return(x@farm)
})

# 2. Crop Data ------------------------------------------------------------

#' @rdname accessors
#' @export
setGeneric("crop_data", function(x) standardGeneric("crop_data"))

#' @rdname accessors
#' @export
setMethod("crop_data", "FADN2Footprint", function(x) {
  return(x@crop)
})


# 3. Herd Data ------------------------------------------------------------

#' @rdname accessors
#' @export
setGeneric("herd_data", function(x) standardGeneric("herd_data"))

#' @rdname accessors
#' @export
setMethod("herd_data", "FADN2Footprint", function(x) {
  return(x@herd)
})


# 4. Input Data -----------------------------------------------------------

#' @rdname accessors
#' @export
setGeneric("input_data", function(x) standardGeneric("input_data"))

#' @rdname accessors
#' @export
setMethod("input_data", "FADN2Footprint", function(x) {
  return(x@input)
})


# 5. Output Data ----------------------------------------------------------

#' @rdname accessors
#' @export
setGeneric("output_data", function(x) standardGeneric("output_data"))

#' @rdname accessors
#' @export
setMethod("output_data", "FADN2Footprint", function(x) {
  return(x@output)
})
