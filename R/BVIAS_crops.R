#' Estimate Biodiversity Value Index (BVI) for Crops
#'
#' @description
#' Calculates the Biodiversity Value Index (BVI) per hectare and per tonne for crop outputs
#' within a `FADN2Footprint` object.
#'
#' This function assesses biodiversity performance by:
#' 1. Calculating raw metric scores based on non-linear response functions.
#' 2. Rescaling scores to a [0, 1] range.
#' 3. Aggregating scores using weighted mean.
#' 4. Normalizing based on land use type (Arable vs Grassland) following Gallego-Zamorano et al. (2022) and Lindner et al. (2019).
#' 5. Converting the result into a per-tonne impact using yield data.
#'
#' @param object An object of class \code{FADN2Footprint}.
#' @param BVIAS_constants A data.frame containing the model constants (\deqn{\alpha, \beta, \gamma, \delta, \epsilon, \sigma})
#' for the non-linear response functions. If \code{NULL}, uses the default
#' package data (\code{data_extra$BVIAS_var_constant}).
#' @param BVIAS_weights A data.frame containing the weights for each metric.
#' If \code{NULL}, uses the default package data (\code{data_extra$BVIAS_var_weight}).
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @details
#' \strong{Caching:}
#' If \code{object@footprints$BVIAS$BVI_crop} is already populated, the function returns the cached value immediately.
#'
#' \strong{Methodology:}
#' The raw score \eqn{y} for each metric is calculated using the following response function:
#' \deqn{y = \gamma + \epsilon \cdot \exp\left(-\frac{|(x_{norm}^\delta - \beta)^\alpha|}{2\sigma^\alpha}\right)}{y = gamma + epsilon * exp(-|(x_norm^delta - beta)^alpha| / (2 * sigma^alpha))}
#'
#' These raw scores are then rescaled to [0, 1] based on the minimum and maximum values observed
#' per metric and land use type.
#'
#' The final \code{BV_norm} is calculated by interpolating the weighted mean of \eqn{y} (\code{BV_LU})
#' between reference values for "grassland" (min: 0.44, max: 0.92) and "arable" (min: 0.23, max: 0.52)
#' following Gallego-Zamorano et al. (2022) and Lindner et al. (2019).
#'
#' Finally:
#' \itemize{
#'   \item \eqn{BVI_{ha} = 1 - BV_{norm}} (Assuming \eqn{BV_{loc} \approx BV_{norm}})
#'   \item \eqn{BVI_{t} = BVI_{ha} / yield}
#' }
#'
#' @return A list containing two data frames:
#' \describe{
#'   \item{\code{y}}{Intermediate data frame with raw and rescaled metric scores per observation and metric code.}
#'   \item{\code{BVIAS}}{The aggregated results per farm/crop/year. Key columns include:
#'     \itemize{
#'       \item \code{BV_LU}: Weighted mean of metric scores.
#'       \item \code{BV_norm}: Normalized biodiversity value based on land use type ranges.
#'       \item \code{BVI_ha}: Biodiversity Value Index per hectare (inverse of local quality).
#'       \item \code{BVI_t}: Biodiversity Value Index per tonne of crop produced.
#'     }
#'   }
#' }
#'
#' @references
#' Lindner, J.P., Fehrenbach, H., Winter, L., Bloemer, J., Knuepffer, E., 2019. Valuing Biodiversity in Life Cycle Impact Assessment. Sustainability 11, 5628. https://doi.org/10.3390/su11205628
#' Gallego-Zamorano, J., Huijbregts, M.A.J., Schipper, A.M., 2022. Changes in plant species richness due to land use and nitrogen deposition across the globe. Diversity and Distributions 28, 745–755. https://doi.org/10.1111/ddi.13476
#'
#' @importFrom dplyr left_join mutate summarise group_by filter select case_when join_by
#' @importFrom stats weighted.mean
#' @importFrom tibble tibble
#'
#' @concept footprint-biodiv
#'
#' @export
#'
#'
#'


# estimate BVI per ha and per t for crops
f_BVIAS_crops <- function(object,
                          overwrite = FALSE,
                          BVIAS_constants = NULL,
                          BVIAS_weights = NULL) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$BVIAS$BVI_crop) && !overwrite) {
    message("Using cached values stored in object@footprints$BVIAS$BVI_crop.")
    return(object@footprints$BVIAS$BVI_crop)  # use cached value
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



  # Input data ------------------------------------------------------------------------------
  input_data <- data_for_BVIAS(object)

  # tmp = input_data |> group_by(metric_code, x_norm) |> summarise(n = n(), .groups = "drop")
  # ggplot(tmp) + aes(x = x_norm, y = n) + geom_point(colour = "#112446") + theme_minimal() + facet_wrap(vars(metric_code), scales = "free_y")

  # TODO: redo model optimization with only the four BVI parameters we've got in FADN

  tmp_y_raw <- input_data |>
    # add variable constants
    dplyr::left_join(
      BVIAS_constants,
      by = join_by(land_use_type, metric_code)) |>
    ## calculate BV
    dplyr::mutate(
      ## calculate BV
      y = gamma + epsilon * exp(-(
        abs((((x_norm)^delta) - beta) ^ alpha) /
          (2*sigma^alpha)))
    )

  # Constrain y
  tmp_y_limits = tmp_y_raw |>
    dplyr::summarise(
      y_min = min(y,na.rm = T),
      y_max = max(y,na.rm = T),
      .by = c(metric_code, land_use_type)
    )

  tmp_y <- tmp_y_raw |>
    # re scale y to range in [0;1]
    ## add limits
    dplyr::left_join(
      tmp_y_limits, by = join_by(metric_code,land_use_type)
    ) |>
    ## rescale
    dplyr::mutate(
      y = (y - y_min) / (y_max - y_min)
    ) |>
    dplyr::mutate(
      y = dplyr::case_when(
        # optimized constants can produced NAs at limits (i.e., 0 or 1), so I force y
        ## increasing functions
        metric_code %in% c("hedge_density","ground_cover","crop_diversity") & !is.finite(y) & x_norm == 0 ~ 0,
        metric_code %in% c("hedge_density","ground_cover","crop_diversity") & !is.finite(y) & x_norm == 1 ~ 1,
        ## decreasing functions
        !(metric_code %in% c("hedge_density","ground_cover","crop_diversity")) & !is.finite(y) & x_norm == 0 ~ 1,
        !(metric_code %in% c("hedge_density","ground_cover","crop_diversity")) & !is.finite(y) & x_norm == 1 ~ 0,
        .default = y
      )
    )

  # Estimate biodiversity value
  tmp_BVIAS <- tmp_y |>
    # add variable weights
    dplyr::left_join(
      BVIAS_weights,
      by= join_by(land_use_type, metric_code)) |>
    # remove variables without weight (e.g. tillage for grasslands, etc)
    dplyr::filter(!is.na(weight)) |>
    # BV LU
    # aggregate variables
    dplyr::group_by(dplyr::across(dplyr::all_of(object@traceability$id_cols)),ORGANIC,land_use_type,FADN_code_letter) |>
    dplyr::summarise(
      BV_LU = stats::weighted.mean(y,weight),
      .groups = "drop"
    ) |>
    # BV NORM
    # add land use type ranges
    dplyr::left_join(
      tibble(
        # set BV_norm min and max according to Gallego-Zamorano et al. (2022) and Lindner et al. (2019)
        land_use_type = c("grassland","arable"),
        LU_min = c(0.44,0.23),
        LU_max = c(0.92,0.52)
      ),
      by = "land_use_type") |>
    # normalize BV
    dplyr::mutate(
      BV_norm = LU_min + BV_LU * (LU_max - LU_min)) |>
    dplyr::mutate(
      # BV LOC
      #BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)), # we do not use the skewed function from Lindner et al. (2019)
      BV_loc = BV_norm,
      # BVI
      BVI_ha = 1- BV_loc
    ) |>
    # add yields
    dplyr::left_join(
      object@crop |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,yield),
      by = c(object@traceability$id_cols,"FADN_code_letter")
    ) |>
    # remove rows without yields
    dplyr::filter(!is.na(yield)) |>
    # convert BVI_ha to BVI_t
    dplyr::mutate(
      BVI_t = BVI_ha / yield
    )

  return(list(y = tmp_y, BVIAS = tmp_BVIAS))

}
