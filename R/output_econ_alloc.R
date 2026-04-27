#' @title Economic allocation of farm outputs
#'
#' @description
#' Estimates economic allocation keys at three levels of aggregation:
#' \enumerate{
#'   \item \strong{All outputs (farm level):} Each output's share of total farm
#'     sales (crops + herd), useful for allocating shared farm-level inputs
#'     (e.g. electricity).
#'   \item \strong{Crop outputs:} Each crop output's share of total crop output
#'     value, useful for allocating crop-specific inputs (e.g. off-road diesel)
#'     across crop products. Feed crops with no market sales are
#'     assigned an imputed economic value (farm use value) based on total herd
#'     output value distributed proportionally to their dry matter use as feed.
#'   \item \strong{Herd outputs:} Each herd output's share of total herd sales
#'     at the farm level and within species × activity, useful for allocating
#'     herd-specific inputs (e.g. heating fuel) across animal products
#'     (milk, meat, eggs, wool).
#' }
#'
#' Economic allocation follows the life cycle assessment (LCA) approach where
#' environmental burdens of a multi-output process are partitioned among
#' co-products proportionally to their economic value.
#'
#' @param object An object of class \code{FADN2Footprint}. Must contain at
#'   least the following slots:
#'   \describe{
#'     \item{\code{output$crop}}{A data frame of crop outputs with at least
#'       \code{sales_e} (sales in EUR) and \code{FADN_code_letter} columns.}
#'     \item{\code{output$meat}}{A data frame of meat outputs with at least
#'       \code{sales_e}, \code{output}, \code{FADN_code_letter}, and
#'       \code{species} columns.}
#'     \item{\code{output$other_herd_products}}{A data frame of other herd
#'       product outputs (milk, eggs, wool, etc.) with the same columns as
#'       \code{output$meat}.}
#'     \item{\code{practices$herding$feed$feed_produced}}{A data frame of
#'       on-farm feed production with \code{FADN_code_feed} (crop code) and
#'       \code{DM_t_livcat} (dry matter tonnes per livestock category) columns,
#'       used to impute the economic value of feed crops.}
#'     \item{\code{traceability$id_cols}}{A character vector of column names
#'       used as farm/year identifiers for grouping operations.}
#'   }
#'
#' @return A named list with three data frames:
#'   \describe{
#'     \item{\code{all_outputs}}{Farm-level allocation across all outputs
#'       (crops + herd). Contains \code{sum_sales_e_farm} (total farm sales)
#'       and \code{econ_alloc_ratio_farm} (output sales / total farm sales).}
#'     \item{\code{crop_outputs}}{Crop-level allocation across all crops.
#'       Contains \code{TO_e} (total output value: market sales + imputed farm
#'       use value for feed crops), \code{farm_use_e} (imputed value for
#'       feed crops, \code{NA} for cash crops), \code{sum_sales_e_farm}
#'       (total crop output value at farm level), and
#'       \code{econ_alloc_ratio_crop} (\code{TO_e} / total crop output value).}
#'     \item{\code{herd_outputs}}{Herd-level allocation. Contains
#'       \code{sum_sales_e_farm} (total herd sales at farm level),
#'       \code{sum_sales_e_activity} (total sales per species × activity),
#'       \code{econ_alloc_ratio_herd} (output sales / total herd farm sales),
#'       and \code{econ_alloc_ratio_herd_activity} (output sales / species ×
#'       activity sales).}
#'   }
#'
#' @details
#' \strong{Handling of feed crops:}
#'
#' Feed crops (e.g. fodder maize, grasslands, temporary pastures) typically
#' have no market sales because they are consumed on-farm by livestock. To
#' include them in the crop-level economic allocation, a farm use value
#' (\code{farm_use_e}) is imputed as follows:
#' \enumerate{
#'   \item Total herd output value at the farm level is computed (sum of
#'     \code{sales_e} across all herd products).
#'   \item Total on-farm feed dry matter production is computed per crop and
#'     across all feed crops.
#'   \item The herd output value is distributed across feed crops
#'     proportionally to their dry matter production:
#'     \deqn{farm\_use\_e_i = \sum sales\_e^{herd} \times
#'       \frac{DM_i}{\sum DM_{feed}}}
#' }
#'
#' The total output value (\code{TO_e}) for each crop is then the sum of
#' market sales and imputed farm use value:
#' \deqn{TO\_e = sales\_e + farm\_use\_e}
#'
#' This ensures that feed crops receive an economic weight reflecting their
#' contribution to herd production, enabling a unified economic allocation
#' across all crops.
#'
#' \strong{Herd output classification:}
#'
#' Herd outputs are classified into activities based on pattern matching on the
#' \code{output} and \code{FADN_code_letter} columns:
#' \itemize{
#'   \item \code{"milk"}: outputs matching \code{"milk"} or dairy cow codes
#'     (\code{LCOWDAIR}).
#'   \item \code{"meat"}: outputs matching \code{"meat"}.
#'   \item \code{"eggs"}: outputs matching \code{"eggs"}.
#'   \item \code{"wool"}: outputs matching \code{"wool"}.
#'   \item \code{"farm"}: all other herd outputs.
#' }
#'
#' @seealso
#' \code{\link{f_herd_output}} for herd output estimation,
#' \code{\link{f_crop_output}} for crop output estimation,
#' \code{\link{f_feed_intake}} for on-farm feed production estimation.
#'
#' @references
#' \itemize{
#'   \item ISO 14044:2006. Environmental management — Life cycle assessment —
#'     Requirements and guidelines.
#'   \item European Commission (2018). Product Environmental Footprint Category
#'     Rules Guidance (PEFCR Guidance v6.3).
#' }
#'
#' @examples
#' \dontrun{
#' # Assuming 'fadn_obj' is a valid FADN2Footprint object
#' alloc <- f_output_econ_alloc(fadn_obj)
#'
#' # Farm-level allocation ratios
#' head(alloc$all_outputs)
#'
#' # Crop-level allocation ratios (cash + feed with imputed value)
#' head(alloc$crop_outputs)
#'
#' # Herd-level allocation ratios
#' head(alloc$herd_outputs)
#' }
#'
#' @export
#' @importFrom dplyr mutate case_when if_else bind_rows summarise left_join
#'   select rename coalesce all_of
#' @importFrom stringr str_detect

#'
#' @concept data-preparation
#' @export
#' @importFrom dplyr mutate case_when if_else bind_rows all_of
#' @importFrom stringr str_detect


f_output_econ_alloc <- function(object) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  # Steps:
  ## 1. Aggregate all outputs
  ## 2. Estimate economic allocation at three levels:
  ##    a. Across ALL outputs (farm level)
  ##    b. Across CROP outputs only
  ##    c. Across HERD outputs only

  id_cols <- object@traceability$id_cols

  # 1. Aggregate outputs -------------------------------------------------------------------

  ## 1a. Herd outputs
  herd_output <- Reduce(bind_rows, list(
    #object@output@living_animals,
    object@output$meat,
    object@output$other_herd_products
  )) |>
    dplyr::mutate(
      activity = dplyr::case_when(
        str_detect(output, "milk")       ~ "milk",
        FADN_code_letter == "LCOWDAIR"   ~ "milk",
        str_detect(output, "meat")       ~ "meat",
        str_detect(output, "eggs")       ~ "eggs",
        str_detect(output, "wool")       ~ "wool",
        # TODO: check for wool, honey, etc
        .default = "farm"
      )
    ) |>
    dplyr::mutate(
      TO_e = dplyr::coalesce(sales_e,0)
    )

  ## 1b. Crop outputs

  ### Retrieve cash crop value
  cash_crops <- object@output$crop |>
    dplyr::mutate(
      activity = "crop"
    )

  ### Estimate feed crop value
  fodder_crops <- object@practices$herding$feed$feed_produced |>
    # total DM_t per crop
    dplyr::summarise(
      feed_t_DM = sum(DM_t_livcat),
      .by = c(id_cols, 'FADN_code_feed')
    ) |>
    dplyr::mutate(farm_t_DM = sum(feed_t_DM, na.rm = T),
                  .by = id_cols) |>
    dplyr::rename(FADN_code_letter = FADN_code_feed) |>
    # add herd product total output per farm
    dplyr::left_join(
      herd_output |>
        dplyr::summarise(feed_TO = sum(sales_e,na.rm = T),
                         .by = id_cols),
      by = id_cols
    )|>
    # estimate farm use value
    dplyr::mutate(
      farm_use_e = feed_TO * (feed_t_DM / farm_t_DM)
    )

  ### Combine cash crop and feed crop value

  crop_output <- cash_crops |>
    dplyr::left_join(
      fodder_crops |>
        dplyr::select(dplyr::all_of(id_cols),
                      FADN_code_letter, farm_use_e),
      by = c(id_cols, 'FADN_code_letter')
    ) |>
    dplyr::mutate(
      TO_e = dplyr::coalesce(sales_e,0) + dplyr::coalesce(farm_use_e, 0))




  ## 1c. All outputs combined
  all_output <- dplyr::bind_rows(crop_output, herd_output)

  # 2. Economic allocation -----------------------------------------------------------------

  ## 2a. Allocation across ALL outputs (farm level) ----------------------------------------
  ##     Each output's share of total farm sales
  all_output_econ_alloc <- all_output |>
    dplyr::mutate(
      sum_sales_e_farm = sum(sales_e, na.rm = TRUE),
      .by = c(all_of(id_cols))
    ) |>
    dplyr::mutate(
      econ_alloc_ratio_farm = dplyr::if_else(
        sum_sales_e_farm > 0,
        sales_e / sum_sales_e_farm,
        NA_real_
      )
    )

  ## 2b. Allocation across CROP outputs only -----------------------------------------------
  ##     Each crop output's share of total crop sales
  crop_output_econ_alloc <- crop_output |>
    dplyr::mutate(
      sum_sales_e_farm = sum(TO_e, na.rm = TRUE),
      .by = c(all_of(id_cols))
    ) |>
    dplyr::mutate(
      econ_alloc_ratio_crop = dplyr::if_else(
        sum_sales_e_farm > 0,
        TO_e / sum_sales_e_farm,
        NA_real_
      )
    )

  ## 2c. Allocation across HERD outputs only -----------------------------------------------
  ##     Each herd output's share within species and/or activity
  herd_output_econ_alloc <- herd_output |>
    # sum sales per farm
    dplyr::mutate(
      sum_sales_e_farm = sum(sales_e, na.rm = TRUE),
      .by = c(all_of(id_cols))
    ) |>
    # sum sales per species and activity
    dplyr::mutate(
      sum_sales_e_activity = sum(sales_e, na.rm = TRUE),
      .by = c(all_of(id_cols), species, activity)
    ) |>
    # economic allocation ratio within or across herd activities
    dplyr::mutate(
      econ_alloc_ratio_herd_activity = sales_e / sum_sales_e_activity,
      econ_alloc_ratio_herd = sales_e / sum_sales_e_farm
      )

  # 3. Return results ----------------------------------------------------------------------

  return(list(
    all_outputs  = all_output_econ_alloc,
    crop_outputs = crop_output_econ_alloc,
    herd_outputs = herd_output_econ_alloc
  ))
}
