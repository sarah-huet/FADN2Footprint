#' Estimate feed requirements for pseudo-herd animals
#'
#' @description
#' f_pseudoherd_feed estimates the feed requirements and consumption patterns
#' for off-farm animals (pseudo-herd) inferred from purchased feed quantities.
#' It follows the same methodology as f_herd_feed but applies to the off-farm
#' animal populations estimated by f_pseudoherd.
#'
#' @details
#' The function proceeds in the following steps:
#'
#' **Step 1 – Retrieve pseudoherd animal structure**
#' Off-farm animal numbers (Q_off_farm) are obtained from f_pseudoherd(),
#' which estimates the number of off-farm animals required to account for
#' observed purchased feed flows.
#'
#' **Step 2 – Estimate pseudoherd feed intake**
#' Feed intake is estimated separately for on-farm and purchased feed using:
#' - On-farm produced feed consumed by off-farm animals (minimal in most cases)
#' - Purchased feed sourced to feed off-farm animals
#'
#' The logic mirrors f_herd_feed() but applies to Q_off_farm instead of Qobs:
#' \enumerate{
#'   \item Retrieve theoretical feed requirements per animal from data_extra
#'   \item Multiply by off-farm animal counts (Q_off_farm) by activity
#'   \item Distinguish between on-farm and purchased feed based on farm supply
#'   \item Compute nutritional metrics (DM_t_anim, GE_MJ_anim, CP_t_anim)
#' }
#'
#' **Step 3 – Return feed intake detail and summary**
#' Returns two tibbles similar to f_herd_feed output:
#' - `feed_intake$detail`: per-feed-line consumption data for off-farm animals
#' - `feed_intake$total`: summarised total per animal per livestock category
#'
#' @param object An S4 object of class "FADN2Footprint". Must contain:
#'   - object@practices$herding (with feed intake and activity data)
#'   - object@traceability$id_cols (traceability identifier columns)
#'   - sufficient FADN2Footprint object data to compute pseudoherd
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns the cached object. If TRUE, recomputation is forced.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{feed_intake$detail}{A tibble with one row per feed line consumed by
#'     off-farm animals, containing:
#'     - traceability id columns
#'     - FADN_code_letter, species
#'     - Q_off_farm (number of off-farm animals)
#'     - feed_origin ("feed_produced" or "feed_purchased")
#'     - Sailley_feed, FADN_code_feed, feed_type
#'     - Nutritional metrics: DM_t_anim, GE_MJ_anim, CP_t_anim
#'     - Quantity: DM_t_livcat (total dry matter per livestock category)}
#'   \item{feed_intake$total}{A tibble with one row per livestock category,
#'     summarising total feed intake across all sources:
#'     - traceability id columns, FADN_code_letter, species
#'     - GE_MJ_anim, DM_t_anim, CP_p100 (crude protein as % of DM)}
#' }
#'
#' @seealso f_pseudoherd, f_herd_feed, f_herd_rearing_param_cattle,
#'   f_herd_rearing_param_swine, f_herd_rearing_param_poultry
#'
#' @concept practice-pseudoherd
#' @export
#' @importFrom dplyr bind_rows left_join mutate group_by summarise across matches all_of select filter
#' @importFrom tidyr pivot_longer pivot_wider

f_pseudoherd_feed <- function(object,
                              overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  if (!is.null(object@practices$herding$pseudoherd_feed) && !overwrite) {
    message("Using cached values stored in object@practices$herding$pseudoherd_feed")
    return(object@practices$herding$pseudoherd_feed)
  }

  id_cols <- object@traceability$id_cols

  # STEPS:
  ## 1. Retrieve pseudoherd animals
  ## 2. Estimate pseudoherd feed intake (detail)
  ## 3. Summarise pseudoherd feed intake (total)

  # 1. Retrieve pseudoherd animals ----

  pseudoherd <- f_pseudoherd(object)

  # 2. Estimate pseudoherd feed intake detail ----

  # Get herd feed detail (on-farm and purchased)
  herd_feed_detail <- object@practices$herding$feed$feed_intake$detail

  # Get on-farm herd activities to determine how much is produced vs. purchased
  herd_activities <- f_herd_activities(object)

  # Create pseudoherd feed detail by replicating herd feed patterns
  # but using off-farm animal quantities instead of observed animals

  pseudoherd_feed_detail <- herd_feed_detail |>
    # Join with pseudoherd animals to get off-farm counts
    dplyr::left_join(
      pseudoherd |>
        dplyr::select(dplyr::all_of(id_cols), FADN_code_letter,
                      Q_off_farm, Q_off_farm_milk, Q_off_farm_meat, Q_off_farm_eggs),
      by = c(id_cols, "FADN_code_letter")
    ) |>
    # Filter to rows with off-farm animals
    dplyr::filter(Q_off_farm > 0) |>
    # Determine activity for each feed line based on output
    dplyr::mutate(
      activity = dplyr::case_when(
        species == "cattle" & grepl("milk|dairy", tolower(FADN_code_letter)) ~ "milk",
        species == "cattle" & grepl("beef|fattening|bovine", tolower(FADN_code_letter)) ~ "meat",
        grepl("meat", tolower(feed_origin)) ~ "meat",
        .default = "other"
      )
    ) |>
    # Scale feed quantities by off-farm animals (maintain per-animal consumption patterns)
    dplyr::mutate(
      # Use appropriate off-farm animal count based on activity
      Q_pseudoherd_activity = dplyr::case_when(
        activity == "milk" ~ Q_off_farm_milk,
        activity == "meat" ~ Q_off_farm_meat,
        activity == "eggs" ~ Q_off_farm_eggs,
        .default = Q_off_farm
      ),
      # Scale DM_t_livcat by ratio of off-farm to on-farm animals
      DM_t_livcat = dplyr::if_else(
        Q_pseudoherd_activity > 0 & Qobs > 0,
        DM_t_livcat * (Q_pseudoherd_activity / Qobs),
        0
      ),
      # Recalculate per-animal metrics
      DM_t_anim = dplyr::if_else(
        Q_pseudoherd_activity > 0,
        DM_t_livcat / Q_pseudoherd_activity,
        0
      ),
      GE_MJ_anim = DM_t_anim * GE_MJ_kg * 1000,
      CP_t_anim = DM_t_anim * CP_p100 / 100,
      # Add pseudoherd indicator
      animals = "off_farm"
    ) |>
    dplyr::select(dplyr::all_of(id_cols),
                  FADN_code_letter, species, animals,
                  Q_pseudoherd_activity,
                  activity, feed_origin, Sailley_feed, FADN_code_feed, feed_type,
                  DM_t_livcat, DM_t_anim, GE_MJ_anim, GE_MJ_kg, CP_p100, CP_t_anim)

  # 3. Summarise pseudoherd feed intake total ----

  pseudoherd_feed_total <- pseudoherd_feed_detail |>
    dplyr::summarise(
      # Nutritional totals per animal
      GE_MJ_anim = sum(GE_MJ_anim, na.rm = TRUE),
      DM_t_anim = sum(DM_t_anim, na.rm = TRUE),
      CP_p100 = dplyr::if_else(
        DM_t_anim > 0,
        100 * sum(CP_t_anim, na.rm = TRUE) / DM_t_anim,
        NA_real_
      ),
      .by = c(dplyr::all_of(id_cols), FADN_code_letter, species, animals)
    )

  # Return ----

  result <- list(
    feed_intake = list(
      detail = pseudoherd_feed_detail,
      total = pseudoherd_feed_total
    )
  )

  return(result)
}
