#' Estimate feed requirements for pseudo-herd animals from regional averages
#'
#' @description
#' f_pseudoherd_feed estimates the feed requirements and consumption patterns
#' for off-farm animals (pseudo-herd) inferred from purchased feed quantities.
#' Unlike f_herd_feed which computes individual farm feed patterns, this function
#' derives pseudoherd feed from regional (COUNTRY × YEAR × FADN_code_letter)
#' average herd feed consumption patterns, then scales them by the off-farm
#' animal population.
#'
#' @details
#' The function proceeds in the following steps:
#'
#' **Step 1 – Retrieve pseudoherd animals**
#' Off-farm animal numbers (Q_off_farm) are obtained from f_pseudoherd(),
#' which estimates the number of off-farm animals required to account for
#' observed purchased feed flows.
#'
#' **Step 2 – Compute average herd feed by region and year**
#' For each combination of COUNTRY, YEAR, and FADN_code_letter, the function:
#' \enumerate{
#'   \item Aggregates feed quantities (both on-farm and purchased) across all farms
#'   \item Computes weighted means of feed patterns using farm size (SYS02) as weight
#'   \item Derives average per-animal feed composition and nutritional content
#' }
#'
#' **Step 3 – Apply average patterns to pseudoherd**
#' The computed regional averages are joined to the pseudoherd data and:
#' \enumerate{
#'   \item Scaled by the number of off-farm animals: \eqn{DM_t = avg\_DM_t \times (Q_{off-farm} / avg\_Qobs)}
#'   \item Maintains the feed composition (feed type, Sailley_feed) from regional average
#'   \item Recomputes per-animal nutritional metrics
#' }
#'
#' **Step 4 – Return feed intake detail and summary**
#' Returns two tibbles analogous to f_herd_feed output:
#' - `feed_intake$detail`: per-feed-line consumption data for off-farm animals
#' - `feed_intake$total`: summarised nutritional totals per livestock category
#'
#' @param object An S4 object of class "FADN2Footprint". Must contain:
#'   - object@practices$herding$feed (computed from infer_practices)
#'   - object@farm with COUNTRY, SYS02 (for weighting)
#'   - object@traceability$id_cols (traceability identifier columns)
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns the cached object. If TRUE, recomputation is forced.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{feed_intake$detail}{A tibble with one row per feed line consumed by
#'     off-farm animals, containing:
#'     - traceability id columns (ID, YEAR, COUNTRY, etc.)
#'     - FADN_code_letter, species
#'     - Q_off_farm (number of off-farm animals)
#'     - feed_origin ("feed_produced" or "feed_purchased")
#'     - Sailley_feed, FADN_code_feed, feed_type
#'     - DM_t_livcat (scaled total dry matter for off-farm animals)
#'     - DM_t_anim, GE_MJ_anim, CP_t_anim (per-animal nutritional metrics)
#'     - animals = "off_farm" indicator}
#'   \item{feed_intake$total}{A tibble with one row per livestock category,
#'     summarising total feed intake across all feed lines:
#'     - traceability id columns, FADN_code_letter, species, animals
#'     - GE_MJ_anim, DM_t_anim (total per animal)
#'     - CP_p100 (crude protein as % of DM)}
#' }
#'
#' @seealso f_pseudoherd, f_herd_feed, h_average_practices
#'
#' @concept practice-pseudoherd
#' @export
#' @importFrom dplyr bind_rows left_join mutate group_by summarise across matches all_of select filter if_else coalesce
#' @importFrom stats weighted.mean
#'

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
  ## 2. Compute average herd feed by COUNTRY, YEAR, FADN_code_letter
  ## 3. Apply averages to pseudoherd animals
  ## 4. Summarise pseudoherd feed intake

  # 1. Retrieve pseudoherd animals ----

  pseudoherd <- f_pseudoherd(object)

  # 2. Compute average herd feed by region and year ----

  # Get on-farm herd feed detail
  herd_feed_detail <- object@practices$herding$feed$feed_intake$detail

  # Add farm characteristics (COUNTRY, SYS02 for weighting)
  herd_feed_with_farm <- herd_feed_detail |>
    dplyr::left_join(
      object@farm |>
        dplyr::select(dplyr::all_of(id_cols), COUNTRY, SYS02),
      by = id_cols
    )

  # Compute weighted average feed per feed type by COUNTRY, YEAR, FADN_code_letter
  # Using h_average_practices pattern
  target_feed_vars <- c("DM_t_livcat", "DM_t_anim", "GE_MJ_anim", "GE_MJ_kg",
                        "CP_t_anim", "CP_p100")

  # Compute regional averages for each feed type and origin
  avg_herd_feed <- h_average_practices(
    data = herd_feed_with_farm,
    target_vars = target_feed_vars,
    primary_grp = c("FADN_code_letter", "FADN_code_feed", "feed_origin", "Sailley_feed", "YEAR", "COUNTRY"),
    secondary_grp = c("FADN_code_letter", "FADN_code_feed", "feed_origin"),
    weight_var = "SYS02"
  )

  # Also compute average Qobs per livestock category
  avg_Qobs <- herd_feed_with_farm |>
    dplyr::summarise(
      Qobs = stats::weighted.mean(Qobs, w = SYS02, na.rm = TRUE),
      .by = c("FADN_code_letter", "YEAR", "COUNTRY")
    )

  # 3. Apply average patterns to pseudoherd animals ----

  pseudoherd_feed_detail <- pseudoherd |>
    dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, species,
                  Q_off_farm, Q_off_farm_milk, Q_off_farm_meat, Q_off_farm_eggs) |>
    # Add average Qobs to compute scaling ratio
    dplyr::left_join(
      avg_Qobs,
      by = c("YEAR", "COUNTRY", "FADN_code_letter")
    ) |>
    # Add average feed per feed type
    dplyr::left_join(
      avg_herd_feed,
      by = c("YEAR", "COUNTRY", "FADN_code_letter", "FADN_code_feed", "feed_origin"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(!is.na(DM_t_livcat)) |>
    # Scale by off-farm animal ratio
    dplyr::mutate(
      # Scaling factor: off-farm animals / average observed animals
      scale_factor = dplyr::if_else(
        Qobs > 0,
        Q_off_farm / Qobs,
        0
      ),
      # Scale quantities
      DM_t_livcat = DM_t_livcat * scale_factor,
      DM_t_anim = dplyr::if_else(
        Q_off_farm > 0,
        DM_t_livcat / Q_off_farm,
        0
      ),
      GE_MJ_anim = dplyr::if_else(
        Q_off_farm > 0,
        DM_t_anim * GE_MJ_kg * 1000,
        0
      ),
      CP_t_anim = dplyr::if_else(
        Q_off_farm > 0,
        DM_t_anim * CP_p100 / 100,
        0
      ),
      # Add pseudoherd indicator
      animals = "off_farm"
    ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter, species, animals, Q_off_farm,
      feed_origin, Sailley_feed, FADN_code_feed, feed_type,
      DM_t_livcat, DM_t_anim, GE_MJ_anim, GE_MJ_kg, CP_p100, CP_t_anim
    )

  # 4. Summarise pseudoherd feed intake total ----

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
