#' Estimate livestock feed quantity and quality from on-farm and purchased sources
#'
#' @description
#' f_herd_feed estimates the quantity and nutritional quality of livestock
#' feed consumed on each farm, combining both on-farm produced (farm-grown)
#' and off-farm purchased feed to form a complete "pseudo-farm" feed budget.
#' The estimation is based on a theoretical feed ration constructed for each
#' farm's livestock category, derived from dry matter intake requirements per
#' livestock unit (AROPAJ model; Jayet et al., 2023) combined with
#' species-specific national feed consumption volumes (Sailley et al., 2021;
#' see `FADN2Footprint::data_extra$Sailley_2021_feed_flows`).
#'
#' @details
#' The function implements the following steps:
#'
#' **Step 1 – Retrieve observed herd structure:**
#' Calls f_herd_activities(object) to retrieve the farm herd composition,
#' including the allocation of each animal category to activities
#' (milk, meat, eggs). Observed animal stock is considered as the mean
#' between average, opening, and closing stock variables.
#'
#' **Step 2 – Estimate livestock feed intake:**
#' Calls f_feed_onfarm(object) and f_feed_offfarm(object) to estimate the
#' quantities and nutritional quality (dry matter, gross energy, crude
#' protein, ash) of on-farm produced and purchased feed respectively.
#' These are combined into a single feed intake table with a feed_origin
#' label ("feed_produced" or "feed_purchased").
#'
#' **Step 3 – Combine herd structure with feed:**
#' Joins the combined feed intake table to the herd activity table by
#' traceability id columns and FADN animal code. Per-animal feed quantities
#' and quality metrics are computed by dividing crop-level totals by the
#' observed number of animals (Qobs):
#' - DM_t_anim (tonnes dry matter per animal per year),
#' - GE_MJ_anim, GE_kcal_anim (gross energy per animal),
#' - CP_t_anim (crude protein per animal),
#' - Ash_t_anim (ash per animal).
#'
#' A summarised total feed intake per animal (GE_MJ_anim, DM_t_anim) and
#' crude protein content (CP_p100) are then computed by aggregating across
#' all feed sources per animal category.
#'
#' The function returns cached results stored in object@practices$herding$feed
#' when present and `overwrite = FALSE`.
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the package
#'   workflow. The function depends on:
#'   - object@traceability$id_cols (vector of id column names used for joins),
#'   - f_herd_activities(object) to supply observed herd structure and activity
#'     allocation,
#'   - f_feed_onfarm(object) and f_feed_offfarm(object) to supply feed quantity
#'     and quality by crop and feed type,
#'   - package data (data_extra$Sailley_2021_feed_flows) and theoretical
#'     ration helpers for feed estimation.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{feed_produced}{A tibble of on-farm produced feed quantities and
#'     nutritional quality per farm and crop (output of f_feed_onfarm).}
#'   \item{feed_purchased}{A tibble of purchased feed quantities and
#'     nutritional quality per farm and feed type (output of f_feed_offfarm).}
#'   \item{feed_intake$detail}{A tibble joining feed quantities (both origins)
#'     to herd activity, with per-animal nutritional metrics
#'     (DM_t_anim, GE_MJ_anim, GE_kcal_anim, CP_t_anim, Ash_t_anim).
#'     Columns include traceability id columns, FADN_code_letter, species,
#'     Qobs*, feed_origin, and nutritional summaries.}
#'   \item{feed_intake$total}{A summarised tibble with total feed intake
#'     per farm and animal category (FADN_code_letter): GE_MJ_anim,
#'     DM_t_anim, and crude protein content CP_p100 (percent of DM).}
#' }
#'
#' @references
#' Jayet, P.-A. et al. (2023). AROPAJ model. [reference details to be added]
#'
#' Sailley, S.F. et al. (2021). National feed consumption volumes by species.
#' See `FADN2Footprint::data_extra$Sailley_2021_feed_flows`.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' herd_feed <- f_herd_feed(f)
#'
#' # Inspect detailed feed intake
#' head(herd_feed$feed_intake$detail)
#'
#' # Inspect total per-animal feed intake summary
#' head(herd_feed$feed_intake$total)
#'
#' # Force recomputation, ignoring cached values
#' herd_feed2 <- f_herd_feed(f, overwrite = TRUE)
#' }
#'
#' @seealso f_feed_onfarm, f_feed_offfarm, f_herd_activities,
#'   f_feed_theo_ration, [data_extra$Sailley_2021_feed_flows]
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr bind_rows left_join mutate group_by summarise across matches all_of


f_herd_feed <- function(object,
                        overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }


  if (!is.null(object@practices$herding$feed)&& !overwrite) {
    message("Using cached values stored in object@practices$herding$feed.")
    return(object@practices$herding$feed)  # use cached value
  }

  ## Steps:
  ## 1. Retrieve observed herd structure
  ### We considered the observed animal stock as the mean between average, opening, and closing variables
  ## 2. Estimate livestock intake of feed (both produced and purchased)
  ## 3. Combine herd structure with feed

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_activity = f_herd_activities(object, overwrite = overwrite)

  # 2. Estimate livestock intake of feed (both produced and purchased) ---------------------------------------------------------------------------------

  feed_produced = f_feed_onfarm(object, overwrite = overwrite)

  feed_purchased = f_feed_offfarm(object, overwrite = overwrite)

  feed_intake <- bind_rows(
    feed_produced |>
      dplyr::mutate(feed_origin = "feed_produced"),
    feed_purchased |>
      dplyr::mutate(feed_origin = "feed_purchased"))

  # 3. Combine herd structure with feed ---------------------------------------------------------------------------------

  herd_feed_detail <- left_join(
    feed_intake,
    herd_activity,
    by = c(object@traceability$id_cols, "FADN_code_letter")
  ) |>
    # DM of each crop by animal: t DM animal-1 y-1
    dplyr::mutate(
      DM_t_anim = DM_t_livcat / Qobs,
      GE_MJ_anim = GE_MJ_livcat / Qobs,
      GE_kcal_anim = GE_kcal_livcat / Qobs,
      CP_t_anim = CP_t_livcat / Qobs,
      Ash_t_anim = Ash_t_livcat / Qobs
    )

  feed_intake <- herd_feed_detail |>
    # estimate total feed intake per animal
    dplyr::group_by(dplyr::across(object@traceability$id_cols),
                    FADN_code_letter,species,
                    dplyr::across(dplyr::matches("Qobs"))) |>
    dplyr::summarise(
      GE_MJ_anim = sum(GE_MJ_anim,na.rm = T),
      DM_t_anim = sum(DM_t_anim,na.rm = T),
      # add crude protein content
      CP_p100 = sum(CP_t_livcat,na.rm = T) / sum(DM_t_livcat,na.rm = T) * 100,
      .groups = "drop"
    )

  # Output ----

  return(list(
    'feed_produced' = feed_produced,
    'feed_purchased' = feed_purchased,
    'feed_intake' = list(
      'detail' = herd_feed_detail,
      'total' = feed_intake))
    )

}

# Checks
#nrow(herd_feed_detail) == length(unique(paste0(herd_feed_detail$ID,
#                                               herd_feed_detail$YEAR,
#                                               herd_feed_detail$FADN_code_letter,
#                                               herd_feed_detail$FADN_code_feed,
#                                               herd_feed_detail$Sailley_feed,
#                                               herd_feed_detail$feed_origin)))
#

#nrow(feed_intake) == length(unique(paste0(feed_intake$ID,
#                                          feed_intake$YEAR,
#                                          feed_intake$FADN_code_letter)))
#
