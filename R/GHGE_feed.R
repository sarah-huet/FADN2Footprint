#' Estimate GHG Emissions from Livestock Feed (On-Farm and Off-Farm)
#'
#' @description
#' Calculates the greenhouse gas (GHG) emissions associated with feed
#' consumption by livestock, covering both on-farm produced and purchased
#' (off-farm) feed. Results are returned from cache if already computed for
#' the provided \code{FADN2Footprint} object.
#'
#' The function follows a four-step approach:
#' \enumerate{
#'   \item **Feed estimation**: on-farm and off-farm feed intake is retrieved
#'     from \code{\link{infer_practices}}, providing feed quantities per
#'     livestock category and feed type.
#'   \item **On-farm feed impact**: GHG emissions from on-farm produced crops
#'     used as feed are retrieved from \code{\link{f_GHGE_crops}} and matched
#'     to feed flows; crop area allocated to feed is back-calculated from dry
#'     matter intake and crop yield.
#'   \item **Off-farm feed impact**: GHG emissions for purchased feed are
#'     estimated by matching feed categories (Sailley typology) to average
#'     crop-level GHG intensities, computed from observed on-farm practices at
#'     COUNTRY × farming system × organic status level, with a hierarchical
#'     fallback to broader averages when local data are insufficient.
#'     Soybean meal is treated separately using literature values (see below).
#'   \item **Aggregation**: on-farm and off-farm feed impacts are combined into
#'     a single herd-level feed impact table.
#' }
#'
#' @details
#' ## On-Farm Feed Impact
#' For feed produced on the farm, GHG emissions per tonne and per hectare are
#' taken directly from \code{\link{f_GHGE_crops}}. The area allocated to feed
#' production is estimated as:
#' \deqn{area\_ha_{feed} = \frac{DM\_t\_crop}{yield}}
#' where \eqn{DM\_t\_crop} is the dry matter tonnage of the crop allocated to
#' the feed ration and \eqn{yield} is the observed on-farm crop yield
#' (t DM ha\eqn{^{-1}}).
#'
#' ## Off-Farm Feed Impact
#' Purchased feed GHG intensities are estimated via a two-level spatial
#' averaging procedure using \code{\link{h_average_practices}}:
#' \itemize{
#'   \item **Primary grouping**: FADN livestock category ×  Sailley feed
#'     category × survey year × COUNTRY region × organic status, weighted by
#'     utilised agricultural area (\code{SYS02}).
#'   \item **Secondary fallback**: FADN livestock category × Sailley feed
#'     category × organic status (cross-year, cross-region mean), used when
#'     primary-level averages are unavailable (e.g., farms that purchase all
#'     their feed and record no crop data).
#' }
#' Feed categories follow the Sailley et al. (2021) feed flow typology
#' (\code{\link{data_extra}}\code{$Sailley_2021_feed_flows}), which maps FADN
#' crop codes to aggregated feed ingredient classes.
#'
#' ## Soybean Meal
#' Soybean meal (\code{Sailley_feed == "Dont_tourteau_de_soja"}) is treated
#' separately because it is predominantly imported from Latin America
#' (Overmars et al., 2015). Fixed literature values are applied:
#' \itemize{
#'   \item Yield: 2.45 t ha\eqn{^{-1}} (Latin American average,
#'     Overmars et al., 2015)
#'   \item GHG intensity: 0.85 kg CO2-eq kg\eqn{^{-1}} soybean meal,
#'     excluding land-use change and deforestation (Ecoalim v9)
#'   \item Derived: \eqn{total\_ghg\_crop\_kgCO2e\_per\_ha} =
#'     \eqn{0.85 \times 10^3 \times 2.45}
#' }
#'
#' @note
#' \itemize{
#'   \item The cache key used is \code{object@footprints$BVIAS$BVI_feed};
#'     if non-null, the cached result is returned immediately without
#'     recomputation.
#'   \item Additional arguments passed via \code{...} are currently reserved
#'     for future use (e.g., alternative emission factor sources or
#'     deforestation adjustments for soybean).
#'   \item GHG emissions from soybean meal currently exclude land-use change
#'     (deforestation) contributions; inclusion is planned in a future release.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param ... Additional arguments reserved for future use.
#'
#' @return A named \code{list} with two elements:
#' \describe{
#'   \item{herd_feed}{\code{\link[tibble]{tibble}}. Detailed feed ration per
#'     livestock category and feed type, as produced by
#'     \code{\link{infer_practices}}, enriched with farm characteristics
#'     (COUNTRY, organic status, UAA).}
#'   \item{herd_feed_impact}{\code{\link[tibble]{tibble}}. Feed ration with
#'     associated GHG impact estimates. One row per farm × livestock category ×
#'     feed item combination, with the following key columns:
#'     \describe{
#'       \item{...}{Traceability identifier columns
#'         (\code{object@traceability$id_cols}).}
#'       \item{FADN_code_letter}{\code{character}. FADN livestock category code.}
#'       \item{FADN_code_feed}{\code{character}. FADN crop code of the feed
#'         ingredient (on-farm feed only).}
#'       \item{Sailley_feed}{\code{character}. Sailley feed ingredient category.}
#'       \item{feed_origin}{\code{character}. \code{"feed_produced"} for on-farm
#'         feed or \code{"feed_purchased"} for purchased feed.}
#'       \item{DM_t_livcat}{\code{numeric}. Dry matter quantity of the feed
#'         ingredient allocated to the livestock category (t DM yr\eqn{^{-1}}).}
#'       \item{yield}{\code{numeric}. Crop yield used to back-calculate feed
#'         area (t DM ha\eqn{^{-1}}).}
#'       \item{area_ha_livcat}{\code{numeric}. Area allocated to feed production (ha).}
#'       \item{..._kgCO2e}{\code{numeric}. GHG emission components from
#'         \code{\link{f_GHGE_crops}} (kg CO2-eq yr\eqn{^{-1}}), including
#'         \code{total_ghg_crop_kgCO2e}, \code{N2O_d_kgCO2e},
#'         \code{N2O_ATD_kgCO2e}, \code{N2O_L_kgCO2e},
#'         \code{ghg_ferti_prod_kgCO2e}, \code{ghg_diesel_crop_kgCO2e},
#'         \code{ghg_elec_crop_kgCO2e}.}
#'       \item{total_ghg_crop_kgCO2e_per_t}{\code{numeric}. GHG intensity per
#'         tonne of dry matter (kg CO2-eq t\eqn{^{-1}} DM).}
#'       \item{total_ghg_crop_kgCO2e_per_ha}{\code{numeric}. GHG intensity per
#'         hectare (kg CO2-eq ha\eqn{^{-1}}).}
#'     }
#'   }
#' }
#'
#' @references
#' Overmars, K.P., Gerber, P., Leip, A., Lesschen, J.P., Witzke, H.P. and
#' Verburg, P.H. (2015). Linking land use to food supply chains to assess
#' the environmental footprint of EU diets. \emph{Global Environmental Change},
#' 35, 335–346. \doi{10.1016/j.gloenvcha.2015.09.007}
#'
#' Sailley, S.F. et al. (2021). Feed use in European livestock systems:
#' A review and application to national feed balance sheets.
#' \emph{Animal Feed Science and Technology}, 272, 114782.
#' \doi{10.1016/j.anifeedsci.2020.114782}
#'
#' Ecoalim (v9). LCA database for animal feed ingredients.
#' INRAE, France. \url{https://ecoalim.inrae.fr}
#'
#' @seealso
#' \code{\link{f_GHGE_crops}}, \code{\link{infer_practices}},
#' \code{\link{f_feed_onfarm}}, \code{\link{f_feed_offfarm}},
#' \code{\link{h_average_practices}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr left_join filter select mutate across all_of bind_rows summarise distinct anti_join matches case_when
#' @importFrom tidyr separate_longer_delim
#'
#' @concept footprint-ghge
#'
#' @export

f_GHGE_feed <- function(object,
                        overwrite = FALSE,
                        ...) {
  if (!inherits(object,  "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_feed)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_feed")
    return(object@footprints$GHGE$GHGE_feed)  # use cached value
  }

  id_cols = object@traceability$id_cols
  # STEPS:
  ## 1. Estimate on-farm and off-farm feed
  ## 2. Estimate on-farm crop impact
  ## 3. Estimate off-farm crop impact

  # 1. Estimate on-farm and off-farm feed ------------------------------------------------------------------------------

  #object <- infer_practices(object, overwrite = overwrite)

  herd_feed <- f_herd_feed(object, overwrite = overwrite)

  herd_feed <- herd_feed$feed_intake$detail |>
    # add farm characteristics
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols),
                                     ORGANIC, SYS02),
                     by = id_cols)

  # 2. Estimate on-farm crop practices and impact ------------------------------------------------------------------------------

  onfarm_crops_ghge = f_GHGE_crops(object, overwrite = overwrite)

  feed_produced_impact = herd_feed |>
    dplyr::filter(feed_origin == "feed_produced") |>
    # add impact
    dplyr::left_join(
      onfarm_crops_ghge |>
        dplyr::rename(FADN_code_feed = FADN_code_letter),
      by = c(id_cols, "FADN_code_feed")
    ) |>
    # recalculate area for feed
    dplyr::mutate(area_ha_livcat = DM_t_livcat / yield) |>
    # select variables
    dplyr::select(dplyr::all_of(id_cols),
                  ORGANIC, SYS02,
                  FADN_code_letter, FADN_code_feed, feed_type, feed_origin, Sailley_feed,
                  dplyr::matches("Qobs"),
                  DM_t_livcat, yield, area_ha_livcat,
                  dplyr::matches("kgCO2e"))

  # 3. Estimate off-farm crop practices and impact ------------------------------------------------------------------------------

  ## 3.1. Average feed impact ----

  # To estimate off-farm feed impact, we use the averages of on-farm feed, not all crop averages
  ## as the practices applied to on-farm feed production are likely closer to those used for off-farm feed production
  ## (e.g., if a farmer sells his maize as livestock feed, he will sell it for less than he would have for human consumption
  ## and will therefore likely have used as many inputs as a farmer who produces his own maize for his livestock)

  tmp_target_vars = c("yield",grep("kgCO2e", names(feed_produced_impact), value = TRUE))

  tmp_avrg_crop_impact0 <- onfarm_crops_ghge  |>
    # add farm characteristics
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(id_cols),
                                     ORGANIC, SYS02),
                     by = id_cols)  |>
    # add yields
    dplyr::left_join(object@crop |>
                       dplyr::select(dplyr::all_of(id_cols),
                                     FADN_code_letter, yield),
                     by = c(id_cols, 'FADN_code_letter')) |>
    # add all Sailley feed names
    dplyr::rename(FADN_code_feed = FADN_code_letter) |>
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::select(Sailley_feed, FADN_code_feed) |>
        tidyr::separate_longer_delim(FADN_code_feed,";") |>
        dplyr::filter(!is.na(FADN_code_feed)),
      by = 'FADN_code_feed',
      relationship = "many-to-many"
    )

  tmp_avrg_crop_impact = h_average_practices(data = tmp_avrg_crop_impact0,
                                             target_vars = tmp_target_vars,
                                             primary_grp = c( 'Sailley_feed', 'YEAR', 'COUNTRY', 'ORGANIC'),
                                             secondary_grp = c( 'Sailley_feed', 'ORGANIC'),
                                             weight_var = 'SYS02')


  ## 3.2. Estimate purchased feed impact ----

  feed_purchased_impact = herd_feed |>
    dplyr::filter(feed_origin == "feed_purchased") |>
    # add land use type
    dplyr::mutate(
      land_use_type = case_when(
        grepl("Herbe", Sailley_feed) ~ "grassland",
        .default = "arable"
      )
    ) |>
    # select variables
    dplyr::select(dplyr::all_of(id_cols),
                  COUNTRY,ORGANIC,SYS02,
                  FADN_code_letter, FADN_code_feed, Sailley_feed,
                  feed_type, feed_origin,
                  dplyr::matches("Qobs"),
                  DM_t_livcat) |>
    # add average impact
    dplyr::left_join(tmp_avrg_crop_impact,
                     by = c('Sailley_feed', 'YEAR', 'COUNTRY', 'ORGANIC')) |>
    # recalculate area for feed
    dplyr::mutate(area_ha_livcat = DM_t_livcat / yield)

  ## 3.3. Soybean ----

  # soybean meal used to feed livestock in France mostly comes from Brasil (Overmars et al.,  2015)
  # we have default values for Brazilian soybean meal yields and total emission per ton
  ## to decompose total ghge, we
  # TODO: we could retrieve values from the different sources of emissions from other sources

  # soybean estimates from literature values
  soy_impact <- feed_purchased_impact |>
    # select soy meal
    dplyr::filter(Sailley_feed == "Dont_tourteau_de_soja") |>
    # sum up variables
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(tmp_target_vars),
        ~ mean(.x, na.rm = TRUE)
      ),
      .by =  c(Sailley_feed, ORGANIC)
    ) |>
    dplyr::mutate(
      # version 9 of Ecoalim: 0.85 kgCO2e/kg soybean meal,  excluding deforestation
      ecoalim_total_ghg_per_t = 0.85*10^3,
      # estimate a correction ratio for GHGE variables
      correction_ratio = if_else(
        Sailley_feed == "Dont_tourteau_de_soja",
        ecoalim_total_ghg_per_t / total_ghg_crop_kgCO2e_per_t,
        1  # ratio = 1 for all other feeds → values unchanged
      )
    ) |>
    # Apply the ratio to all target variables
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(tmp_target_vars),
        ~ if_else(
          Sailley_feed == "Dont_tourteau_de_soja",
          .x * correction_ratio,
          .x
        )
      ),
      # From Overmars et al. 2015: 2.45 t/ha for Latin America soybean
      yield = ifelse(Sailley_feed == "Dont_tourteau_de_soja", 2.45, yield),
    ) |>
    # Drop helper columns
    dplyr::select(-ecoalim_total_ghg_per_t, -correction_ratio)


  tmp_cols <- setdiff(dplyr::intersect(colnames(feed_purchased_impact), colnames(soy_impact)),
                      c('Sailley_feed', 'YEAR', 'COUNTRY', 'ORGANIC'))

  tmp_impact_soy <- feed_purchased_impact |>
    dplyr::filter(Sailley_feed == "Dont_tourteau_de_soja") |>
    dplyr::select(-dplyr::all_of(tmp_cols)) |>
    dplyr::left_join(soy_impact,
                     by = c('ORGANIC', 'Sailley_feed'))

  # TODO: just add estimates for soybean (soy_impact) into feed_purchased_impact (keep ID columns and Sailley feed and organic)

  feed_purchased_impact_w_soy <- feed_purchased_impact |>
    dplyr::filter(Sailley_feed != "Dont_tourteau_de_soja") |>
    # add Brazilian soybean values
    dplyr::bind_rows(tmp_impact_soy)


  # 4. Aggregate feed impact for herd ------------------------------------------------------------------------------

  herd_feed_GHGE <- dplyr::bind_rows(
    feed_produced_impact,
    feed_purchased_impact_w_soy
  ) |>
    dplyr::select(dplyr::all_of(id_cols),
                  FADN_code_letter,
                  Qobs,
                  Sailley_feed,FADN_code_feed,feed_origin,
                  DM_t_livcat,area_ha_livcat,
                  dplyr::matches("per_ha$|per_t$"))

  # Output ----

  return(herd_feed_GHGE)

}

