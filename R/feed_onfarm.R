#' Estimate on‑farm produced livestock feed quantities and qualities
#'
#' @description
#' f_feed_onfarm estimates the quantity and nutritional quality of feed
#' produced on each farm in a FADN2Footprint object. It combines crop
#' production/use data from the FADN object with theoretical livestock rations
#' (from f_feed_theo_ration) and external feed‑tables to (1) estimate how much
#' of on‑farm crop production is used as livestock feed, (2) allocate produced
#' feed across livestock categories, and (3) compute nutritional summaries
#' (dry matter, gross energy, crude protein, ash) for the produced feed.
#'
#' @details
#' The function implements the following main steps:
#' - validate that `object` is an S4 object of class "FADN2Footprint";
#' - use cached results from object@practices$herding$feed$feed_produced when
#'   available and `overwrite = FALSE`;
#' - compute theoretical livestock rations with f_feed_theo_ration(object);
#' - identify crops that are used as feed (using package data_extra$crops
#'   feed_type) and compute on‑farm feed quantity as production minus sales;
#' - join theoretical ration lines to FADN crop codes using a Sailley
#'   crosswalk (data_extra$Sailley_2021_feed_flows) restricted to flows marked
#'   as produced on farm, then allocate on‑farm crop feed to livestock
#'   categories in proportion to the theoretical crop demand (th_DMI_t_livcat_y);
#' - attach average feed quality values (GE, CP, Ash) from feed tables
#'   (data_extra$feed_table_all_as_DM) aggregated by Sailley feed name and
#'   compute per‑farm totals: DM_t_livcat, GE (MJ and kcal), CP_t_livcat, Ash_t_livcat;
#' - return a tibble with one (or more) rows per farm/crop/feed line containing
#'   the produced feed quantity and nutritional summaries.
#'
#' Important assumptions and notes:
#' - Crop production values are considered to be reported in dry matter (DM).
#'   The function prints a message to this effect.
#' - The function assumes co‑products (e.g. oilseed meals, bran) are treated as
#'   purchased rather than produced on farm (see TODO in code). Only crops
#'   flagged as feed in data_extra$crops are considered.
#' - The theoretical ration is used only to allocate produced feed across
#'   livestock categories, not to change total on‑farm production quantities.
#' - The function depends on package data tables (data_extra$crops,
#'   data_extra$Sailley_2021_feed_flows, data_extra$feed_table_all_as_DM) and on
#'   the helper f_feed_theo_ration; these must be available in the package
#'   environment and compatible with the FADN2Footprint object structure.
#'
#' @param object An S4 object of class "FADN2Footprint". The object must
#'   contain standard slots used by the package (for example:
#'   object@crop, object@traceability$id_cols) and be prepared by the package
#'   workflow so that helper functions (f_feed_theo_ration) work correctly.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble/data.frame with one or more rows per traceable farm and
#' feed line containing:
#' - traceability id columns (as in object@traceability$id_cols),
#' - FADN_code_letter (crop FADN code) and FADN_code_feed (mapped feed code),
#' - feed_type and Sailley_feed (feed naming used for feed tables),
#' - yield (crop yield, when available),
#' - DM_t_livcat: produced feed dry matter (tonnes),
#' - GE_MJ_livcat, GE_kcal_livcat: gross energy totals for the produced feed,
#' - CP_t_livcat: crude protein (tonnes) contained in the produced feed,
#' - Ash_t_livcat: ash (tonnes) contained in the produced feed.
#'
#' Rows with zero produced DM are filtered out.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Estimate on‑farm produced feed (uses cache unless overwrite = TRUE)
#' feed_prod <- f_feed_onfarm(f)
#' head(feed_prod)
#'
#' # Force recomputation
#' feed_prod2 <- f_feed_onfarm(f, overwrite = TRUE)
#' }
#'
#' @seealso f_feed_theo_ration
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr filter pull mutate select rename left_join group_by ungroup summarise across all_of coalesce
#' @importFrom tidyr separate_longer_delim



f_feed_onfarm <- function(object,
                          overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$herding$feed$feed_produced) && !overwrite) {
    message("Using cached values stored in object@practices$herding$feed$feed_produced.")
    return(object@practices$herding$feed$feed_produced)  # use cached value
  }

  # Estimate theoretical livestock feed ration and value ---------------------------------------------------------------------------------
  th_feed <- f_feed_theo_ration(object)

  # On farm produced feed quantity ---------------------------------------------------------------------------------
  ## Steps:
  ## 1. Estimate on-farm produced feed quantity
  ### To estimate the amount of feed produced at the farm, we use the farm use value variable registered in the FADN and convert values to tonnes using EUROSTAT data
  ## 2. Add theoretical feed ration estimated through f_feed_theo_ration(object)
  ### TODO: check the following hypothesis
  ### While farmers can purchase grains, co products, and rough feed, we consider that only grains and rough feed are produced on farm, co products (such as wheat bran, soymeal,) are only purchased by farmer
  ## 3. Estimate share of produced feed quantity per livestock category

  ## 1. Estimate on-farm produced feed quantity


  # We consider only crops used to feed livestock
  crop_feed <- data_extra$crops |>
    dplyr::filter(!is.na(feed_type)) |>
    dplyr::pull(FADN_code_letter)

  onfarm_feed_prod <- object@crop |>
    # filter crops used as feed
    dplyr::filter(FADN_code_letter %in% crop_feed) |>
    # estimate produced feed quantity
    dplyr::mutate(
      feed_t_crop_total = coalesce(prod_t,0) - coalesce(sales_t,0)) |>
    # remove crops with zero quantity as feed
    dplyr::filter(feed_t_crop_total >0) |>
    # select
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,feed_t_crop_total,yield) |>
    rename(FADN_code_feed = FADN_code_letter)

  ## 2. Add theoretical feed ration estimated through f_feed_theo_ration(object) ----
  feed_produced_qty = th_feed$th_feed_ration |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,Qobs,Sailley_livestock,
                  Sailley_feed,th_DMI_t_livcat_y)|>
    # Add FADN-to-Sailley name match
    dplyr::left_join(
      data_extra$Sailley_2021_feed_flows |>
        dplyr::filter(feed_produced_onfarm == "T") |>
        dplyr::select(Sailley_feed,FADN_code_feed,feed_type) |>
        tidyr::separate_longer_delim(FADN_code_feed,";"),
      by = 'Sailley_feed',
      relationship = "many-to-many"
    ) |>
    # Add on farm feed production
    dplyr::left_join(
      onfarm_feed_prod,
      by = c(object@traceability$id_cols, "FADN_code_feed")
    ) |>
    #filter(feed_t_crop_total >0) |>
    # 3. Estimate share of produced feed for each livestock category ----
  ## sum theoretical ration for each feed category
    dplyr::mutate(
      sum_th_DMI_t_livcat_y = sum(th_DMI_t_livcat_y,na.rm = T),
      .by = c(object@traceability$id_cols,FADN_code_feed,feed_type)
    ) |>
    # Allocate produced feed to livestock categories
    dplyr::mutate(
      feed_prod_t_livcat = feed_t_crop_total * (th_DMI_t_livcat_y / sum_th_DMI_t_livcat_y)
    )


  # Estimate Feed quality ------------------------------------------------------
  ## For each feed, we assign an average dry matter (DM), crude protein content (CP) in DM, gross energy (GE) in DM and ash (Ash) in DM based on the INRA-CIRAD-AFZ feed tables
  message("We consider that crop productions are recorded as dry matter.")

  # average crop quality
  ## average GE, DM & CP (DM is not estimated as it is always 100%)
  Sailley_crop_qlty <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(Sailley_feed,feed_tables) |>
    tidyr::separate_longer_delim(feed_tables,";") |>
    # add feed quality
    dplyr::left_join(
      data_extra$feed_table_all_as_DM,
      by = join_by(feed_tables)
    ) |>
    # summarise by Sailley crop name
    dplyr::summarise(
      GE_MJ_kg = mean(`GE MJ/kg`,na.rm = T),
      GE_kcal_kg = mean(`GE kcal/kg`,na.rm = T),
      CP_pc = mean(`CP %`,na.rm = T),
      Ash_pc = mean(`Ash %`,na.rm = T),
      .by = "Sailley_feed"
    )

  ## Total GE, DM & CP
  feed_produced_qlty <- feed_produced_qty |>
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,FADN_code_feed,feed_type,Sailley_feed,feed_prod_t_livcat,yield) |>
    # add average feed quality
    dplyr::left_join(
      Sailley_crop_qlty,
      by = join_by(Sailley_feed)
    ) |>
    # convert
    dplyr::mutate(
      DM_t_livcat = feed_prod_t_livcat,
      GE_MJ_livcat = feed_prod_t_livcat*10^3 * GE_MJ_kg,
      GE_kcal_livcat = feed_prod_t_livcat*10^3 * GE_kcal_kg,
      CP_t_livcat = feed_prod_t_livcat * (CP_pc / 100),
      Ash_t_livcat = feed_prod_t_livcat * (Ash_pc / 100)
    )

  # Output ---------------------------------------------------------------------
  feed_produced <- feed_produced_qlty |>
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,FADN_code_feed,feed_type,Sailley_feed,yield,DM_t_livcat,GE_MJ_livcat,GE_kcal_livcat,CP_t_livcat,Ash_t_livcat) |>
    dplyr::filter(DM_t_livcat >0)

  return(feed_produced)

}

