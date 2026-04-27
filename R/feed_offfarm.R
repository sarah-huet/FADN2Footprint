#' Estimate off‑farm (purchased) livestock feed quantities and qualities
#'
#' @description
#' f_feed_offfarm estimates the quantity and nutritional quality of feed
#' purchased by each farm in a FADN2Footprint object. It combines farm
#' purchase values recorded in the FADN input table with theoretical livestock
#' ration/value shares (from f_feed_theo_ration) and external feed tables to:
#' (1) allocate purchased feed value to feed types and crops, (2) convert
#' allocated value to tonnes using price/€-to-tonne conversion factors, and
#' (3) compute nutritional summaries (dry matter, gross energy, crude protein,
#' ash) for the purchased feed.
#'
#' @details
#' The function executes the following sequence:
#' - validate that `object` inherits from the S4 class "FADN2Footprint";
#' - return cached results from object@practices$herding$feed$feed_purchased if
#'   present and `overwrite = FALSE`;
#' - compute theoretical ration/value shares using f_feed_theo_ration(object);
#' - extract farm purchase variables (columns matching "feed_purch_*") from
#'   object@input and pivot to long format to obtain per‑farm feed purchase
#'   values in EUR. The function recognises concentrate vs rough feed and maps
#'   to species groups (grazing, swine, poultry) based on the purchase variable
#'   name;
#' - join theoretical per‑crop/per‑feed value shares to the purchase values and
#'   allocate purchased value among crops and livestock categories using the
#'   th_feed$th_feed_value table (th_crop_value_p100_feed_type);
#' - convert allocated values to physical tonnes using an euros_t conversion
#'   factor (expected to be supplied in th_feed tables);
#' - attach average feed quality values (GE, CP, Ash) from feed tables
#'   (data_extra$feed_table_all_as_DM aggregated by Sailley feed) and compute
#'   per‑farm totals: DM_t_crop, GE (MJ and kcal), CP_t_crop, Ash_t_crop;
#' - return a tibble with one (or more) rows per farm / feed line with produced
#'   purchased feed quantities and nutritional summaries. Rows with zero DM are
#'   filtered out.
#'
#' Important assumptions and notes:
#' - The conversion from euros to tonnes (euros_t) is provided by the theoretical
#'   ration/value outputs (th_feed) and reflects price information or default
#'   conversion data; the function does not itself estimate prices.
#' - Theoretical ration/value shares come from f_feed_theo_ration(object) and
#'   must include fields used for joining and allocation (feed_type, species,
#'   th_crop_value_p100_feed_type, euros_t, FADN_code_letter, Sailley_feed, ...).
#' - The function depends on package data tables (data_extra$Sailley_2021_feed_flows,
#'   data_extra$feed_table_all_as_DM) and on the helper f_feed_theo_ration;
#'   these must be present and consistent with the FADN2Footprint object.
#' - Yields for purchased crops are not estimated here (TODO in code); national
#'   average yields could be added in future versions to improve traceability.
#'
#' @param object An S4 object of class "FADN2Footprint". The object must
#'   contain the usual slots used by the package (for example:
#'   object@input, object@traceability$id_cols) and be prepared so helper
#'   functions (f_feed_theo_ration) operate correctly.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble/data.frame with one or more rows per traceable farm and
#' feed line containing:
#' - traceability id columns (as in object@traceability$id_cols),
#' - FADN_code_letter (crop FADN code),
#' - feed_type and Sailley_feed (feed naming used for feed tables),
#' - DM_t_crop: purchased feed dry matter (tonnes),
#' - GE_MJ_crop, GE_kcal_crop: gross energy totals for the purchased feed,
#' - CP_t_crop: crude protein (tonnes) contained in the purchased feed,
#' - Ash_t_crop: ash (tonnes) contained in the purchased feed.
#'
#' Rows with zero purchased DM are filtered out.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Estimate purchased feed (uses cache unless overwrite = TRUE)
#' feed_purch <- f_feed_offfarm(f)
#' head(feed_purch)
#'
#' # Force recomputation
#' feed_purch2 <- f_feed_offfarm(f, overwrite = TRUE)
#' }
#'
#' @seealso f_feed_theo_ration, f_feed_onfarm
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr select filter mutate left_join group_by ungroup summarise across matches
#' @importFrom tidyr pivot_longer separate_longer_delim
#' @importFrom tidyselect all_of matches
#' @importFrom rlang .data


f_feed_offfarm <- function(object,
                           overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@practices$herding$feed$feed_purchased) && !overwrite) {
    message("Using cached values stored in object@practices$herding$feed$feed_purchased.")
    return(object@practices$herding$feed$feed_purchased)  # use cached value
  }


  # Estimate theoretical livestock feed ration and value ---------------------------------------------------------------------------------
  th_feed <- f_feed_theo_ration(object)

  # Farms purchased feed quantity ---------------------------------------------------------------------------------
  ## Steps:
  ## 1. Retrieve farm purchases values for concentrate and rough feed
  ## 2. Add theoretical feed values estimated through f_feed_theo_ration(object)
  ## 3. Estimate share of purchased feed value per livestock category and per crop
  ## 4. Convert estimated feed value in ton
  feed_purchased_qty <- object@input |>
    # Retrieve farm purchases values for concentrate and rough feed
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  dplyr::matches("feed_purch")) |>
    ## Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value in EUR
    ##feed_purch_concent_graz = IGRFEDCNCTRPUR_V,
    ## Purchased coarse fodder for grazing stock (equines, ruminants) Value in EUR
    ##feed_purch_rough_graz = IGRFEDCRSPUR_V,
    ## Purchased feedstuffs for pigs Value in EUR
    ##feed_purch_concent_pigs = IPIGFEDPUR_V,
    ## Purchased feedstuffs for poultry and other small animals Value in EUR
    ##feed_purch_concent_poultry = IPLTRFEDPUR_V,
    tidyr::pivot_longer(cols = tidyselect::matches("feed_purch"),
                        names_to = "feed_var",
                        values_to = "feed_purch_value") |>
    dplyr::filter(feed_purch_value >0) |>
    dplyr::mutate(
      feed_type = dplyr::case_when(
        feed_var == "feed_purch_rough_graz" ~ "feed_rough",
        .default = "feed_concent"
      ),
      species = dplyr::case_when(
        feed_var %in% c("feed_purch_concent_graz","feed_purch_rough_graz") ~ "grazing",
        feed_var == "feed_purch_concent_pigs" ~ "swine",
        feed_var == "feed_purch_concent_poultry" ~ "poultry"
      )
    ) |>
    #filter(feed_purch_value >0) |> # keep null purchases for traceability
    # Add theoretical values
    dplyr::left_join(th_feed$th_feed_value |>
                       dplyr::select(dplyr::all_of(object@traceability$id_cols),
                                     species, FADN_code_letter,
                                     feed_type, Sailley_feed,
                                     th_crop_value_p100_feed_type, euros_t),
                     by = c(object@traceability$id_cols,"feed_type","species")) |>
    # Estimate share of purchased feed value per livestock category and per crop
    dplyr::mutate(
      feed_purch_value_share = feed_purch_value * th_crop_value_p100_feed_type
    ) |>
    # Convert estimated value in ton
    dplyr::mutate(
      feed_purch_t_livcat = feed_purch_value_share / euros_t
    )



  # Estimate Feed quality ------------------------------------------------------
  ## For each feed, we assign an average dry matter (DM), crude protein content (CP) in DM, gross energy (GE) in DM and ash (Ash) in DM based on the INRA-CIRAD-AFZ feed tables

  # average crop quality
  ## average GE, DM & CP (DM is not estimated as it is always 100%)
  Sailley_crop_qlty <- data_extra$Sailley_2021_feed_flows |>
    dplyr::select(Sailley_feed,feed_tables) |>
    tidyr::separate_longer_delim(feed_tables,";") |>
    # add feed quality
    dplyr::left_join(
      data_extra$feed_table_all_as_DM,
      by = 'feed_tables'
    ) |>
    # summarise by Sailley crop name
    dplyr::summarise(
      GE_MJ_kg = mean(`GE MJ/kg`,na.rm = T),
      GE_kcal_kg = mean(`GE kcal/kg`,na.rm = T),
      CP_pc = mean(`CP %`,na.rm = T),
      Ash_pc = mean(`Ash %`,na.rm = T),
      .by = 'Sailley_feed'
    )

  ## Total GE, DM & CP
  feed_purchased_qlty <- feed_purchased_qty |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,
                  feed_type,Sailley_feed,
                  feed_purch_t_livcat) |>
    # add average feed quality
    dplyr::left_join(
      Sailley_crop_qlty,
      by = join_by(Sailley_feed)
    ) |>
    # convert
    dplyr::mutate(
      DM_t_livcat = feed_purch_t_livcat,
      GE_MJ_livcat = feed_purch_t_livcat * (GE_MJ_kg*10^3),
      GE_kcal_livcat = feed_purch_t_livcat * (GE_kcal_kg*10^3),
      CP_t_livcat = feed_purch_t_livcat * (CP_pc / 100),
      Ash_t_livcat = feed_purch_t_livcat * (Ash_pc / 100)
    )

  # Output ---------------------------------------------------------------------
  feed_purchased <- feed_purchased_qlty |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,
                  feed_type,Sailley_feed,
                  GE_MJ_livcat,GE_kcal_livcat,DM_t_livcat,CP_t_livcat,Ash_t_livcat) |>
    dplyr::filter(DM_t_livcat >0)


  return(feed_purchased)

}

