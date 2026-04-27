#' Estimate biodiversity value-impact scores (BVI) per hectare and per tonne for herd feed
#'
#' @description
#' f_BVIAS_feed estimates biodiversity impact (BVI) indicators for feed consumed
#' by herds (dairy and other livestock) and returns per‑feed-line results
#' including BVI per hectare (BVI_ha) and BVI per tonne (BVI_t), together with
#' feeding, area and practice variables used for the assessment.
#'
#' @details
#' The function:
#' - validates that the input is an S4 object of class "FADN2Footprint";
#' - optionally uses cached results stored at object@footprints$BVIAS$BVI_feed;
#' - loads or checks BVIAS model parameter tables (constants and weights) either
#'   from the provided arguments, from object@footprints$BVIAS$model_parameters,
#'   or from package defaults (data_extra$BVIAS_var_constant and
#'   data_extra$BVIAS_var_weight);
#' - ensures farm practices are inferred (calls infer_practices internally)
#'   so that herd feed intake detail and farm practices exist on the object;
#' - computes on‑farm crop impacts via f_BVIAS_crops and joins these to produced
#'   feed lines, computes off‑farm (purchased) feed impacts by joining average
#'   practice/impact estimates to purchased feed categories, and treats special
#'   cases (e.g. soybean meal sourcing and adjusted soybean practices/impacts);
#' - returns a row per feed line (joined with traceability id columns) containing
#'   feed quantities (DM_t_livcat), area estimates, yield, estimated practice
#'   variables, and biodiversity impact metrics (BVI_ha, BVI_t).
#'
#' The implementation follows three main steps:
#' 1) estimate on‑farm vs off‑farm feed quantities and attach farm metadata;
#' 2) estimate on‑farm crop practices & impacts using f_BVIAS_crops;
#' 3) estimate off‑farm (purchased) crop practice averages and impacts and
#'    combine results. Soybean meal (imported soybean) is treated using a
#'    dedicated routine that maps Lindner/Overmars-derived parameters to FADN
#'    practice space.
#'
#' @param object An S4 object of class "FADN2Footprint". The object must contain
#'   the traceability id columns referenced in object@traceability$id_cols and
#'   have or be able to compute herd feed details (infer_practices must succeed).
#' @param BVIAS_constants Optional data.frame of model constants (metric codes,
#'   land_use_type and model parameters). If NULL the function will attempt to
#'   retrieve constants from object@footprints$BVIAS$model_parameters$constants
#'   or fall back to package defaults (data_extra$BVIAS_var_constant). Must be a
#'   data.frame when provided.
#' @param BVIAS_weights Optional data.frame of model weights used to aggregate
#'   per‑metric biodiversity values. If NULL the function will attempt to use
#'   object@footprints$BVIAS$model_parameters$weights or package defaults
#'   (data_extra$BVIAS_var_weight). Must be a data.frame when provided.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A tibble/data.frame containing one row per feed detail line (per
#'   traceability id). Returned columns include:
#'   - traceability id columns (as in object@traceability$id_cols),
#'   - feed descriptors (FADN_code_feed, feed_type, feed_origin, Sailley_feed, ...),
#'   - quantity columns (DM_t_livcat, Qobs variants where present),
#'   - area_ha (recomputed area = DM_t_livcat / yield),
#'   - practice variables joined from on‑farm or averaged off‑farm estimates
#'     (e.g. tillage, N_ferti, pesticides, crop_diversity, mean_field_size, ground_cover, ...),
#'   - biodiversity metrics BVI_ha and BVI_t and the yield used to compute BVI_t,
#'   - other intermediate variables used in the BVIAS computations.
#'
#' @section Caching and messages:
#' If a cached result is present at object@footprints$BVIAS$BVI_feed the function
#' prints a message and returns the cached value. If BVIAS_constants or
#' BVIAS_weights are not supplied, the function will attempt to use parameters
#' stored in the object and otherwise fall back to package defaults, printing a
#' message to indicate which source is used.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' # Use defaults from the object or package
#' bvi_feed <- f_BVIAS_feed(f)
#'
#' # Provide custom constants and weights
#' bvi_feed_custom <- f_BVIAS_feed(f,
#'                                 BVIAS_constants = my_constants_df,
#'                                 BVIAS_weights = my_weights_df,
#'                                 overwrite = TRUE)
#' }
#'
#' @seealso f_BVIAS_crops, infer_practices, data_extra
#'
#' @return A tibble/data.frame with per‑feed-line biodiversity impact estimates (BVI_ha, BVI_t) and supporting variables.
#'
#' @export
#' @concept footprint-biodiv
#' @importFrom dplyr left_join filter mutate select matches bind_rows summarise across case_when all_of distinct rename
#' @importFrom tidyr pivot_wider separate_longer_delim
#' @importFrom stats weighted.mean


# estimate BVI per ha and per t for herds
f_BVIAS_feed <- function(object,
                         overwrite = FALSE,
                         BVIAS_constants = NULL,
                         BVIAS_weights = NULL) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$BVIAS$BVI_feed) && !overwrite) {
    message("Using cached values stored in object@footprints$BVIAS$BVI_feed")
    return(object@footprints$BVIAS$BVI_feed)  # use cached value
  }

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

  # STEPS:
  ## 1. Estimate on-farm and off-farm feed
  ## 2. Estimate on-farm crop practices and impact
  ## 3. Estimate off-farm crop practices and impact

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. Estimate on-farm and off-farm feed ------------------------------------------------------------------------------
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  object <- infer_practices(object)

  herd_feed <- object@practices$herding$feed$feed_intake$detail |>
    # add farm characteristics
    dplyr::left_join(object@farm |>
                       dplyr::select(dplyr::all_of(object@traceability$id_cols),
                                     COUNTRY,ORGANIC,SYS02),
                     by = object@traceability$id_cols)

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. Estimate on-farm crop practices and impact ------------------------------------------------------------------------------
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  onfarm_feed = f_BVIAS_crops(object = object, BVIAS_constants = BVIAS_constants, BVIAS_weights = BVIAS_weights)
  #message("BVIAS model uses the parameters stored in the S4 FADN2Footprint object.")

  feed_produced_impact = herd_feed |>
    dplyr::filter(feed_origin == "feed_produced") |>
    # add practices
    dplyr::left_join(
      onfarm_feed$y |>
        dplyr::rename(FADN_code_feed = FADN_code_letter) |>
        tidyr::pivot_wider(
          id_cols = c(dplyr::all_of(object@traceability$id_cols), land_use_type,FADN_code_feed),
          names_from = metric_code,
          values_from = value
        ),
      by = c(object@traceability$id_cols,"FADN_code_feed")
    ) |>
    # add impact
    dplyr::left_join(
      onfarm_feed$BVIAS |>
        dplyr::rename(FADN_code_feed = FADN_code_letter) |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols), land_use_type,FADN_code_feed,
                      BVI_ha,BVI_t),
      by = c(object@traceability$id_cols,'land_use_type',"FADN_code_feed")
    ) |>
    # recalculate area for feed
    dplyr::mutate(area_ha = DM_t_livcat / yield)

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. Estimate off-farm crop practices and impact ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.1. Average crop practices and impact ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  # Estimate average crop impact
  tmp_target_vars = c(intersect(names(feed_produced_impact),names(object@practices$crops)),
                      "yield",
                      grep("BVI", names(feed_produced_impact), value = TRUE))

  tmp_avrg_crop_impact = h_average_practices(data = feed_produced_impact,
                                             target_vars = tmp_target_vars,
                                             primary_grp = c('FADN_code_letter','FADN_code_feed','YEAR','COUNTRY','ORGANIC'),
                                             secondary_grp = c('FADN_code_letter','FADN_code_feed','ORGANIC'),
                                             weight_var = 'SYS02')
  # Estimate average feed impact
  tmp_avrg_Sailley_feed_impact <- data_extra$Sailley_2021_feed_flows |>
    # retrieve feed categories
    dplyr::select(Sailley_feed,FADN_code_feed) |>
    tidyr::separate_longer_delim(FADN_code_feed,";") |>
    dplyr::filter(!is.na(FADN_code_feed)) |>
    # add average crop impact
    dplyr::left_join(tmp_avrg_crop_impact,
                     by = 'FADN_code_feed',
                     relationship = "many-to-many") |>
    # sum up per Sailley feed category
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(tmp_target_vars),
        ~ mean(.x, na.rm = T),
        .names = "{.col}"

      ),
      .by =  c('YEAR', 'FADN_code_letter', 'Sailley_feed', 'COUNTRY', 'ORGANIC')
    )

  ## look at missing averages
  ### e.g., some farmers buy all their feed and do not have registered crops
  missing_avrg = dplyr::anti_join(
    herd_feed |> dplyr::select(YEAR,FADN_code_letter,Sailley_feed,COUNTRY,ORGANIC) |> dplyr::distinct(),
    tmp_avrg_Sailley_feed_impact |> dplyr::select(YEAR,FADN_code_letter,Sailley_feed,COUNTRY,ORGANIC) |> dplyr::distinct(),
    by = c('YEAR', 'FADN_code_letter', 'Sailley_feed', 'COUNTRY', 'ORGANIC')
  ) |>
    # add global average
    dplyr::left_join(
      tmp_avrg_Sailley_feed_impact |>
        # sum up per Sailley feed category
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(tmp_target_vars),
            ~ mean(.x, na.rm = T),
            .names = "{.col}"

          ),
          .by =  c('Sailley_feed','ORGANIC')
        ),
      by = c('Sailley_feed', 'ORGANIC')
    )

  ## add missing averegaes
  tmp_avrg_Sailley_feed_impact <- dplyr::bind_rows(
    tmp_avrg_Sailley_feed_impact,
    missing_avrg
  )


  # Estimate purchased feed impact
  feed_purchased_impact = herd_feed |>
    dplyr::filter(feed_origin == "feed_purchased") |>
    # add land use type
    dplyr::mutate(
      land_use_type = case_when(
        grepl("Herbe",Sailley_feed) ~ "grassland",
        .default = "arable"
      )
    ) |>
    # select variables
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  COUNTRY,ORGANIC,SYS02,
                  FADN_code_letter, feed_type, feed_origin, Sailley_feed,
                  dplyr::matches("Qobs"),
                  DM_t_livcat) |>
    # add average practices
    dplyr::left_join(tmp_avrg_Sailley_feed_impact |>
                       # select variables
                       dplyr::select(YEAR,
                                     COUNTRY,ORGANIC,
                                     FADN_code_letter, Sailley_feed,
                                     dplyr::all_of(tmp_target_vars)),
                     by = c('YEAR', 'FADN_code_letter', 'Sailley_feed', 'COUNTRY', 'ORGANIC')) |>
    # recalculate area for feed
    dplyr::mutate(area_ha = DM_t_livcat / yield)

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ## 3.2. Soybean ----
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+
  # soybean meal used to feed livestock in France mostly comes from Brasil (Overmars et al., 2015)
  # to estimate the biodiversity impact of soybean meal produced in Brasil, we compared the agricultural practices estimated by Lindner (Lindner 2022) for soy and wheat production from Agribalyse data
  # to the agricultural practices we estimated for wheat production from the user data
  # For practices not estimated in Lindner et al. 2022, we take the most intensive quartile of French wheat production in the user data

  # average soybean practices and impact
  BVIAS_soybean <- tibble(
    practice = c("tillage","N_ferti","share_Nmin_ferti","pesticides"),
    wheat_Lindner = c(1.488558,180.7,162.7/180.7,51580.36245417),
    soy_Lindner = c(1.2926064,0,0,36888.53241)) |>
    # add FADN average for wheat
    dplyr::full_join(
      tmp_avrg_crop_impact |>
        dplyr::filter(FADN_code_feed == "CWHTC" & ORGANIC == F) |>
        tidyr::pivot_longer(cols = tmp_target_vars,
                            names_to = 'practice',
                            values_to = 'w_mean'
        ) |>
        dplyr::summarise(wheat_FADN = mean(w_mean,na.rm = T),
                         wheat_FADN_q75 = unlist(quantile(w_mean,0.75,na.rm = T)),
                         wheat_FADN_q25 = unlist(quantile(w_mean,0.25,na.rm = T)),
                         wheat_FADN_max = unlist(quantile(w_mean,0.95,na.rm = T)),
                         .by = c('practice')),
      by = 'practice'
    ) |>
    # estimate values for soybean
    dplyr::mutate(
      soy_FADN = soy_Lindner * wheat_FADN / wheat_Lindner
    ) |>
    # for other variables, we considered that soybean cultivation in Brasil is an intensive crop cultivation, we thus apply the variable value of the most intensive quartile of FADN wheat cultivation
    dplyr::mutate(
      soy_FADN = case_when(
        ## for mean_field_size & ground_cover, most intensive quartile is q75
        is.na(soy_FADN) & practice %in% c("mean_field_size","ground_cover") ~ wheat_FADN_q75,
        ## for hedge_density & crop_diversity, most intensive quartile is q25
        is.na(soy_FADN) & practice %in% c("hedge_density","crop_diversity") ~ wheat_FADN_q25,
        .default = soy_FADN)
    ) |>
    # estimate BVI_ha from most intensive wheat
    dplyr::left_join(
      BVIAS_constants |>
        dplyr::filter(land_use_type == "arable") |>
        dplyr::rename(practice = metric_code) |>
        dplyr::select(-dplyr::matches("metric_number_in_Lindner_2019"),-dplyr::matches("description")),
      by = join_by(practice)
    ) |>
    dplyr::left_join(
      BVIAS_weights |>
        dplyr::filter(land_use_type == "arable") |>
        dplyr::rename(practice = metric_code) |>
        dplyr::select(-dplyr::matches("metric_number_in_Lindner_2019")),
      by = join_by(practice,land_use_type)
    ) |>
    # BVI
    dplyr::mutate(
      ## calculate x_norm
      x_norm = soy_FADN / wheat_FADN_max,
      ## calculate BV
      ## calculate BV
      y = gamma + epsilon * exp(-(
        abs((((x_norm)^delta) - beta) ^ alpha) /
          (2*sigma^alpha))),
      # BV LU
      BV_LU = stats::weighted.mean(y,weight,na.rm = T),
      # BV NORM
      ## set BV_norm min and max according to Lindner et al. (2019) for arable land
      BV_norm = 0.23 + BV_LU * (0.52 - 0.23),
      # BV LOC
      ## BV_loc = 1.017626088*(1-exp(-4.055847776*BV_norm)), # we do not use the skewed function from Lindner et al., 2019
      BV_loc = BV_norm,
      # BVI
      BVI_ha = 1- BV_loc,
      # yield estimated from Overmars et al., 2015 as 2.45 t ha-1 for soy cultivated in Latin America
      yield = 2.45,
      # BVI_t
      BVI_t= BVI_ha / yield,
      # add feed type and origin
      feed_type = "feed_concent",
      feed_origin = "feed_purchased"
    )


  # replace soybean values in pseudofarm
  feed_purchased_impact <- feed_purchased_impact |>
    dplyr::mutate(
      tillage = ifelse(Sailley_feed == "Dont_tourteau_de_soja",
                       BVIAS_soybean$soy_FADN[BVIAS_soybean$practice == "tillage"],tillage),
      crop_diversity = ifelse(Sailley_feed == "Dont_tourteau_de_soja",
                              BVIAS_soybean$soy_FADN[BVIAS_soybean$practice == "crop_diversity"],crop_diversity),
      share_Nmin_ferti = ifelse(Sailley_feed == "Dont_tourteau_de_soja" & ORGANIC == F,
                                BVIAS_soybean$soy_FADN[BVIAS_soybean$practice == "share_Nmin_ferti"],share_Nmin_ferti),
      N_ferti = ifelse(Sailley_feed == "Dont_tourteau_de_soja",
                       BVIAS_soybean$soy_FADN[BVIAS_soybean$practice == "N_ferti"],N_ferti),
      pesticides = ifelse(Sailley_feed == "Dont_tourteau_de_soja" & ORGANIC == F,
                          BVIAS_soybean$soy_FADN[BVIAS_soybean$practice == "pesticides"],pesticides),
      BVI_ha = ifelse(Sailley_feed == "Dont_tourteau_de_soja",unique(BVIAS_soybean$BVI_ha),BVI_ha),
      BVI_t = ifelse(Sailley_feed == "Dont_tourteau_de_soja",unique(BVIAS_soybean$BVI_t),BVI_t),
      yield = ifelse(Sailley_feed == "Dont_tourteau_de_soja",unique(BVIAS_soybean$yield),yield),
      area_ha = ifelse(Sailley_feed == "Dont_tourteau_de_soja",DM_t_livcat / unique(BVIAS_soybean$yield),area_ha)
    )

  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # 4. Aggregate feed impact for herd ------------------------------------------------------------------------------
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  herd_feed_impact <- dplyr::bind_rows(
    feed_produced_impact,
    feed_purchased_impact
  )


  #table(is.na(herd_feed_impact$BVI_t))

  # Output ----

  return(herd_feed_impact)

}



