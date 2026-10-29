#' Estimate off-farm pseudoherd output (meat and living animals)
#'
#' @description
#' f_pseudoherd_output_off_farm estimates the production (meat and living
#' animals sold) attributable to off-farm pseudoherd animals, by assigning
#' NUTS2/national average sales shares, sales values and live weights to the
#' number of estimated off-farm animals.
#'
#' @details
#' The function proceeds in the following steps:
#'
#' **1. Cache check:** if `object@practices$herding$pseudoherd$output_off_farm`
#' is already populated and `overwrite` is FALSE, the cached value is
#' returned directly.
#'
#' **2. Estimate off-farm animal numbers:** the pseudoherd animals
#' (f_pseudoherd_animals) provide, for each farm and livestock category, the
#' observed number of animals on-farm (Qobs) and the estimated total
#' (on-farm + off-farm) equivalent number (Qeq). The off-farm number of
#' animals involved in the meat activity is computed as
#' Qofffarm_meat = Qeq_meat - Qobs_meat. Note that milk production is not
#' estimated off-farm, since there are no off-farm dairy cows.
#'
#' **3. Average sales shares and ratios:** using h_average_practices,
#' national/NUTS2 average shares and ratios are computed from
#' `object@herd`, weighted by SYS02:
#' \itemize{
#'   \item share_SSN, share_SRN: shares of animals sold for slaughter (SSN)
#'     and for rearing/further production (SRN);
#'   \item prop_SSN_Qobs, prop_SRN_Qobs: proportion of observed animals
#'     (Qobs) sold for slaughter or rearing;
#'   \item ratio_SS_V_N, ratio_SR_V_N: ratio of sales value to sales number
#'     for slaughter (SSV / SSN) and rearing (SRV / SRN) sales.
#' }
#' These averages are computed primarily at the FADN_code_letter × YEAR ×
#' COUNTRY × NUTS2 level, falling back to FADN_code_letter × YEAR × COUNTRY
#' when NUTS2-level data is insufficient.
#'
#' **4. Allocate off-farm sales:** the estimated off-farm animal numbers
#' (Qofffarm_meat) are split between slaughter and rearing sales using the
#' average proportions (prop_SSN_Qobs, prop_SRN_Qobs), and corresponding
#' sales values are derived using the average value-to-number ratios
#' (ratio_SS_V_N, ratio_SR_V_N).
#'
#' **5. Assign live weights:** average live weights (live_weight_kg) are
#' derived per FADN_code_letter, species and country from
#' `UNFCCC_data$table3As2`, based on `data_extra$livestock`'s UNFCCC_cat
#' mapping. Missing country-specific weights are replaced by the
#' FADN_code_letter-level mean.
#'
#' **6. Build output tables:**
#' - **Meat**: for each farm and livestock category, the number of animals
#'   sold for slaughter off-farm (sales_nb_offfarm_SSN) is converted to
#'   production in tonnes (prod_t) using the assigned live weight, and
#'   associated with an output category (meat_veal, meat_cull_cow,
#'   meat_beef, meat_pork, meat_chicken, or meat), sales value (sales_e) and
#'   sales tonnage (sales_t).
#' - **Living animals**: for each farm and livestock category, the number of
#'   animals sold off-farm for rearing (sales_nb_offfarm_SRN) is reported as
#'   prod_nb/sales_nb along with the corresponding sales value (sales_e).
#'
#' @param object An S4 object of class "FADN2Footprint" prepared by the
#'   package workflow, providing `object@traceability$id_cols`,
#'   `object@herd`, `object@farm` (NUTS2, SYS02), and
#'   `object@practices$herding$pseudoherd$output_off_farm` (for caching).
#' @param overwrite Logical. If TRUE, forces recomputation instead of
#'   reusing cached values stored in
#'   `object@practices$herding$pseudoherd$output_off_farm`. Default is
#'   FALSE.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{pseudoherd_output_meat}{A tibble with one row per farm ×
#'     livestock category (FADN_code_letter), containing the output
#'     category, species, number of animals sold (sales_nb), estimated meat
#'     production (prod_t), sales tonnage (sales_t) and sales value
#'     (sales_e) attributable to off-farm animals.}
#'   \item{pseudoherd_output_living_animals}{A tibble with one row per farm
#'     × livestock category, containing the output category
#'     ("living_animals"), species, number of animals produced/sold
#'     (prod_nb, sales_nb) and sales value (sales_e) attributable to
#'     off-farm animals sold for rearing.}
#' }
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' off_farm_output <- f_pseudoherd_output_off_farm(f)
#' head(off_farm_output$pseudoherd_output_meat)
#' head(off_farm_output$pseudoherd_output_living_animals)
#' }
#'
#' @seealso f_pseudoherd_animals, h_average_practices,
#'   f_GHGE_pseudoherd_output_cattle
#'
#' @export
#' @concept practice-pseudoherd
#' @import dplyr

f_pseudoherd_output_off_farm <- function(object,
                                overwrite = FALSE
) {
        if (!inherits(object, "FADN2Footprint")) {
                stop("Input must be a valid 'FADN2Footprint' object.")
        }

        if (!is.null(object@practices$herding$pseudoherd$output_off_farm)&& !overwrite) {
                message("Using cached values stored in object@practices$herding$pseudoherd$output_off_farm.")
                return(object@practices$herding$pseudoherd$output_off_farm)  # use cached value
        }

        id_cols = object@traceability$id_cols



        pseudoherd_animals <- f_pseudoherd_animals(object)
        # with Qobs: number of animal on-farm
        # and Qofffarm: number of animals off-farm

        # Milk ----
        # no off-farm production of milk as dairy cows no off-farm dairy cows

        # MEAT ----

        # herd output
        #avrg_output_meat <- h_average_practices(data = object@output$meat |>
        #                                                dplyr::left_join(object@farm |>
        #                                                                         dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
        #                                                                 by = id_cols),
        #                                        target_vars = c("sales_nb", "prod_t", "prop_sales_Qobs", "yield_t_Qobs"),
        #                                        primary_grp = c("FADN_code_letter", "COUNTRY", "NUTS2"),
        #                                        secondary_grp = c("FADN_code_letter", "COUNTRY"),
        #                                        weight_var = "SYS02")
        # sales_nb = dplyr::coalesce(SSN, 0)
        # prop_sales_Qobs = sales_nb / Qobs
        # yield_t_Qobs     = dplyr::if_else(Qobs > 0, sales_t / Qobs, 0)

        # FADN_averages$sales_shares
        # with share_SSN: share of sales for slaughter
        # and share_SRN: share of sales for slaughter
        avrg_share_sales <- h_average_practices(data = object@herd |>
                                                        # estimate proportion of sales on observed animals
                                                        dplyr::mutate(prop_SSN_Qobs = SSN / Qobs,
                                                                      prop_SRN_Qobs = SRN / Qobs,
                                                                      ratio_SS_V_N = SSV / SSN,
                                                                      ratio_SR_V_N = SRV / SRN) |>
                                                        # Add SYS02
                                                        dplyr::left_join(object@farm |>
                                                                                 dplyr::select(dplyr::all_of(id_cols), SYS02),
                                                                         by = id_cols),
                                                target_vars = c("share_SSN", "share_SRN",
                                                                "prop_SSN_Qobs", "prop_SRN_Qobs",
                                                                'ratio_SS_V_N', 'ratio_SR_V_N'),
                                                primary_grp = c("FADN_code_letter", "YEAR", "COUNTRY", "NUTS2"),
                                                secondary_grp = c("FADN_code_letter", "YEAR", "COUNTRY"),
                                                weight_var = "SYS02")





        # for animals off-farm number (Qofffarm), estimate the number of animals sold for slaughter and those sold for rearing

        pseudoherd_output <- pseudoherd_animals |>
                # join farm identification info (NUTS2, COUNTRY) to the pseudoherd activities
                dplyr::left_join(object@farm |>
                                         dplyr::select(dplyr::all_of(id_cols), NUTS2),
                                 by = id_cols) |>
                # estimate number of off-farm animals for meat activity
                dplyr::filter(Qeq_meat >0) |>
                dplyr::mutate(Qofffarm_meat = Qeq_meat - dplyr::coalesce(Qobs_meat, 0)) |>
                # join the sales shares (slaughter vs rearing) for the off-farm output
                dplyr::left_join(avrg_share_sales,
                                 by = c("YEAR", "COUNTRY", "NUTS2", "FADN_code_letter")) |>
                # estimate the output attributable to off-farm animals (Qofffarm_meat)
                dplyr::mutate(
                        # split the estimated off-farm sales between slaughter and rearing
                        # based on the average national/NUTS2 shares
                        sales_nb_offfarm_SSN = Qofffarm_meat * prop_SSN_Qobs ,
                        sales_nb_offfarm_SRN = Qofffarm_meat * prop_SRN_Qobs,
                        sales_nb_offfarm = sales_nb_offfarm_SSN + sales_nb_offfarm_SRN,

                        sales_e_offfarm_SSN = sales_nb_offfarm_SSN * dplyr::coalesce(ratio_SS_V_N, 0),
                        sales_e_offfarm_SRN = sales_nb_offfarm_SRN * dplyr::coalesce(ratio_SR_V_N, 0),
                        sales_e_offfarm = sales_e_offfarm_SSN + sales_e_offfarm_SRN
                ) |>
                # select relevant columns for output
                dplyr::select(dplyr::all_of(id_cols),
                              FADN_code_letter,
                              dplyr::matches("Qobs|Qeq|Qoff"),
                              sales_nb_offfarm,
                              sales_nb_offfarm_SSN,
                              sales_nb_offfarm_SRN,
                              sales_e_offfarm_SSN,
                              sales_e_offfarm_SRN,
                              sales_e_offfarm
                ) |>
                # add animal species
                dplyr::left_join(
                        data_extra$livestock |>
                                dplyr::select(FADN_code_letter, species),
                        by = c('FADN_code_letter')
                )


                # Default animal weights
                #UNFCCC_data$table3As2
                livestock_weights <- data_extra$livestock |>
                dplyr::filter(!is.na(UNFCCC_cat)) |>
                dplyr::select(FADN_code_letter, UNFCCC_cat) |>
                # add UNFCCC data
                dplyr::left_join(
                        UNFCCC_data$table3As2 |>
                                dplyr::filter(!is.na(UNFCCC_cat)) |>
                                dplyr::summarise(
                                        live_weight_kg = mean(Weight, na.rm = TRUE),
                                        .by = c(species,UNFCCC_cat,Country_ISO_3166_1_A3)
                                ),
                        by = 'UNFCCC_cat',
                        relationship = "many-to-many"
                ) |>
                # estimate average per livestock category
                dplyr::mutate(
                        mean_live_weight_kg = mean(live_weight_kg, na.rm = TRUE),
                        .by = FADN_code_letter
                ) |>
                # replace missing values by average
                dplyr::mutate(
                        live_weight_kg = dplyr::coalesce(live_weight_kg, mean_live_weight_kg)
                )



        ## MEAT ----
        pseudoherd_output_meat <- pseudoherd_output |>
                # add country iso names
                dplyr::left_join(
                        data_extra$country_names |>
                                dplyr::select(COUNTRY = country_FADN,
                                              Country_ISO_3166_1_A3),
                        by = 'COUNTRY'
                ) |>
                # add animal live weights
                dplyr::left_join(
                        livestock_weights,
                        by = c("FADN_code_letter", 'species', 'Country_ISO_3166_1_A3')) |>
                # define output
                dplyr::mutate(
                        # Categorize Output
                        output = dplyr::case_when(
                                FADN_code_letter == "LBOV1" ~ "meat_veal",
                                FADN_code_letter == "LCOWDAIR" ~ "meat_cull_cow",
                                species == "cattle" ~ "meat_beef",
                                species == "swine" ~ "meat_pork",
                                species == "poultry" ~ "meat_chicken",
                                TRUE ~ "meat"
                        ),
                        # Meat Logic
                        ## estimate number of animals sold for slaughter
                        sales_nb = dplyr::coalesce(sales_nb_offfarm_SSN, 0),
                        ## estimate live weight meat quantity
                        prod_t   = sales_nb * (live_weight_kg / 1000), # kg to tonnes
                        sales_t  = prod_t,
                        sales_e  = dplyr::coalesce(sales_e_offfarm_SSN, 0)
                ) |>
                # Clean
                dplyr::select(
                        dplyr::all_of(id_cols),
                        FADN_code_letter, output, species,
                        sales_nb, prod_t, sales_t, sales_e
                ) |>
                dplyr::filter(prod_t > 0 | sales_e > 0)

        ## Living animals ----
        pseudoherd_output_living_animals <- pseudoherd_output |>
                dplyr::mutate(
                        output = "living_animals",
                        prod_nb = dplyr::coalesce(sales_nb_offfarm_SRN, 0), # Number of animals
                        sales_nb = prod_nb,
                        sales_e = dplyr::coalesce(sales_e_offfarm_SRN, 0)
                ) |>
                dplyr::select(
                        dplyr::all_of(id_cols),
                        FADN_code_letter, output, species,
                        prod_nb, sales_nb, sales_e
                ) |>
                dplyr::filter(prod_nb > 0 | sales_e > 0)


        return(list(
                pseudoherd_output_meat = pseudoherd_output_meat,
                pseudoherd_output_living_animals = pseudoherd_output_living_animals
        ))
}
