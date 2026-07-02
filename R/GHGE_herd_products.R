#' Estimate Greenhouse Gas Emissions for Herd Outputs
#'
#' @description
#' Computes greenhouse gas emissions (GHG, kg CO\eqn{_2}e) allocated to
#' livestock outputs (milk, meat, eggs) for each farm × year × livestock
#' category combination. The function combines emissions from enteric
#' fermentation, manure management, and feed production, then allocates
#' the total impact to individual outputs using economic allocation ratios.
#'
#' Currently implemented for **cow milk**; meat and egg outputs are
#' reserved for future implementation.
#'
#' @details
#' ## Step 1 — Herd Output Economic Allocation Ratios
#' Economic allocation ratios are estimated via
#' \code{\link{f_herd_output_econ_alloc}}. If \code{account_pseudoherd =
#' TRUE}, a pseudo-herd (representing off-farm animals associated with
#' purchased feed) is also considered via
#' \code{\link{f_pseudoherd_output_econ_alloc}}, and on-farm and off-farm
#' outputs are combined with an \code{animals} flag
#' (\code{"on_farm"} vs. pseudo-herd rows).
#'
#' ## Step 2 — Herd and Feed Impact Estimation
#'
#' ### 2.1 Direct Herd Emissions
#' Three emission sources are computed:
#' \itemize{
#'   \item **Enteric fermentation** (CH\eqn{_4}): via
#'     \code{\link{GHGE_ch4_enteric}}.
#'   \item **Manure management CH\eqn{_4}**: via
#'     \code{\link{GHGE_ch4_manure}}.
#'   \item **Manure management N\eqn{_2}O** (direct, grazing, leaching):
#'     via \code{\link{GHGE_n2o_manure}}.
#' }
#'
#' ### 2.2 Feed-Related Emissions
#' Feed-related GHG impacts (kg CO\eqn{_2}e ha\eqn{^{-1}}) are retrieved
#' from \code{\link{f_GHGE_feed}}, then summarised per farm × year ×
#' livestock category as:
#' \itemize{
#'   \item \code{sum_feed_area_ha}: total feed area (ha).
#'   \item Weighted means of GHG intensity (kg CO\eqn{_2}e ha\eqn{^{-1}})
#'     across feed crops, weighted by area.
#'   \item Sums of total GHG from feed (kg CO\eqn{_2}e), obtained by
#'     multiplying intensity by area and summing.
#' }
#'
#' ### 2.3 Aggregation
#' All emission components are joined to the herd table and summed into
#' \code{total_ghg_livestock_cat_kgCO2e}:
#' \deqn{
#'   \text{total} = \text{feed GHG} + \text{CH}_4^\text{enteric} +
#'   \text{CH}_4^\text{MM} + \text{N}_2\text{O}^\text{D,MM} +
#'   \text{N}_2\text{O}^\text{G,MM} + \text{N}_2\text{O}^\text{L,MM}
#' }
#'
#' ## Step 3 — Allocation to Outputs
#'
#' ### Cow Milk
#' Farms with a dairy cattle activity (\code{Qobs_milk > 0}) and
#' positive milk production are identified. Total herd impact is allocated
#' to milk using the economic allocation ratio (\code{econ_alloc_ratio_herd})
#' from Step 1. Per-hectare and per-tonne GHG intensities are computed for
#' each emission component:
#' \deqn{
#'   \text{GHG per ha} = \frac{\text{GHG} \times r_\text{alloc}}{A_\text{feed}}
#'   \qquad
#'   \text{GHG per t} = \frac{\text{GHG} \times r_\text{alloc}}{Q_\text{milk}}
#' }
#'
#' @param object A valid \code{FADN2Footprint} S4 object, as created by
#'   \code{\link{new_FADN2Footprint}}. Must contain populated \code{@@herd},
#'   \code{@@output}, and \code{@@traceability} slots.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#' @param account_pseudoherd \code{logical}. If \code{TRUE}, off-farm
#'   pseudo-herd outputs estimated from purchased feed are included
#'   alongside on-farm herd outputs in the economic allocation step.
#'   Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to internal helper functions.
#'
#' @return A named \code{list} with three elements:
#' \describe{
#'   \item{\code{GHGE_milk}}{A \code{\link[tibble]{tibble}} with one row
#'     per farm × year × dairy livestock category × milk output, containing:
#'     \itemize{
#'       \item \code{id_cols}: farm × year identifiers.
#'       \item \code{FADN_code_letter}: livestock category code.
#'       \item \code{FADN_code_letter_output}: output product code (milk).
#'       \item \code{output}: \code{"milk"}.
#'       \item \code{prod_t}: milk production (t yr\eqn{^{-1}}).
#'       \item \code{sum_feed_area_ha}: total feed area (ha).
#'       \item \code{econ_alloc_ratio_herd}: economic allocation ratio for milk.
#'       \item GHG components in kg CO\eqn{_2}e (total, per ha, per t):
#'         feed-related, CH\eqn{_4} enteric, CH\eqn{_4} MM,
#'         N\eqn{_2}O direct/grazing/leaching MM, and total.
#'     }
#'   }
#'   \item{\code{GHGE_meat}}{\code{NULL} — reserved for future
#'     implementation.}
#'   \item{\code{GHGE_eggs}}{\code{NULL} — reserved for future
#'     implementation.}
#' }
#'
#' The result is also cached in \code{object@@footprints$BVIAS$BVI_milk}
#' on first computation.
#'
#' @seealso
#' \code{\link{GHGE_ch4_enteric}}, \code{\link{GHGE_ch4_manure}},
#' \code{\link{GHGE_n2o_manure}}, \code{\link{f_GHGE_feed}},
#' \code{\link{f_GHGE_crops}}, \code{\link{f_herd_output_econ_alloc}},
#' \code{\link{f_pseudoherd_output_econ_alloc}},
#' \code{\link{f_herd_activities}}, \code{\link{new_FADN2Footprint}}
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories}, Volume 4: Agriculture, Forestry and Other Land Use.
#' Intergovernmental Panel on Climate Change.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for
#' National Greenhouse Gas Inventories}, Volume 4. IPCC.
#'
#' @concept footprint-ghge
#' @importFrom dplyr select filter mutate left_join inner_join across all_of matches starts_with ends_with summarise rename_with bind_rows





# Steps:
## 1. Estimate herd outputs (if considering pseudo-farm, estimate associated output)
## 2. Estimate herd and feed impact
## 3. Allocate herd impact to outputs

# estimate BVI per ha and per t for herds
f_GHGE_herd_output <- function(object,
                               overwrite = FALSE,
                               account_pseudoherd = FALSE,
                               ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_milk)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_milk.")
    return(object@footprints$GHGE$GHGE_milk)  # use cached value
  }
  if (!is.null(object@footprints$GHGE$GHGE_meat)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_meat.")
    return(object@footprints$GHGE$GHGE_meat)  # use cached value
  }
  if (!is.null(object@footprints$GHGE$GHGE_eggs)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_eggs.")
    return(object@footprints$GHGE$GHGE_eggs)  # use cached value
  }

  id_cols = object@traceability$id_cols

  # 1. Estimate herd activities and outputs ------------------------------------------------------------------------------

  ## Distribute animals across activities ----
  herd_activities = f_herd_activities(object, overwrite = overwrite) |>
    # add livestock unit coefficients
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter, livestock_unit_coef),
      by = 'FADN_code_letter'
    ) |>
    # pivot longer to have one row per activity
    tidyr::pivot_longer(
      cols = dplyr::matches("Qobs_"),
      names_to = "activity",
      values_to = "Qobs_activity"
    ) |>
    dplyr::mutate(
      activity = gsub("Qobs_", "", activity)
    ) |>
    dplyr::filter(Qobs_activity > 0) |>
    # convert to livestock units
    dplyr::mutate(
      Qobs_LU = Qobs * livestock_unit_coef,
      Qobs_activity_LU = Qobs_activity * livestock_unit_coef
    ) |>
    # total LU per activity
    dplyr::mutate(
      Qobs_activity_LU_sum = sum(Qobs_activity_LU, na.rm = TRUE),
      .by = c(dplyr::all_of(id_cols), activity)
    )

  ## Estimate activity area ----
  # TODO: check how I estimate feed area => I should do it through avrg_FADN
  ## estimated in f_GHGE_herd and following code


  # 2. Estimate herd impact ------------------------------------------------------------------------------

  GHGE_herd = f_GHGE_herd(object, overwrite =  overwrite)

  GHGE_elec = f_GHGE_elec(object, overwrite = overwrite)

  GHGE_fuels = f_GHGE_fuels(object, overwrite = overwrite)

  # 3. Allocate impact to activity outputs ------------------------------------------------------------------------------

  ## Combine herd activities and impacts
  herd_act_impact <- herd_activities |>
    dplyr:: left_join(
      GHGE_herd |>
        dplyr::select(dplyr::all_of(id_cols),
                      FADN_code_letter,
                      dplyr::matches('kgCO2e|area_ha|DM_t')),
      by = c(id_cols, 'FADN_code_letter')) |>
    # Allocate impact across activities for each livestock category
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("kgCO2e_livcat$|area_ha|DM_t"),
        list(act_livcat = ~ .x * (Qobs_activity / Qobs)),
        .names = "{stringr::str_replace(.col, '_livcat$', '')}_{.fn}"
      )
    ) |>
    # sum impact per activity
    dplyr::summarise(
      dplyr::across(
        dplyr::matches("_act_livcat$"),
        list(act = ~ sum(.x, na.rm = TRUE)),
        .names = "{stringr::str_replace(.col, '_act_livcat$', '')}_{.fn}"
      ),
      .by = c(dplyr::all_of(id_cols), 'activity', 'species')
    )

  ## Output economic allocation ratio
  tmp_econ_alloc = f_output_econ_alloc(object, account_pseudoherd = account_pseudoherd)

  ## Outputs

  herd_outputs <- tmp_econ_alloc$herd_outputs

  ## allocate herd impact to outputs

  herd_output_impact <- herd_outputs |>
    dplyr::select(
      dplyr::all_of(id_cols),
      activity, FADN_code_letter, FADN_code_letter_output, output, species,
      sales_nb, prod_t, sales_t, sales_e, output_e, TO_e, sum_sales_e_farm, sum_sales_e_activity, econ_alloc_ratio_herd, econ_alloc_ratio_herd_activity
    ) |>
    # add impact
    dplyr::left_join(
      herd_act_impact ,
      by = c(id_cols, 'activity', 'species')
    ) |>
    # allocate
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("_act$"),
        list(output = ~ .x * econ_alloc_ratio_herd_activity),
        .names = "{stringr::str_replace(.col, '_act$', '')}_{.fn}"
      )
    ) #|>
    # add energy impact
    ## remove energy variables
    ## we keep variables of GHGE from energy as estimated in the energy functions (f_GHGE_elec, f_GHGE_fuels)
    #dplyr::select(-dplyr::matches("elec|fuel|diesel")) |>
    #dplyr::left_join(
    #  GHGE_elec$alloc_GHGE_electricity,
    #  by = join_by(ID, YEAR, COUNTRY, activity, FADN_code_letter, FADN_code_letter_output, output, species)
    #) |>
    ### plot(herd_output_impact$ghg_elec_kgCO2e_output.x,herd_output_impact$ghg_elec_kgCO2e_output.y)
    #dplyr::left_join(
    #  GHGE_fuels$alloc_GHGE_fuels,
    #  by = join_by(ID, YEAR, COUNTRY, activity, FADN_code_letter, FADN_code_letter_output, output, species)
    #)
 ## plot(herd_output_impact$ghg_heat_fuel_kgCO2e_output.x,herd_output_impact$ghg_heat_fuel_kgCO2e_output.y)

  # nrow(herd_output_impact) == nrow(herd_output_impact |> select(ID, COUNTRY,YEAR, FADN_code_letter, output) |> distinct())


  # 4. Sum impact per output ----

  co2_cols = names(herd_output_impact)[grepl("kgCO2e_output$", names(herd_output_impact))]

  ### Calculate per ha and per t for each CO2e variable
  herd_output_impact_ha_t <- herd_output_impact |>
    # change cull cow meat as beef output
    dplyr::mutate(output = ifelse(output == "meat_cull_cow", "meat_beef", output)) |>
    # sum up impact per output
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(c(co2_cols, 'feed_farm_area_ha_output', 'feed_pseudofarm_area_ha_output', 'prod_t')),
        list(sum = ~sum(.x, na.rm = TRUE)),
        .names = "{.col}"
      ),
      .by = dplyr::all_of(c(id_cols, 'activity', 'output', 'species'))
    ) |>
    # allocate impact per ha and per ton
    dplyr::mutate(
      # per ha farm
      dplyr::across(dplyr::all_of(co2_cols[-grep("feed_pseudofarm_|^pseudofarm_ghge_",co2_cols)]),
                    list(per_ha_farm = ~ .x / feed_farm_area_ha_output),
                    .names = "{str_replace(.col, '_output$', '')}_{.fn}"  # Remove "_output" and append {.fn}
      ),
      # per ha pseudofarm
      dplyr::across(dplyr::all_of(co2_cols[-grep("feed_farm_|^farm_ghge_",co2_cols)]),
                    list(per_ha_pseudofarm = ~ .x / feed_pseudofarm_area_ha_output),
                    .names = "{str_replace(.col, '_output$', '')}_{.fn}"  # Remove "_output" and append {.fn}
      ),
      # per t of product
      dplyr::across(dplyr::all_of(co2_cols),
                    list(per_t  = ~ .x / prod_t),
                    .names = "{str_replace(.col, '_output$', '')}_{.fn}"  # Remove "_output" and append {.fn}
      ),

    )

# 5. Per product type tables ------------------------------------------------------------------------------

  milk_impact <- herd_output_impact_ha_t |>
    dplyr::filter(output == "milk")

  meat_impact <- herd_output_impact_ha_t |>
    dplyr::filter(activity == "meat")

  eggs_impact <- herd_output_impact_ha_t |>
    dplyr::filter(activity == "eggs")



  return(list(
    GHGE_milk = milk_impact,
    GHGE_meat = meat_impact,
    GHGE_eggs = eggs_impact
  ))

}



