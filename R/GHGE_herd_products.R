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
#' @importFrom dplyr select filter mutate left_join inner_join across
#'   all_of matches starts_with ends_with summarise rename_with bind_rows





# Steps:
## 1. Estimate herd outputs (if considering pseudo-farm, estimate associated output)
## 2. Estimate herd and feed impact
## 3. Allocate herd impact to outputs

# estimate BVI per ha and per t for herds
f_GHGE_herd_output <- function(object,
                               overwrite = FALSE,
                               account_pseudoherd = F, ...) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_milk)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_milk.")
    return(object@footprints$GHGE$GHGE_milk)  # use cached value
  }

  id_cols = object@traceability$id_cols
  # 1. Estimate herd outputs ------------------------------------------------------------------------------

  ## output economic allocation ratio
  if (account_pseudoherd == F) {

    tmp_econ_alloc = f_output_econ_alloc(object)

  } else {

    tmp_econ_alloc = f_output_econ_alloc(object)
    # TODO add pseudofarm estimation
  }

  # 2. Estimate herd and feed impact ------------------------------------------------------------------------------

  herd_impact = f_GHGE_herd(object, overwrite =  overwrite)

  # Identify CO2e columns
  co2_cols <- names(herd_impact |>
                      dplyr::select(dplyr::matches("kgCO2e")
                                    & dplyr::matches("_livcat$")
                      ))

  # 3. Allocate impact to activity outputs ------------------------------------------------------------------------------

  herd_activities = f_herd_activities(object, overwrite = overwrite)

## 3.1. COW MILK ----

  ### Herd impact ----
  cow_milk_herd_impact <- herd_activities |>
    # farms with dairy cattle activity
    dplyr::filter(species == "cattle" & Qobs_milk >0) |>
    dplyr::select(dplyr::all_of(id_cols),FADN_code_letter,
                  dplyr::matches("Qobs")) |>
    # add animals and feed impact
    dplyr::left_join(
      herd_impact |>
        dplyr::select(-dplyr::matches("Qobs")),
      by = c(id_cols, 'FADN_code_letter')
    ) |>
    # add livestock unit coefficient
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,livestock_unit_coef),
      by = 'FADN_code_letter'
    ) |>
    # total animals and feed impact for activity
    dplyr::summarise(
      dplyr::across(dplyr::all_of(co2_cols),
                    list(activity = ~ sum(.x * (Qobs_milk / Qobs), na.rm = T)),
                    .names = "{str_replace(.col, '_livcat$', '')}_{.fn}"  # Remove "_livcat" and append {.fn}
      ),
      Qobs_LU = sum(Qobs * livestock_unit_coef, na.rm = T),
      Qobs_milk_LU = sum(Qobs_milk * livestock_unit_coef, na.rm = T),
      feed_farm_area_ha_activity = sum(feed_farm_area_ha_livcat, na.rm = T),
      feed_pseudofarm_area_ha_activity = sum(feed_pseudofarm_area_ha_livcat, na.rm = T),
      .by = dplyr::all_of(id_cols)
    )

  ### Energy impact ----

  #### Heating fuel ----
  ## We assume that no diesel and all heating fuel is allocated to herd
  tmp_GHGE_fuels = GHGE_fuels(object)
  tmp_GHGE_fuels_alloc = tmp_econ_alloc$herd_outputs |> # select economic allocation
    # select activity
    dplyr::filter(activity == "milk") |>
    dplyr::left_join(
      tmp_GHGE_fuels,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the herd output
    dplyr::summarise(
      ghg_heat_fuel_kgCO2e_activity = sum(ghg_heat_fuel_kgCO2e * econ_alloc_ratio_herd, na.rm = T),
      .by = id_cols
    )

  #### Electricity ----
  tmp_GHGE_elec = GHGE_elec(object)
  # select economic allocation across outputs
  tmp_GHGE_elec_alloc = tmp_econ_alloc$all_outputs |>
    # select activity
    dplyr::filter(activity == "milk") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_elec,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::summarise(
      ghg_elec_kgCO2e_activity = sum(ghg_elec_kgCO2e * econ_alloc_ratio_farm, na.rm = T),
      .by = id_cols
    )

### Combine herd and energy impacts ----

  cow_milk_impact_tot <- cow_milk_herd_impact |>
    # add heating fuels
    dplyr::left_join(tmp_GHGE_fuels_alloc,
                     by = id_cols) |>
    # add electricity
    dplyr::left_join(tmp_GHGE_elec_alloc,
                     by = id_cols)

  ### Compute per ha and per t impact ----

  co2_cols_act = names(cow_milk_impact_tot |>
                        dplyr::select(dplyr::matches("kgCO2e")
                                      & dplyr::matches("_activity$")
                        ))

  ### Calculate per ha and per t for each CO2e variable
  cow_milk_impact <- cow_milk_impact_tot |>
    # add economic allocation ratios for milk (to distribute impact among milk and cull cow meat)
    dplyr::left_join(
      tmp_econ_alloc$herd_outputs |>
        dplyr::filter(species == "cattle" & activity == "milk") |>
        dplyr::select(dplyr::all_of(id_cols),
                      FADN_code_letter, FADN_code_letter_output, activity, output,
                      econ_alloc_ratio_herd_activity),
      by = c(id_cols)
    ) |>
    # add milk and cull cow meat production
    dplyr::inner_join(
      bind_rows(
        object@output$other_herd_products |>
          dplyr::filter(species == "cattle" & output == "milk") |>
          dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, output, prod_t),
        object@output$meat |>
          dplyr::filter(species == "cattle" & output == "meat_cull_cow") |>
          dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, output, prod_t)
      ),
      by = c(id_cols, 'FADN_code_letter', 'output')
    ) |>
    # filter out farms without milk production
    dplyr::filter(prod_t >0) |>
    # allocate impact to output
    dplyr::mutate(
      # per ha farm
      dplyr::across(dplyr::all_of(co2_cols_act[-grep("feed_pseudofarm_|pseudofarm_ghge_",co2_cols_act)]),
                    list(per_ha_farm = ~ (.x * econ_alloc_ratio_herd_activity) / feed_farm_area_ha_activity),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),
      # per ha pseudofarm
      dplyr::across(dplyr::all_of(co2_cols_act[-grep("feed_farm_|^farm_ghge_",co2_cols_act)]),
                    list(per_ha_pseudofarm = ~ (.x * econ_alloc_ratio_herd_activity) / feed_pseudofarm_area_ha_activity),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),
      # per t of milk
      dplyr::across(dplyr::all_of(co2_cols_act),
                    list(per_t  = ~ (.x * econ_alloc_ratio_herd_activity) / prod_t),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),

    ) |>
    dplyr::select(-dplyr::matches("_activity$"))

  ## 3.2. BEEF ----

  ### Herd impact ----
  cow_meat_herd_impact <- herd_activities |>
    # farms with meat cattle activity
    dplyr::filter(species == "cattle" & Qobs_meat >0) |>
    dplyr::select(dplyr::all_of(id_cols),FADN_code_letter,
                  dplyr::matches("Qobs")) |>
    # add animals and feed impact
    dplyr::left_join(
      herd_impact |>
        dplyr::select(-dplyr::matches("Qobs")),
      by = c(id_cols, 'FADN_code_letter')
    ) |>
    # add livestock unit coefficient
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,livestock_unit_coef),
      by = 'FADN_code_letter'
    ) |>
    # total animals and feed impact for activity
    dplyr::summarise(
      dplyr::across(dplyr::all_of(co2_cols),
                    list(activity = ~ sum(.x * (Qobs_meat / Qobs), na.rm = T)),
                    .names = "{str_replace(.col, '_livcat$', '')}_{.fn}"  # Remove "_livcat" and append {.fn}
      ),
      Qobs_LU = sum(Qobs * livestock_unit_coef, na.rm = T),
      Qobs_meat_LU = sum(Qobs_meat * livestock_unit_coef, na.rm = T),
      feed_farm_area_ha_activity = sum(feed_farm_area_ha_livcat, na.rm = T),
      feed_pseudofarm_area_ha_activity = sum(feed_pseudofarm_area_ha_livcat, na.rm = T),
      .by = id_cols
    )

  ### Energy impact ----

  #### Heating fuel ----
  ## We assume that no diesel and all heating fuel is allocated to herd
  tmp_GHGE_fuels = GHGE_fuels(object)
  tmp_GHGE_fuels_alloc = tmp_econ_alloc$herd_outputs |> # select economic allocation
    # select activity
    dplyr::filter(activity == "meat") |>
    dplyr::left_join(
      tmp_GHGE_fuels,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the herd output
    dplyr::summarise(
      ghg_heat_fuel_kgCO2e_activity = sum(ghg_heat_fuel_kgCO2e * econ_alloc_ratio_herd, na.rm = T),
      .by = id_cols
    )

  #### Electricity ----
  tmp_GHGE_elec = GHGE_elec(object)
  # select economic allocation across outputs
  tmp_GHGE_elec_alloc = tmp_econ_alloc$all_outputs |>
    # select activity
    dplyr::filter(activity == "meat") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_elec,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::summarise(
      ghg_elec_kgCO2e_activity = sum(ghg_elec_kgCO2e * econ_alloc_ratio_farm, na.rm = T),
      .by = id_cols
    )

  ### Combine herd and energy impacts ----

  cow_meat_impact_tot <- cow_meat_herd_impact |>
    # add heating fuels
    dplyr::left_join(tmp_GHGE_fuels_alloc,
                     by = id_cols) |>
    # add electricity
    dplyr::left_join(tmp_GHGE_elec_alloc,
                     by = id_cols)

  ### Compute per ha and per t impact ----

  co2_cols_act = names(cow_meat_impact_tot |>
                        dplyr::select(dplyr::matches("kgCO2e")
                                      & dplyr::matches("_activity$")
                        ))

  ### Calculate per ha and per t for each CO2e variable
  cow_meat_impact <- cow_meat_impact_tot |>
    # add economic allocation ratios for meat (to distribute impact among meat and cull cow meat)
    dplyr::left_join(
      tmp_econ_alloc$herd_outputs |>
        dplyr::filter(species == "cattle" & activity == "meat") |>
        dplyr::select(dplyr::all_of(id_cols),
                      FADN_code_letter, FADN_code_letter_output, activity, output,
                      econ_alloc_ratio_herd_activity),
      by = c(id_cols)
    ) |>
    # add meat production
    dplyr::inner_join(
      object@output$meat |>
        dplyr::filter(species == "cattle") |>
        dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, output, prod_t),
      by =  c(id_cols, 'FADN_code_letter', 'output')
    ) |>
    # filter out farms without meat production
    dplyr::filter(prod_t >0) |>
    # allocate impact to output
    dplyr::mutate(
      # per ha farm
      dplyr::across(dplyr::all_of(co2_cols_act[-grep("feed_pseudofarm_|pseudofarm_ghge_",co2_cols_act)]),
                    list(per_ha_farm = ~ (.x * econ_alloc_ratio_herd_activity) / feed_farm_area_ha_activity),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),
      # per ha pseudofarm
      dplyr::across(dplyr::all_of(co2_cols_act[-grep("feed_farm_|^farm_ghge_",co2_cols_act)]),
                    list(per_ha_pseudofarm = ~ (.x * econ_alloc_ratio_herd_activity) / feed_pseudofarm_area_ha_activity),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),
      # per t of meat
      dplyr::across(dplyr::all_of(co2_cols_act),
                    list(per_t  = ~ (.x * econ_alloc_ratio_herd_activity) / prod_t),
                    .names = "{str_replace(.col, '_activity$', '')}_{.fn}"  # Remove "_activity" and append {.fn}
      ),

    ) |>
    dplyr::select(-dplyr::matches("_activity$"))

  # 4. Per product type tables ------------------------------------------------------------------------------

  milk_impact <- cow_milk_impact |>
    dplyr::filter(output == "milk")

  meat_impact <- cow_meat_impact |>
    dplyr::bind_rows(
      cow_milk_impact |>
        dplyr::filter(output == "meat_cull_cow")
    )


  return(list(
    GHGE_milk = milk_impact,
    GHGE_meat = meat_impact,
    GHGE_eggs = NULL
  ))

}



