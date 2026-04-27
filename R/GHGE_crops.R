#' Estimate Greenhouse Gas Emissions for Crop Activities
#'
#' @description
#' Calculates the total greenhouse gas emissions (GHGE) associated with crop
#' production at the farm level, combining multiple emission sources and
#' expressing results both in absolute terms and per unit of land area and
#' product output. Results are returned from cache if already computed for
#' the provided \code{FADN2Footprint} object.
#'
#' The following emission sources are aggregated:
#' \itemize{
#'   \item **Managed soils**: direct and indirect N2O emissions from synthetic
#'     and organic fertilizer application, crop residues, and grazing livestock
#'     dung and urine deposition (IPCC Equations 11.1, 2006/2019).
#'   \item **Fertilizer production**: upstream GHG emissions associated with the
#'     manufacturing of synthetic fertilizers.
#'   \item **Off-road diesel combustion**: farm machinery fuel use, allocated to
#'     each crop via economic allocation.
#'   \item **Electricity consumption**: on-farm electricity use, allocated to each
#'     crop via economic allocation.
#' }
#'
#' @details
#' ## Economic Allocation
#' Fuel and electricity emissions are shared across crops proportionally to each
#' crop's contribution to the total farm crop output value (turnover, \code{TO}):
#' \deqn{econ\_alloc_c = \frac{TO_c}{\sum_c TO_c}}
#' where \eqn{TO_c} is the economic output of crop \eqn{c}.
#'
#' ## Emission Components
#' All emission components (kg CO2-eq) are summed to obtain the total crop-level
#' GHG footprint:
#' \deqn{total\_ghg\_crop = N2O_d + N2O_{ATD} + N2O_L +
#'   ghg_{ferti\_prod} + ghg_{diesel} + ghg_{elec}}
#' where:
#' \itemize{
#'   \item \eqn{N2O_d} = direct N2O emissions from managed soils
#'     (kg CO2-eq, from \code{\link{GHGE_n2o_msoils}})
#'   \item \eqn{N2O_{ATD}} = indirect N2O emissions from atmospheric deposition
#'     (kg CO2-eq, from \code{\link{GHGE_n2o_msoils}})
#'   \item \eqn{N2O_L} = indirect N2O emissions from leaching and run-off
#'     (kg CO2-eq, from \code{\link{GHGE_n2o_msoils}})
#'   \item \eqn{ghg_{ferti\_prod}} = upstream emissions from fertilizer production
#'     (kg CO2-eq, from \code{\link{GHGE_ferti_prod}})
#'   \item \eqn{ghg_{diesel}} = off-road diesel combustion emissions allocated to
#'     the crop (kg CO2-eq, from \code{\link{GHGE_fuels}})
#'   \item \eqn{ghg_{elec}} = electricity consumption emissions allocated to the
#'     crop (kg CO2-eq, from \code{\link{GHGE_elec}})
#' }
#'
#' ## Intensity Indicators
#' For each emission component and for the total, emissions are additionally
#' expressed as:
#' \itemize{
#'   \item per hectare (kg CO2-eq ha\eqn{^{-1}}): dividing by \code{area_ha}
#'   \item per tonne of product (kg CO2-eq t\eqn{^{-1}}): dividing by \code{prod_t}
#' }
#' Column names follow the pattern \code{{emission_col}_per_ha} and
#' \code{{emission_col}_per_t}.
#'
#' @note
#' \itemize{
#'   \item If \code{object@footprints$GHGE$GHGE_crops} is already populated,
#'     the cached result is returned immediately without recomputation.
#'   \item N2O emissions from soil organic matter mineralisation following land
#'     use change are not yet included (TODO).
#'   \item Land use change emissions related to other farm practices (hedge
#'     density, ground cover, grassland management, etc.) are not yet included
#'     (TODO).
#'   \item \code{NA} values in individual emission components are treated as zero
#'     when computing \code{total_ghg_crop_kgCO2e} (via \code{na.rm = TRUE}).
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A \code{\link[tibble]{tibble}} containing one row per farm-crop
#' combination, with the following columns:
#' \describe{
#'   \item{...}{Traceability identifier columns as defined in
#'     \code{`object@traceability$id_cols`} (e.g., farm ID, year, NUTS2 region).}
#'   \item{FADN_code_letter}{\code{character}. FADN crop category code.}
#'   \item{area_ha}{\code{numeric}. Harvested crop area (ha).}
#'   \item{prod_t}{\code{numeric}. Crop production (t).}
#'   \item{N2O_d_kgCO2e}{\code{numeric}. Direct N2O emissions from managed soils
#'     (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{N2O_ATD_kgCO2e}{\code{numeric}. Indirect N2O emissions from atmospheric
#'     deposition (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{N2O_L_kgCO2e}{\code{numeric}. Indirect N2O emissions from leaching and
#'     run-off (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{ghg_ferti_prod_kgCO2e}{\code{numeric}. Upstream emissions from
#'     fertilizer production (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{ghg_diesel_crop_kgCO2e}{\code{numeric}. Off-road diesel emissions
#'     allocated to the crop (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{ghg_elec_crop_kgCO2e}{\code{numeric}. Electricity emissions allocated
#'     to the crop (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{total_ghg_crop_kgCO2e}{\code{numeric}. Total GHG emissions from crop
#'     production (kg CO2-eq yr\eqn{^{-1}}).}
#'   \item{..._per_ha}{\code{numeric}. Each emission column expressed per hectare
#'     (kg CO2-eq ha\eqn{^{-1}}).}
#'   \item{..._per_t}{\code{numeric}. Each emission column expressed per tonne of
#'     product (kg CO2-eq t\eqn{^{-1}}).}
#' }
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas Inventories,
#' Volume 4: Agriculture, Forestry and Other Land Use, Chapter 11: N2O Emissions
#' from Managed Soils, and CO2 Emissions from Lime and Urea Application}.
#' Eggleston H.S., Buendia L., Miwa K., Ngara T. and Tanabe K. (eds). IGES, Japan.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land Use,
#' Chapter 11}.
#' Calvo Buendia, E., Tanabe, K., Kranjc, A., Baasansuren, J., Fukuda, M.,
#' Ngarize, S., Osako, A., Pyrozhenko, Y., Shermanau, P. and Federici, S. (eds).
#' IPCC, Switzerland.
#'
#' @seealso
#' \code{\link{GHGE_n2o_msoils}}, \code{\link{GHGE_ferti_prod}},
#' \code{\link{GHGE_fuels}}, \code{\link{GHGE_elec}},
#' \code{\link{f_GHGE_livestock}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr left_join select mutate across all_of
#' @importFrom purrr reduce
#'
#' @concept footprint-ghge
#' @export



# estimate GHGE per ha and per t of crops
f_GHGE_crops <- function(object,
                         overwrite = FALSE) {
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }
  if (!is.null(object@footprints$GHGE$GHGE_crops)&& !overwrite) {
    message("Using cached values stored in object@footprints$GHGE$GHGE_crops.")
    return(object@footprints$GHGE$GHGE_crops)  # use cached value
  }

  id_cols = object@traceability$id_cols

  # Managed soils (including direct and indirect N2O emissions) ----

  ## Direct emissions from synthetic and organic fertilizers, crop residues and grazing livestock deposited dung and urine
  ### see EQUATION 11.1 (IPCC Guidelines, 2006, 2019)
  ### TODO: add mineralisation of soil organic matter in land use
  ## Indirect emissions from atmospheric deposition and leaching/run-off
  tmp_GHGE_n2o_msoils = GHGE_n2o_msoils(object, overwrite = overwrite)

  # Emissions from fertilizer production ----

  tmp_GHGE_ferti_prod = GHGE_ferti_prod(object, overwrite = overwrite)


  # Economic allocation for crops ----
  # for diesel and electricity allocation

  ## We estimate economic allocation ratios
  tmp_econ_alloc = f_output_econ_alloc(object)


  # Fuels ----

  tmp_GHGE_fuels = GHGE_fuels(object)

  ## Heating fuels ----
  tmp_GHGE_heating_fuels_p_crop = tmp_econ_alloc$all_outputs |>
    # select economic allocation across crop outputs
    dplyr::filter(activity == "crop") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_fuels,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::mutate(
      ghg_heat_fuel_crop_kgCO2e = ghg_heat_fuel_kgCO2e * econ_alloc_ratio_farm
    )

  ## Off-road diesel ----
  # We estimate the quantity of off-road diesel (L/ha) used for ploughing
  # and allocate the remain according to crop area
  ## Tillage
  tmp_tillage = f_tillage(object, overwrite = overwrite)
  ## Area allocation
  tmp_area_alloc = object@crop |>
    dplyr::mutate(sum_farm_area_ha = sum(area_ha, na.rm = T),
                  .by = dplyr::all_of(id_cols)) |>
    dplyr::reframe(area_alloc_ratio = area_ha / sum_farm_area_ha,
                   .by = c(dplyr::all_of(id_cols), 'FADN_code_letter'))
  ## Allocate
  tmp_GHGE_diesel_p_crop =  Reduce(
    x = list(object@crop |>
               dplyr::select(dplyr::all_of(id_cols), FADN_code_letter,
                             area_ha,prod_t,sales_t),
             tmp_tillage,
             tmp_area_alloc),
    f = function(x,y) dplyr::left_join(x, y, by = c(id_cols,"FADN_code_letter"))
  ) |>
    # add activity data
    dplyr::left_join(tmp_GHGE_fuels,
                     by = id_cols) |>
    # Diesel for tillage
    dplyr::mutate(
      # Diesel used for tillage per crop
      ## set at zeros for crops without tillage
      diesel_tillage_l = dplyr::coalesce(tillage,0) * area_ha
    ) |>
    dplyr::mutate(
      # Total diesel used for tillage per farm
      diesel_tillage_tot_farm_l = sum(tillage * area_ha, na.rm = T),
      .by = id_cols) |>
    # Remaining diesel
    dplyr::mutate(diesel_remain_tot_farm_l = diesel_l - diesel_tillage_tot_farm_l,
                  diesel_remain_l = diesel_remain_tot_farm_l * area_alloc_ratio) |>
    # share of total GHGE from diesel
    dplyr::mutate(
      ghg_diesel_tillage_crop_kgCO2e = ghg_diesel_kgCO2e * (diesel_tillage_l / diesel_l),
      ghg_diesel_remain_crop_kgCO2e = ghg_diesel_kgCO2e * (diesel_remain_l / diesel_l),
      ghg_diesel_crop_kgCO2e = ghg_diesel_tillage_crop_kgCO2e + ghg_diesel_remain_crop_kgCO2e
    )

  # Check
  # tmp = tmp_GHGE_diesel_p_crop |> mutate(test = sum(ghg_diesel_tillage_kgCO2e + ghg_diesel_remain_kgCO2e), .by = id_cols)
  # plot(tmp$ghg_diesel_kgCO2e, tmp$test)

  # Electricity ----
  tmp_GHGE_elec = GHGE_elec(object)
  # select economic allocation across crop outputs
  tmp_GHGE_elec_p_crop = tmp_econ_alloc$all_outputs |>
    # select crops
    dplyr::filter(activity == "crop") |>
    # add activity data
    dplyr::left_join(
      tmp_GHGE_elec,
      by = id_cols
    ) |>
    # GHG kg CO2-eq/MJ with an economic allocation to the crop
    dplyr::mutate(
      ghg_elec_crop_kgCO2e = ghg_elec_kgCO2e * econ_alloc_ratio_farm
    )

  # TODO: add land use with other practices such as hedge density, ground cover, grasslands, etc.

  # Combine components ----
  duplicated_columns = setdiff(colnames(object@crop), c(id_cols,"FADN_code_letter"))

  crop_impact_tot <- Reduce(
    x = list(object@crop,
             tmp_GHGE_n2o_msoils |> dplyr::select(-dplyr::matches(duplicated_columns)),
             tmp_GHGE_ferti_prod |> dplyr::select(-dplyr::matches(duplicated_columns)),
             tmp_GHGE_heating_fuels_p_crop |> dplyr::select(-dplyr::matches(duplicated_columns)),
             tmp_GHGE_diesel_p_crop |> dplyr::select(-dplyr::matches(duplicated_columns)),
             tmp_GHGE_elec_p_crop |> dplyr::select(-dplyr::matches(duplicated_columns))),
    f = function(x,y) dplyr::left_join(x, y, by = c(id_cols,"FADN_code_letter"))
  ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter,
      area_ha, prod_t,
      N2O_d_kgCO2e, N2O_ATD_kgCO2e, N2O_L_kgCO2e,
      ghg_ferti_prod_kgCO2e, ghg_heat_fuel_crop_kgCO2e, ghg_diesel_crop_kgCO2e, ghg_elec_crop_kgCO2e
    ) |>
    dplyr::mutate(
      total_ghg_crop_kgCO2e =  rowSums(dplyr::across(c(N2O_d_kgCO2e, N2O_ATD_kgCO2e, N2O_L_kgCO2e,
                                                       ghg_ferti_prod_kgCO2e,
                                                       ghg_heat_fuel_crop_kgCO2e,
                                                       ghg_diesel_crop_kgCO2e,
                                                       ghg_elec_crop_kgCO2e)), na.rm = TRUE)
    )

  # Compute per ha and per t impact ----
  # Identify CO2e columns
  co2_cols <- grep("kgCO2e$", names(crop_impact_tot), value = TRUE)

  # Calculate per ha and per t for each CO2e variable
  crop_impact <- crop_impact_tot |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(co2_cols),
                    list(per_ha = ~ .x / area_ha,
                         per_t  = ~ .x / prod_t),
                    .names = "{.col}_{.fn}")
    )


  return(crop_impact)

}
