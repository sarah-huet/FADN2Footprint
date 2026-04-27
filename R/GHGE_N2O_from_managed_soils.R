#' Calculate N2O Emissions from Managed Soils
#'
#' @description
#' Estimates direct and indirect nitrous oxide (N2O) emissions from managed
#' soils (cropland and grassland) following IPCC Tier 2/3 methodology (IPCC
#' 2006, 2019 Refinements), anchored on country-specific emission factors
#' reported to the UNFCCC. The function covers three emission pathways:
#' \enumerate{
#'   \item **Direct N2O emissions** from synthetic fertilizers, organic
#'     amendments, crop residues, drained organic soils, and grazing animal
#'     excreta deposited on pastures (IPCC Eq. 11.2).
#'   \item **Indirect N2O emissions from atmospheric deposition** of NH3 and
#'     NOx volatilised from managed soils (IPCC Eq. 11.11).
#'   \item **Indirect N2O emissions from leaching and runoff** of N added to
#'     or mineralised in managed soils (IPCC Eq. 11.10).
#' }
#' All emission fluxes are converted to CO2-equivalents using
#' \code{\link{GWP}}.
#'
#' @details
#' ## Activity Data Assembly
#' Activity data are assembled by joining:
#' \itemize{
#'   \item Crop-level data (\code{object@crop}): area, yield, land use type,
#'     species.
#'   \item Nitrogen fertilization data from \code{\link{f_n_ferti}}:
#'     synthetic N (\eqn{F_{SN}}) and organic N (\eqn{F_{ON}} or
#'     \eqn{F_{PRP}}) per crop and farm.
#'   \item Country- and year-specific emission factors from UNFCCC submissions
#'     (\code{\link{UNFCCC_data}}\code{$EF_managed_soils}), matched by
#'     \code{Country_ISO_3166_1_A3} and \code{YEAR}; cross-country means are
#'     used as fallback when values are missing.
#'   \item IPCC crop-specific parameters (Tables 11.1 & 11.2) from
#'     \code{\link{data_extra}}\code{$IPCC_default_values}, matched via
#'     \code{condition_species}; generic values are applied when
#'     crop-specific parameters are absent.
#' }
#'
#' ## Nitrogen Input Variables
#' \describe{
#'   \item{\eqn{F_{SN}}}{Synthetic N applied (kg N yr\eqn{^{-1}}),
#'     from \code{\link{f_n_ferti}}.}
#'   \item{\eqn{F_{ON}}}{Organic N applied to arable soils (kg N
#'     yr\eqn{^{-1}}), from \code{\link{f_n_ferti}}; set to 0 for
#'     grassland.}
#'   \item{\eqn{F_{PRP}}}{N deposited by grazing animals on pasture (kg N
#'     yr\eqn{^{-1}}), taken as organic fertilisation of grassland; set to
#'     0 for arable land.}
#'   \item{\eqn{F_{CR}}}{N in crop residues returned to soil (kg N
#'     yr\eqn{^{-1}}), estimated from above-ground (AGR) and below-ground
#'     (BGR) dry matter residues:
#'     \deqn{AG_{DM,T} = Crop_T \times Slope_T + Intercept_T}
#'     \deqn{AGR_T = AG_{DM,T} \times Area_T}
#'     \deqn{BGR_T = (Crop_T + AG_{DM,T}) \times RS_T \times Area_T
#'       \times Frac_{renew,T}}
#'     \deqn{F_{CR} = AGR_T \times N_{AG,T} + BGR_T \times N_{BG,T}}
#'     Grasslands are assumed to be renewed every six years
#'     (\eqn{Frac_{renew} = 1/6}); annual crops have
#'     \eqn{Frac_{renew} = 1}.}
#'   \item{\eqn{F_{SOM}}}{N mineralised from soil organic matter following
#'     land use change (kg N yr\eqn{^{-1}}); currently set to 0 (TODO:
#'     integrate land use change module).}
#'   \item{\eqn{F_{OS}}}{Area of drained organic soils (ha); currently set
#'     to 0 (TODO: retrieve from FADN if available).}
#' }
#'
#' ## Direct N2O Emissions (IPCC Eq. 11.2)
#' \deqn{N_2ON_d = F_{SN} \times EF_{SN} + F_{ON} \times EF_{ON}
#'   + F_{CR} \times EF_{CR} + F_{SOM} \times EF_{SOM}
#'   + F_{OS} \times EF_{OS} + F_{PRP} \times EF_{PRP}}
#' \deqn{N_2O_d = N_2ON_d \times \frac{44}{28}}
#' \deqn{ghg_{N2O,d} = N_2O_d \times GWP_{N_2O}}
#' Country-specific emission factors (\eqn{EF_{SN}}, \eqn{EF_{ON}},
#' \eqn{EF_{CR}}, \eqn{EF_{SOM}}, \eqn{EF_{OS}}, \eqn{EF_{PRP}}) are
#' sourced from UNFCCC submissions.
#'
#' ## Indirect N2O from Volatilisation (IPCC Eq. 11.11)
#' \deqn{N_2ON_{ATD} = \bigl(F_{SN} \times Frac_{GASF} +
#'   (F_{ON} + F_{PRP}) \times Frac_{GASM}\bigr) \times EF_4}
#' \deqn{N_2O_{ATD} = N_2ON_{ATD} \times \frac{44}{28}}
#' \deqn{ghg_{N2O,ATD} = N_2O_{ATD} \times GWP_{N_2O}}
#' Default values: \eqn{Frac_{GASF} = 0.11}; \eqn{Frac_{GASM} = 0.21}
#' (IPCC Table 11.3). \eqn{EF_4} is sourced from UNFCCC submissions.
#'
#' ## Indirect N2O from Leaching and Runoff (IPCC Eq. 11.10)
#' \deqn{N_2ON_L = (F_{SN} + F_{ON} + F_{PRP} + F_{CR} + F_{SOM})
#'   \times Frac_{leach,H} \times EF_5}
#' \deqn{N_2O_L = N_2ON_L \times \frac{44}{28}}
#' \deqn{ghg_{N2O,L} = N_2O_L \times GWP_{N_2O}}
#' Default values: \eqn{Frac_{leach,H} = 0.24}; \eqn{EF_5} is sourced from
#' UNFCCC submissions (IPCC Table 11.3).
#'
#' @note
#' \itemize{
#'   \item Mineralisation from soil organic matter (\eqn{F_{SOM}}) and
#'     emissions from drained organic soils (\eqn{F_{OS}}) are currently set
#'     to zero, pending integration with a land use change module.
#'   \item The fraction of manure deposited on pasture (\eqn{F_{PRP}}) is
#'     currently approximated using organic fertilisation of grassland; a
#'     proper AWMS-based estimation is planned (TODO).
#'   \item Residue removal and burning are currently assumed to be zero
#'     (\eqn{Frac_{remove} = 0}; \eqn{Frac_{burn} = 0}).
#'   \item The \eqn{\frac{44}{28}} ratio converts kg N2O-N to kg N2O.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm observation ×
#' crop category, containing:
#' \describe{
#'   \item{...}{Traceability identifier columns
#'     (\code{object@traceability$id_cols}).}
#'   \item{FADN_code_letter}{\code{character}. FADN crop category code.}
#'   \item{species}{\code{character}. Crop or forage species.}
#'   \item{land_use_type}{\code{character}. Land use type
#'     (\code{"arable"} or \code{"grassland"}).}
#'   \item{N2O_d_kgCO2e}{\code{numeric}. Direct N2O emissions from managed
#'     soils (kg CO2-eq yr\eqn{^{-1}}), from IPCC Eq. 11.2.}
#'   \item{N2O_ATD_kgCO2e}{\code{numeric}. Indirect N2O emissions from
#'     atmospheric deposition of volatilised N (kg CO2-eq yr\eqn{^{-1}}),
#'     from IPCC Eq. 11.11.}
#'   \item{N2O_L_kgCO2e}{\code{numeric}. Indirect N2O emissions from N
#'     leaching and runoff (kg CO2-eq yr\eqn{^{-1}}), from IPCC Eq. 11.10.}
#' }
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories, Volume 4: Agriculture, Forestry and Other Land Use, Chapter 11:
#' N2O Emissions from Managed Soils, and CO2 Emissions from Lime and Urea
#' Application}. Eggleston H.S., Buendia L., Miwa K., Ngara T. and Tanabe K.
#' (eds). IGES, Japan.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land
#' Use, Chapter 11}. Calvo Buendia, E., Tanabe, K., Kranjc, A., Baasansuren,
#' J., Fukuda, M., Ngarize, S., Osako, A., Pyrozhenko, Y., Shermanau, P. and
#' Federici, S. (eds). IPCC, Switzerland.
#'
#' @seealso
#' \code{\link{f_n_ferti}}, \code{\link{f_n_excr}},
#' \code{\link{GHGE_n2o_manure}}, \code{\link{UNFCCC_data}},
#' \code{\link{data_extra}}, \code{\link{GWP}},
#' \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr select left_join filter mutate summarise across all_of
#'   starts_with if_else case_when join_by distinct where
#' @importFrom tidyr separate_longer_delim pivot_wider
#'
#' @concept footprint-ghge
#' @export
#'

GHGE_n2o_msoils <- function(object,
                            overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Retrieve activity data
  ## 2. Estimate direct N2O emissions from managed soils in accordance with IPCC recommendations (IPCC, 2019, 2006)
  ## 3. Estimate indirect N2O emissions from managed soils in accordance with IPCC recommendations (IPCC, 2019, 2006)
  ### Conversion to CO2 equivalents: data("GWP")


  # FADN data ----

  crop_data <- object@crop

  n_ferti_data = f_n_ferti(object, overwrite = overwrite)

  n_excr_from_livestock = f_n_excr(object, overwrite = overwrite)
  # TODO: estimate fraction of manure deposited on pasture

  # IPCC data ----

  IPCC_data <- data_extra$crops |>
    # create a matching variable between FADN code and IPCC data
    dplyr::select(FADN_code_letter,land_use_type,species,IPCC_default_values) |>
    dplyr::mutate(
      condition_species = dplyr::case_when(
        is.na(IPCC_default_values) & grepl("cereal|oilseeds|legumes",species)  ~ "Generic Grains;Grains",
        is.na(IPCC_default_values) ~ "Generic value for crops not indicated below",
        .default = IPCC_default_values
      )
    ) |>
    tidyr::separate_longer_delim(condition_species,";") |>
    # add IPCC data
    dplyr::left_join(
      data_extra$IPCC_default_values |>
        dplyr::filter(grepl("Table 11.1|Table 11.2",source_details)) |>
        dplyr::select(parameter,condition_species,value) |>
        tidyr::pivot_wider(
          id_cols = condition_species,
          names_from = parameter,
          values_from = value
        ),
      by = join_by(condition_species)
    ) |>
    dplyr::mutate(
      ## For annual crops Frac_Renew = 1 (IPCC-2019-vol.4;	Notes for EQUATION 11.6)
      ## For countries where pastures are renewed on average every X years, Frac_Renew = 1/X. (IPCC-2019-vol.4;	Notes for EQUATION 11.6)
      ## Here, we consider that grasslands are renewed only once every six years
      # TODO: check this assumption
      Frac_renew_T = ifelse(land_use_type == "grassland", 1/6, 1)
    ) |>
    # summarize for each FADN code
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~mean(.x, na.rm = TRUE)),
      .by = c(FADN_code_letter,land_use_type,species)) |>
    # replace NAs
    dplyr::mutate(
      ## In Table 11.1, R_AG_T = 1 as a generic value for crops not indicated. Thus, as we use slope and intercept instead of R_AG_T as suggested in Table 11.2, when no slope and intercept are available, Slope_T = 1 and Intercept_T = 0 (IPCC 2019, vol. 4)
      Slope_T = ifelse(is.na(Slope_T),1,Slope_T),
      Intercept_T = ifelse(is.na(Intercept_T),0,Intercept_T),
      ## In Table 11.1, a generic value is given for N_AG_T, N_BG_T, and RS_T (IPCC 2019, vol. 4)
      N_AG_T = ifelse(is.na(N_AG_T),0.008,N_AG_T),
      N_BG_T = ifelse(is.na(N_BG_T),0.009,N_BG_T),
      RS_T = ifelse(is.na(RS_T),0.22,RS_T)
    )

  # Activity data ----

  activity_data <- crop_data |>
    # add fertilization
    dplyr::left_join(
      n_ferti_data,
      by = c(object@traceability$id_cols, "FADN_code_letter", "area_ha")
    ) |>
    # add country ISO codes
    dplyr::left_join(
      data_extra$country_names |>
        dplyr::select(country_FADN,Country_ISO_3166_1_A3) |>
        dplyr::distinct(),
      by = dplyr::join_by(COUNTRY == country_FADN)
    ) |>
    # add EF from UNFCCC data
    dplyr::left_join(
      UNFCCC_data$EF_managed_soils,
      by = c('Country_ISO_3166_1_A3', 'YEAR')
    ) |>
    ## replace NAs by EF mean
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("EF"),
        .fns = ~ dplyr::if_else(is.na(.), mean(., na.rm = TRUE), .)
      )
    ) |>
    # add IPCC data
    dplyr::left_join(
      IPCC_data,
      by = c('FADN_code_letter', 'land_use_type', 'species')
    ) |>

    # estimate N amounts
    dplyr::mutate(

      ## F_SN = annual amount of synthetic fertilizer N applied to soils, kg N yr-1
      F_SN = N_ferti_min*area_ha,

      ### EQUATION 11.3
      ## F_ON = annual amount of animal manure, compost, sewage sludge and other organic N additions applied to soils (Note: If including sewage sludge, cross-check with Waste Sector to ensure there is no double counting of N2O emissions from the N in sewage sludge), kg N yr-1
      F_ON = dplyr::case_when(
        land_use_type == "arable" ~ N_ferti_org *area_ha,
        land_use_type == "grassland" ~ 0
      ),

      ### EQUATION 11.5
      ## F_PRP = annual amount of urine and dung N deposited on pasture, range, paddock by grazing animals, kg N yr-1
      ### TODO: should I use AWMS to estimate fraction of manure deposited on pasture?
      F_PRP = dplyr::case_when(
        land_use_type == "arable" ~ 0,
        land_use_type == "grassland" ~ N_ferti_org*area_ha
      ),

      ### EQUATION 11.6
      ## F_CR = annual amount of N in crop residues (above-ground and below-ground), including N-fixing crops, and from forage/pasture renewal, returned to soils, kg N yr-1

      ## AGR_T = annual total amount of above-ground crop residue for crop T, kg d.m. yr-1.
      ## N_AG_T = N content of above-ground residues for crop T, kg N (kg d.m.) -1 (Table 11.1a)
      ## Frac_remove_T = fraction of above-ground residues of crop T removed annually for purposes such as feed, bedding and construction, dimensionless. Survey of experts in country is required to obtain data. If data for Frac_Remove are not available, assume no removal
      ## Frac_burn_T = fraction of annual harvested area of crop T burnt, dimensionless
      ## C_f = combustion factor (dimensionless) (refer to Chapter 2, Table 2.6)
      ### /!\ We assume no removal nor burnt residues
      ## BGR_T = annual total amount of belowground crop residue for crop T, kg d.m. yr-1
      ## N_BG_T = N content of below-ground residues for crop T, kg N (kg d.m.)-1, (Table 11.1a)
      ## AG_DM_T = Above-ground residue dry matter for crop T, kg d.m. ha-1 (Use factors for R_AG_T in Table 11.1a, or alternatively, AG_DM_T may be estimated using the method and data in Table 11.2)
      ## Crop_T = harvested annual dry matter yield for crop T, kg d.m. ha-1
      Crop_T = yield * 10^3,
      ## R_AG_T = ratio of above-ground residue dry matter to harvested yield for crop T (Crop(T)), kg d.m. ha1 (kg d.m. ha-1)-1, (Table 11.1a)
      ## R_AG_T replace by Slope_T and Intercept_T to estimate AG_DM_T using the method and data in Table 11.2
      ## Area_T = total annual area harvested of crop T, ha yr-1
      Area_T = area_ha,
      ## Frac_renew = fraction of total area under crop T that is renewed annually, dimensionless. For countries where pastures are renewed on average every X years, Frac_Renew = 1/X. For annual crops Frac_Renew = 1
      ## RS_T = ratio+ of below-ground root biomass to above-ground shoot biomass for crop T, kg d.m. ha-1 (kg d.m. ha-1)-1, (Table 11.1a)
      ## T = crop or forage type

      AG_DM_T = Crop_T * Slope_T + Intercept_T,
      AGR_T = AG_DM_T * Area_T,
      BGR_T = (Crop_T + AG_DM_T) * RS_T * Area_T * Frac_renew_T,
      F_CR = (AGR_T * N_AG_T)+(BGR_T*N_BG_T),

      ### EQUATION 11.8
      ## F_SOM = annual amount of N in mineral soils that is mineralized, in association with loss of soil C from soil organic matter as a result of changes to land use or management, kg N yr-1
      ### TODO: how to estimate F_SOM? put it in LUC
      #### we retrieve country specific values from UNFCCC data: see EF_SOM
      F_SOM = 0,

      # EQUATION 11.1
      ## N2ON_OS = annual direct N2O–N emissions from managed organic soils, kg N2O–N yr-1
      ## F_OS = = annual area of managed/drained organic soils, ha
      ### TODO: check in FADN if we have such information
      F_OS = 0,
      ### EF2 = emission factor for N2O emissions from drained/managed organic soils, kg N2O–N ha-1 yr
      #### we retrieve country specific values from UNFCCC data: see EF_OS
      #N2ON_OS = F_OS * EF2
      ## N2ON_PRP = annual direct N2O–N emissions from urine and dung inputs to grazed soils, kg N2O–N yr-
      ## EF3_PRP = = emission factor for N2O emissions from urine and dung N deposited on pasture, range and paddock by grazing animals, kg N2O–N (kg N input)-1; (Table 11.1) (Note: the subscripts CPP and SO refer to Cattle, Poultry and Pigs, and Sheep and Other animals, respectively)
      #### we retrieve country specific values from UNFCCC data: see EF_PRP
      #N2ON_PRP = F_PRP * EF3_PRP
    )


  # DIRECT N2O EMISSIONS FROM MANAGED SOILS ----

  N2O_d_MS <- activity_data |>
    dplyr::mutate(
      # EQUATION 11.2

      ## N2ON_d = annual direct N2O–N emissions produced from managed soils, kg N2O–N yr-1

      ## F_SN = annual amount of synthetic fertilizer N applied to soils, kg N yr-1
      ## F_ON = annual amount of animal manure, compost, sewage sludge and other organic N additions applied to soils (Note: If including sewage sludge, cross-check with Waste Sector to ensure there is no double counting of N2O emissions from the N in sewage sludge), kg N yr-1
      ## EF_1_i = emission factors developed for N2O emissions from synthetic fertilizer and organic N application under conditions i (kg N2O–N (kg N input)-1); i = 1, ...n
      ## EF_1 = emission factor for N2O emissions from N inputs, kg N2O–N (kg N input)-1 (Table 11.1)
      ### we retrieve country specific values from UNFCCC data: see EF_SN and EF_ON
      ### TODO: check EF1_i values for conditions i => what are these conditions? wet and dry climates as in Table 11.2 (for a definition of climates see footnote 9 p.11.12)?
      ## F_CR = annual amount of N in crop residues (above-ground and below-ground), including N-fixing crops, and from forage/pasture renewal, returned to soils, kg N yr-1
      ### we retrieve country specific values from UNFCCC data: see EF_CR
      ## N2O-N_PRP = annual direct N2O–N emissions from urine and dung inputs to grazed soils, kg N2O–N yr-1
      ### we retrieve country specific values from UNFCCC data: see EF_PRP

      # we neglect emissions from mineral and organic soils
      # TODO: check this assumption
      ## F_SOM = annual amount of N in mineral soils that is mineralized, in association with loss of soil C from soil organic matter as a result of changes to land use or management, kg N yr-1
      ## N2O-N_OS = annual direct N2O–N emissions from managed organic soils, kg N2O–N yr-1

      ## Equation 11.2 may be modified in a variety of ways to accommodate any combination of N source-, crop type-, management-, land use-, climate-, soil- or other condition-specific emission factors that a country may be able to obtain for each of the individual N input variables (FSN, FON, FCR, FSOM, FOS, FPRP).
      ### We modified Equation 11.2 to account for country-specific emission factors retrieved from UNFCCC data
      #N2ON_d = (F_SN + F_ON)*EF1_i + (F_CR+F_SOM)*EF1 + N2ON_OS + N2ON_PRP
      N2ON_d = F_SN*EF_SN + F_ON*EF_ON + F_CR*EF_CR + F_SOM*EF_SOM + F_OS*EF_OS + F_PRP*EF_PRP,

      # Conversion of N2O–N emissions to N2O emissions for reporting purposes is performed by using the following equation:  N2O = N2O–N * 44/28
      N2O_d = N2ON_d * (44/28),

      # Conversion of nitrous oxide (N20) to CO2 equivalents
      N2O_d_kgCO2e = N2O_d * GWP[["N20"]]
    )

  # INDIRECT N2O EMISSIONS FROM MANAGED SOILS ----

  N2O_id_MS <- activity_data |>
    dplyr::mutate(

      ## Volatilization ----

      ## EQUATION 11.11 N2O FROM ATMOSPHERIC DEPOSITION OF N VOLATILISED FROM MANAGED SOILS (TIER 2)
      ### N20N_ATD = annual amount of N2O–N produced from atmospheric deposition of N volatilized from managed soils, kg N2O–N yr-1
      ### F_SN_i =  annual amount of synthetic fertilizer N applied to soils under different conditions i, kg N yr-1
      ### Frac_GASF_i = fraction of synthetic fertilizer N that volatilizes as NH3 and NOx under different conditions i, kg N volatilized (kg of N applied)-1
      Frac_GASF = 0.11,
      ### F_ON = annual amount of managed animal manure, compost, sewage sludge and other organic N additions applied to soils, kg N yr-1
      ### F_PRP = annual amount of urine and dung N deposited by grazing animals on pasture, range and paddock, kg N yr-1
      ### Frac_GASM = fraction of applied organic N fertilizer materials (F_ON) and of urine and dung N deposited by grazing animals (F_PRP) that volatilizes as NH3 and NOx, kg N volatilized (kg of N applied or deposited)-1 (Table 11.3)
      Frac_GASM = 0.21,
      ### EF4 = emission factor for N2O emissions from atmospheric deposition of N on soils and water surfaces, [kg N–N2O (kg NH3–N + NOx–N volatilized)-1] (Table 11.3)
      N2ON_ATD = ((F_SN * Frac_GASF) + ((F_ON + F_PRP) * Frac_GASM))*EF4,


      # Conversion of N2O–N emissions to N2O emissions for reporting purposes is performed by using the following equation:  N2O = N2O–N * 44/28
      N2O_ATD = N2ON_ATD * (44/28),

      # Conversion of nitrous oxide (N20) to CO2 equivalents
      N2O_ATD_kgCO2e = N2O_ATD * GWP[["N20"]],

      ## Leaching - run-off ----

      ## EQUATION 11.10
      ### N2ON_L = annual amount of N2O–N produced from leaching and runoff of N additions tomanaged soils in regions where leaching/runoff occurs, kg N2O–N yr-1
      ### Frac_leach_H = fraction of all N added to/mineralised in managed soils in regions where leaching/runoff occurs that is lost through leaching and runoff, kg N (kg of N additions)-1 (Table 11.3)
      Frac_leach_H = 0.24,
      ### EF5 = emission factor for N2O emissions from N leaching and runoff, kg N2O–N (kg N leached and runoff)-1 (Table 11.3)
      N2ON_L = (F_SN + F_ON + F_PRP + F_CR + F_SOM) * Frac_leach_H * EF5,

      # Conversion of N2O–N emissions to N2O emissions for reporting purposes is performed by using the following equation:  N2O = N2O–N * 44/28
      N2O_L = N2ON_L * (44/28),

      # Conversion of nitrous oxide (N20) to CO2 equivalents
      N2O_L_kgCO2e = N2O_L * GWP[["N20"]]


    )

  # Output ----

  N2O_MS <- Reduce(x = list(N2O_d_MS,
                            N2O_id_MS),
                   f = function(x,y) dplyr::left_join(x,y,
                                                      by = c(object@traceability$id_cols,
                                                             "FADN_code_letter","species","land_use_type"))|>
                     dplyr::select(dplyr::all_of(object@traceability$id_cols),
                                   FADN_code_letter,species,land_use_type,
                                   #N2O_d,N2O_ATD,N2O_L,
                                   N2O_d_kgCO2e,N2O_ATD_kgCO2e,N2O_L_kgCO2e
                     ))

  return(N2O_MS)

}







