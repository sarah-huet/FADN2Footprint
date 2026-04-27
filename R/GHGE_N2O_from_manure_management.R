#' Calculate N2O Emissions from Manure Management
#'
#' @description
#' Estimates direct and indirect nitrous oxide (N2O) emissions from manure
#' management for all livestock categories using IPCC Tier 2/3 methodology
#' (IPCC 2006, 2019 Refinements), anchored on country-specific parameters
#' reported to the UNFCCC. The function follows a three-step approach:
#' \enumerate{
#'   \item Retrieval of the observed herd structure from the
#'     \code{FADN2Footprint} object, enriched with IPCC and UNFCCC category
#'     codes and country ISO codes.
#'   \item Estimation of livestock nitrogen excretion per head and per
#'     livestock category via \code{\link{f_n_excr}}.
#'   \item Calculation of three N2O emission pathways from manure management,
#'     converted to CO2-equivalents using \code{\link{GWP}}.
#' }
#'
#' @details
#' ## Nitrogen Excretion
#' Total nitrogen excretion (\eqn{N_{ex}}, kg N yr\eqn{^{-1}}) per livestock
#' category is estimated by \code{\link{f_n_excr}} and joined across all
#' livestock species. When species-specific outputs differ in column structure,
#' only common columns are retained before row-binding.
#'
#' ## Manure Management System (MMS) Fractions
#' Country- and category-specific MMS fractions (\eqn{AWMS_{T,S}}) are derived
#' from UNFCCC submission data (\code{\link{UNFCCC_data}}\code{$table3Bas2}).
#' Population-weighted MMS fractions are computed per UNFCCC livestock category
#' and country:
#' \deqn{AWMS_{T,S} = \frac{N_T \times AWMS_{T,S}^{raw}}
#'   {\sum_S N_T \times AWMS_{T,S}^{raw}}}
#'
#' ## Emission Pathways
#'
#' ### 1. Direct N2O Emissions (IPCC Eq. 10.25)
#' \deqn{N_2O_{D,MM} = \sum_{T,S} N_{ex,T} \times AWMS_{T,S} \times EF3_S
#'   \times \frac{44}{28}}
#' where \eqn{EF3_S} is the direct N2O emission factor for MMS \eqn{S}
#' (kg N2O-N kg\eqn{^{-1}} N), sourced from UNFCCC submissions
#' (\code{\link{UNFCCC_data}}\code{$table3Bb}), with IPCC default values
#' (\code{\link{data_extra}}\code{$IPCC_default_values}, parameter
#' \code{"EF3"}) used as fallback when country-specific values are absent.
#'
#' ### 2. Indirect N2O from Volatilisation (IPCC Eqs. 10.26 & 10.28)
#' Nitrogen lost through volatilisation of NH3 and NOx:
#' \deqn{N_{vol,MMS} = \sum_{T,S} N_{ex,T} \times AWMS_{T,S}
#'   \times Frac_{gasMS,T,S}}
#' Resulting indirect N2O emissions:
#' \deqn{N_2O_{G,mm} = N_{vol,MMS} \times EF_4 \times \frac{44}{28}}
#' where \eqn{Frac_{gasMS,T,S}} (IPCC Table 10.22) and \eqn{EF_4}
#' (IPCC Chapter 11, Table 11.3) are taken from IPCC default values.
#' Species mapping for \eqn{Frac_{gasMS}} follows:
#' \code{LCOWDAIR} → dairy cattle; other cattle; swine; poultry;
#' all others → other animals.
#'
#' ### 3. Indirect N2O from Leaching (IPCC Eqs. 10.27 & 10.29)
#' Nitrogen lost through leaching and runoff:
#' \deqn{N_{leach,MMS} = \sum_{T,S} N_{ex,T} \times AWMS_{T,S}
#'   \times Frac_{leachMS,T,S}}
#' Resulting indirect N2O emissions:
#' \deqn{N_2O_{L,mm} = N_{leach,MMS} \times EF_5 \times \frac{44}{28}}
#' where \eqn{Frac_{leachMS,T,S}} (IPCC Table 10.22) and \eqn{EF_5}
#' (IPCC Chapter 11, Table 11.3) are taken from IPCC default values.
#'
#' ## Conversion to CO2-equivalents
#' All N2O fluxes are converted to kg CO2-eq using the global warming potential
#' for N2O stored in \code{\link{GWP}}\code{[["N20"]]}.
#'
#' @note
#' \itemize{
#'   \item UNFCCC emission factor data (\code{table3Bb}) do not currently
#'     include a year dimension; a TODO is in place to account for inter-annual
#'     variability in future versions.
#'   \item A quality-check routine comparing estimated parameters (VS, Bo,
#'     weighted MCF × AWMS, and EF) against UNFCCC-reported values is work in
#'     progress (\code{WIP}).
#'   \item The \eqn{\frac{44}{28}} ratio converts kg N2O-N to kg N2O.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm observation ×
#' livestock category, containing:
#' \describe{
#'   \item{...}{Traceability identifier columns
#'     (\code{object@traceability$id_cols}).}
#'   \item{FADN_code_letter}{\code{character}. FADN livestock category code.}
#'   \item{species}{\code{character}. Livestock species
#'     (e.g., \code{"cattle"}, \code{"swine"}, \code{"poultry"}).}
#'   \item{Qobs}{\code{numeric}. Observed number of livestock heads
#'     (annual average).}
#'   \item{N2O_D_MM_kgCO2e}{\code{numeric}. Direct N2O emissions from manure
#'     management (kg CO2-eq yr\eqn{^{-1}}), from IPCC Eq. 10.25.}
#'   \item{N2O_G_mm_kgCO2e}{\code{numeric}. Indirect N2O emissions from
#'     atmospheric deposition of volatilised NH3 and NOx (kg CO2-eq
#'     yr\eqn{^{-1}}), from IPCC Eqs. 10.26 & 10.28.}
#'   \item{N2O_L_mm_kgCO2e}{\code{numeric}. Indirect N2O emissions from
#'     nitrogen leaching and runoff (kg CO2-eq yr\eqn{^{-1}}), from IPCC
#'     Eqs. 10.27 & 10.29.}
#' }
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas
#' Inventories, Volume 4: Agriculture, Forestry and Other Land Use, Chapter 10:
#' Emissions from Livestock and Manure Management}.
#' Eggleston H.S., Buendia L., Miwa K., Ngara T. and Tanabe K. (eds).
#' IGES, Japan.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land
#' Use, Chapter 10: Emissions from Livestock and Manure Management}.
#' Calvo Buendia, E., Tanabe, K., Kranjc, A., Baasansuren, J., Fukuda, M.,
#' Ngarize, S., Osako, A., Pyrozhenko, Y., Shermanau, P. and Federici, S.
#' (eds). IPCC, Switzerland.
#'
#' @seealso
#' \code{\link{f_n_excr}}, \code{\link{GHGE_ch4_manure}},
#' \code{\link{GHGE_n2o_msoils}}, \code{\link{UNFCCC_data}},
#' \code{\link{GWP}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr select left_join filter mutate summarise group_by across
#'   all_of case_when join_by distinct summarise
#' @importFrom tidyr separate_longer_delim
#'
#' @concept footprint-ghge
#' @export


GHGE_n2o_manure <- function(object,
                            overwrite = F,
                            ...){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }


  ## Steps:
  ## 1. Retrieve observed herd structure
  ## 2. Estimate livestock N excretion
  ## 3. Calculate the total N2O emissions from manure management in accordance with IPCC recommendations (IPCC, 2019, 2006)
  ### Conversion to CO2 equivalents: data("GWP")

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_data <- object@herd |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,species,Qobs) |>
    # add IPCC and UNFCCC categories
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,IPCC_mix_cat,UNFCCC_cat),
      by = join_by(FADN_code_letter)
    ) |>
    # add country ISO codes
    dplyr::left_join(
      data_extra$country_names |>
        dplyr::select(country_FADN,Country_ISO_3166_1_A3) |>
        dplyr::distinct(),
      by = dplyr::join_by(COUNTRY == country_FADN)
    )

  # 2. Estimate livestock N excretion ---------------------------------------------------------------------------------

  N_excr = f_n_excr(object, overwrite = overwrite)

  common_cols <- Reduce(intersect, lapply(N_excr, names))

  N_excr_tbl <- do.call(
    rbind,
    lapply(names(N_excr), function(sp) {
      cbind(
        species = sp,
        N_excr[[sp]][ , common_cols, drop = FALSE]
      )
    })
  )

  # 3. Calculate the total N2O emissions from manure management ---------------------------------------------------------------------------------

  ## Direct N2O emissions ----

  # TIER 3
  ## Step 1: Collect population data from the Livestock Population Characterisation
  ## Step 2: Use default values or develop the annual average nitrogen excretion rate per head (Nex(T,P)) for each defined livestock species/category T, and productivity system P, when applicable
  ## Step 3: Use default values or determine the fraction of total annual nitrogen excretion for each livestock species/category T that is managed in each manure management system S (AWMS(T,S,P))
  ## Step 4: Use default values or develop N 2O emission factors for each manure management system S (EF3(S))
  ## Step 5: For each manure management system type S, multiply its emission factor (EF 3(S)) by the total amount of nitrogen managed (from all livestock species/categories) in that system, to estimate N 2O emissions from that manure management system. Then sum over all manure management systems.

  tmp_N2O_MM_d <- herd_data |>
    # filter cattle
    #filter(species == "cattle") |>

    # add livestock N excretion
    dplyr::left_join(
      N_excr_tbl |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,Nex_kgN_anim_y),
      by = c(object@traceability$id_cols, "FADN_code_letter")) |>

    # add UNFCCC country specific values for:

    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
    dplyr::left_join(
      UNFCCC_data$table3Bas2 |>
        dplyr::filter(!is.na(UNFCCC_cat) & AWMS >0) |>
        # population per MMS
        dplyr::mutate(
          pop_MMS = Population__size*AWMS/100
        ) |>
        # proportion by country and matching category
        dplyr::mutate(
          AWMS = pop_MMS/sum(pop_MMS,na.rm = T),
          .by = c(UNFCCC_cat,Country_ISO_3166_1_A3)
        ) |>
        dplyr::summarise(AWMS = sum(AWMS, na.rm = T),
                         .by = c(Country_ISO_3166_1_A3,UNFCCC_cat,MMS)),
      by = join_by(UNFCCC_cat, Country_ISO_3166_1_A3),
      relationship = "many-to-many") |>

    ## EF3
    ### EF3_S = emission factor for direct N2O emissions from manure management system S in the country, kg N2O-N/kg N in manure management system S
    dplyr::left_join(
      UNFCCC_data$table3Bb |>
        dplyr::filter(!is.na(EF3)) |>
        # TODO: change when accounting for year in UNFCCC data
        dplyr::select(-YEAR),
      by = join_by(MMS, Country_ISO_3166_1_A3),
      relationship = "many-to-many"
    ) |>
    ### add IPCC default EF for country-specific missing values
    dplyr::left_join(
      data_extra$IPCC_default_values |>
        dplyr::filter(parameter == "EF3") |>
        dplyr::select(condition_tech,condition_species,value) |>
        dplyr::rename(MMS = condition_tech,
                      species = condition_species,
                      default_EF3 = value) |>
        tidyr::separate_longer_delim(species,";"),
      by = join_by(species, MMS),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      EF3 = ifelse(is.na(EF3),default_EF3,EF3)
    ) |>

    # Estimate direct N2O emission from MM per livestock category
    dplyr::group_by(dplyr::across(dplyr::all_of(object@traceability$id_cols)),species,FADN_code_letter,Qobs) |>
    dplyr::summarise(
      # EQUATION 10.25 (UPDATED) DIRECT N2O EMISSIONS FROM MANURE MANAGEMENT
      ## N2O_D_mm = direct N2O emissions from Manure Management in the country, kg N2O yr-1
      ## N_T = number of head of livestock species/category T in the country
      ## Nex_T = annual average N excretion per head of species/category T in the country, in kg N animal-1 yr-1
      ### here, we already have total N excretion per livestock category
      ## AWMS_{(T,S)} = fraction of total annual nitrogen excretion for each livestock species/category T that is managed in manure management system S in the country, dimensionless
      N2O_D_MM = sum(
        Nex_kgN_anim_y*Qobs*AWMS*EF3*(44/28),
        na.rm = T),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ### Conversion of N20 to CO2 equivalents
      N2O_D_MM_kgCO2e_livcat = N2O_D_MM * GWP[["N20"]]
    )


  ## Indirect N2O from manure management - Volatilization ----

  tmp_N2O_MM_gas <- herd_data |>
    # filter cattle
    #filter(species == "cattle") |>

    # add livestock N excretion
    dplyr::left_join(
      N_excr_tbl |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,Nex_kgN_anim_y),
      by = c(object@traceability$id_cols, "FADN_code_letter")) |>

    # add UNFCCC country specific values for:

    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
    dplyr::left_join(
      UNFCCC_data$table3Bas2 |>
        dplyr::filter(!is.na(UNFCCC_cat) & AWMS >0) |>
        # population per MMS
        dplyr::mutate(
          pop_MMS = Population__size*AWMS/100
        ) |>
        # proportion by country and matching category
        dplyr::mutate(
          AWMS = pop_MMS/sum(pop_MMS,na.rm = T),
          .by = c(UNFCCC_cat,Country_ISO_3166_1_A3)
        ) |>
        dplyr::summarise(AWMS = sum(AWMS, na.rm = T),
                         .by = c(Country_ISO_3166_1_A3,UNFCCC_cat,MMS)),
      by = join_by(UNFCCC_cat, Country_ISO_3166_1_A3),
      relationship = "many-to-many") |>

    ## add IPCC default values for country-specific missing parameters:
    ## Frac_{gasMS(T,S)} = fraction of managed manure nitrogen for livestock category T that volatilises as NH3 and NOx in the manure management system S (from Table 10.22)
    ## EF_4 = = emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces, kg N2O-N (kg NH3-N + NOx-N volatilised)-1 ; given in Chapter 11, Table 11.3

    ## Frac_{gasMS(T,S)} = fraction of managed manure nitrogen for livestock category T that volatilises as NH3 and NOx in the manure management system S (from Table 10.22)
    dplyr::mutate(
      condition_species = dplyr::case_when(
        FADN_code_letter == "LCOWDAIR" ~ "dairy_cattle",
        species == "cattle" ~ "other_cattle",
        species %in% c("swine","poultry") ~ species,
        .default = "other_animals"
      )
    ) |>
    dplyr::left_join(
      data_extra$IPCC_default_values |>
        dplyr::filter(parameter == "frac_Gas_MS") |>
        dplyr::select(condition_tech,condition_species,value) |>
        dplyr::summarise(frac_Gas_MS = mean(value, na.rm = T),
                         .by = c(condition_tech,condition_species)
        ) |>
        dplyr::rename(MMS = condition_tech),
      by = join_by(condition_species, MMS)
    ) |>

    ## EF_4 = = emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces, kg N2O-N (kg NH3-N + NOx-N volatilised)-1 ; given in Chapter 11, Table 11.3
    dplyr::mutate(
      EF4 = data_extra$IPCC_default_values$value[data_extra$IPCC_default_values$parameter == "EF4"]
    ) |>

    # Estimate direct N2O emission from MM per livestock category
    dplyr::group_by(dplyr::across(dplyr::all_of(object@traceability$id_cols)),species,FADN_code_letter,Qobs,EF4) |>
    dplyr::summarise(
      # EQUATION 10.26 (UPDATED) N LOSSES DUE TO VOLATILISATION FROM MANURE MANAGEMENT
      ## N_{volatilization-MMS} = amount of manure nitrogen that is lost due to volatilisation of NH3 and NOx, kg N yr-1
      ## Frac_{gasMS(T,S)} = fraction of managed manure nitrogen for livestock category T that volatilises as NH3 and NOx in the manure management system S (from Table 10.22)
      N_vol_mms = sum(
        Nex_kgN_anim_y*Qobs*AWMS*frac_Gas_MS,
        na.rm = T),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # EQUATION 10.28 INDIRECT N2O EMISSIONS DUE TO VOLATILISATION OF N FROM MANURE MANAGEMENT
      ## N_2O_{G(mm)} = indirect N2O emissions due to volatilization of N from Manure Management in the country, kg N2O yr-1
      ## EF_4 = = emission factor for N2O emissions from atmospheric deposition of nitrogen on soils and water surfaces, kg N2O-N (kg NH3-N + NOx-N volatilised)-1 ; given in Chapter 11, Table 11.3
      N2O_G_mm = (N_vol_mms * EF4) *(44/28),

      ### Conversion of N20 to CO2 equivalents
      N2O_G_mm_kgCO2e_livcat = N2O_G_mm * GWP[["N20"]]
    )

  ## Indirect N2O from manure management - Leaching ----


  tmp_N2O_MM_leach <- herd_data |>
    # filter cattle
    #filter(species == "cattle") |>

    # add livestock N excretion
    dplyr::left_join(
      N_excr_tbl |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,Nex_kgN_anim_y),
      by = c(object@traceability$id_cols, "FADN_code_letter")) |>

    # add UNFCCC country specific values for:

    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
    dplyr::left_join(
      UNFCCC_data$table3Bas2 |>
        dplyr::filter(!is.na(UNFCCC_cat) & AWMS >0) |>
        # population per MMS
        dplyr::mutate(
          pop_MMS = Population__size*AWMS/100
        ) |>
        # proportion by country and matching category
        dplyr::mutate(
          AWMS = pop_MMS/sum(pop_MMS,na.rm = T),
          .by = c(UNFCCC_cat,Country_ISO_3166_1_A3)) |>
        dplyr::summarise(AWMS = sum(AWMS, na.rm = T),
                         .by = c(Country_ISO_3166_1_A3,UNFCCC_cat,MMS)),
      by = join_by(UNFCCC_cat, Country_ISO_3166_1_A3),
      relationship = "many-to-many") |>

    ## add IPCC default values for country-specific missing parameters:
    ## Frac_{LeachMS(T,S)} = fraction of managed manure nitrogen for livestock category T that is leached from the manure management system S (from Table 10.22)
    ## EF_5 = emission factor for N2O emissions from nitrogen leaching and runoff, kg N 2O-N/kg N leached and runoff, given in Chapter 11, Table 11.3

    ## Frac_{LeachMS(T,S)} = fraction of managed manure nitrogen for livestock category T that is leached from the manure management system S (from Table 10.22)
    dplyr::mutate(
      condition_species = dplyr::case_when(
        FADN_code_letter == "LCOWDAIR" ~ "dairy_cattle",
        species == "cattle" ~ "other_cattle",
        species %in% c("swine","poultry") ~ species,
        .default = "other_animals"
      )
    ) |>
    dplyr::left_join(
      data_extra$IPCC_default_values |>
        dplyr::filter(parameter == "frac_leach_MS") |>
        dplyr::select(condition_tech,condition_species,value) |>
        dplyr::summarise(frac_leach_MS = mean(value, na.rm = T),
                         .by = c(condition_tech,condition_species)) |>
        dplyr::rename(MMS = condition_tech),
      by = join_by(condition_species, MMS)
    ) |>

    ## EF_5 = emission factor for N2O emissions from nitrogen leaching and runoff, kg N 2O-N/kg N leached and runoff, given in Chapter 11, Table 11.3
    dplyr::mutate(
      EF5 = data_extra$IPCC_default_values$value[data_extra$IPCC_default_values$parameter == "EF5"]
    ) |>

    # Estimate direct N2O emission from MM per livestock category
    dplyr::group_by(dplyr::across(dplyr::all_of(object@traceability$id_cols)),species,FADN_code_letter,Qobs,EF5) |>
    dplyr::summarise(

      # EQUATION 10.27 (UPDATED) N LOSSES DUE TO LEACHING FROM MANURE MANAGEMENT
      ## N_{leaching-MMS} = amount of manure nitrogen that is lost due to leaching, kg N yr-1
      ## Frac_{LeachMS(T,S)} = fraction of managed manure nitrogen for livestock category T that is leached from the manure management system S (from Table 10.22)
      N_leach_mms = sum(
        Nex_kgN_anim_y*Qobs*AWMS*frac_leach_MS,
        na.rm = T),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # EQUATION 10.29 INDIRECT N2O EMISSIONS DUE TO LEACHING FROM MANURE MANAGEMENT
      ## N_2O_{L(mm)} = indirect N2O emissions due to leaching and runoff from Manure Management in the country, kg N2O yr-1
      ## EF_5 = emission factor for N2O emissions from nitrogen leaching and runoff, kg N 2O-N/kg N leached and runoff, given in Chapter 11, Table 11.3
      N2O_L_mm = (N_leach_mms * EF5) *(44/28),

      ### Conversion of N20 to CO2 equivalents
      N2O_L_mm_kgCO2e_livcat = N2O_L_mm * GWP[["N20"]]

    )

  # quality check => WIP pb avec sum_MCF_AWMS
  ## VS
  ## TABLE 10.13A (NEW) DEFAULT VALUES FOR VOLATILE SOLID EXCRETION RATE (KG VS (1000 KG ANIMAL MASS)-1 DAY-1)
  ### Dairy cattle in Western EU = 8.4 / Eastern EU = 6.7
  #quantile(tmp_cattle_CH4_MM$VS[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])
  ## Bo
  ## TABLE 10.16A (UPDATED) DEFAULT VALUES FOR MAXIMUM METHANE PRODUCING CAPACITY (B0) (M3 CH4 KG-1 VS)
  ### Dairy cattle 0.24
  #quantile(tmp_cattle_CH4_MM$Bo[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])

  ## UNFCCC sum_MCF_AWMS
  ## DNM_2023_2018_14042023_135925 <- read_excel("data-raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
  ## weigthed sum 0.1299
  #unique(tmp_cattle_CH4_MM$sum_MCF_AWMS[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])
  ## UNFCCC EF
  ## DNM_2023_2018_14042023_135925 <- read_excel("data-raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
  ### EF Dairy cattle = 56.66 (kg CH4/head/yr)
  #quantile(tmp_cattle_CH4_MM$EF[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])

  # Output ----

  N2O_MM <- Reduce(x = list(tmp_N2O_MM_d,
                            tmp_N2O_MM_gas,
                            tmp_N2O_MM_leach),
                   f = function(x,y) dplyr::left_join(x,y,
                                                      by = c(object@traceability$id_cols,
                                                             "FADN_code_letter","species","Qobs"))) |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,
                  species,Qobs,
                  N2O_D_MM_kgCO2e_livcat,N2O_G_mm_kgCO2e_livcat,N2O_L_mm_kgCO2e_livcat)

  return(N2O_MM)

  #rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])

}
