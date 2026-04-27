#' Calculate CH4 Emissions from Manure Management
#'
#' @description
#' Estimates methane (CH4) emissions from manure management for all livestock
#' categories using IPCC Tier 2/3 methodology (IPCC 2006, 2019 Refinements),
#' anchored on country-specific parameters reported to the UNFCCC.
#' The function follows a five-step approach:
#' \enumerate{
#'   \item Retrieval of the observed herd structure from the \code{FADN2Footprint}
#'     object, enriched with IPCC and UNFCCC category codes and country ISO codes.
#'   \item Estimation of livestock feed intake (both on-farm produced and purchased
#'     feed), including dry matter (DM), gross energy (GE), crude protein (CP),
#'     and ash content.
#'   \item Collection of country-specific volatile solid (VS) excretion rates and
#'     maximum methane producing capacity (Bo) from UNFCCC national submissions
#'     (Table 3.B(a)s1).
#'   \item Derivation of effective methane conversion factors (MCF) weighted by
#'     animal waste management system fractions (AWMS) per country and UNFCCC
#'     livestock category (Table 3.B(a)s2).
#'   \item Calculation of CH4 emission factors (IPCC Equation 10.23) and total
#'     emissions, converted to CO2 equivalents using the GWP of CH4.
#' }
#'
#' @details
#' ## Volatile Solid Excretion (Equation 10.24)
#' Volatile solid excretion per animal per day is estimated as:
#' \deqn{VS = \left(GE \times \left(1 - \frac{DE}{100}\right) + UE \times GE\right)
#'   \times \frac{1 - ASH}{18.45}}
#' where:
#' \itemize{
#'   \item \eqn{GE} = gross energy intake (MJ head\eqn{^{-1}} day\eqn{^{-1}})
#'   \item \eqn{DE} = digestibility of feed (\%), sourced from UNFCCC national
#'     submissions (Table 3.A s2), weighted by livestock population size; fallback
#'     to the cross-country weighted mean when country-specific values are missing
#'   \item \eqn{UE} = urinary energy fraction of GE (default = 0.04 for ruminants;
#'     reduced to 0.02 for animals fed ≥ 85\% grain or for swine)
#'   \item \eqn{ASH} = ash content of feed as a fraction of dry matter intake,
#'     estimated from feed composition data
#'   \item 18.45 = conversion factor for dietary GE per kg of dry matter
#'     (MJ kg\eqn{^{-1}})
#' }
#'
#' ## CH4 Emission Factor from Manure Management (Equation 10.23)
#' \deqn{EF_T = VS_T \times 365 \times B_{o,T} \times 0.67
#'   \times \sum_{S,k} MCF_{S,k} \times AWMS_{T,S,k}}
#' where:
#' \itemize{
#'   \item \eqn{EF_T} = annual CH4 emission factor for livestock category \eqn{T}
#'     (kg CH4 head\eqn{^{-1}} yr\eqn{^{-1}})
#'   \item \eqn{VS_T} = daily volatile solid excretion for livestock category
#'     \eqn{T} (kg VS head\eqn{^{-1}} day\eqn{^{-1}})
#'   \item \eqn{B_{o,T}} = maximum methane producing capacity for livestock
#'     category \eqn{T} (m\eqn{^3} CH4 kg\eqn{^{-1}} VS), weighted by population
#'     size across UNFCCC animal subcategories
#'   \item 0.67 = conversion factor from m\eqn{^3} CH4 to kg CH4
#'   \item \eqn{MCF_{S,k}} = methane conversion factor for manure management
#'     system \eqn{S} in climate region \eqn{k} (\%)
#'   \item \eqn{AWMS_{T,S,k}} = fraction of livestock category \eqn{T}'s manure
#'     handled using management system \eqn{S} in climate region \eqn{k}
#'     (dimensionless)
#' }
#' The term \eqn{\sum_{S,k} MCF_{S,k} \times AWMS_{T,S,k}} is pre-computed from
#' UNFCCC Table 3.B(a)s2 as a population-weighted mean across animal subcategories
#' and climate regions within each country.
#'
#' ## Total Emissions
#' \deqn{CH4_{MM} = EF_T \times N_T}
#' where \eqn{N_T} is the observed number of animals of category \eqn{T}.
#'
#' ## CO2 Equivalents Conversion
#' All CH4 emissions are converted to CO2 equivalents using the GWP coefficient
#' stored in \code{\link{GWP}}.
#'
#' @note
#' \itemize{
#'   \item Country-specific Bo, VS, MCF, AWMS, and DE values are sourced from
#'     UNFCCC national inventory submissions compiled in
#'     \code{\link{UNFCCC_data}}.
#'   \item Climate region allocation currently follows the country-level assignment
#'     in UNFCCC national submissions. Multi-climate-region countries are handled
#'     by population-weighted averaging. Adaptation for overseas territories or
#'     sub-national climate regions is not yet implemented (TODO).
#'   \item Digestibility (DE) values fall back to a cross-country population-weighted
#'     mean when country-specific UNFCCC submissions are unavailable for a given
#'     UNFCCC livestock category.
#'   \item Quality checks comparing estimated VS, Bo, sum_MCF_AWMS, and EF values
#'     against UNFCCC reported values are included as internal comments for
#'     validation purposes.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A \code{\link[tibble]{tibble}} containing one row per farm-livestock
#' category combination, with the following columns (among others):
#' \describe{
#'   \item{...}{Traceability identifier columns as defined in
#'     \code{object@traceability$id_cols} (e.g., farm ID, year, NUTS2 region).}
#'   \item{FADN_code_letter}{\code{character}. FADN livestock category code.}
#'   \item{species}{\code{character}. Livestock species.}
#'   \item{IPCC_mix_cat}{\code{character}. IPCC mixed livestock category.}
#'   \item{UNFCCC_cat}{\code{character}. UNFCCC livestock category used to match
#'     national inventory parameters.}
#'   \item{Qobs}{\code{numeric}. Observed number of animals (heads).}
#'   \item{GE}{\code{numeric}. Gross energy intake (MJ head\eqn{^{-1}}
#'     day\eqn{^{-1}}).}
#'   \item{VS}{\code{numeric}. Volatile solid excretion rate (kg VS head\eqn{^{-1}}
#'     day\eqn{^{-1}}).}
#'   \item{Bo}{\code{numeric}. Maximum methane producing capacity
#'     (m\eqn{^3} CH4 kg\eqn{^{-1}} VS), population-weighted country average.}
#'   \item{sum_MCF_AWMS}{\code{numeric}. Population-weighted sum of
#'     \eqn{MCF \times AWMS} products across manure management systems and
#'     climate regions (dimensionless).}
#'   \item{DE}{\code{numeric}. Feed digestibility (\%), from country-specific
#'     UNFCCC submissions or cross-country fallback.}
#'   \item{EF}{\code{numeric}. Annual CH4 emission factor
#'     (kg CH4 head\eqn{^{-1}} yr\eqn{^{-1}}).}
#'   \item{CH4_MM}{\code{numeric}. Total CH4 emissions from manure management
#'     (kg CH4 yr\eqn{^{-1}}).}
#'   \item{CH4_MM_kgCO2e_livcat}{\code{numeric}. Total CH4 emissions from manure
#'     management expressed in kg CO2 equivalents per year.}
#' }
#'
#' @references
#' IPCC (2006). \emph{2006 IPCC Guidelines for National Greenhouse Gas Inventories,
#' Volume 4: Agriculture, Forestry and Other Land Use, Chapter 10: Emissions from
#' Livestock and Manure Management}. Eggleston H.S., Buendia L., Miwa K., Ngara T.
#' and Tanabe K. (eds). IGES, Japan.
#'
#' IPCC (2019). \emph{2019 Refinement to the 2006 IPCC Guidelines for National
#' Greenhouse Gas Inventories, Volume 4: Agriculture, Forestry and Other Land Use,
#' Chapter 10: Emissions from Livestock and Manure Management}.
#' Calvo Buendia, E., Tanabe, K., Kranjc, A., Baasansuren, J., Fukuda, M.,
#' Ngarize, S., Osako, A., Pyrozhenko, Y., Shermanau, P. and Federici, S. (eds).
#' IPCC, Switzerland.
#'
#' UNFCCC National Inventory Submissions.
#' \url{https://unfccc.int/ghg-inventories-annex-i-parties/2023}
#'
#' @seealso
#' \code{\link{GHGE_ch4_enteric}}, \code{\link{f_feed_onfarm}},
#' \code{\link{f_feed_offfarm}}, \code{\link{GWP}},
#' \code{\link{UNFCCC_data}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr left_join filter select rename inner_join mutate group_by
#'   summarise across all_of bind_rows case_when join_by distinct summarise
#'
#' @importFrom stringr str_detect
#'
#' @concept footprint-ghge
#' @export


GHGE_ch4_manure <- function(object,
                            overwrite = F,
                            ...){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }

  ## Steps:
  ## 1. Retrieve observed herd structure
  ## 2. Estimate livestock intake of feed (both produced and purchased)
  ## 3. Calculate the total CH4 emissions from manure management in accordance with IPCC recommendations (IPCC, 2019, 2006)
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

  # 2. Estimate livestock intake of feed (both produced and purchased) ---------------------------------------------------------------------------------

  # feed
  feed_produced = f_feed_onfarm(object, overwrite = overwrite)

  feed_purchased = f_feed_offfarm(object, overwrite = overwrite)

  # overall herd feed
  herd_feed <- dplyr::bind_rows(
    feed_produced |>
      mutate(feed_origin = "feed_produced"),
    feed_purchased |>
      mutate(feed_origin = "feed_purchased")
  ) |>
    # add number of animals
    dplyr::left_join(
      herd_data |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,species,Qobs),
      by = c(object@traceability$id_cols, "FADN_code_letter")
    )

  # feed intake per animal

  feed_intake <- herd_feed |>
    # estimate feed intake per animal
    dplyr::group_by(dplyr::across(dplyr::all_of(object@traceability$id_cols)),FADN_code_letter) |>
    dplyr::summarise(
      DM_t_animal = sum(DM_t_livcat/Qobs),
      GE_MJ_animal = sum(GE_MJ_livcat/Qobs),
      # add crude protein content
      CP_p100 = (sum(CP_t_livcat/Qobs) / DM_t_animal)*100,
      Ash_p100 = (sum(Ash_t_livcat/Qobs) / DM_t_animal)*100,
      .groups = "drop")

  # Number of climate regions by country ----

  # TODO: adapt for overseas territories and by climate regions

  # which country have more than one climate region?
  UNFCCC_data$table3Bas2 |>
    dplyr::filter(AWMS >0) |>
    dplyr::summarise(
      nb_climate_reg = length(unique(climate_region)),
      .by = c(Country_ISO_3166_1_A3)) |>
    dplyr::filter(nb_climate_reg >1)
  # for now, I use the climate region allocation in UNFCCC_MMS_Bo


  # IPCC 2019 Ref to 2006 Guidlines vol4 p10.127 ???


  # 3. Calculate the total CH4 emissions from manure management ---------------------------------------------------------------------------------

  ## TIER 3
  ### Step 1: Collect population data based on the Livestock Population Characterization (see Section 10.2).
  ### Step 2: Identify default (Table 10A.5) [or collect country-specific [...] typical animal mass (TAM) values.
  #### Calculate volatile solid excretion according to Equation 10.22a or develop country- specific volatile solid excretion rates according to Equation 10.24.
  ### Step 3: Collect country-specific information on manure management system methods and develop country-specific manure management system fractions or use default manure storage fractions presented in Annex Tables 10A.6 to Tables 10A.9.
  ### Step 4: Identify either default emission factors Table 10.14 or build country-specific emission factors for each livestock subcategory based on climate zones and manure management system fractions. • Tier 1: Identify default values (Table 10.14) for emission factors for each livestock category in terms of grams of methane per kg VS per year for the appropriate climate zone and productivity class if using advanced Tier 1a. • Tier 2: Select local manure management specific methane conversion factors (MCF’s, Table 10.17) for different climate zones and the animal categories specific maximum methane producing capacity (B0).
  ### Step 5: Calculate methane emission for each livestock subcategory. According to Equation 10.23, for each livestock category and climate zone calculate the country-specific emission factor based on the country-specific or default quantity of volatile solids (Step 2 ), the manure management system fraction (AWMS) and the MCF and B0 factors ( Step 4); To estimate total emissions, the country specific emission factor is then multiplied by the population number (Step 1).

  # Animal waste management system (manure management systems) data have been collected for regions and countries by the FAO and average manure fractions treated by different management systems are presented in Annex 10A.2, Tables 10A.6 to 10A.9.

  tmp_CH4_MM <- herd_data |>
    # filter cattle
    #dplyr::filter(species == "cattle") |>

    # add feed intake data
    dplyr::inner_join(
      feed_intake,
      by = c(object@traceability$id_cols,"FADN_code_letter")) |>

    # add UNFCCC country specific values for:
    ## Bo = maximum methane producing capacity for manure produced by livestock category T, m3 CH4 kg-1 of VS excreted
    ## MCF = methane conversion factors for each manure management system S by climate region k, percent
    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
    ## DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)

    # VS and Bo
    dplyr::left_join(
      UNFCCC_data$table3Bas1 |>
        #dplyr::filter(species == "cattle") |>
        dplyr::summarise(
          VS = weighted.mean(`VS(2)_daily_excretion_(average)`,Population__size),
          Bo = weighted.mean(`CH4_producing_potential_(Bo)(2)_(average)`,Population__size),
          .by = c(UNFCCC_cat,Country_ISO_3166_1_A3)),
      by = dplyr::join_by(UNFCCC_cat, Country_ISO_3166_1_A3)
    ) |>

    # sum of MCF weighted by AWMS
    ## MCF = methane conversion factors for each manure management system S by climate region k, percent
    ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless
    dplyr::left_join(
      UNFCCC_data$table3Bas2 |>
        #dplyr::filter(species == "cattle") |>
        dplyr::summarise(
          Population__size = unique(Population__size),
          sum_MCF_AWMS = sum(MCF/100*AWMS/100,na.rm = T),
          .by = c(Animal_cat,UNFCCC_cat,Country_ISO_3166_1_A3)) |>
        dplyr::summarise(
          sum_MCF_AWMS = weighted.mean(sum_MCF_AWMS,Population__size),
          .by = c(UNFCCC_cat,Country_ISO_3166_1_A3)),
      by = dplyr::join_by(UNFCCC_cat, Country_ISO_3166_1_A3)) |>

    # Digestibility
    ## DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)
    ### DE per country
    dplyr::left_join(
      UNFCCC_data$table3As2 |>
        # add population size
        dplyr::left_join(
          UNFCCC_data$table3Bas1 |>
            dplyr::select(Country_ISO_3166_1_A3,Animal_cat,UNFCCC_cat,species,Population__size),
          by = join_by(Animal_cat, Country_ISO_3166_1_A3, species, UNFCCC_cat)) |>
        dplyr::filter(!is.na(`Digestibility of feed`) & !is.na(Population__size)) |>
        # summarize DE
        dplyr::summarise(DE = weighted.mean(`Digestibility of feed`,Population__size),
                         .by = c(Country_ISO_3166_1_A3,UNFCCC_cat)),
      by = join_by(UNFCCC_cat, Country_ISO_3166_1_A3)
    ) |>
    ### missing DE
    dplyr::left_join(
      UNFCCC_data$table3As2 |>
        # add population size
        dplyr::left_join(
          UNFCCC_data$table3Bas1 |>
            dplyr::select(Country_ISO_3166_1_A3,Animal_cat,UNFCCC_cat,species,Population__size),
          by = join_by(Animal_cat, Country_ISO_3166_1_A3, species, UNFCCC_cat)) |>
        dplyr::filter(!is.na(`Digestibility of feed`) & !is.na(Population__size)) |>
        # summarize DE
        dplyr::summarise(missing_DE = weighted.mean(`Digestibility of feed`,Population__size),
                         .by = c(UNFCCC_cat)),
      by = join_by(UNFCCC_cat)
    ) |>
    dplyr::mutate(
      DE = ifelse(is.na(DE),missing_DE,DE)
    ) |>

    # estimate
    dplyr::mutate(

      ## Digestibility
      ## DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)
      ## Tables 10A.1 & 10A.2
      #DE = dplyr::case_when(
      #str_detect(IPCC_mix_cat,"cows_milk_prod") ~ 71,
      #str_detect(IPCC_mix_cat,"bulls_breed") ~ 60,
      #str_detect(IPCC_mix_cat,"other_mature_cattle") ~ 60, # as mature males
      #str_detect(IPCC_mix_cat,"growing_cattle_postweaning") ~ 65,
      #str_detect(IPCC_mix_cat,"growing_cattle") ~ 65,
      #str_detect(IPCC_mix_cat,"calves_preweaning") ~ (95+73)/2 # average of calves on milk and calves on forage
      #),
      # WIP we could estimate DE = qté aliment - prod lait

      # EQUATION 10.24 VOLATILE SOLID EXCRETION RATES
      ## VS = volatile solid excretion per day on a dry-organic matter basis, kg VS day-1
      ## GE = gross energy intake, MJ day-1
      GE = GE_MJ_animal /365,
      ## DE% = digestibility of the feed in percent (e.g. 60%)
      ## (UE •GE) = urinary energy expressed as fraction of GE. Typically 0.04GE can be considered urinary energy excretion by most ruminants (reduce to 0.02 for ruminants fed with 85 percent or more grain in the diet or for swine). Use country-specific values where available.
      UE_GE = 0.04,
      ## ASH = the ash content of feed calculated as a fraction of the dry matter feed intake (e.g., 0.06 for sows: Dämmgen et al. 2011). Use country-specific values where available.
      ASH = Ash_p100/100,
      ## 18.45 = conversion factor for dietary GE per kg of dry matter (MJ kg-1). This value is relatively constant across a wide range of forage and grain-based feeds commonly consumed by livestock.
      VS = (GE*(1-(DE/100))+(UE_GE))*((1-ASH)/18.45),

      # EQUATION 10.23 CH4 EMISSION FACTOR FROM MANURE MANAGEMENT
      ## EF = annual CH4 emission factor for livestock category T, kg CH4 animal-1 yr-1
      ## VS = daily volatile solid excreted for livestock category T, kg dry matter animal-1 day-1
      ## 365 = basis for calculating annual VS production, days yr-1
      ## Bo = maximum methane producing capacity for manure produced by livestock category T, m3 CH4 kg-1 of VS excreted
      ## 0.67 = conversion factor of m3 CH4 to kilograms CH4
      ## MCF = methane conversion factors for each manure management system S by climate region k, percent
      ## AWMS = fraction of livestock category T's manure handled using animal waste management system S in climate region k, dimensionless

      EF = (VS*365)*(Bo* 0.67 * sum_MCF_AWMS),

      # Total CH4 emission from manure management
      ## To estimate total emissions, the country specific emission factor is then multiplied by the population number (Step 1).
      ## CH4 = CH4 emissions from Manure Management in the country, kg CH4 yr-1
      # N = number of head of livestock species/category T in the country, for productivity system P, when applicable
      CH4_MM = EF * Qobs,

      ### Conversion of CH4 to CO2 equivalents
      CH4_MM_kgCO2e_livcat = CH4_MM * GWP[["CH4"]]

    )





  # quality check => WIP pb avec sum_MCF_AWMS
  ## VS
  ## TABLE 10.13A (NEW) DEFAULT VALUES FOR VOLATILE SOLID EXCRETION RATE (KG VS (1000 KG ANIMAL MASS)-1 DAY-1)
  ### Dairy cattle in Western EU = 8.4 / Eastern EU = 6.7
  # quantile(tmp_cattle_CH4_MM$VS[tmp_cattle_CH4_MM$country_UNFCCC == 'FRA' & tmp_cattle_CH4_MM$FADN_code_letter == 'LCOWDAIR'])
  ## Bo
  ## TABLE 10.16A (UPDATED) DEFAULT VALUES FOR MAXIMUM METHANE PRODUCING CAPACITY (B0) (M3 CH4 KG-1 VS)
  ### Dairy cattle 0.24
  # quantile(tmp_cattle_CH4_MM$Bo[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])

  ## UNFCCC sum_MCF_AWMS
  ## DNM_2023_2018_14042023_135925 <- read_excel("data_raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
  ## weigthed sum 0.1299
  # unique(tmp_cattle_CH4_MM$sum_MCF_AWMS[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])
  ## UNFCCC EF
  ## DNM_2023_2018_14042023_135925 <- read_excel("data_raw/DNM_2023_2018_14042023_135925.xlsx", sheet = "Table3.B(a)s1")
  ### EF Dairy cattle = 56.66 (kg CH4/head/yr)
  # quantile(tmp_cattle_CH4_MM$EF[tmp_cattle_CH4_MM$COUNTRY == 'DAN' & tmp_cattle_CH4_MM$IPCC_mix_cat == 'cows_milk_prod'])





  # Output ----

  CH4_MM <- tmp_CH4_MM |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,
                  species,Qobs,
                  CH4_MM_kgCO2e_livcat)

  return(CH4_MM)

}


