#' Calculate CH4 Emissions from Enteric Fermentation
#'
#' @description
#' Estimates methane (CH4) emissions from enteric fermentation for cattle, sheep,
#' and goats using IPCC Tier 2 methodology (IPCC 2006, 2019 Refinements).
#' The function follows a three-step approach:
#' \enumerate{
#'   \item Retrieval of the observed herd structure from the \code{FADN2Footprint} object.
#'   \item Estimation of livestock feed intake (both on-farm produced and purchased feed).
#'   \item Calculation of total CH4 emissions from enteric fermentation, converted to
#'   CO2 equivalents using the global warming potential (GWP) of CH4.
#' }
#'
#' @details
#' ## Cattle (Tier 2)
#' For cattle, the methane conversion factor is estimated using two complementary
#' IPCC methods:
#' \itemize{
#'   \item **Ym method** (methane conversion factor, \% of gross energy converted to CH4):
#'     Emission factors are derived from IPCC Table 10.12 and differentiated by
#'     livestock category (dairy cows by milk productivity level, other cattle by
#'     digestibility of feed). The emission factor is computed as:
#'     \deqn{EF_{Ym} = \frac{GE \times (Y_m / 100)}{55.65}}
#'     where \eqn{GE} is the gross energy intake (MJ head\eqn{^{-1}} yr\eqn{^{-1}}),
#'     \eqn{Y_m} is the methane conversion factor (\%), and 55.65 MJ kg\eqn{^{-1}} CH4
#'     is the energy content of methane (IPCC Equation 10.21).
#'   \item **MY method** (methane yield, g CH4 kg DMI\eqn{^{-1}}):
#'     Emission factors are derived from IPCC Table 10.12, similarly differentiated
#'     by livestock category. The emission factor is computed as:
#'     \deqn{EF_{MY} = DMI \times \frac{MY}{1000}}
#'     where \eqn{DMI} is the dry matter intake (kg head\eqn{^{-1}} yr\eqn{^{-1}})
#'     and \eqn{MY} is the methane yield (g CH4 kg DMI\eqn{^{-1}}) (IPCC Equation 10.21A).
#' }
#' Total cattle CH4 emissions are estimated as the average of the two methods:
#' \deqn{CH4_{enteric} = Q_{obs} \times \frac{EF_{Ym} + EF_{MY}}{2}}
#'
#' ## Sheep (Tier 2)
#' For sheep, a fixed methane conversion factor \eqn{Y_m = 6.7\%} (IPCC Table 10.13)
#' is used with the Ym method (IPCC Equation 10.21):
#' \deqn{EF_{Ym} = \frac{GE \times (Y_m / 100)}{55.65}}
#' \deqn{CH4_{enteric} = Q_{obs} \times EF_{Ym}}
#'
#' ## Goats (Tier 2)
#' For goats, a fixed methane conversion factor \eqn{Y_m = 5.5\%} (IPCC Table 10.13)
#' is used with the Ym method (IPCC Equation 10.21):
#' \deqn{EF_{Ym} = \frac{GE \times (Y_m / 100)}{55.65}}
#' \deqn{CH4_{enteric} = Q_{obs} \times EF_{Ym}}
#'
#' ## CO2 equivalents conversion
#' All CH4 emissions are converted to CO2 equivalents using the GWP coefficient
#' stored in \code{\link{GWP}}.
#'
#' @note
#' \itemize{
#'   \item Digestibility values (DE\%) for cattle are sourced from IPCC Tables 10A.1
#'   and 10A.2.
#'   \item Dairy cow milk productivity thresholds used to assign Ym and MY values
#'   follow IPCC Table 10.12 guidelines. Differentiation by Neutral Detergent Fibre
#'   (NDF, \% DMI) for high-producing dairy cows is not yet implemented (TODO).
#'   \item Pigs and poultry are currently not included in this function, as their
#'   enteric fermentation emissions are negligible under IPCC guidelines.
#' }
#'
#' @param object An object of class \code{\link{FADN2Footprint}}.
#' @param overwrite Logical (default FALSE). If FALSE and cached results exist,
#'   the function returns
#'   the cached object and no recomputation is performed. If TRUE, existing
#'   cached GHGE results are ignored and computations are re-run.
#'
#' @return A \code{\link[tibble]{tibble}} containing one row per farm-livestock
#' category combination, with the following columns:
#' \describe{
#'   \item{...}{Traceability identifier columns as defined in
#'     \code{object@traceability$id_cols} (e.g., farm ID, year, NUTS2 region).}
#'   \item{FADN_code_letter}{\code{character}. FADN livestock category code.}
#'   \item{species}{\code{character}. Livestock species (\code{"cattle"},
#'     \code{"sheep"}, or \code{"goats"}).}
#'   \item{Qobs}{\code{numeric}. Observed number of animals (heads).}
#'   \item{CH4_enteric_kgCO2e_livcat}{\code{numeric}. Total CH4 emissions from enteric
#'     fermentation expressed in kg CO2 equivalents per year.}
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
#' @seealso
#' \code{\link{f_feed_onfarm}}, \code{\link{f_feed_offfarm}},
#' \code{\link{GWP}}, \code{\link{FADN2Footprint-class}}
#'
#' @importFrom dplyr left_join filter select rename inner_join mutate group_by
#'   summarise across all_of bind_rows case_when
#' @importFrom stringr str_detect
#'
#' @concept footprint-ghge
#' @export




GHGE_ch4_enteric <- function(object,
                             overwrite = F,
                             ...){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }


  ## Steps:
  ## 1. Retrieve observed herd structure
  ## 2. Estimate livestock intake of feed (both produced and purchased)
  ## 3. Calculate the total CH4 emissions from enteric fermentation in accordance with IPCC recommendations (IPCC, 2019, 2006)
  ### Conversion to CO2 equivalents: data("GWP")

  # 1. Retrieve observed herd structure ---------------------------------------------------------------------------------

  herd_data <- object@herd |>
    # add IPCC categories
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,IPCC_mix_cat),
      by = join_by(FADN_code_letter)
    ) |>
    # add milk productivity for dairy cows
    dplyr::left_join(
      object@output$other_herd_products |>
        dplyr::filter(FADN_code_letter == "LCOWDAIR") |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,yield) |>
        dplyr::rename(milk_t_anim = yield),
      by = c(object@traceability$id_cols,"FADN_code_letter")
    )

  # 2. Estimate livestock intake of feed (both produced and purchased) ---------------------------------------------------------------------------------

  feed_intake <- object@practices$herding$feed$feed_intake$total

  # 3. Calculate the total CH4 emissions from enteric fermentation in accordance with IPCC recommendations (IPCC, 2019, 2006)  ---------------------------------------------------------------------------------

  activity_data <- herd_data |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,species,IPCC_mix_cat,
                  Qobs,milk_t_anim) |>
    # add feed intake data
    dplyr::inner_join(
      feed_intake |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),FADN_code_letter,
                      GE_MJ_anim,DM_t_anim,CP_p100),
      by = c(object@traceability$id_cols, "FADN_code_letter"))
  ## CATTLE ----

  ### Tier 2
  #### Step 1: Obtaining methane conversion factor (two methods: Ym and MY, we used the average estimation from both methods)
  #### Step 2: Emission factor development
  #### Step 3: Estimate total emissions

  cattle_CH4_EF <- activity_data |>
    # filter cattle
    dplyr::filter(species == "cattle") |>
    # estimate CH4 emission from enteric fermentation
    dplyr::mutate(

      ## Digestibility

      ## DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)
      ## Tables 10A.1 & 10A.2
      DE = dplyr::case_when(
        #        str_detect(output,"milk") ~ "milk",
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") ~ 71,
        stringr::str_detect(IPCC_mix_cat,"bulls_breed") ~ 60,
        stringr::str_detect(IPCC_mix_cat,"other_mature_cattle") ~ 60, # as mature males
        stringr::str_detect(IPCC_mix_cat,"growing_cattle_postweaning") ~ 65,
        stringr::str_detect(IPCC_mix_cat,"growing_cattle") ~ 65,
        stringr::str_detect(IPCC_mix_cat,"calves_preweaning") ~ (95+73)/2, # average of calves on milk and calves on forage
      ),
      # WIP !!! we could estimate DE = qté aliment - prod lait

      #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.12)
      ## In Table 10.12, the MY of dairy cows is linked to annual milk production levels and to feed quantity and quality
      ## diary cow productivity in kg milk /head/yr-1
      Ym = dplyr::case_when(
        # diary cattle
        ## TODO: differentiate between high producing cows depending on Neutral Detergent Fibre (NDF, % DMI)
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim > 8.500 ~ (5.7+6)/2,
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim <= 8.500 & milk_t_anim >= 5.000 ~ 6.3,
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim < 5.000 ~ 6.5,

        # other cattle
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE <= 62 ~ 7.0,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE > 62 & DE <= 71 ~ 6.3,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE >= 72 & DE < 75 ~ 4.0,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE >= 75 ~ 3.0
      ),

      # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
      ## EF = (GE *(Ym / 100)*365)/ 55.65
      ### EF = emission factor, kg CH4 head-1 yr-1
      ### GE = gross energy intake, MJ head-1 day-1
      ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
      ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
      ### here GE_MJ_anim= gross energy intake, MJ head-1 yr-1
      EF_Ym = (GE_MJ_anim * (Ym/100)) / 55.65,

      #  MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
      ## In Table 10.12, the MY of dairy cows is linked to annual milk production levels and to feed quantity and quality
      ## diary cow productivity in kg milk /head/yr-1
      MY = dplyr::case_when(
        # diary cattle
        ## TODO: differentiate between high producing cows depending on Neutral Detergent Fibre (NDF, % DMI)
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim > 8.500 ~ (19+20)/2,
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim <= 8.500 & milk_t_anim >= 5.000 ~ 21,
        stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & milk_t_anim < 5.000 ~ 21.4,

        # other cattle
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE <= 62 ~ 23.3,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE > 62 & DE <= 71 ~ 21,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE >= 72 & DE < 75 ~ 13.6,
        !stringr::str_detect(IPCC_mix_cat,"cows_milk_prod") & DE >= 75 ~ 10
      ),

      # EQUATION 10.21A (NEW) METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
      ### EF = emission factor, kg CH4 head-1 yr-1
      ### DMI = kg DMI day-1
      ### MY = Methane yield, g CH4 kg DMI-1 (Table 10.12)
      #### MY (g CH4 kg DMI-1) = MY (kg CH4 t DMI-1)
      EF_MY = DM_t_anim * MY,

      # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above[...], the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
      ## CH4_enteric_kgCH4 = kg CH4 yr-1
      CH4_EF_Ym = Qobs * EF_Ym,
      CH4_EF_MY = Qobs * EF_MY,

      CH4_enteric_kgCH4 = (CH4_EF_Ym+CH4_EF_MY)/2,

      ### Conversion to CO2 equivalents
      CH4_enteric_kgCO2e_livcat = CH4_enteric_kgCH4 * GWP[["CH4"]]
    )

  ## SHEEP ----

  ### Tier 2
  #### Step 1: Obtaining methane conversion factor (one methods: Ym)
  #### Step 2: Emission factor developpement
  #### Step 3: Estimate total emissions

  sheep_CH4_EF <-  activity_data |>
    # filter sheep
    dplyr::filter(species == "sheep") |>
    # estimate CH4 emission from enteric fermentation
    dplyr::mutate(

      #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.13)
      Ym = 6.7,

      # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
      ## EF = (GE *(Ym / 100)*365)/ 55.65
      ### EF = emission factor, kg CH4 head-1 yr-1
      ### GE = gross energy intake, MJ head-1 day-1
      ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
      ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
      ### here GE_MJ_anim= gross energy intake, MJ head-1 yr-1
      EF_Ym = (GE_MJ_anim* (Ym/100)) / 55.65,


      # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above under Tier 1, the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
      ## CH4_enteric_kgCH4 = kg CH4 yr-1
      CH4_enteric_kgCH4 = Qobs * EF_Ym,

      ### Conversion of CH4 to CO2 equivalents
      CH4_enteric_kgCO2e_livcat = CH4_enteric_kgCH4 * GWP[["CH4"]]

    )

  ## GOATS ----

  ### Tier 2
  #### Step 1: Obtaining methane conversion factor (one methods: Ym)
  #### Step 2: Emission factor developpement
  #### Step 3: Estimate total emissions

  goats_CH4_EF <-  activity_data |>
    # filter goats
    dplyr::filter(species == "goats") |>
    # estimate CH4 emission from enteric fermentation
    dplyr::mutate(

      #  Ym = methane conversion factor, per cent of gross energy in feed converted to methane (Table 10.13)
      Ym = 5.5,

      # EQUATION 10.21 METHANE EMISSION FACTORS FOR ENTERIC FERMENTATION FROM A LIVESTOCK CATEGORY
      ## EF = (GE *(Ym / 100)*365)/ 55.65
      ### EF = emission factor, kg CH4 head-1 yr-1
      ### GE = gross energy intake, MJ head-1 day-1
      ### Ym = methane conversion factor, per cent of gross energy in feed converted to methane
      ### The factor 55.65 (MJ/kg CH4) is the energy content of methane
      ### here GE_MJ_anim= gross energy intake, MJ head-1 yr-1
      EF_Ym = (GE_MJ_anim* (Ym/100)) / 55.65,


      # "To estimate total emissions, the selected emission factors are multiplied by the associated animal population [...]. As described above under Tier 1, the emissions estimates should be reported in gigagrams (Gg)." (IPCC Guidelines 2019 Refinements)
      ## CH4_enteric_kgCH4 = kg CH4 yr-1
      CH4_enteric_kgCH4 = Qobs * EF_Ym,

      ### Conversion of CH4 to CO2 equivalents
      CH4_enteric_kgCO2e_livcat = CH4_enteric_kgCH4 * GWP[["CH4"]]

    )

  # Output ----

  CH4_enteric <- Reduce(x = list(cattle_CH4_EF,
                                 sheep_CH4_EF,
                                 goats_CH4_EF),
                        f = function(x,y) dplyr::bind_rows(x,y)
  ) |>
    dplyr::select(dplyr::all_of(object@traceability$id_cols),
                  FADN_code_letter,species,Qobs,
                  CH4_enteric_kgCO2e_livcat)

  return(CH4_enteric)

}


