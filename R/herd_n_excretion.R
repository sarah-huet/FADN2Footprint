#' Estimate livestock nitrogen excretion by species and herd category
#'
#' @description
#' f_n_excr estimates annual nitrogen (N) excretion per animal and per herd
#' category for farms contained in a FADN2Footprint object. The function
#' combines estimated feed intake (produced + purchased) with species- and
#' category-specific physiological equations following IPCC guidance
#' (IPCC 2006/2019/2023) to compute N intake, N retention and N excretion
#' (kg N animal⁻¹ yr⁻¹ and kg N livestock category⁻¹ yr⁻¹). Results are returned for
#' the main livestock groups (cattle, swine, poultry, sheep, goats).
#'
#' @details
#' Workflow and main choices:
#' - Input validation: the function requires an S4 object of class
#'   "FADN2Footprint".
#' - Caching: if `object@practices$herding$N_excretion` exists and
#'   overwrite = FALSE, the cached value is returned.
#' - Feed intake: herd feed intake is obtained via f_herd_feed(object). The
#'   function uses the aggregated per-animal dry matter intake (DM_t_anim),
#'   GE, CP and other nutritional indicators from that helper.
#' - Grazing share: for cattle the fraction of grazed feed (Herbe_paturee) is
#'   estimated from the detailed feed_intake table and used to adjust activity
#'   energy (NE_a) in IPCC equations.
#' - Two approaches for estimating N intake for cattle are implemented:
#'   (1) direct estimation from observed DMI and crop CP (Nin_DMI), and
#'   (2) calculation from IPCC energy-based equations (Nin_IPCC). In the
#'   current implementation the DMI-based pathway (Nin_DMI) is used to derive
#'   Nex_kgN_anim_y (see code TODOs and comments for alternatives).
#' - For swine, poultry, sheep and goats, N intake is computed from DMI and CP
#'   (Equation 10.32 / 10.32A style) and an assumed retention fraction (Table
#'   10.20 style) is applied to estimate excretion (Tier 2 approach).
#' - The cattle block implements IPCC-style calculations for:
#'   - Net energy for maintenance (NE_m), activity (NE_a), growth (NE_g),
#'     lactation (NE_l), pregnancy (NE_p), digestibility (DE), REM/REG and
#'     GE_IPCC_MJ_anim_day; CP and weights are assigned by IPCC mix categories; milk
#'     protein and retention are computed following IPCC equations.
#'
#' Important methodological notes:
#' - The function relies heavily on outputs from f_herd_feed and the package
#'   data table data_extra$livestock (to map FADN categories to IPCC mix
#'   categories). Ensure those helpers/data are available and appropriate for
#'   the country/region under study.
#' - Several fixed choices and defaults are coded (e.g. average live weights,
#'   retention fractions, default milk fat), matching IPCC tables used for
#'   "Western Europe". Review and adapt these to national parameters where
#'   possible (for higher tier accuracy).
#' - The implementation contains TODO notes and simple choices (e.g. using the
#'   DMI-based Nex by default). Users aiming for strict Tier 2/3 accounting
#'   should inspect and, if needed, modify the selection/averaging logic.
#'
#' @param object An S4 object of class "FADN2Footprint". The object must
#'   contain the usual package slots used by feed/herd helpers (e.g.
#'   `object@traceability$id_cols`, `object@herd`, `object@output` and `data_extra`
#'   accessible in the package environment).
#' @param overwrite Logical (default FALSE). If FALSE and
#'   `object@practices$herding$N_excretion` exists the cached value is returned.
#'   If TRUE the computation is rerun and the result should be stored by the
#'   caller if caching is desired.
#'
#' @return A named list with elements for each major livestock group:
#' - cattle: tibble with per‑farm / per‑FADN category variables including
#'   GE_DMI_MJ_anim_day, Nin_DMI_kgN_anim_day, Nin_IPCC_kgN_anim_day,
#'   N_retention (daily), Nex_kgN_anim_y (annual per animal), Nex_kgN_livcat_y
#'   (annual per herd category), and intermediate IPCC variables (NE_m, NE_g,
#'   DE, REM, REG, GE_IPCC_MJ_anim_day, CP, etc.);
#' - swine, poultry, sheep, goats: tibbles with DMI-based Nin_DMI and annual
#'   Nex_kgN_anim_y and Nex_kgN_livcat_y using the retention fractions or
#'   formulas indicated in the code.
#'
#' All returned tables keep the traceability id columns (`object@traceability$id_cols`)
#' and FADN_code_letter. Units: Nex_kgN_anim_y and Nex_kgN_livcat_y are in kg N
#' per animal per year and kg N per livestock category per year, respectively.
#'
#' @examples
#' \dontrun{
#' # f is a prepared FADN2Footprint object
#' N_excretion <- f_n_excr(f)
#' # Inspect cattle excretion rates
#' head(N_excretion$cattle)
#' # Force recomputation
#' N_excretion2 <- f_n_excr(f, overwrite = TRUE)
#' }
#'
#' @references
#' - IPCC Guidelines for National Greenhouse Gas Inventories (relevant chapters
#'   on livestock; 2006 / 2019 / 2023 as used in the code).
#' - Jayet et al. (2023) — AROPAJ model reference used to derive DMI requirements.
#' - Sailley et al. (2021) — national species feed volumes (see
#'   FADN2Footprint::data_extra$Sailley_2021_feed_flows).
#'
#' @seealso f_herd_feed, f_feed_onfarm, f_feed_offfarm, data_extra$livestock
#'
#' @concept practice-herding
#' @export
#' @importFrom dplyr filter mutate left_join case_when select distinct across summarise group_by pull
#' @importFrom stringr str_detect
#' @importFrom rlang .data


f_n_excr <- function(object,
                     overwrite = FALSE){
  if (!inherits(object, "FADN2Footprint")) {
    stop("Input must be a valid 'FADN2Footprint' object.")
  }


  if (!is.null(object@practices$herding$N_excretion)&& !overwrite) {
    message("Using cached values stored in object@practices$herding$N_excretion.")
    return(object@practices$herding$N_excretion)  # use cached value
  }

  ## Steps:
  ## 1. Estimate livestock intake of feed (both produced and purchased)
  ## 2. Calculate the amount of nitrogen excreted by livestock in accordance with IPCC recommendations (IPCC, 2019, 2006)

  # 1. Estimate livestock feed intake ---------------------------------------------------------------------------------

  herd_feed <- f_herd_feed(object, overwrite = overwrite)
  #object <- remove_farms(object,herd_feed$farms_to_remove)
  #herd_feed <- f_herd_feed(object)

  feed_intake <- herd_feed$feed_intake$total |>
    # convert DM t to kg
    mutate(
      DM_kg_anim = DM_t_anim*10^3
    )

  # Estimate share of grazing in the livestock feed
  tmp_grazing_share <- herd_feed$feed_intake$detail |>
    # sum total GE per animal
    mutate(total_GE_MJ = sum(GE_MJ_anim,na.rm = T),
           .by = c(object@traceability$id_cols,'FADN_code_letter')) |>
    # sum total grazed GE per animal
    filter(
      feed_origin == "feed_produced" &
        Sailley_feed == "Herbe_paturee") |>
    mutate(
      grazed_GE_MJ = sum(GE_MJ_anim,na.rm = T),
      .by = c(object@traceability$id_cols,'FADN_code_letter', 'feed_origin', 'Sailley_feed')) |>
    # share of GE per feed type and per animal
    mutate(GE_grazed = grazed_GE_MJ / total_GE_MJ) |>
    # select
    dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter, GE_grazed) |>
    distinct()

  ### estimate milk production for diary cows
  milk_prod <- object@output$other_herd_products |>
    ### select milk data
    filter(output == "milk" & species == "cattle") |>
    dplyr::select(all_of(object@traceability$id_cols),prod_t) |>
    rename(MILK_total = prod_t) |>
    # add dairy cow population
    left_join(
      object@herd |>
        filter(FADN_code_letter == "LCOWDAIR") |>
        dplyr::select(all_of(object@traceability$id_cols),FADN_code_letter,Qobs),
      by = object@traceability$id_cols) |>
    # calculate MILK t animal-1 day-1
    dplyr::mutate(MILK = (MILK_total / Qobs)/365)

  # 2. Calculate the amount of nitrogen excreted by livestock ---------------------------------------------------------------------------------

  # CATTLE ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # we will calculate the N excretion rates using two method:
  ## (1) using calculated DMI from our data feed intake estimation,
  ## (2) using directly the set of equations from IPCC Guidelines.
  ## (3) Then, we consider the average of (1) and (2)
  cattle_Nin <- feed_intake |>
    # filter cattle
    dplyr::filter(species == "cattle") |>
    # add IPCC categories
    dplyr::left_join(data_extra$livestock |>
                       dplyr::select(FADN_code_letter,IPCC_mix_cat),
                     by = 'FADN_code_letter') |>
    # is cattle grazing?
    ## we considered that the more cattle is fed with grasslands, the more it graze
    ## we thus estimate here the share of grassland in the cattle feed, for use in the NE_a estimation afterwards
    dplyr::left_join(
      tmp_grazing_share |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),
                      FADN_code_letter, GE_grazed),
      by = c(object@traceability$id_cols,"FADN_code_letter")
    ) |>
    # add milk production for dairy cows
    dplyr::left_join(
      milk_prod |>
        dplyr::select(dplyr::all_of(object@traceability$id_cols),
                      FADN_code_letter,MILK),
      by = c(object@traceability$id_cols,"FADN_code_letter")
    ) |>
    # estimate N intake
    dplyr::mutate(

      ## (1) N intake from our DMI estimation ---------
      # Gross energy
      ### GE: gross energy, MJ animal-1 day-1
      GE_DMI_MJ_anim_day = GE_MJ_anim/365,
      # N_intake from DMI
      ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## DMI: dry matter intake, kg DM animal-1 day-1
      ## CP_p100: percent crude protein in dry matter, %
      ## Equation 10.32
      Nin_DMI_kgN_anim_day = (DM_kg_anim/365)*((CP_p100/100)/6.25),


      ## (2) N intake from IPCC Guidelines which provide a full set of equations to estimate it.-----
      ## Net Energy for Maintenance NE_m

      ### Cfi: a coefficient which varies for each animal category, MJ day-1 kg-1
      ### Table 10.4
      Cfi = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 0.386,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 0.370,
        .default = 0.322
      ),

      ### Weight: live-weight of animal, kg
      ### Table 10A.1 & Table 10A.2 for "Regions"="Western Europe" for "calves_preweaning"
      ### Source: FRA_2023_2020_13042023_110851.xlsx from fra-2023-crf-25apr23_AR5
      Weight = dplyr::case_when( # !!! use average body weight as we don't have live-weight of animals
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 676.08,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 676.08,
        str_detect(IPCC_mix_cat, "\\bother_mature_cattle\\b") ~ 676.08,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle_postweaning\\b") ~ 439.92,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 439.92,
        str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") ~ 230
      ),

      ### NE_m: net energy required by the animal for maintenance, MJ day-1
      ### Equation 10.3
      NE_m = Cfi*(Weight^0.75),


      ## Net Energy for Activity NE_a

      ### Ca: coefficient corresponding to animal’s feeding situation, dimensionless
      ### Table 10.5: Ca ranges from 0 to 0.36
      ### We apply the maximum Ca in proportion of the share of the GE that is grazed
      Ca = 0.36*GE_grazed,
      ### NE_a: net energy for animal activity, MJ day-1
      ### Equation 10.4
      NE_a = Ca*NE_m,
      ## /!\ voir moyenne fr +/- cahier charge labels | variable RA


      ## Net Energy for Growth NE_g

      ### BW: the average live body weight of the animals in the population, kg
      ### Tables 10A.1 & 10A.2
      BW = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 600,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 600,
        str_detect(IPCC_mix_cat, "\\bother_mature_cattle\\b") ~ 600,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle_postweaning\\b") ~ 400,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 400,
        str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") ~ 230
      ),
      ### C: a coefficient with a value of 0.8 for females, 1.0 for castrates and 1.2 for bulls
      C =
        dplyr::case_when(
          str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 1.2,
          str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 0.8,
          str_detect(IPCC_mix_cat, "\\bother_mature_cattle\\b") ~ 0.8,
          str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 0.8,
          .default = 1
        ),
      ### MW: the mature body weight of an adult animal individually, mature females, mature males and steers) in moderate body condition, kg
      MW = 600, # choice: mature weight = 600 kg as in Tables 10A.1 & 10A.2
      ### WG: the average daily weight gain of the animals in the population, kg day-1
      WG = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 0,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 0,
        str_detect(IPCC_mix_cat, "\\bother_mature_cattle") ~ 0,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle_postweaning\\b") ~ 0.4,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 0.4,
        str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") ~ 0.3
      ),
      ### NE_g: net energy needed for growth, MJ day-1
      ### Equation 10.6
      NE_g = 22.02*((BW/(C*MW))^0.75)*(WG^1.097),


      ## Net Energy for Lactation NE_l

      ### net energy for lactation, MJ day-1
      ### Equation 10.8 & Table 10A.1
      #### Milk = amount of milk produced, kg of milk day-1
      NE_l = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ (MILK*10^3)*(1.47+0.40*4.2),
        .default = 0
      ),
      # /!\ milk production !!!

      ## Net Energy for Work NE_work
      NE_work = 0,  # choice: omit work


      ## Net Energy for Pregnancy NE_p

      ### C_preg: pregnancy coefficient
      ### Table 10.7 & /!\ Table 10A.1 mature_dairy_cattle = 90% of indiv. pregnant
      C_preg = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 0.10,
        .default = 0
      ),
      ### NE_p: net energy required for pregnancy, MJ day-1
      ### Equation 10.13
      NE_p = C_preg * NE_m,

      # TODO: add pregnancy % from UNFCCC

      ## Digestibility

      ### DE: digestibility of feed expressed as a fraction of gross energy (digestible energy/gross energy*100, i.e. DE%)
      ### Tables 10A.1 & 10A.2
      DE = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 71,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 60,
        str_detect(IPCC_mix_cat, "\\bother_mature_cattle\\b") ~ 60, # as mature males
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle_postweaning\\b") ~ 65,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 65,
        # WIP: how to distinguish calves on milk than calves on forage?
        str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") ~ (95+73)/2
        #str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") & code_livestock !=  932 ~ 95, # calves on milk
        #str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") & code_livestock == 932 ~ 73 # calves on forage (broutards only ???)
      ),
      # !!! we could estimate DE = qté aliment - prod lait


      ## Ratio of net energy for maintenance REM

      ### REM: ratio of net energy available in diet for maintenance to digestible energy
      ### Equation 10.14
      REM = 1.123-(4.092*(10^-3)*DE)+(1.126*(10^-5)*(DE^2))-(25.4/DE),


      ## Ratio of net energy for growth REG

      # /!\ choice: even for non-growing cattle? seems yes as in Table 10.3
      ### REG: ratio of net energy available for growth in a diet to digestible energy consumed
      ### Equation 10.15
      REG = 1.164 - (5.16*(10^-3)*DE)+(1.308*(10^-5)*(DE^2))-(37.4/DE),

      ## Gross Energy GE

      ### GE: gross energy, MJ animal-1 day-1
      ### Equation 10.16
      GE_IPCC_MJ_anim_day = (((NE_m+NE_a+NE_l+NE_work+NE_p)/REM) + (NE_g/REG))/(DE/100),

      ## CP
      ### CP: percent crude protein in dry matter for growth stage “i”
      ### Tables 10A.1 & 10A.2
      CP = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") ~ 16.1,
        str_detect(IPCC_mix_cat, "\\bbulls_breed\\b") ~ 14.7,
        str_detect(IPCC_mix_cat, "\\bother_mature_cattle\\b") ~ 14.7, # as mature males
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle_postweaning\\b") ~ 16.5,
        str_detect(IPCC_mix_cat, "\\bgrowing_cattle\\b") ~ 16.5,
        # WIP: how to distinguish calves on milk than calves on forage?
        str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") ~ (17.1+16.5)/2
        #str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") & code_livestock != 932  ~ 17.1, # calves on milk
        #str_detect(IPCC_mix_cat, "\\bcalves_preweaning\\b") & code_livestock == 932 ~ 16.5 # calves on forage (broutards only ???)
      ),

      ## N_intake estimate
      ## N_intake: daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## Equation 10.32
      Nin_IPCC_kgN_anim_day = (GE_IPCC_MJ_anim_day/18.45)*((CP/100)/6.25)
    )


  ## (3) calculate N excretion rates -------

  ### MILK_PR: percent of protein in milk, calculated as 1.9+0.4.%Fat, where %Fat is an input assumed to be 4% (see IPCC Guidelines Equation 10.33)
  MILK_PR <- 1.9+0.4 *0.04

  ### N excretion
  cattle_Nexc <- cattle_Nin |>
    # N retention
    ## daily N retained per animal of category T, kg N animal-1 day-1
    ## Equation 10.33
    dplyr::mutate(
      N_retention = dplyr::case_when(
        str_detect(IPCC_mix_cat, "\\bcows_milk_prod\\b") & MILK >0 ~ ( ( (MILK*10^3) * ( MILK_PR /100) ) /6.38 ),
        NE_g > 0 ~ ((WG * ( (268-(7.03*NE_g/WG))/1000) )/6.25),
        .default = 0
      ),

      ### ANNUAL N EXCRETION RATES, OPTION 2 (TIER 2)
      ## N_ex: annual N excretion rates, kg N animal-1 yr-1
      ## Equation 10.31
      Nex_IPCC_kgN_anim_y = (Nin_IPCC_kgN_anim_day - N_retention) * 365,
      Nex_DMI_kgN_anim_y = (Nin_DMI_kgN_anim_day - N_retention) * 365,

      #Nex_kgN_anim_y = ((Nex_IPCC_kgN_anim_y+Nex_DMI_kgN_anim_y)/2),
      Nex_kgN_anim_y = Nex_DMI_kgN_anim_y,

      # estimate total amount of N excreted, kg N yr-1
      Nex_kgN_livcat_y = Nex_kgN_anim_y * Qobs
    )

  # quality check
  # quantile(cattle_Nexc$Nex_kgN_anim_y[cattle_Nexc$FADN_code_letter == "LCOWDAIR"])
  ## Table 10.19: N excretion rate for dairy cattle in Western EU = 0.54 kg N / 1000kg animal mass / day
  ## Table 10A.5: Weight dairy cattle in Western EU = 600 kg
  ## => 118 kg N / animal / an
  ## Table 10.19: N excretion rate for other cattle in Western EU = 0.42 kg N / 1000kg animal mass / day
  ## Table 10A.5: Weight other cattle in Western EU = 405 kg
  ## => 62 kg N / animal / an



  # SWINE (PIG) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## see IPCC Guidelines 2019
  ## N intake
  swine_Nin <- feed_intake |>
    # filter swine
    dplyr::filter(species == "swine") |>
    # estimate N intake
    dplyr::mutate(
      # N_intake from DMI
      ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## DMI: dry matter intake, kg DM animal-1 day-1
      ## CP_p100: percent crude protein in dry matter, %
      ## Equation 10.32A
      Nin_DMI_kgN_anim_day = (DM_kg_anim/365)*((CP_p100/100)/6.25))

  ## N excretion
  swine_Nexc <- swine_Nin |>
    dplyr::mutate(
      # N retention
      ## Default N retention fraction
      ## Table 10.20
      N_retention_frac = 0.30,
      # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
      ## N_ex: annual N excretion rates, kg N animal-1 yr-1
      ## Equation 10.31
      Nex_kgN_anim_y = Nin_DMI_kgN_anim_day * (1- N_retention_frac) * 365,

      # estimate total amount of N excreted, kg N yr-1
      Nex_kgN_livcat_y = Nex_kgN_anim_y * Qobs
    )

  # quality check => quantile(swine_Nexc$Nex_kgN_anim_y)
  ## Table 10.19: N excretion rate for swine in Western EU = 0.65 kg N / 1000kg animal mass / day
  ## Table 10A.5: Weight swine in Western EU = 76 kg
  ## => 18 kg N / animal / an
  ## swine livestock unit = 0.3
  ## => 60 kg N / livestock unit / an (range from 36 to 72) with 0.65/1000*76/0.3*365



  # POULTRY ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## see IPCC Guidelines 2023
  ## N intake
  poultry_Nin <- feed_intake |>
    # filter poultry
    dplyr::filter(species == "poultry") |>
    # estimate N intake
    dplyr::mutate(

      # N_intake from DMI
      ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## DMI: dry matter intake, kg DM animal-1 day-1
      ## CP_p100: percent crude protein in dry matter, %
      ## Equation 10.32A
      Nin_DMI_kgN_anim_day = (DM_kg_anim/365)*((CP_p100/100)/6.25))

  ## N excretion
  poultry_Nexc <- poultry_Nin |>

    dplyr::mutate(
      # N retention
      ## Default N retention fraction
      ## Table 10.20
      N_retention_frac = 0.30,
      # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
      ## N_ex: annual N excretion rates, kg N animal-1 yr-1
      ## Equation 10.31
      Nex_kgN_anim_y = Nin_DMI_kgN_anim_day * (1- N_retention_frac) * 365,

      # estimate total amount of N excreted, kg N yr-1
      Nex_kgN_livcat_y = Nex_kgN_anim_y * Qobs
    )

  # quality check => quantile(poultry_Nexc$Nex_kgN_anim_y)
  ## Table 10.19: N excretion rate for chickens in Western EU = 0.99 kg N / 1000kg animal mass / day (range from 0.58 to 1.14)
  ## Table 10A.5: Weight chickens in Western EU = 1.4 kg
  ## => 0.5 kg N / animal / an (range from 0.3 to 0.6)
  ## poultry livestock unit = 0.007 for broilers and 0.014 for laying hens => mean = 0.0105
  ## => 48 kg N / livestock unit / an (range from 36 to 72) with 0.99/1000*1.4/0.0105*365


  # SHEEP ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## see IPCC Guidelines 2023
  ## N intake
  sheep_Nin <- feed_intake |>
    # filter sheep
    dplyr::filter(species == "sheep") |>
    # estimate N intake
    dplyr::mutate(

      # N_intake from DMI
      ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## DMI: dry matter intake, kg DM animal-1 day-1
      ## CP_p100: percent crude protein in dry matter, %
      ## Equation 10.32A
      Nin_DMI_kgN_anim_day = (DM_kg_anim/365)*((CP_p100/100)/6.25))

  ## N excretion
  sheep_Nexc <- sheep_Nin |>

    dplyr::mutate(
      # N retention
      ## Default N retention fraction
      ## Table 10.20
      N_retention_frac = 0.10,
      # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
      ## N_ex: annual N excretion rates, kg N animal-1 yr-1
      ## Equation 10.31
      Nex_kgN_anim_y = Nin_DMI_kgN_anim_day * (1- N_retention_frac) * 365,

      # estimate total amount of N excreted, kg N yr-1
      Nex_kgN_livcat_y = Nex_kgN_anim_y * Qobs
    )

  # quality check => quantile(sheep_Nexc$Nex_kgN_anim_y)
  ## Table 10.19: N excretion rate for sheep in Western EU = 0.36 kg N / 1000kg animal mass / day
  ## Table 10A.5: Weight sheep in Western EU = 40 kg
  ## => 5 kg N / animal / an


  # GOATS -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  ## see IPCC Guidelines 2023
  ## N intake
  goats_Nin <- feed_intake |>
    # filter goats
    dplyr::filter(species == "goats") |>
    # estimate N intake
    dplyr::mutate(

      # N_intake from DMI
      ## daily N consumed per animal of category T, kg N animal-1 day-1, per growth stage-1 “i" when applicable
      ## DMI: dry matter intake, kg DM animal-1 day-1
      ## CP_p100: percent crude protein in dry matter, %
      ## Equation 10.32A
      Nin_DMI_kgN_anim_day = (DM_kg_anim/365)*((CP_p100/100)/6.25))

  ## N excretion
  goats_Nexc <- goats_Nin |>

    dplyr::mutate(
      # N retention
      ## Default N retention fraction
      ## Table 10.20
      N_retention_frac = 0.10,
      # ANNUAL N EXCRETION RATES, OPTION 1 (TIER 2)
      ## N_ex: annual N excretion rates, kg N animal-1 yr-1
      ## Equation 10.31
      Nex_kgN_anim_y = Nin_DMI_kgN_anim_day * (1- N_retention_frac) * 365,

      # estimate total amount of N excreted, kg N yr-1
      Nex_kgN_livcat_y = Nex_kgN_anim_y * Qobs
    )

  # quality check => quantile(goats_Nexc$Nex_kgN_anim_y)
  ## Table 10.19: N excretion rate for goats in Western EU = 0.46 kg N / 1000kg animal mass / day
  ## Table 10A.5: Weight goats in Western EU = 40 kg
  ## => 7 kg N / animal / an



  ##### Output ----

  N_excr <- list(
    cattle = cattle_Nexc,
    swine = swine_Nexc,
    poultry = poultry_Nexc,
    sheep = sheep_Nexc,
    goats = goats_Nexc)

  return(N_excr)

}


