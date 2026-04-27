#' UNFCCC Sectoral Background Data for Agriculture and Energy
#'
#' A dataset containing selected Sectoral Background Data tables from UNFCCC
#' National Inventory Reports (CRF tables). These tables provide activity data
#' and implied emission factors required for calculating the carbon footprint
#' of agricultural activities.
#'
#' @format A named list containing 7 data frames (tibbles):
#' \describe{
#'   \item{table3As2}{**Table 3.A (Sheet 2 of 2): Enteric Fermentation.**
#'   Contains Implied Emission Factors (IEF) for methane emissions from enteric fermentation per livestock category.}
#'
#'   \item{table3Bas1}{**Table 3.B(a) (Sheet 1 of 2): CH4 Emissions from Manure Management.**
#'   Contains data on manure management system allocation (MMS) and nitrogen excretion rates.}
#'
#'   \item{table3Bas2}{**Table 3.B(a) (Sheet 2 of 2): CH4 Emissions from Manure Management.**
#'   Contains Implied Emission Factors (IEF) for methane from manure management.}
#'
#'   \item{table3Bb}{**Table 3.B(b): N2O Emissions from Manure Management.**
#'   Contains Nitrogen excretion data and N2O Implied Emission Factors per manure management system.}
#'
#'   \item{table3D}{**Table 3.D: Agricultural Soils.**
#'   Contains fractions and activity data for direct and indirect N2O emissions from managed soils (fertilizers, crop residues, etc.).}
#'
#'   \item{EF_managed_soils}{**Derived Emission Factors for Soils.**
#'   A subset or processed version of Table 3.D focusing specifically on emission factors for direct/indirect soil emissions.}
#'
#'   \item{EF_fuel}{**Table 1.A(a) (Sheet 4 of 4): Fuel combustion activities.**
#'   Contains Implied Emission Factors of Liquid fuels for CO2, CH4, and N2O from fuel combustion in the agriculture/forestry/fisheries sector (in t CO2 TJ-1, kg CH4 TJ-1, and kg N2O TJ-1).}
#' }
#'
#' @details
#' These tables are derived from the Common Reporting Format (CRF) used by Annex I Parties
#' to report to the UNFCCC. They allow for the calculation of specific emission factors
#' adapted to national contexts.
#'
#' @source United Nations Framework Convention on Climate Change (UNFCCC) Greenhouse Gas Inventory Data.
#' Available at: \url{https://unfccc.int/process-and-meetings/transparency-and-reporting/greenhouse-gas-data}
#'
#' @usage data(UNFCCC_data)
"UNFCCC_data"



library(readxl)
library(zip)

library(tibble)
library(dplyr)
library(tidyr)
library(stringr)



# do I need all tables Table3?

# empty tibble
UNFCCC_data <- list()

#TODO: combine several years

for (loop_zip in list.files("inst/ext_data/UNFCCC_ghg_inventories/")) {
  # loop_zip = list.files("inst/ext_data/UNFCCC_ghg_inventories/")[9]

  # UNFCCC files ----
  loop_files = zip_list(paste0("inst/ext_data/UNFCCC_ghg_inventories/",loop_zip))
  loop_data_country = toupper(str_sub(loop_zip,1,3))
  print(loop_data_country)
  loop_unzip <- tempfile()
  tmp_data_year = "2018"
  loop_unzip <- utils::unzip(paste0("inst/ext_data/UNFCCC_ghg_inventories/",loop_zip),
                             loop_files$filename[grep(tmp_data_year,loop_files$filename)])

  # TABLE 1.A(a)  SECTORAL BACKGROUND DATA  FOR  ENERGY - Fuel combustion activities - sectoral approach - (Sheet 4 of 4)

  # TABLE 3 SECTORAL BACKGROUND DATA FOR AGRICULTURE

  # TABLE 3.As1: Enteric Fermentation (Sheet 1 of 2)
  # TABLE 3.As2: Enteric Fermentation (Sheet 2 of 2)

  # TABLE 3.B(a)s1: CH4 Emissions from Manure Management (Sheet 1 of 2)
  # TABLE 3.B(a)s2: CH4 Emissions from Manure Management (Sheet 2 of 2)
  # TABLE 3.B(b): N2O Emissions from Manure Management

  # TABLE 3.C: Rice Cultivation

  # TABLE 3.D: Direct and indirect N2O emissions from agricultural soils

  # TABLE 3.E: Prescribed burning of savannas

  # TABLE 3.F: Field burning of agricultural residues(1)

  # TABLE 3.G-I: CO2 emissions from liming, urea application and other carbon-containing fertilizers

  loop_list_data <- list()

  # EMISSIONS FROM LIVESTOCK ----
  ## TABLE 3.As2: Enteric Fermentation (Sheet 2 of 2) ----

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table3.As2", col_names = F)
  # select table
  loop_range <- t(loop_raw_sheet[seq(which(grepl("Indicators",loop_raw_sheet$...1)),which(is.na(loop_raw_sheet$...1))[2]-1),])[-c(1:2),]
  # retrieve column names
  loop_colnames <- loop_raw_sheet$...1[seq(which(grepl("Indicators",loop_raw_sheet$...1)),which(is.na(loop_raw_sheet$...1))[2]-1)]
  loop_colnames[1] <- "Animal_cat"
  colnames(loop_range) <- loop_colnames

  # frame tibble
  loop_table3As2 <- as_tibble(loop_range) %>%
    # remove empty rows
    filter(Weight >0 | `Digestibility of feed`>0 | `Gross energy` >0) %>%
    # clean animal categories
    mutate(Animal_cat =  tolower(gsub(" ","_", gsub("\\([a-zA-Z0-9]+\\)","",Animal_cat))))

  loop_list_data[["table3As2"]] <- loop_table3As2

  ## TABLE 3.B(a)s1: CH4 Emissions from Manure Management (Sheet 1 of 2) ----

  ## Allocation by climate region (%): Cool | Temperate | Warm
  ## Typical animal mass (average, kg)
  ## VS daily excretion (average, kg dm/head/day)
  ## CH4 producing potential (Bo) (average, m3 CH4/kg VS)

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table3.B(a)s1", col_names = F)
  # select table
  loop_range <- loop_raw_sheet[seq(
    which(grepl("(1000s)",loop_raw_sheet$...2))+1,
    sort(which(!is.na(loop_raw_sheet[,ncol(loop_raw_sheet)])),decreasing = T)[1]
  ),]
  # retrieve column names
  loop_colnames <- t(loop_raw_sheet[seq(
    which(grepl("ACTIVITY",loop_raw_sheet$...2)),
    which(grepl("(1000s)",loop_raw_sheet$...2))
  ),]) |>
    transform(V2 = ifelse(is.na(V2),V1,V2)) |>
    transform(V2 = ifelse(grepl("Cool|Temperate|Warm",V3),paste0("alloc_clim_reg_",V3),V2)) |>
    transform(V2 = gsub(" ","_",V2))
  loop_colnames <- loop_colnames$V2
  loop_colnames[1] <- "Animal_cat"
  colnames(loop_range) <- loop_colnames

  # frame tibble
  loop_table3Bas1 <- as_tibble(loop_range) %>%
    # remove empty rows
    filter(alloc_clim_reg_Cool >0 | alloc_clim_reg_Temperate >0 | alloc_clim_reg_Warm >0) %>%
    # clean animal categories
    mutate(Animal_cat = tolower(gsub(" ","_", gsub("\\([a-zA-Z0-9]+\\)","",Animal_cat))))

  loop_list_data[["table3Bas1"]] <- loop_table3Bas1


  ## TABLE 3.B(a)s2: CH4 Emissions from Manure Management (Sheet 2 of 2) ----

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table3.B(a)s2", col_names = F)
  # select table
  loop_range <- loop_raw_sheet[seq(
    sort(which(!is.na(loop_raw_sheet$...2)),decreasing = F)[1],
    sort(which(!is.na(loop_raw_sheet$...4)),decreasing = T)[1]
  ),-1]
  # retrieve column names
  loop_colnames <- t(loop_raw_sheet[seq(
    which(grepl("Indicator",loop_raw_sheet$...3)),
    which(grepl("Manure",loop_raw_sheet$...5))+1
  ),-1]) |>
    transform(V2 = ifelse(is.na(V2),V1,paste0("MMS_",V2)))
  loop_colnames <-  tolower(gsub(",","", gsub(" ","_", loop_colnames$V2)))
  loop_colnames[1] <- "Animal_cat"
  colnames(loop_range) <- loop_colnames


  # fill empty cells of animal categories and indicators
  # for the +/-30 first rows
  loop_table3Bas2_1 <- loop_range[1:(sort(which(grepl("MCF",loop_range$indicator)),decreasing = T)[1]+2),] %>%
    mutate(
      indicator = rep(unique(na.omit(indicator)),
                      each = length(unique(na.omit(climate_region))),
                      times = (length(unique(na.omit(Animal_cat))))),
      Animal_cat = rep(unique(na.omit(Animal_cat)),
                       each = (length(unique(na.omit(climate_region)))) * length(unique(na.omit(indicator))))) %>%
    # filter rows with only NAs in MMS
    filter(!if_all(all_of(loop_colnames[grepl("mms",loop_colnames)]), is.na))

  # for other rows
  loop_table3Bas2_2 <- loop_range[(sort(which(grepl("MCF",loop_range$indicator)),decreasing = T)[1]+3):nrow(loop_range),] %>%
    mutate(
      extracted = str_match(climate_region, "^(.+)\\s-\\s(.+)\\s-\\s(.+)$"),
      Animal_cat = extracted[, 2],
      indicator = extracted[, 3],
      climate_region = extracted[, 4]
    ) %>%
    select(-extracted)

  # combine parts of TABLE 3.B(a)s2
  loop_table3Bas2 <- rbind(
    loop_table3Bas2_1,
    loop_table3Bas2_2
  ) %>%
    # clean indicator variable
    mutate(
      indicator = case_when(
        str_detect(indicator,"Allocation") ~ "MMS_alloc",
        str_detect(indicator,"MCF") ~ "MCF"
      ))  %>%
    # clean animal categories
    mutate(Animal_cat = tolower(gsub(" ","_", gsub("\\([a-zA-Z0-9]+\\)","",Animal_cat))))

  # AWMS
  loop_table3Bas2_AWMS <- loop_table3Bas2 %>%
    filter(indicator == "MMS_alloc") %>%
    # pivot MMS
    pivot_longer(cols = all_of(loop_colnames[grepl("mms",loop_colnames)]),
                 names_to = "MMS",values_to = "AWMS") %>%
    select(-indicator)

  # MCF
  loop_table3Bas2_MCF <- loop_table3Bas2 %>%
    filter(indicator == "MCF") %>%
    # pivot MMS
    pivot_longer(cols = all_of(loop_colnames[grepl("mms",loop_colnames)]),
                 names_to = "MMS",values_to = "MCF") %>%
    select(-indicator)

  # combine AWMS and MCF
  loop_table3Bas2 <- left_join(
    loop_table3Bas2_AWMS,
    loop_table3Bas2_MCF,
    by = join_by(Animal_cat, climate_region, MMS)
  ) %>%
    # add population size from Table3.B(a)s1
    left_join(
      loop_table3Bas1 %>%
        select(Animal_cat,Population__size),
      by = join_by(Animal_cat)
    )

  loop_list_data[["table3Bas2"]] <- loop_table3Bas2

  ## TABLE 3.B(b): N2O Emissions from Manure Management ----

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table3.B(b)", col_names = F)
  # select table
  loop_range <- as.data.frame(t(loop_raw_sheet[c(
    which(grepl("Anaerobic",loop_raw_sheet$...5)),
    which(grepl("IEF",loop_raw_sheet$...1))),]))
  loop_range <- loop_range[c(
    sort(which(!is.na(loop_range$V1)),decreasing = F)[1]:
      sort(which(!is.na(loop_range$V2)),decreasing = T)[1]
  ),]

  # retrieve column names
  loop_colnames <- c("MMS","EF3")
  colnames(loop_range) <- loop_colnames

  # table
  loop_table3Bb <- loop_range %>%
    # clean MMS variable
    mutate(MMS = paste0("mms_",tolower(gsub(" ","_", gsub("\\([a-zA-Z0-9]+\\)| \\([a-zA-Z0-9]+\\)","",unique(MMS)))))) %>%
    # add animal cat variable for following code
    mutate(
      Animal_cat = "all"
    )

  loop_list_data[["table3Bb"]] <- loop_table3Bb



  ## Add livestock matching variables ----
  # TODO: add climate regions from NUTS2 registered in FADN

  loop_list_data <- loop_list_data %>%
    lapply(., function(df) {
      df <- df %>%
        mutate(
          species = case_when(
            #grepl("(?i)cattle|cow|calve|bull",Animal_cat) ~ "cattle",
            str_detect(Animal_cat,"(?i)cattle|cow|calve|bull") ~ "cattle",
            str_detect(Animal_cat,"(?i)swine|sow|pig|weaner") ~ "swine",
            str_detect(Animal_cat,"(?i)poultry") ~ "poultry",
            str_detect(Animal_cat,"(?i)sheep|lamb|ewe|ram") ~ "sheep",
            str_detect(Animal_cat,"(?i)goat") ~ "goats",
            str_detect(Animal_cat,"(?i)horse") ~ "horse"
          ),
          UNFCCC_cat = case_when(
            # Option A: Dairy cattle | Non-dairy cattle
            # Option B: Mature dairy cattle | Other mature cattle | Growing cattle
            species == "cattle" & grepl("(?i)non-dairy|non-lactating",Animal_cat,) ~ "growing_cattle;other_mature_cattle",
            species == "cattle" & grepl("(?i)dairy",Animal_cat) ~ "dairy_cattle",
            species == "cattle" & grepl("(?i)growing|calve|suckler|young",Animal_cat) ~ "growing_cattle",
            species == "cattle" & grepl("(?i)mature|bull|other",Animal_cat) ~ "other_mature_cattle",

            species == "swine" & grepl("(?i)breeding|sow",Animal_cat) ~ "breeding_swine",
            species == "swine" & grepl("(?i)Weaners",Animal_cat) ~ "piglets",
            species == "swine" & grepl("(?i)fattening|over 20 kg|market",Animal_cat) ~ "fattening_swine",
            species == "swine" ~ "breeding_swine;piglets;fattening_swine",

            species == "poultry" ~ "poultry",

            species == "sheep" & grepl("(?i)ewes",Animal_cat) ~ "breeding_sheep",
            species == "sheep" & grepl("(?i)lambs|over 1yr|rams|Other Mature",Animal_cat) ~ "fattening_sheep",
            species == "sheep" ~ "breeding_sheep;fattening_sheep",

            species == "goats" ~ "goats",
            species == "horse" ~ "horse"
          ))

      # missing UNFCCC cat
      tmp_missing_UNFCCC_cat = setdiff(
        unique(na.omit(FADN2Footprint::data_extra$livestock$UNFCCC_cat)),
        unique(na.omit(df$UNFCCC_cat))
      )

      df <- df %>%
        left_join(
          FADN2Footprint::data_extra$livestock %>%
            select(species,UNFCCC_cat) %>%
            filter(UNFCCC_cat %in% tmp_missing_UNFCCC_cat) %>%
            group_by(species) %>%
            summarise(
              missing_UNFCCC_cat = paste0(unique(UNFCCC_cat),collapse = ";")
            ),
          by = join_by(species)
        ) %>%
        mutate(
          UNFCCC_cat = ifelse(is.na(UNFCCC_cat),missing_UNFCCC_cat,UNFCCC_cat)
        ) %>%
        separate_longer_delim(UNFCCC_cat,";")

    })

  # EMISSIONS FROM MANAGED SOILS ----

  ## TABLE 3.D: Direct and indirect N2O emissions from agricultural soils ----

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table3.D", col_names = F)
  # select table
  loop_range <- as.data.frame(loop_raw_sheet[c(
    which(grepl("a. Direct",loop_raw_sheet$...1)):
      which(grepl("Nitrogen leaching",loop_raw_sheet$...1))),])

  # retrieve column names
  loop_colnames <- t(loop_raw_sheet[c(
    which(grepl("GREENHOUSE GAS",loop_raw_sheet$...1)):
      (which(grepl("GREENHOUSE GAS",loop_raw_sheet$...1))+2)),]) |>
    transform(V0 = paste(coalesce(V1,""),coalesce(V2,""),coalesce(V3,"")))
  colnames(loop_range) <- loop_colnames$V0

  # table
  loop_table3D <- loop_range

  loop_list_data[["table3D"]] <- loop_table3D

  ## EF ----

  loop_table_MS_EF <- Reduce(
    function(x,y) dplyr::bind_cols(x,y),
    list(

      # Direct emissions
      ## N2ON_d = F_SN*EF_SN + F_ON*EF_ON + F_CR*EF_CR + F_SOM*EF_SOM + N2ON_OS + F_PRP*EF_PRP
      ## EF_SN = emission factors developed for N2O emissions from synthetic fertilizer N application (kg N2O–N (kg N input)-1)
      loop_table3D |>
        filter(grepl("Inorganic N",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_SN = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ## EF_ON = emission factors developed for N2O emissions from organic fertilizer N application (kg N2O–N (kg N input)-1)
      loop_table3D |>
        filter(grepl("Organic N",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_ON = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ## EF_CR = emission factors developed for N2O emissions from N in crop residues returned to soils (kg N2O–N (kg N input)-1)
      loop_table3D |>
        filter(grepl("Crop residues",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_CR = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ## EF_PRP = = emission factor for N2O emissions from urine and dung N deposited on pasture, range and paddock by grazing animals, kg N2O–N (kg N input)-1; (Table 11.1) (Note: the subscripts CPP and SO refer to Cattle, Poultry and Pigs, and Sheep and Other animals, respectively)
      loop_table3D |>
        filter(grepl("grazing animals",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_PRP = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ## EF_SOM
      loop_table3D |>
        filter(grepl("soil organic matter",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_SOM = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ## EF_OS
      loop_table3D |>
        filter(grepl("organic soils",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF_OS = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),

      # Indirect N20 emissions
      ### EF4 = emission factor for N2O emissions from atmospheric deposition of N on soils and water surfaces, [kg N–N2O (kg NH3–N + NOx–N volatilized)-1] (Table 11.3)
      loop_table3D |>
        filter(grepl("Atmospheric deposition",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF4 = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`),
      ### EF5 = emission factor for N2O emissions from N leaching and runoff, kg N2O–N (kg N leached and runoff)-1 (Table 11.3)
      loop_table3D |>
        filter(grepl("leaching and run-off",`GREENHOUSE GAS SOURCE AND SINK CATEGORIES  `)) |>
        select(`IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`) |>
        rename(EF5 = `IMPLIED EMISSION FACTORS  kg N2O-N/kg N(1)(2)`)
    )) |>
    # replace NAs with the EF mean
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(., mean(., na.rm = TRUE))))

  loop_list_data[["EF_managed_soils"]] <- loop_table_MS_EF

  # EMISSIONS FROM LIQUID FUELS CONSUMPTION ----
  # TABLE 1.A(a)  SECTORAL BACKGROUND DATA  FOR  ENERGY - Fuel combustion activities - sectoral approach - (Sheet 4 of 4)

  # read file
  loop_raw_sheet <- read_excel(loop_unzip,sheet = "Table1.A(a)s4", col_names = F)
  # select table
  loop_range <-loop_raw_sheet[which(grepl("c.  Agriculture/forestry/fishing",loop_raw_sheet$...1))+1,4:6]
  names(loop_range) <- c("EF_fuel_CO2","EF_fuel_CH4","EF_fuel_N2O")

  loop_list_data[["EF_fuel"]] <- loop_range

  # Add country and year ----

tmp_country_df = FADN2Footprint::data_extra$country_names |>
    dplyr::filter(Country_ISO_3166_1_A3 == loop_data_country
                  | country_FADN == loop_data_country)
  tmp_country = ifelse(dim(tmp_country_df)[1] >0,
                       unique(tmp_country_df$Country_ISO_3166_1_A3),
                       loop_data_country)

  loop_list_data <- loop_list_data %>%
    lapply(., function(df) {
      df <- df |>
        dplyr::mutate(
          Country_ISO_3166_1_A3 = tmp_country,
          YEAR = as.character(tmp_data_year)
        )})


  # extract data ----

  UNFCCC_data$table3As2 <- bind_rows(
    UNFCCC_data$table3As2,
    loop_list_data$table3As2
  )

  UNFCCC_data$table3Bas1 <- bind_rows(
    UNFCCC_data$table3Bas1,
    loop_list_data$table3Bas1
  )

  UNFCCC_data$table3Bas2 <- bind_rows(
    UNFCCC_data$table3Bas2,
    loop_list_data$table3Bas2
  )

  UNFCCC_data$table3Bb <- bind_rows(
    UNFCCC_data$table3Bb,
    loop_list_data$table3Bb
  )

  UNFCCC_data$table3D <- bind_rows(
    UNFCCC_data$table3D,
    loop_list_data$table3D
  )

  UNFCCC_data$EF_managed_soils <- bind_rows(
    UNFCCC_data$EF_managed_soils,
    loop_list_data$EF_managed_soils
  )

  UNFCCC_data$EF_fuel <- bind_rows(
    UNFCCC_data$EF_fuel,
    loop_list_data$EF_fuel
  )

  unlink(loop_unzip)
  rm(list = names(.GlobalEnv)[grep("loop",names(.GlobalEnv))])

}

# Clean data ----

# convert variables to numeric when appropriate
UNFCCC_data <- UNFCCC_data %>%
  lapply(., function(df) {

    df <- df %>%
      # convert to numeric variables when appropriate
      lapply(., function(col) {
        # Try converting to numeric
        col_num <- suppressWarnings(as.numeric(col))

        # Check if there's at least one non-NA after conversion (i.e. it's a numeric column)
        if (any(!is.na(col_num))) {
          return(col_num)
        } else {
          return(col)  # Keep as character if not numeric
        }
      })  %>% as_tibble(.) %>%
      # ensure YEAR is a character
      dplyr::mutate(YEAR = as.character(YEAR))

  })


UNFCCC_data$table3Bas2 <- UNFCCC_data$table3Bas2 %>%
  # remove rows without AWMS or MCF
  filter(AWMS >0 | MCF >0)

UNFCCC_data$table3Bb <- UNFCCC_data$table3Bb %>%
  # remove unnecessary variable
  select(-c(Animal_cat,species,UNFCCC_cat,missing_UNFCCC_cat)) %>%
  distinct()

# For each numeric column, replace NAs with the group mean
UNFCCC_data$EF_managed_soils <- UNFCCC_data$EF_managed_soils |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
              ~ ifelse(is.na(.),
                       mean(., na.rm = TRUE),
                       .)))


# export data
usethis::use_data(UNFCCC_data, overwrite = T)

rm(list = names(.GlobalEnv)[grep("tmp",names(.GlobalEnv))])


# WIP for Sweden, less than 100% AWMS
# UNFCCC_MMS_AWMS %>% group_by(COUNTRY,UNFCCC_cat) %>% summarise(sum = sum(AWMS)) %>% filter(round(sum) != 100)
# WIP adapt for overseas territories
