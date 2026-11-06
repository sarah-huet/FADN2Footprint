if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.")
}

input_file <- file.path("AI_agents", "outputs", "crt_variables_long.csv")
if (!file.exists(input_file)) {
  stop("The verified CRT aggregate is missing: ", input_file)
}

load(file.path("data", "data_extra.rda"))

header <- names(utils::read.csv(
  input_file,
  nrows = 0,
  check.names = FALSE
))
required_columns <- c(
  "iso3", "year", "sheet", "variable", "category_code", "category",
  "value", "unit_expected", "status", "cell"
)
missing_columns <- setdiff(required_columns, header)
if (length(missing_columns) > 0) {
  stop("CRT aggregate is missing columns: ", paste(missing_columns, collapse = ", "))
}

column_classes <- rep("NULL", length(header))
names(column_classes) <- header
column_classes[required_columns] <- c(
  "character", "character", "character", "character", "character",
  "character", "numeric", "character", "character", "character"
)
crt <- utils::read.csv(
  input_file,
  colClasses = unname(column_classes),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
if (anyNA(crt$iso3) || anyNA(crt$year) || anyNA(as.integer(crt$year))) {
  stop("CRT aggregate contains missing or invalid country/year keys.")
}
crt$YEAR <- as.character(crt$year)

denmark_rows <- which(crt$iso3 == "DNK")
if (length(denmark_rows) == 0) {
  stop("Expected Danish archive blocks are absent; recheck CRT provenance.")
}
denmark_years <- as.integer(crt$year[denmark_rows])
denmark_block_starts <- which(c(TRUE, diff(denmark_years) < 0))
if (length(denmark_block_starts) != 2) {
  stop("Expected two ordered Danish archive blocks; recheck CRT provenance.")
}
first_block_years <- denmark_years[seq_len(denmark_block_starts[[2]] - 1)]
second_block_years <- denmark_years[seq.int(denmark_block_starts[[2]], length(denmark_years))]
if (!identical(range(first_block_years), range(second_block_years))) {
  stop("Danish archive blocks do not cover the same inventory-year range.")
}
denmark_block_sizes <- c(
  denmark_block_starts[[2]] - 1L,
  length(denmark_years) - denmark_block_starts[[2]] + 1L
)
if (!identical(denmark_block_sizes, c(115981L, 112540L))) {
  stop("Danish archive block sizes changed; recheck which block is the V2 submission.")
}

# The extractor follows the index order: the complete V2 submission precedes its V1 annex.
crt <- crt[-denmark_rows[seq.int(denmark_block_starts[[2]], length(denmark_rows))], ]
duplicate_keys <- c("iso3", "year", "sheet", "variable", "cell")
if (anyDuplicated(crt[duplicate_keys])) {
  stop("Duplicate CRT cells remain after excluding the Danish annex.")
}

country_year <- dplyr::distinct(
  crt,
  Country_ISO_3166_1_A3 = iso3,
  YEAR
)

clean_category <- function(category) {
  category[is.na(category)] <- ""
  category <- sub(
    "^\\s*[0-9]+(?:\\.[[:alnum:]]+)*\\.?\\s*",
    "",
    category,
    perl = TRUE
  )
  trimws(category)
}

animal_key <- function(category, category_code) {
  label <- clean_category(category)
  code <- ifelse(is.na(category_code), "", category_code)
  label <- gsub("\\s*>\\s*", " over ", label)
  label <- gsub("\\s*<\\s*", " under ", label)
  key <- tolower(gsub("[^[:alnum:]]+", "_", label))
  key <- gsub("^_+|_+$", "", key)
  key[grepl("^3\\.B\\.2", code) & grepl("other", label, ignore.case = TRUE)] <- "sheep"
  key[grepl("^3\\.B\\.3", code) & grepl("other", label, ignore.case = TRUE)] <- "swine"
  key
}

animal_species <- function(category, category_code) {
  label <- clean_category(category)
  code <- ifelse(is.na(category_code), "", category_code)
  dplyr::case_when(
    grepl("^3\\.[AB]\\.1", code) |
      grepl("cattle|cow|calve|bull", label, ignore.case = TRUE) ~ "cattle",
    grepl("^3\\.B\\.2", code) |
      grepl("sheep|lamb|ewe|ram", label, ignore.case = TRUE) ~ "sheep",
    grepl("^3\\.B\\.3", code) |
      grepl("swine|sow|pig|weaner", label, ignore.case = TRUE) ~ "swine",
    grepl("^3\\.B\\.4\\.d", code) |
      grepl("goat", label, ignore.case = TRUE) ~ "goats",
    grepl("^3\\.B\\.4\\.e", code) |
      grepl("horse", label, ignore.case = TRUE) ~ "horse",
    grepl("^3\\.B\\.4\\.g", code) |
      grepl("poultry", label, ignore.case = TRUE) ~ "poultry",
    TRUE ~ NA_character_
  )
}

animal_unfccc_category <- function(category, category_code) {
  label <- clean_category(category)
  species <- animal_species(category, category_code)
  dplyr::case_when(
    species == "cattle" & grepl("non.?dairy|non.?lactating", label, ignore.case = TRUE) ~
      "growing_cattle;other_mature_cattle",
    species == "cattle" & grepl("dairy", label, ignore.case = TRUE) ~ "dairy_cattle",
    species == "cattle" & grepl("growing|calve|suckler|young", label, ignore.case = TRUE) ~
      "growing_cattle",
    species == "cattle" & grepl("mature|bull|other", label, ignore.case = TRUE) ~
      "other_mature_cattle",
    species == "swine" & grepl("breeding|sow", label, ignore.case = TRUE) ~
      "breeding_swine",
    species == "swine" &
      grepl("weaner|under\\s*20|<\\s*20", label, ignore.case = TRUE) ~ "piglets",
    species == "swine" &
      grepl("fattening|over\\s*20|market|>\\s*20", label, ignore.case = TRUE) ~
      "fattening_swine",
    species == "swine" ~ "breeding_swine;piglets;fattening_swine",
    species == "poultry" ~ "poultry",
    species == "sheep" & grepl("ewes", label, ignore.case = TRUE) ~ "breeding_sheep",
    species == "sheep" & grepl("lambs|over 1yr|rams|other mature", label, ignore.case = TRUE) ~
      "fattening_sheep",
    species == "sheep" ~ "breeding_sheep;fattening_sheep",
    species == "goats" ~ "goats",
    species == "horse" ~ "horse",
    TRUE ~ NA_character_
  )
}

map_animals <- function(data) {
  data |>
    dplyr::mutate(
      Animal_cat = animal_key(category, category_code),
      species = animal_species(category, category_code),
      UNFCCC_cat = animal_unfccc_category(category, category_code)
    )
}

prefer_leaf_categories <- function(data) {
  key_columns <- c("iso3", "YEAR", "sheet", "variable")
  coded <- data |>
    dplyr::filter(!is.na(category_code), nzchar(category_code)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key_columns, "category_code"))))
  if (nrow(coded) == 0) {
    return(data)
  }

  ancestor_rows <- lapply(seq_len(nrow(coded)), function(i) {
    parts <- strsplit(coded$category_code[[i]], ".", fixed = TRUE)[[1]]
    if (length(parts) < 2) {
      return(NULL)
    }
    data.frame(
      coded[rep(i, length(parts) - 1), key_columns, drop = FALSE],
      category_code = vapply(
        seq_len(length(parts) - 1),
        function(n) paste(parts[seq_len(n)], collapse = "."),
        character(1)
      ),
      stringsAsFactors = FALSE
    )
  })
  ancestor_rows <- dplyr::bind_rows(ancestor_rows)
  parent_codes <- dplyr::inner_join(
    ancestor_rows,
    coded,
    by = c(key_columns, "category_code")
  ) |>
    dplyr::distinct()
  data <- dplyr::anti_join(
    data,
    parent_codes,
    by = c(key_columns, "category_code")
  )

  coded_animals <- data |>
    dplyr::filter(!is.na(category_code), nzchar(category_code)) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(key_columns, "Animal_cat"))))
  uncoded <- data |>
    dplyr::filter(is.na(category_code) | !nzchar(category_code)) |>
    dplyr::anti_join(
      coded_animals,
      by = c(key_columns, "Animal_cat")
    )
  dplyr::bind_rows(
    data |> dplyr::filter(!is.na(category_code), nzchar(category_code)),
    uncoded
  )
}

expand_unfccc_categories <- function(data) {
  tidyr::separate_longer_delim(data, UNFCCC_cat, ";")
}

weight_rows <- crt |>
  dplyr::filter(sheet == "Table3.A", variable == "Weight", !is.na(value)) |>
  map_animals()
UNFCCC_table3As2 <- weight_rows |>
  dplyr::transmute(
    Animal_cat,
    Weight = value,
    species,
    UNFCCC_cat,
    Country_ISO_3166_1_A3 = iso3,
    YEAR
  ) |>
  expand_unfccc_categories()

bas1 <- crt |>
  dplyr::filter(sheet == "Table3.B(a)") |>
  dplyr::mutate(
    feature = dplyr::case_when(
      grepl("Population size", variable, ignore.case = TRUE) ~ "Population__size",
      grepl("Allocation by climate region.*Cool", variable, ignore.case = TRUE) ~
        "alloc_clim_reg_Cool",
      grepl("Allocation by climate region.*Temperate", variable, ignore.case = TRUE) ~
        "alloc_clim_reg_Temperate",
      grepl("Allocation by climate region.*Warm", variable, ignore.case = TRUE) ~
        "alloc_clim_reg_Warm",
      grepl("Typical animal mass", variable, ignore.case = TRUE) ~
        "Typical_animal_mass_(average)",
      grepl("VS\\(2\\).*daily excretion", variable, ignore.case = TRUE) ~
        "VS(2)_daily_excretion_(average)",
      grepl("CH4 producing potential \\(Bo\\)", variable, ignore.case = TRUE) ~
        "CH4_producing_potential_(Bo)(2)_(average)",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(feature), !is.na(category), nzchar(category)) |>
  map_animals() |>
  prefer_leaf_categories()

bas1_key <- c("iso3", "YEAR", "Animal_cat", "feature")
bas1_duplicates <- dplyr::filter(
  dplyr::count(bas1, dplyr::across(dplyr::all_of(bas1_key))),
  n > 1
)
if (nrow(bas1_duplicates) > 0) {
  duplicate_rows <- dplyr::inner_join(
    bas1,
    utils::head(bas1_duplicates, 3),
    by = bas1_key
  ) |>
    dplyr::select(
      iso3, YEAR, variable, category, category_code, cell,
      Animal_cat, feature, value
    )
  examples <- utils::capture.output(print(duplicate_rows))
  stop(
    "Table 3.B(a) has repeated animal-category values after hierarchy filtering. ",
    paste(examples, collapse = " ")
  )
}
bas1_values <- bas1 |>
  dplyr::select(iso3, YEAR, Animal_cat, feature, value) |>
  tidyr::pivot_wider(
    names_from = feature,
    values_from = value
  )
bas1_mapping <- bas1 |>
  dplyr::distinct(iso3, YEAR, Animal_cat, species, UNFCCC_cat)
UNFCCC_table3Bas1 <- bas1_values |>
  dplyr::left_join(
    bas1_mapping,
    by = c("iso3", "YEAR", "Animal_cat")
  ) |>
  dplyr::mutate(Country_ISO_3166_1_A3 = iso3) |>
  dplyr::select(-iso3) |>
  expand_unfccc_categories()

population_lookup <- bas1 |>
  dplyr::filter(feature == "Population__size") |>
  dplyr::transmute(
    iso3,
    YEAR,
    Animal_cat,
    Population__size = value
  ) |>
  dplyr::distinct()
if (anyDuplicated(population_lookup[c("iso3", "YEAR", "Animal_cat")])) {
  stop("Table 3.B(a) population data are not unique by animal category.")
}

bas2 <- crt |>
  dplyr::filter(sheet == "Table3.B(a)") |>
  dplyr::mutate(
    mms_match = stringr::str_match(
      variable,
      "^Manure Management Systems \\(b\\) - (.+) - (Allocation|MCF \\(c\\)) - (Cool|Temperate|Warm)$"
    )
  ) |>
  dplyr::filter(!is.na(mms_match[, 1]), !is.na(category), nzchar(category)) |>
  dplyr::mutate(
    MMS_label = gsub("\\s*\\([^)]*\\)", "", mms_match[, 2]),
    MMS = paste0(
      "mms_",
      tolower(gsub("[^[:alnum:]]+", "_", trimws(MMS_label)))
    ),
    climate_region = mms_match[, 4],
    feature = ifelse(mms_match[, 3] == "Allocation", "AWMS", "MCF"),
    Animal_cat = animal_key(category, category_code)
  )
bas2_key <- c("iso3", "YEAR", "Animal_cat", "climate_region", "MMS", "feature")
if (nrow(dplyr::filter(dplyr::count(bas2, dplyr::across(dplyr::all_of(bas2_key))), n > 1)) > 0) {
  stop("Table 3.B(a) has repeated manure-system values.")
}
UNFCCC_table3Bas2 <- bas2 |>
  dplyr::select(iso3, YEAR, Animal_cat, climate_region, MMS, feature, value) |>
  tidyr::pivot_wider(
    names_from = feature,
    values_from = value
  ) |>
  dplyr::left_join(
    population_lookup,
    by = c("iso3", "YEAR", "Animal_cat")
  ) |>
  dplyr::mutate(
    species = animal_species(Animal_cat, NA_character_),
    UNFCCC_cat = animal_unfccc_category(Animal_cat, NA_character_),
    Country_ISO_3166_1_A3 = iso3
  ) |>
  dplyr::select(-iso3) |>
  expand_unfccc_categories()

direct_ief <- crt |>
  dplyr::filter(
    sheet == "Table3.B(b)",
    grepl("Nitrogen excretion per manure management system \\(MMS\\)", variable),
    grepl("^IEF direct N2O", category)
  ) |>
  dplyr::mutate(
    MMS_label = sub("^.*MMS\\)\\s*[^[:alnum:]]+", "", variable, perl = TRUE),
    MMS_label = gsub("\\s*\\([^)]*\\)", "", MMS_label),
    MMS = paste0(
      "mms_",
      tolower(gsub("[^[:alnum:]]+", "_", trimws(MMS_label)))
    )
  )
UNFCCC_table3Bb <- direct_ief |>
  dplyr::transmute(
    MMS,
    EF3 = value,
    Country_ISO_3166_1_A3 = iso3,
    YEAR
  )
if (anyDuplicated(UNFCCC_table3Bb[c("Country_ISO_3166_1_A3", "YEAR", "MMS")])) {
  stop("CRT Table 3.B(b) direct IEFs are not unique by country, year, and MMS.")
}

UNFCCC_table3D <- crt |>
  dplyr::filter(sheet == "Table3.D") |>
  dplyr::transmute(
    variable,
    category_code,
    category,
    value,
    unit = unit_expected,
    status,
    Country_ISO_3166_1_A3 = iso3,
    YEAR
  )
soil_factor_codes <- data.frame(
  category_code = c(
    "3.D.1.a", "3.D.1.b", "3.D.1.d", "3.D.1.c",
    "3.D.1.e", "3.D.1.f", "3.D.2.a", "3.D.2.b"
  ),
  feature = c("EF_SN", "EF_ON", "EF_CR", "EF_PRP", "EF_SOM", "EF_OS", "EF4", "EF5"),
  stringsAsFactors = FALSE
)
soil_ief <- UNFCCC_table3D |>
  dplyr::filter(
    variable == "IMPLIED EMISSION FACTORS -",
    category_code %in% soil_factor_codes$category_code
  ) |>
  dplyr::left_join(soil_factor_codes, by = "category_code") |>
  dplyr::select(Country_ISO_3166_1_A3, YEAR, feature, value)
if (anyDuplicated(soil_ief[c("Country_ISO_3166_1_A3", "YEAR", "feature")])) {
  stop("CRT Table 3.D implied emission factors are not unique.")
}
UNFCCC_EF_managed_soils <- country_year |>
  dplyr::left_join(
    tidyr::pivot_wider(soil_ief, names_from = feature, values_from = value),
    by = c("Country_ISO_3166_1_A3", "YEAR")
  )

# Cell row 52 is the Liquid fuels row immediately below the 1.A.4.c
# Agriculture/forestry/fishing heading in the fixed 2026 CRT sheet template.
fuel_ief <- crt |>
  dplyr::filter(
    sheet == "Table1.A(a)s4",
    cell %in% c("E52", "F52", "G52"),
    category == "Liquid fuels"
  ) |>
  dplyr::mutate(
    feature = dplyr::case_when(
      cell == "E52" ~ "EF_fuel_CO2",
      cell == "F52" ~ "EF_fuel_CH4",
      cell == "G52" ~ "EF_fuel_N2O"
    )
  ) |>
  dplyr::transmute(
    Country_ISO_3166_1_A3 = iso3,
    YEAR,
    variable,
    cell,
    feature,
    value
  )
expected_fuel_gases <- c(E52 = "CO2", F52 = "CH4", G52 = "N2O")
expected_gas <- unname(expected_fuel_gases[fuel_ief$cell])
if (any(!vapply(
  seq_len(nrow(fuel_ief)),
  function(i) grepl(expected_gas[[i]], fuel_ief$variable[[i]], fixed = TRUE),
  logical(1)
))) {
  stop("CRT liquid-fuel factor cells no longer match their expected gases.")
}
if (anyDuplicated(fuel_ief[c("Country_ISO_3166_1_A3", "YEAR", "feature")])) {
  stop("CRT agriculture liquid-fuel factors are not unique.")
}
UNFCCC_EF_fuel <- country_year |>
  dplyr::left_join(
  tidyr::pivot_wider(
    dplyr::select(fuel_ief, -variable, -cell),
    names_from = feature,
    values_from = value
  ),
    by = c("Country_ISO_3166_1_A3", "YEAR")
  )

UNFCCC_data <- list(
  table3As2 = UNFCCC_table3As2,
  table3Bas1 = UNFCCC_table3Bas1,
  table3Bas2 = UNFCCC_table3Bas2,
  table3Bb = UNFCCC_table3Bb,
  table3D = UNFCCC_table3D,
  EF_managed_soils = UNFCCC_EF_managed_soils,
  EF_fuel = UNFCCC_EF_fuel
)
if (!nrow(UNFCCC_data$table3Bb) || !nrow(UNFCCC_data$EF_managed_soils)) {
  stop("CRT extraction produced no manure or managed-soils factors.")
}

usethis::use_data(UNFCCC_data, overwrite = TRUE, compress = "xz")
