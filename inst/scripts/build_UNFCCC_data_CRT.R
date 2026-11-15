if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.")
}

source(file.path("inst", "scripts", "unfccc_mcf_units.R"), local = TRUE)

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
  # CRT footnote markers, e.g. "Bulls (2)" or "Fur-bearing animals (5)".
  category <- gsub("\\s*\\(\\d+\\)", "", category, perl = TRUE)
  trimws(category)
}

cell_row <- function(cell) {
  suppressWarnings(as.integer(sub("^[A-Z]+", "", cell)))
}

species_from_code <- function(category_code) {
  code <- ifelse(is.na(category_code), "", category_code)
  dplyr::case_when(
    grepl("^3\\.[AB]\\.1(\\.|$)", code) ~ "cattle",
    grepl("^3\\.[AB]\\.2(\\.|$)", code) ~ "sheep",
    grepl("^3\\.[AB]\\.3(\\.|$)", code) ~ "swine",
    grepl("^3\\.[AB]\\.4\\.a(\\.|$)", code) ~ "buffalo",
    grepl("^3\\.[AB]\\.4\\.d(\\.|$)", code) ~ "goats",
    grepl("^3\\.[AB]\\.4\\.[ef](\\.|$)", code) ~ "horse",
    grepl("^3\\.[AB]\\.4\\.g(\\.|$)", code) ~ "poultry",
    grepl("^3\\.[AB]\\.4\\.h\\.i$", code) ~ "rabbit",
    TRUE ~ NA_character_
  )
}

species_from_label <- function(category) {
  label <- clean_category(category)
  dplyr::case_when(
    grepl("buffalo", label, ignore.case = TRUE) ~ "buffalo",
    grepl("cattle|\\bcows?\\b|calve|calf|bull|heifer|steer", label, ignore.case = TRUE) ~
      "cattle",
    grepl("sheep|lamb|\\bewes?\\b|\\brams?\\b", label, ignore.case = TRUE, perl = TRUE) ~
      "sheep",
    grepl("swine|\\bsows?\\b|\\bpigs?\\b|piglet|weaner|gilt|guilt|boar", label,
          ignore.case = TRUE, perl = TRUE) ~ "swine",
    grepl("goat", label, ignore.case = TRUE) ~ "goats",
    grepl("horse|\\bmules?\\b|\\basses\\b", label, ignore.case = TRUE, perl = TRUE) ~ "horse",
    grepl("poultry|broiler|turkey|duck|\\bhens?\\b|laying|chicken|goose|geese", label,
          ignore.case = TRUE, perl = TRUE) ~ "poultry",
    grepl("rabbit", label, ignore.case = TRUE) ~ "rabbit",
    TRUE ~ NA_character_
  )
}

animal_key <- function(category, species) {
  label <- clean_category(category)
  label <- gsub("\\s*>\\s*", " over ", label)
  label <- gsub("\\s*<\\s*", " under ", label)
  key <- tolower(gsub("[^[:alnum:]]+", "_", label))
  key <- gsub("^_+|_+$", "", key)
  # Generic labels are only unique within a species.
  generic <- grepl("^(other|other_please_specify|additional_information|unspecified)$", key)
  prefix <- generic & !is.na(species)
  key[prefix] <- paste(species[prefix], key[prefix], sep = "_")
  key
}

# FADN livestock codes used to translate CRT categories ------------------------
livestock <- data_extra$livestock
fadn_species_codes <- function(species) {
  livestock$FADN_code_letter[livestock$species %in% species]
}
buffalo_codes <- c("LBUFDAIRPRS", "LCOWBUFDAIR_X")
cattle_codes <- setdiff(fadn_species_codes("cattle"), buffalo_codes)
non_dairy_cattle_codes <- setdiff(cattle_codes, "LCOWDAIR")
growing_cattle_codes <- c("LBOV1", "LBOV1_2F", "LBOV1_2M", "LHEIFBRE", "LHEIFFAT")
stopifnot(
  all(c(buffalo_codes, growing_cattle_codes, "LCOWDAIR", "LCOWOTH", "LBOV2") %in%
        livestock$FADN_code_letter)
)
fadn_codes <- function(...) paste(c(...), collapse = ";")

# Closest FADN match for each CRT livestock category. Rules are evaluated in
# order within a species; the first matching label pattern wins.
# match_level: 1 = specific FADN category, 2 = group of FADN categories,
# 3 = whole species (used only for FADN codes without a closer CRT match).
generic_label <- "please specify|unspecified|^all\\b|additional information|^other$"
fadn_rules <- tibble::tribble(
  ~species, ~pattern, ~fadn_match, ~FADN_code_letter, ~match_level,
  "buffalo", ".", "buffalo", fadn_codes(buffalo_codes), 1L,
  "cattle", "non.?dairy|non.?lactating", "non_dairy_cattle",
  fadn_codes(non_dairy_cattle_codes), 2L,
  "cattle", "dairy", "LCOWDAIR", "LCOWDAIR", 1L,
  "cattle", "suckler|other cows", "LCOWOTH", "LCOWOTH", 1L,
  "cattle", "bulls?\\s*(under|<)\\s*2", "LBOV1_2M", "LBOV1_2M", 1L,
  "cattle", "bull", "LBOV2", "LBOV2", 1L,
  "cattle", "calve|calf", "LBOV1", "LBOV1", 1L,
  "cattle", "other mature", "other_mature_cattle", fadn_codes("LCOWOTH", "LBOV2"), 2L,
  "cattle", "growing|young", "growing_cattle", fadn_codes(growing_cattle_codes), 2L,
  "cattle", ".", "cattle", fadn_codes(cattle_codes), 3L,
  "sheep", generic_label, "sheep", fadn_codes(fadn_species_codes("sheep")), 3L,
  "sheep", "other mature ewes", "LSHEPOTH", "LSHEPOTH", 1L,
  "sheep", "\\bewes?\\b", "breeding_ewes", fadn_codes("LEWEBRE", "LSHEEPEWE"), 1L,
  "sheep", "lamb|\\brams?\\b|other", "LSHEPOTH", "LSHEPOTH", 1L,
  "sheep", ".", "sheep", fadn_codes(fadn_species_codes("sheep")), 3L,
  "swine", generic_label, "swine", fadn_codes(fadn_species_codes("swine")), 3L,
  "swine", "boar", "LPIGOTH", "LPIGOTH", 1L,
  "swine", "piglet|weaner|<\\s*20|under\\s*20|less than 20", "LPIGLET", "LPIGLET", 1L,
  "swine", "fattening|finishing|market|>\\s*20|20\\s*-\\s*50|>\\s*50|more than 50|young pig",
  "fattening_pigs", fadn_codes("LPIGFAT", "LPIGOTH"), 1L,
  "swine", "\\bsows?\\b|gilt|guilt|breeding", "LSOWBRE", "LSOWBRE", 1L,
  "swine", ".", "swine", fadn_codes(fadn_species_codes("swine")), 3L,
  "goats", ".", "goats", fadn_codes(fadn_species_codes("goats")), 1L,
  "horse", ".", "LEQD", "LEQD", 1L,
  "poultry", "broiler", "LPLTRBROYL", "LPLTRBROYL", 1L,
  "poultry", "\\bhens?\\b|laying|layer", "LHENSLAY", "LHENSLAY", 1L,
  "poultry", "turkey|duck|goose|geese|other poultry", "LPLTROTH", "LPLTROTH", 1L,
  "poultry", ".", "poultry", fadn_codes(fadn_species_codes("poultry")), 3L,
  "rabbit", ".", "rabbit", fadn_codes("LRABBRE", "LRABOTH"), 1L
)

match_fadn <- function(data) {
  label <- clean_category(data$category)
  rule <- rep(NA_integer_, nrow(data))
  for (i in seq_len(nrow(fadn_rules))) {
    hit <- is.na(rule) &
      data$species %in% fadn_rules$species[[i]] &
      grepl(fadn_rules$pattern[[i]], label, ignore.case = TRUE, perl = TRUE)
    rule[hit] <- i
  }
  data$fadn_match <- fadn_rules$fadn_match[rule]
  data$FADN_code_letter <- fadn_rules$FADN_code_letter[rule]
  data$match_level <- fadn_rules$match_level[rule]
  data
}

# Expand to one row per FADN code and, for each country, year and FADN code,
# keep only the closest CRT categories (lowest match_level).
expand_fadn_matches <- function(data) {
  data |>
    tidyr::separate_longer_delim(FADN_code_letter, ";") |>
    dplyr::filter(
      is.na(FADN_code_letter) | match_level == min(match_level),
      .by = c(Country_ISO_3166_1_A3, YEAR, FADN_code_letter)
    )
}

# CRT hierarchy ----------------------------------------------------------------
# Uncoded rows (e.g. DNK "Sows", "Fattening pigs", "Weaners") are listed directly
# below the coded category they disaggregate (e.g. 3.B.3.a). A coded row with
# coded descendants or uncoded children is an aggregate and is dropped so that
# each animal is counted once. The hierarchy is read from the sheet rows shared
# by all variables passed in, because parent cells are not reported for every
# variable (e.g. Bo or VS of 3.B.2.a).
add_category_context <- function(data) {
  keys <- c("iso3", "YEAR", "sheet")
  data <- dplyr::mutate(
    data,
    .row = cell_row(cell),
    .coded = !is.na(category_code) & nzchar(category_code)
  )
  structure <- data |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .row, .coded, category_code) |>
    dplyr::mutate(.last_code = dplyr::if_else(.coded, category_code, NA_character_)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)), .row, dplyr::desc(.coded)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    tidyr::fill(.last_code, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::filter(!.coded) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .row, parent_code = .last_code)
  if (anyDuplicated(structure[c(keys, ".row")])) {
    stop("CRT sheet rows hold several categories; cannot rebuild the hierarchy.")
  }
  data |>
    dplyr::left_join(structure, by = c(keys, ".row")) |>
    dplyr::mutate(
      parent_code = dplyr::if_else(.coded, NA_character_, parent_code),
      species = dplyr::coalesce(
        species_from_code(dplyr::if_else(.coded, category_code, parent_code)),
        species_from_label(category)
      )
    )
}

keep_leaf_categories <- function(data) {
  keys <- c("iso3", "YEAR", "sheet")
  coded <- data |>
    dplyr::filter(.coded) |>
    dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "category_code"))))
  ancestors <- coded |>
    dplyr::mutate(
      category_code = lapply(
        strsplit(category_code, ".", fixed = TRUE),
        function(parts) {
          if (length(parts) < 2) {
            return(character())
          }
          vapply(
            seq_len(length(parts) - 1),
            function(n) paste(parts[seq_len(n)], collapse = "."),
            character(1)
          )
        }
      )
    ) |>
    tidyr::unnest_longer(category_code)
  parents <- dplyr::bind_rows(
    dplyr::semi_join(ancestors, coded, by = c(keys, "category_code")),
    data |>
      dplyr::filter(!.coded, !is.na(parent_code)) |>
      dplyr::distinct(dplyr::across(dplyr::all_of(keys)), category_code = parent_code)
  ) |>
    dplyr::distinct()
  data |>
    dplyr::anti_join(parents, by = c(keys, "category_code")) |>
    dplyr::select(-.row, -.coded)
}

population_rows <- crt |>
  dplyr::filter(
    sheet %in% c("Table3.A", "Table3.B(a)"),
    grepl("Population size", variable, ignore.case = TRUE),
    nzchar(clean_category(category))
  ) |>
  add_category_context() |>
  keep_leaf_categories()

# Species of each CRT label in its country-year hierarchy. Table 3.As2 and
# Table 3.B(a)s2 list labels without codes, so their species is taken from here.
label_species <- population_rows |>
  dplyr::filter(!is.na(species)) |>
  dplyr::transmute(iso3, YEAR, .label = clean_category(category), species) |>
  dplyr::distinct() |>
  dplyr::filter(dplyr::n() == 1L, .by = c(iso3, YEAR, .label)) |>
  dplyr::rename(context_species = species)

add_label_species <- function(data) {
  data |>
    dplyr::mutate(.label = clean_category(category)) |>
    dplyr::left_join(label_species, by = c("iso3", "YEAR", ".label")) |>
    dplyr::mutate(
      species = dplyr::coalesce(context_species, species_from_label(category))
    ) |>
    dplyr::select(-.label, -context_species)
}

weight_rows <- crt |>
  dplyr::filter(
    sheet == "Table3.A", variable == "Weight", !is.na(value),
    nzchar(clean_category(category))
  ) |>
  add_label_species() |>
  dplyr::mutate(Animal_cat = animal_key(category, species)) |>
  match_fadn()
UNFCCC_table3As2 <- weight_rows |>
  dplyr::transmute(
    Animal_cat,
    Weight = value,
    species,
    fadn_match,
    FADN_code_letter,
    match_level,
    Country_ISO_3166_1_A3 = iso3,
    YEAR
  ) |>
  expand_fadn_matches()

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
  dplyr::filter(!is.na(feature), nzchar(clean_category(category))) |>
  add_category_context() |>
  keep_leaf_categories() |>
  dplyr::mutate(Animal_cat = animal_key(category, species)) |>
  match_fadn()

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
  dplyr::distinct(iso3, YEAR, Animal_cat, species, fadn_match, FADN_code_letter, match_level)
if (anyDuplicated(bas1_mapping[c("iso3", "YEAR", "Animal_cat")])) {
  stop("Table 3.B(a) animal categories map to several FADN categories.")
}
UNFCCC_table3Bas1 <- bas1_values |>
  dplyr::left_join(
    bas1_mapping,
    by = c("iso3", "YEAR", "Animal_cat")
  ) |>
  dplyr::mutate(Country_ISO_3166_1_A3 = iso3) |>
  dplyr::select(-iso3) |>
  expand_fadn_matches()

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
  dplyr::filter(!is.na(mms_match[, 1]), nzchar(clean_category(category))) |>
  add_label_species() |>
  dplyr::mutate(
    MMS_label = gsub("\\s*\\([^)]*\\)", "", mms_match[, 2]),
    MMS = paste0(
      "mms_",
      tolower(gsub("[^[:alnum:]]+", "_", trimws(MMS_label)))
    ),
    climate_region = mms_match[, 4],
    feature = ifelse(mms_match[, 3] == "Allocation", "AWMS", "MCF"),
    value = dplyr::if_else(
      feature == "MCF",
      normalize_unfccc_mcf_percent(iso3, value),
      value
    ),
    Animal_cat = animal_key(category, species)
  ) |>
  match_fadn()
bas2_key <- c("iso3", "YEAR", "Animal_cat", "climate_region", "MMS", "feature")
if (nrow(dplyr::filter(dplyr::count(bas2, dplyr::across(dplyr::all_of(bas2_key))), n > 1)) > 0) {
  stop("Table 3.B(a) has repeated manure-system values.")
}
bas2_mapping <- bas2 |>
  dplyr::distinct(iso3, YEAR, Animal_cat, species, fadn_match, FADN_code_letter, match_level)
if (anyDuplicated(bas2_mapping[c("iso3", "YEAR", "Animal_cat")])) {
  stop("Table 3.B(a)s2 animal categories map to several FADN categories.")
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
  dplyr::left_join(
    bas2_mapping,
    by = c("iso3", "YEAR", "Animal_cat")
  ) |>
  dplyr::mutate(Country_ISO_3166_1_A3 = iso3) |>
  dplyr::select(-iso3) |>
  expand_fadn_matches()

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

# Package functions resolve the internal copy, not the exported dataset.
internal_data <- new.env(parent = emptyenv())
internal_names <- load(file.path("R", "sysdata.rda"), envir = internal_data)
internal_data$UNFCCC_data <- UNFCCC_data
save(list = union(internal_names, "UNFCCC_data"),
     file = file.path("R", "sysdata.rda"), envir = internal_data,
     compress = "xz", version = 3)
