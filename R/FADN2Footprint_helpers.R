#' Helpers for FADN2Footprint package:
#' Impute Average Practices using Hierarchical Fallback
#'
#' Calculates weighted averages of crop practices at a primary grouping level (e.g., NUTS2 & Organic).
#' If data is missing (NA) or insufficient at that level, it imputes values from a secondary,
#' coarser grouping level (e.g., Country & Organic).
#'
#' @param data A data.frame or tibble containing the FADN/Practice data.
#' @param target_vars A character vector of column names to calculate averages for (e.g., c("fertilizer_n", "yield")).
#' @param primary_grp A character vector of column names for the fine-scale grouping (e.g., c("NUTS2", "ORGANIC")).
#' @param secondary_grp A character vector of column names for the coarse-scale grouping (e.g., c("COUNTRY", "ORGANIC")).
#' @param weight_var (Optional) Character string of the weighting variable (e.g., "SYS02"). If NULL, simple mean is used.
#'
#' @return A data frame containing the primary grouping columns and the imputed average practices.
#' @import dplyr
#'
#' @concept utils
#' @export

h_average_practices <- function(data,
                                target_vars,
                                primary_grp,
                                secondary_grp,
                                weight_var = NULL) {

  require(dplyr)

  # Check that target variables are in the data set
  target_vars_checked = intersect(target_vars,names(data))

  if (length(target_vars) != length(target_vars_checked)) {
    print(paste0(setdiff(target_vars,target_vars_checked), collapse = ", "),
          " variables are not present in the data set and will not be used.")
  }

  # Keep only variable of interest
  data <- data |>
    dplyr::select(dplyr::all_of(c(target_vars_checked,
                                  primary_grp,
                                  secondary_grp,
                                  weight_var)))

  # 1. Helper function for Weighted Mean
  calc_mean <- function(x, w) {
    if (is.null(w)) {
      mean(x, na.rm = TRUE)
    } else {
      # Handle cases where sum of weights is 0 or all values are NA
      if (all(is.na(x)) || sum(w[!is.na(x)], na.rm = TRUE) == 0) {
        return(NA_real_)
      }
      weighted.mean(x, w, na.rm = TRUE)
    }
  }

  # 2. Create the Hierarchy Map
  # We need to know which Secondary keys belong to which Primary keys
  # (e.g., that NUTS2 "FR10" belongs to COUNTRY "FRA")
  hierarchy_map <- data |>
    dplyr::select(dplyr::all_of(unique(c(primary_grp, secondary_grp)))) |>
    dplyr::distinct()

  # 3. Calculate Primary Level Averages
  #message("Calculating averages at Primary level...")
  primary_stats <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(primary_grp))) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(target_vars_checked),
      ~ calc_mean(., if(!is.null(weight_var)) .data[[weight_var]] else NULL)
    ), .groups = "drop")

  # 4. Calculate Secondary Level Averages
  #message("Calculating averages at Secondary level (fallback)...")
  secondary_stats <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(secondary_grp))) %>%
    dplyr::summarise(dplyr::across(
      dplyr::all_of(target_vars_checked),
      ~ calc_mean(., if(!is.null(weight_var)) .data[[weight_var]] else NULL)
    ), .groups = "drop") %>%
    dplyr::rename_with(~ paste0(., "_fallback"), .cols = dplyr::all_of(target_vars_checked))

  # 5. Merge and Impute
  #message("Imputing missing values...")

  # Join primary stats with the hierarchy map to ensure we have the secondary keys available for joining
  result <- primary_stats %>%
    dplyr::left_join(hierarchy_map, by = primary_grp) %>%
    # Join with secondary stats based on the secondary grouping keys
    dplyr::left_join(secondary_stats, by = secondary_grp)

  # Perform the Coalesce (Imputation)
  for (var in target_vars_checked) {
    fallback_col <- paste0(var, "_fallback")

    # Overwrite the original column: if NA, take the fallback
    result[[var]] <- coalesce(result[[var]], result[[fallback_col]])
  }

  # 6. Cleanup
  # Remove the fallback columns and return just the Primary keys + Calculated vars
  final_output <- result %>%
    select(all_of(primary_grp), all_of(target_vars_checked))

  return(final_output)
}

# Dictionary builder ----

#' Create a template Excel file for national-to-EU FADN dictionary
#'
#' @description
#' Generates a pre-populated Excel workbook the user must fill to define the
#' mapping between a national FADN and the EU FADN standard. The workbook
#' contains a \code{variable_pattern} sheet (suffix mapping that drives the
#' pivot), a \code{category_pattern} sheet (product code mapping), a
#' \code{manual_matches} sheet (farm-level variables), and reference sheets.
#'
#' @param output_path Character. File path for the output Excel workbook.
#' @param raw_tables Named list of data frames — the raw national FADN tables.
#' @param id_cols Character vector. Column name(s) identifying individual farms
#'   (e.g., \code{c("IDENT")}). These columns are preserved as-is in the output
#'   and used as grouping keys during pivoting.
#' @param table_config Named list mapping each product group to its table name
#'   and code column. Names must be \code{"CROPS"}, \code{"LIVESTOCK"},
#'   \code{"ANIMAL PRODUCTS AND SERVICES"}. Each element is a list with
#'   \code{table} (name in \code{raw_tables}) and \code{code_col} (column name
#'   holding the product code).
#' @param data_extra List with elements \code{crops}, \code{livestock},
#'   \code{output}, each containing columns \code{FADN_code_letter} and the
#'   column named by \code{national_code_col}.
#' @param national_code_col Character. Column name in \code{data_extra} tables
#'   that holds the national product codes.
#' @param overwrite Logical. Overwrite existing file? Default \code{FALSE}.
#'
#' @return Invisibly returns the output path.
#'
#' @concept utils
#' @export
h_create_dictionary_template <- function(output_path = "dictionary_conversion.xlsx",
                                         raw_tables,
                                         id_cols = "IDENT",
                                         table_config,
                                         data_extra,
                                         national_code_col = "RICA_code_number",
                                         overwrite = FALSE) {

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. Install it with install.packages('openxlsx').")
  }

  if (file.exists(output_path) && !overwrite) {
    cat("File already exists at '", output_path,
        "'. Set overwrite = TRUE or choose a different path.")
  } else {

    wb <- openxlsx::createWorkbook()

    # ── Instructions sheet ──
    openxlsx::addWorksheet(wb, "instructions")
    instructions <- data.frame(
      Step = 1:6,
      Action = c(
        "Open the 'variable_pattern' sheet. For each FADN suffix (pattern_FADN), fill in the corresponding national suffix in 'pattern_national' and its description in 'description_national'. Complete the unit_conversion column with 1 if both FADN and national variable share the same unit. Leave 'pattern_national' empty if no national equivalent exists.",
        "The 'Group' column determines which raw table and code column is used for pivoting: CROPS = veg table, LIVESTOCK = ani table, ANIMAL PRODUCTS AND SERVICES = pan table.",
        "Open the 'category_pattern' sheet. Verify pre-filled national-to-FADN product code mappings. Add 'allocation_key' if one national code maps to multiple FADN codes (values must sum to 1).",
        "Open the 'manual_matches' sheet. Add farm-level variables (not product-level) that need renaming/converting.",
        "Save the file and run validate_dictionary() to check for errors.",
        "Run read_dictionary() then pivot_national_fadn() to convert your data."
      )
    )
    openxlsx::writeDataTable(wb, "instructions", instructions)

    # ── Variable pattern sheet ──
    openxlsx::addWorksheet(wb, "variable_pattern")

    # Collect FADN suffixes from internal reference (or build from known list)
    fadn_suffixes <- .get_fadn_variable_suffixes()

    # Collect national suffixes per group
    national_suffixes_list <- lapply(names(table_config), function(grp) {
      cfg <- table_config[[grp]]
      tbl <- raw_tables[[cfg$table]]
      suffixes <- setdiff(colnames(tbl), c(id_cols, cfg$code_col))
      if (length(suffixes) == 0) {
        return(data.frame(Group = character(0), national_suffix = character(0), stringsAsFactors = FALSE))
      }
      data.frame(
        Group = grp,
        national_suffix = suffixes,
        stringsAsFactors = FALSE
      )
    })
    national_suffixes_df <- do.call(rbind, national_suffixes_list)

    # Build the template: one row per FADN suffix × group, with national suffixes
    # listed as a reference comment
    vp_template <- fadn_suffixes %>%
      dplyr::mutate(
        pattern_national = NA_character_,
        description_national = NA_character_,
        comment_national = NA_character_,
        unit_conversion = NA_real_

      )

    # Add a comment column with available national suffixes per group
    vp_template <- vp_template %>%
      dplyr::left_join(
        national_suffixes_df %>%
          dplyr::summarise(
            available_national_suffixes = paste(sort(national_suffix), collapse = ", "),
            .by = Group
          ),
        by = "Group"
      )

    openxlsx::writeDataTable(wb, "variable_pattern", vp_template)

    # ── Category pattern sheet ──
    openxlsx::addWorksheet(wb, "category_pattern")

    group_map <- c(crops = "CROPS", livestock = "LIVESTOCK", output = "ANIMAL PRODUCTS AND SERVICES")
    cat_pattern <- lapply(names(group_map), function(nm) {
      tbl <- data_extra[[nm]]
      if (is.null(tbl)) return(NULL)
      tbl %>%
        dplyr::select(FADN_code_letter, national_code = !!rlang::sym(national_code_col)) %>%
        tidyr::separate_longer_delim(national_code, ";") %>%
        dplyr::mutate(
          Group = group_map[[nm]],
          allocation_key = NA_real_
        )
    })
    cat_pattern <- do.call(rbind, cat_pattern)

    # Add national codes found in data but not in data_extra
    for (grp in names(table_config)) {
      cfg <- table_config[[grp]]
      tbl <- raw_tables[[cfg$table]]
      data_codes <- unique(as.character(tbl[[cfg$code_col]]))
      mapped_codes <- cat_pattern %>%
        dplyr::filter(Group == grp) %>%
        dplyr::pull(national_code) %>%
        unique()
      missing <- setdiff(data_codes, mapped_codes)
      if (length(missing) > 0) {
        cat_pattern <- rbind(
          cat_pattern,
          data.frame(
            FADN_code_letter = NA_character_,
            national_code = missing,
            allocation_key = NA_real_,
            Group = grp,
            stringsAsFactors = FALSE
          )
        )
      }
    }

    openxlsx::writeDataTable(wb, "category_pattern", cat_pattern)

    # ── Manual matches sheet ──
    openxlsx::addWorksheet(wb, "manual_matches")
    manual_template <- data.frame(
      fadn_variable = c('ORGANIC', 'IGRFEDCNCTRPUR_V', 'IGRFEDCRSPUR_V', 'IGRFEDCNCTRPUR_V', 'IGRFEDCRSPUR_V', 'NUTS3', 'SIZC', 'IFULS_Q', 'IFULS_V', 'IHFULS_Q', 'IHFULS_V', 'IPROT_V', 'IKUSE_Q', 'INUSE_Q', 'IPUSE_Q', 'IELE_V', 'IELE_Q', 'SYS02', 'ID', 'YEAR', 'NUTS1', 'TF_PART3', 'TF_SUBP4', 'TF', 'TF8', 'TF14', 'TF_PRIN2', 'IPIGFEDPUR_V', 'NUTS2', 'IRRAA_X', 'SE025', 'SIRET', 'SE080', 'IPLTRFEDPUR_V'),
      description_fadn = c('Organic farming Code', 'Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value', 'Purchased coarse fodder for grazing stock (equines, ruminants) Value', 'Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value', 'Purchased coarse fodder for grazing stock (equines, ruminants) Value', 'NUTS NUTS3', 'Economic size class (cf. EU typology)', 'NA', 'Motor fuels and lubricants Value', 'NA', 'Heating fuels Value', 'Crop protection products Value', 'Quantity of K2O used in mineral fertilisers Quantity', 'Quantity of N used in mineral fertilisers Quantity', 'Quantity of P2O5 used in mineral fertilisers Quantity', 'Farming overheads. Electricity. Value', 'NA', 'Farms represented', 'Unique Identifier', 'Year', 'NUTS NUTS1', 'Particular TF (3 digits) Typology', 'Subparticular TF (4 digits)', 'Subd/Part TF (3 digits + 0)', 'Type of Farming (8)', 'Type of Farming (14)', 'Principal TF (2 digits)', 'Purchased feedstuffs for pigs Value', 'NUTS NUTS2', 'UAA under irrigation', 'Total Utilised Agricultural Area', 'French firm identifier', 'Total livestock units', 'Purchased feedstuffs for poultry and other small animals Value'),
      comment_fadn = c('From 2000', 'in EUR', 'in EUR', 'in EUR', 'in EUR', 'estimate by DG AGRI based on geographic information of farm', 'Calculated by DG AGRI', 'NA', 'in EUR', 'NA', 'in EUR', 'in EUR', 'in tonnes', 'in tonnes', 'in tonnes', 'in EUR', 'NA', 'number of farm represented in the population', 'Anonymous code allowing constant sample', 'in kWh', 'estimate by DG AGRI based on geographic information of farm', '2-digit Calculated by DG AGRI (cf Typology Regulation)', 'accounting year', 'NA', 'NA', 'NA', '4-digit Calculated by DG AGRI (cf Typology Regulation)', 'in EUR', 'estimate by DG AGRI based on geographic information of farm', 'in ha', 'Area in ha', 'NA', 'in Livestock unit (conversion coefficients see RICC 1750)', 'in EUR'),
      national_variable = NA_character_,
      description_national = NA_character_,
      comment_national = NA_character_,
      fadn_table = c('GENERAL INFORMATION', 'INPUTS', 'INPUTS', 'INPUTS', 'INPUTS', 'GENERAL INFORMATION', 'GENERAL INFORMATION', 'French RICA', 'INPUTS', 'French RICA', 'INPUTS', 'INPUTS', 'INPUTS', 'INPUTS', 'INPUTS', 'INPUTS', 'French RICA', 'REFERENCE', 'REFERENCE', 'FARM CLASSIFICATION', 'GENERAL INFORMATION', 'FARM CLASSIFICATION', 'FARM CLASSIFICATION', 'GENERAL INFORMATION', 'GENERAL INFORMATION', 'GENERAL INFORMATION', 'FARM CLASSIFICATION', 'INPUTS', 'GENERAL INFORMATION', 'GENERAL INFORMATION', 'STANDARD RESULTS', 'French RICA', 'STANDARD RESULTS', 'INPUTS'),
      national_table = NA_character_,
      unit_conversion = NA_integer_,
      notes = NA_character_
    )
    openxlsx::writeDataTable(wb, "manual_matches", manual_template)

    # Save
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    openxlsx::saveWorkbook(wb, output_path, overwrite = overwrite)

    message("Dictionary template created at: ", output_path)
    invisible(output_path)
  }
}


#' Internal: Known FADN variable suffixes by group
#' @noRd
.get_fadn_variable_suffixes <- function() {
  tibble::tribble(
    ~pattern_FADN, ~description_FADN,                     ~comment_fadn,                              ~Group,
    # ANIMAL PRODUCTS AND SERVICES
    "PRQ",  "Production quantity",                "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "OQ",   "Opening quantity",                   "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "CQ",   "Closing quantity",                   "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "SQ",   "Sales quantity",                     "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "TO",   "Total output. Value",                "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    "OV",   "Opening value",                      "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    "CV",   "Closing value",                      "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    "SV",   "Sales value",                        "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    "FCQ",  "Farm consumption. Quantity",          "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "FCV",  "Farm consumption. Value",             "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    "FUQ",  "Farm use. Quantity",                  "in tonnes",                            "ANIMAL PRODUCTS AND SERVICES",
    "FUV",  "Farm use. Value",                     "in EUR",                               "ANIMAL PRODUCTS AND SERVICES",
    # CROPS
    "PRQ",  "Prod quantity",                      "in tonnes",                            "CROPS",
    "SQ",   "Sales quantity",                     "in tonnes",                            "CROPS",
    "A",    "AA",                                 "in ha",                                "CROPS",
    "TA",   "Total area under production",        "in ha",                                "CROPS",
    "IRA",  "irr.AA",                             "in ha. Available up to 2013",          "CROPS",
    "IRTA", "Irrigated total area under prod",    "in ha",                                "CROPS",
    "FCV",  "Farm consumption",                   "in EUR",                               "CROPS",
    "FUV",  "Farm use",                           "in EUR",                               "CROPS",
    "TO",   "Total output",                       "in EUR",                               "CROPS",
    "OV",   "Opening value",                      "in EUR",                               "CROPS",
    "CV",   "Closing value",                      "in EUR",                               "CROPS",
    "SV",   "Sales value",                        "in EUR",                               "CROPS",
    "ENA",  "EC/AA",                              "in ha",                                "CROPS",
    "ENTA", "Energy crop total area under prod",  "in ha",                                "CROPS",
    "GM",   "GMO",                                "in ha",                                "CROPS",
    # LIVESTOCK
    "AN",   "Average number",                     "Average number",                       "LIVESTOCK",
    "PN",   "Purchase number",                    "number of animals purchased",          "LIVESTOCK",
    "ON",   "Opening number",                     "number of animals",                    "LIVESTOCK",
    "CN",   "Closing number",                     "number of animals",                    "LIVESTOCK",
    "SN",   "Sales. Number",                      "number of animals sold",               "LIVESTOCK",
    "PV",   "Purchase value",                     "value (EUR) of animals purchased",     "LIVESTOCK",
    "TO",   "Total output",                       "in EUR",                               "LIVESTOCK",
    "OV",   "Opening value",                      "in EUR",                               "LIVESTOCK",
    "CV",   "Closing value",                      "in EUR",                               "LIVESTOCK",
    "SV",   "Sales. Value",                       "value (EUR) of animals sold",          "LIVESTOCK",
    "ALU",  "Livestock unit",                     "Based on the average number - fractional", "LIVESTOCK",
    "PI",   "Price index",                        "Regional price index (REGIDX##)",      "LIVESTOCK",
    "SSN",  "Sales for slaughtering. Number",     "Number",                               "LIVESTOCK",
    "SSV",  "Sales for slaughtering. Value",      "in EUR",                               "LIVESTOCK",
    "SUN",  "Sales to unknown destination. Number","Number",                              "LIVESTOCK",
    "SUV",  "Sales to unknown destination. Value", "in EUR",                              "LIVESTOCK",
    "SRN",  "Sales for rearing. Number",          "Number",                               "LIVESTOCK",
    "SRV",  "Sales for rearing. Value",           "in EUR",                               "LIVESTOCK",
    "NO",   "Net output",                         "Net output = sales + farmhouse",       "LIVESTOCK",
    "FCN",  "Farm consumption. Number",           "",                                     "LIVESTOCK",
    "FCV",  "Farm consumption. Value",            "",                                     "LIVESTOCK",
    "FUN",  "Farm use. Number",                   "",                                     "LIVESTOCK",
    "FUV",  "Farm use. Value",                    "",                                     "LIVESTOCK"
  )
}

#' Read a filled conversion dictionary from Excel
#'
#' @param dictionary_path Path to the filled Excel workbook.
#'
#' @return A list with elements:
#' \describe{
#'   \item{variable_pattern}{Tibble of suffix mappings by group}
#'   \item{category_pattern}{Tibble of product code mappings}
#'   \item{manual_matches}{Tibble of farm-level variable mappings}
#'   \item{unit_conversion}{Tibble of code-specific unit conversions}
#' }
#'
#' @concept utils
#' @export
h_read_dictionary <- function(dictionary_path) {

  variable_pattern <- readxl::read_excel(dictionary_path, sheet = "variable_pattern") %>%
    dplyr::filter(!is.na(pattern_FADN) | !is.na(pattern_national))

  category_pattern <- readxl::read_excel(dictionary_path, sheet = "category_pattern") %>%
    dplyr::filter(!is.na(FADN_code_letter) & !is.na(national_code))

  manual_matches <- tryCatch(
    readxl::read_excel(dictionary_path, sheet = "manual_matches"),
    error = function(e) {
      message("No 'manual_matches' sheet found. Skipping.")
      tibble::tibble()
    }
  )

  structure(
    list(
      variable_pattern = variable_pattern,
      category_pattern = category_pattern,
      manual_matches   = manual_matches
    ),
    class = "fadn_dictionary"
  )
}

#' Validate a conversion dictionary
#'
#' @param dictionary_path Path to the filled Excel workbook.
#' @param raw_tables Named list of raw national FADN data frames.
#' @param table_config Named list mapping product groups to table/code_col.
#' @param verbose Logical. Print report? Default \code{TRUE}.
#'
#' @return A list of issues (invisibly).
#'
#' @concept utils
#' @export
h_validate_dictionary <- function(dictionary_path,
                                  raw_tables = NULL,
                                  id_cols = "IDENT",
                                  table_config = NULL,
                                  verbose = TRUE) {

  dict <- h_read_dictionary(dictionary_path)
  issues <- list()

  # 1. Check variable_pattern has required columns
  vp_required <- c("pattern_FADN", "Group", "pattern_national")
  missing_cols <- setdiff(vp_required, colnames(dict$variable_pattern))
  if (length(missing_cols) > 0) {
    issues$missing_vp_columns <- missing_cols
  }

  # 2. Check valid groups

  valid_groups <- c("CROPS", "LIVESTOCK", "ANIMAL PRODUCTS AND SERVICES")
  bad_groups <- setdiff(
    unique(c(dict$variable_pattern$Group, dict$category_pattern$Group)),
    valid_groups
  )
  if (length(bad_groups) > 0) {
    issues$invalid_groups <- bad_groups
  }

  # 3. Check for duplicate suffix mappings within group
  vp_filled <- dict$variable_pattern %>%
    dplyr::filter(!is.na(pattern_national))
  dupes <- vp_filled %>%
    dplyr::count(Group, pattern_national) %>%
    dplyr::filter(n > 1)
  if (nrow(dupes) > 0) {
    issues$duplicate_national_suffixes <- dupes
  }

  # 4. Check national suffixes exist in data
  if (!is.null(raw_tables) && !is.null(table_config)) {
    group_table_map <- list(
      CROPS = table_config$CROPS,
      LIVESTOCK = table_config$LIVESTOCK,
      `ANIMAL PRODUCTS AND SERVICES` = table_config$`ANIMAL PRODUCTS AND SERVICES`
    )

    unknown_suffixes <- list()
    for (grp in names(group_table_map)) {
      cfg <- group_table_map[[grp]]
      if (is.null(cfg)) next
      tbl <- raw_tables[[cfg$table]]
      available <- setdiff(colnames(tbl), c(id_cols, cfg$code_col))
      declared <- vp_filled %>%
        dplyr::filter(Group == grp) %>%
        dplyr::pull(pattern_national)
      missing <- setdiff(declared, available)
      if (length(missing) > 0) {
        unknown_suffixes[[grp]] <- missing
      }
    }
    if (length(unknown_suffixes) > 0) {
      issues$unknown_national_suffixes <- unknown_suffixes
    }

    # 5. Check unmapped national codes in data
    unmapped <- list()
    for (grp in names(group_table_map)) {
      cfg <- group_table_map[[grp]]
      if (is.null(cfg)) next
      tbl <- raw_tables[[cfg$table]]
      data_codes <- unique(as.character(tbl[[cfg$code_col]]))
      mapped_codes <- dict$category_pattern %>%
        dplyr::filter(Group == grp) %>%
        dplyr::pull(national_code) %>%
        unique()
      miss <- setdiff(data_codes, mapped_codes)
      if (length(miss) > 0) {
        unmapped[[grp]] <- miss
      }
    }
    if (length(unmapped) > 0) {
      issues$unmapped_national_codes <- unmapped
    }
  }

  # 6. Check allocation keys sum to 1
  if ("allocation_key" %in% colnames(dict$category_pattern)) {
    alloc <- dict$category_pattern %>%
      dplyr::filter(!is.na(allocation_key)) %>%
      dplyr::summarise(total = sum(allocation_key), .by = national_code) %>%
      dplyr::filter(abs(total - 1) > 1e-6)
    if (nrow(alloc) > 0) {
      issues$bad_allocation_keys <- alloc
    }
  }

  # Report
  if (verbose) {
    cat("══════════════════════════════════════════════════════\n")
    cat("  Dictionary Validation Report\n")
    cat("══════════════════════════════════════════════════════\n")
    if (length(issues) == 0) {
      cat("  \u2713 No issues found. Dictionary looks good!\n")
    } else {
      for (nm in names(issues)) {
        cat("\n  \u2717", nm, ":\n")
        print(issues[[nm]])
      }
    }
    cat("══════════════════════════════════════════════════════\n")
  }

  invisible(issues)
}

#' Pivot and convert a national FADN to EU FADN format
#'
#' @param raw_tables Named list of raw national FADN data frames.
#' @param dict_conversion A \code{fadn_dictionary} object from \code{read_dictionary()}.
#' @param table_config Named list mapping product groups to table/code_col.
#' @param id_cols Character vector. Column name(s) identifying individual farms
#'   (e.g., \code{c("IDENT")}). These columns are preserved as-is in the output
#'   and used as grouping keys during pivoting.
#'
#' @return A single wide data frame in EU FADN format (one row per farm).
#'
#' @concept utils
#' @export
h_pivot_national_fadn <- function(raw_tables,
                                  dict_conversion,
                                  table_config,
                                  id_cols = "IDENT") {

  vp <- dict_conversion$variable_pattern %>%
    dplyr::filter(!is.na(pattern_national) & !is.na(pattern_FADN))
  cp <- dict_conversion$category_pattern %>%
    dplyr::filter(!is.na(FADN_code_letter) & !is.na(national_code))
  mm <- dict_conversion$manual_matches

  # ── Pivot each product group ──
  pivoted_tables <- list()

  for (grp in names(table_config)) {
    cfg <- table_config[[grp]]
    tbl <- raw_tables[[cfg$table]]
    code_col <- cfg$code_col

    # Check that id_cols exist in this table
    missing_ids <- setdiff(id_cols, colnames(tbl))
    if (length(missing_ids) > 0) {
      stop("id_cols not found in table '", cfg$table, "': ",
           paste(missing_ids, collapse = ", "))
    }

    # Get prefixes for this group
    grp_prefixes <- cp %>%
      dplyr::filter(Group == grp)

    national_cat <- intersect(grp_prefixes$national_code, unique(as.character(tbl[[code_col]])))

    if (length(national_cat) == 0) {
      message("No matched category for group '", grp, "'. Skipping.")
      next
    }

    # Get suffixes for this group
    grp_suffixes <- vp %>%
      dplyr::filter(Group == grp)

    national_cols <- intersect(grp_suffixes$pattern_national, colnames(tbl))

    if (length(national_cols) == 0) {
      message("No matched suffixes for group '", grp, "'. Skipping.")
      next
    }



    # Grouping columns: id_cols + code_col
    group_cols <- c(id_cols, code_col)
    value_cols <- setdiff(
      names(tbl |> dplyr::select(dplyr::where(is.numeric))),
      group_cols
    )

    # Pivot longer
    tbl_long <- tbl |>
      # sum values if additional character columns besides group_cols
      dplyr::summarise(
        dplyr::across(dplyr::all_of(value_cols),
                      ~ sum(.x, na.rm = TRUE)),
        .by = dplyr::all_of(group_cols)
      ) |>
      # pivot
      tidyr::pivot_longer(
        cols = dplyr::all_of(value_cols),
        names_to = "pattern_national",
        values_to = "value"
      ) |>
      dplyr::mutate(
        !!code_col := as.character(.data[[code_col]])
      ) |>
      # add FADN prefix and suffix
      dplyr::left_join(
        grp_prefixes |>
          dplyr::select(national_code, FADN_code_letter, allocation_key) |>
          dplyr::mutate(allocation_key = ifelse(is.na(allocation_key), 1, allocation_key)) |>
          dplyr::distinct(),
        by = setNames("national_code", code_col)
      ) |>
      dplyr::left_join(
        grp_suffixes |>
          dplyr::select(pattern_national, pattern_FADN, unit_conversion) |>
          dplyr::mutate(unit_conversion = ifelse(is.na(unit_conversion), 1, unit_conversion)) |>
          dplyr::distinct(),
        by = "pattern_national"
      )


    # Sum across
    tbl_summed <- tbl_long |>
      dplyr::filter(!is.na(pattern_FADN) & !is.na(FADN_code_letter)) |>
      dplyr::summarise(
        value = sum(value * allocation_key * unit_conversion, na.rm = TRUE),
        .by = c(id_cols, 'FADN_code_letter', 'pattern_FADN')
      )


    # Pivot wide
    tbl_wide <- tbl_summed %>%
      tidyr::pivot_wider(
        id_cols     = dplyr::all_of(id_cols),
        names_from  = c('FADN_code_letter', 'pattern_FADN'),
        values_from = value,
        values_fill = NA_real_
      )

    pivoted_tables[[grp]] <- tbl_wide

  }

  # ── Join all product tables ──
  if (length(pivoted_tables) == 0) {
    stop("No product tables were successfully pivoted.")
  }

  result <- pivoted_tables[[1]]
  if (length(pivoted_tables) > 1) {
    for (k in 2:length(pivoted_tables)) {
      result <- dplyr::left_join(result, pivoted_tables[[k]], by = id_cols)
    }
  }

  # ── Add manual matches (farm-level variables) ──
  if (!is.null(mm) && nrow(mm) > 0) {

    # Collect all missing variables for a single summary warning
    vars_not_found <- character(0)

    #farm_tables <- names(raw_tables)[grepl(paste(unique(mm$national_table), collapse = "|"),names(raw_tables))]
    farm_tables <- unique(mm$national_table)

    for (ft in farm_tables) {
      if (!ft %in% names(raw_tables)) {
        warning("Table '", ft, "' referenced in manual_matches not found. Skipping.")
        next
      }
      ft_data <- raw_tables[[ft]]

      # Check id_cols exist in farm table
      missing_ids <- setdiff(id_cols, colnames(ft_data))
      if (length(missing_ids) > 0) {
        warning("id_cols not found in table '", ft, "': ",
                paste(missing_ids, collapse = ", "), ". Skipping.")
        next
      }

      ft_matches <- mm %>% dplyr::filter(national_table == ft)


      for (i in seq_len(nrow(ft_matches))) {
        nat_var <- ft_matches$national_variable[i]
        fadn_var <- ft_matches$fadn_variable[i]
        conv <- if ("unit_conversion" %in% colnames(ft_matches) &&
                    is.finite(ft_matches$unit_conversion[i])) {
          ft_matches$unit_conversion[i]
        } else {
          1
        }

        # Skip if the FADN variable is already an id column (redundant mapping)
        if (fadn_var %in% id_cols) next

        if (!nat_var %in% colnames(ft_data)) {
          vars_not_found <- c(vars_not_found, paste0(nat_var, " (", ft, ")"))
          next
        }

        src_col <- ft_data[[nat_var]]
        is_char <- is.character(src_col) || is.factor(src_col)

        # Test if supposedly numeric column is actually coercible
        if (!is_char) {
          coerced <- suppressWarnings(as.numeric(src_col))
          if (all(is.na(coerced) | is.na(src_col))) {
            is_char <- TRUE
          }
        }

        vals <- ft_data %>%
          dplyr::select(dplyr::all_of(c(id_cols, nat_var)))

        if (is_char) {
          # Character/factor: take first non-NA value per group
          vals <- vals %>%
            dplyr::summarise(
              !!fadn_var := dplyr::first(as.character(.data[[nat_var]])[
                !is.na(.data[[nat_var]])
              ]),
              .by = dplyr::all_of(id_cols)
            )
        } else {
          # Numeric: sum with unit conversion
          vals <- vals %>%
            dplyr::summarise(
              !!fadn_var := sum(as.numeric(.data[[nat_var]]) * conv, na.rm = TRUE),
              .by = dplyr::all_of(id_cols)
            )
        }

        result <- dplyr::left_join(result, vals, by = id_cols)
      }
    }

    # Single summary warning for all missing variables
    if (length(vars_not_found) > 0) {
      warning("The following variables were not found and were skipped:\n  ",
              paste(vars_not_found, collapse = "\n  "))
    }
  }

  message("Conversion complete: ", nrow(result), " farm-year obs x ",
          ncol(result), " variables.")
  result
}


#' Stack multiple years of national FADN raw data into combined tables
#'
#' National FADN data typically come as separate files per year. This helper
#' binds them into a single named list of data frames (one per table type),
#' adding a \code{YEAR} column so that \code{pivot_national_fadn()} can
#' distinguish years via \code{id_cols}.
#'
#' @param yearly_data A named list where each element is itself a named list
#'   of data frames for one year (i.e., the structure you get from reading
#'   one year of raw files). Names should be year labels (e.g., \code{"2018"}).
#' @param year_col Character. Name of the year column to create.
#'   Defaults to \code{"YEAR"}.
#'
#' @return A named list of data frames (one per table type), each containing
#'   all years stacked with an added \code{year_col} column.
#'
#' @examples
#' \dontrun{
#' yearly_data <- list(
#'   "2018" = list(rica = rica18, veg = veg18, ani = ani18, pan = pan18),
#'   "2019" = list(rica = rica19, veg = veg19, ani = ani19, pan = pan19),
#'   "2020" = list(rica = rica20, veg = veg20, ani = ani20, pan = pan20)
#' )
#' raw_tables <- stack_yearly_tables(yearly_data)
#' }
#'
#' @concept utils
#' @export
h_stack_yearly_tables <- function(yearly_data, year_col = "YEAR") {

  if (!is.list(yearly_data) || is.null(names(yearly_data))) {
    stop("`yearly_data` must be a named list (names = year labels).")
  }

  # Discover all tables names across list
  all_tables_names <- unique(unlist(lapply(yearly_data, names)))

  result <- stats::setNames(
    vector("list", length(all_tables_names)),
    all_tables_names
  )


  for (tbl_name in all_tables_names) {
    frames <- list()
    for (yr in names(yearly_data)) {
      if (tbl_name %in% names(yearly_data[[yr]])) {
        df <- yearly_data[[yr]][[tbl_name]]
        df[[year_col]] <- as.numeric(yr)
        frames <- c(frames, list(df))
      }
    }
    result[[tbl_name]] <- dplyr::bind_rows(frames)
    message("  ", tbl_name, ": ", nrow(result[[tbl_name]]), " rows (",
            length(frames), " years)")
  }

  message("Stacked ", length(all_tables_names), " tables across ",
          length(yearly_data), " years.")
  result
}

#' Stack multiple countries of EU FADN data into combined tables
#'
#' After stacking years with \code{stack_yearly_tables()}, this function
#' binds country-level tables into a single named list of data frames,
#' adding a \code{country_col} column so that downstream functions can
#' distinguish countries.
#'
#' @param country_data A named list where each element is itself a named list
#'   of data frames for one country. Names should be country codes
#'   (e.g., \code{"FR"}, \code{"DE"}, \code{"IT"}).
#'   Each country element can already contain a \code{YEAR} column from
#'   prior use of \code{stack_yearly_tables()}.
#' @param country_col Character. Name of the country column to create.
#'   Defaults to \code{"COUNTRY"}.
#'
#' @return A named list of data frames (one per table type), each containing
#'   all countries (and years if previously stacked) with an added
#'   \code{country_col} column.
#'
#' @examples
#' \dontrun{
#' # 1. Stack years within each country
#' FR_tables <- stack_yearly_tables(
#'   list("2018" = FR_18_raw, "2019" = FR_19_raw, "2020" = FR_20_raw),
#'   year_col = "YEAR"
#' )
#' DE_tables <- stack_yearly_tables(
#'   list("2018" = DE_18_raw, "2019" = DE_19_raw),
#'   year_col = "YEAR"
#' )
#'
#' # 2. Stack countries
#' FADN_raw <- stack_country_tables(
#'   list("FR" = FR_tables, "DE" = DE_tables),
#'   country_col = "COUNTRY"
#' )
#' }
#'
#' @concept utils
#' @export
h_stack_country_tables <- function(country_data, country_col = "COUNTRY") {

  if (!is.list(country_data) || is.null(names(country_data))) {
    stop("`country_data` must be a named list (names = country codes).")
  }

  # Discover all table names across countries

  all_table_names <- names(country_data)

  result <- tibble::tibble()

  for (tbl_name in all_table_names) {
    df <- country_data[[tbl_name]]
    result <- dplyr::bind_rows(result, df)
    message("  ", tbl_name, ": ", dim(df)[1], " rows and ",
            dim(df)[2], " columns.")
  }

  message("Stacked ", length(all_table_names), " tables across ",
          length(country_data), " countries.")
  result
}


#' Expand code ranges like "911:965" into individual codes
#' @noRd
.expand_code_ranges <- function(codes) {
  expanded <- character(0)
  for (code in codes) {
    if (grepl(":", code)) {
      parts <- as.integer(strsplit(code, ":")[[1]])
      expanded <- c(expanded, as.character(seq(parts[1], parts[2])))
    } else {
      expanded <- c(expanded, code)
    }
  }
  expanded
}

# Mock data ----


#' Generate mock FADN data from descriptive statistics
#'
#' Handles three cases:
#'   1. General stats (no grouping) → one block of n rows
#'   2. Stats grouped by one or more factors (e.g., farm_type, country)
#'      → generates rows per group proportionally or with explicit counts
#'
#' @param n Total number of rows to generate.
#' @param continuous_stats Data.frame with columns: variable, mean, sd, min, max,
#'   and optionally grouping columns (e.g., farm_type, country).
#' @param categorical_stats Data.frame with columns: variable, level, count,
#'   and optionally the same grouping columns.
#' @param group_cols Character vector of column names used for grouping
#'   (e.g., c("farm_type", "country")). NULL = no grouping.
#' @param group_weights Named list or data.frame defining how to split n across
#'   groups. If NULL, rows are split equally across groups.
#'   As data.frame: must contain group_cols + a "weight" or "count" column.
#' @param seed Random seed.
#' @return A data.frame.
h_generate_mock_data <- function(n = 500,
                                 continuous_stats = NULL,
                                 categorical_stats = NULL,
                                 group_cols = NULL,
                                 group_weights = NULL,
                                 seed = 42) {
  set.seed(seed)


  # --- Helper: generate one block (no grouping) ---
  generate_block <- function(n_block, cont_stats, cat_stats) {

    block <- data.frame(row_id = seq_len(n_block))

    # Continuous variables
    if (!is.null(cont_stats) && nrow(cont_stats) > 0) {
      for (i in seq_len(nrow(cont_stats))) {
        v      <- cont_stats$variable[i]
        values <- rnorm(n_block,
                        mean = as.numeric(cont_stats$mean[i]),
                        sd   = as.numeric(cont_stats$sd[i]))
        if ("min" %in% names(cont_stats)) {
          values <- pmax(values, as.numeric(cont_stats$min[i]))
        }
        if ("max" %in% names(cont_stats)) {
          values <- pmin(values, as.numeric(cont_stats$max[i]))
        }
        block[[v]] <- values
      }
    }

    # Categorical variables
    if (!is.null(cat_stats) && nrow(cat_stats) > 0) {
      vars <- unique(cat_stats$variable)
      for (v in vars) {
        sub   <- cat_stats[cat_stats$variable == v, ]
        probs <- as.numeric(sub$count) / sum(as.numeric(sub$count))
        block[[v]] <- sample(sub$level, size = n_block, replace = TRUE, prob = probs)
      }
    }

    block$row_id <- NULL
    return(block)
  }


  # ========================================================
  # CASE 1: No grouping — general stats
  # ========================================================
  if (is.null(group_cols)) {
    return(generate_block(n, continuous_stats, categorical_stats))
  }

  # ========================================================
  # CASE 2: Grouped stats (e.g., by farm_type, country)
  # ========================================================

  # Identify unique groups from the stats themselves
  ref_stats <- if (!is.null(continuous_stats)) continuous_stats else categorical_stats
  groups_df <- unique(ref_stats[, group_cols, drop = FALSE])
  n_groups  <- nrow(groups_df)

  # Determine how many rows per group
  if (is.null(group_weights)) {
    # Equal split
    base_n   <- n %/% n_groups
    leftover <- n %% n_groups
    group_ns <- rep(base_n, n_groups)
    if (leftover > 0) group_ns[seq_len(leftover)] <- group_ns[seq_len(leftover)] + 1

  } else if (is.data.frame(group_weights)) {
    # Merge weights onto groups_df
    gw <- merge(groups_df, group_weights, by = group_cols, all.x = TRUE)
    weight_col <- intersect(c("count", "weight", "n"), names(gw))
    if (length(weight_col) == 0) stop("group_weights must have a 'weight', 'count', or 'n' column.")
    w <- as.numeric(gw[[weight_col[1]]])
    w[is.na(w)] <- 0
    group_ns <- round(w / sum(w) * n)
    # Adjust rounding to hit exactly n
    diff <- n - sum(group_ns)
    if (diff != 0) {
      idx <- sample(seq_len(n_groups), abs(diff))
      group_ns[idx] <- group_ns[idx] + sign(diff)
    }

  } else {
    stop("group_weights must be NULL or a data.frame.")
  }

  # Generate per group
  blocks <- vector("list", n_groups)

  for (g in seq_len(n_groups)) {

    # Build filter mask for this group
    grp_vals <- groups_df[g, , drop = FALSE]

    # Filter continuous stats for this group
    cont_g <- NULL
    if (!is.null(continuous_stats)) {
      mask <- rep(TRUE, nrow(continuous_stats))
      for (gc in group_cols) {
        mask <- mask & (continuous_stats[[gc]] == grp_vals[[gc]])
      }
      cont_g <- continuous_stats[mask, , drop = FALSE]
    }

    # Filter categorical stats for this group
    cat_g <- NULL
    if (!is.null(categorical_stats)) {
      mask <- rep(TRUE, nrow(categorical_stats))
      for (gc in group_cols) {
        mask <- mask & (categorical_stats[[gc]] == grp_vals[[gc]])
      }
      cat_g <- categorical_stats[mask, , drop = FALSE]
    }

    block <- generate_block(group_ns[g], cont_g, cat_g)

    # Add group columns
    for (gc in group_cols) {
      block[[gc]] <- grp_vals[[gc]]
    }

    blocks[[g]] <- block
  }

  result <- do.call(rbind, blocks)
  rownames(result) <- NULL

  # Move group columns to front
  result <- result[, c(group_cols, setdiff(names(result), group_cols))]

  return(result)
}

