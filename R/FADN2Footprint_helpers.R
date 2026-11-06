#' Helpers for FADN2Footprint package:
#' Impute Average Practices using Hierarchical Fallback
#'
#' Calculates weighted averages of crop practices at a primary grouping level (e.g., NUTS2 & Organic).
#' If data is missing (NA) or insufficient at that level, it imputes values from a secondary,
#' coarser grouping level (e.g., Country & Organic).
#'
#' At the primary level, an average is only computed if the number of valid (non-NA)
#' observations exceeds \code{min_obs}; otherwise it is set to \code{NA} and, if possible,
#' replaced by the corresponding secondary-level (fallback) average, which is not subject
#' to this minimum observation filter.
#'
#' @param data A data.frame or tibble containing the FADN/Practice data.
#' @param target_vars A character vector of column names to calculate averages for (e.g., c("fertilizer_n", "yield")).
#' @param primary_grp A character vector of column names for the fine-scale grouping (e.g., c("NUTS2", "ORGANIC")).
#' @param secondary_grp A character vector of column names for the coarse-scale grouping (e.g., c("COUNTRY", "ORGANIC")).
#' @param weight_var (Optional) Character string of the weighting variable (e.g., "SYS02"). If NULL, simple mean is used.
#' @param min_obs (Optional) Minimum number of valid (non-NA) observations required within a primary group
#' for the average to be computed. If the count is not strictly greater than \code{min_obs}, the primary
#' average is set to \code{NA} (and later imputed from the secondary level, if available). Default is 15.
#'
#' @return A \code{\link[tibble]{tibble}} containing the primary grouping columns and the imputed average practices.
#' The tibble includes columns for each of the \code{target_vars} and the grouping columns specified in \code{primary_grp}.
#' 
#' @import dplyr
#'
#' @concept utils
#' @export

h_average_practices <- function(data,
                                target_vars,
                                primary_grp,
                                secondary_grp,
                                weight_var = NULL,
                                min_obs = 15) {

  require(dplyr)

  # Check that target variables are in the data set

  # Capture target_vars as a quosure BEFORE any other operation touches it
  target_vars_quo <- rlang::enquo(target_vars)
  # Resolve target_vars using tidyselect (works whether target_vars is a
  # character vector, a tidyselect helper like dplyr::matches(), or
  # a bare/quoted selection expression)
  target_vars_checked <- names(
    tidyselect::eval_select(target_vars_quo, data = data)
  )

  if (length(target_vars_checked) == 0) {
    stop("No variables matched 'target_vars' in the data set.")
  }

  # Check grouping variables
  grouping_vars <- unique(c(primary_grp, secondary_grp))
  missing_grouping_vars <- setdiff(grouping_vars, names(data))

  if (length(missing_grouping_vars) > 0L) {
    stop(
      "Missing grouping variables: ",
      paste(missing_grouping_vars, collapse = ", "),
      call. = FALSE
    )
  }

  # Check weight variable
  if (!is.null(weight_var) && !weight_var %in% names(data)) {
    stop(
      "Weight variable `", weight_var, "` is missing from `data`.",
      call. = FALSE
    )
  }

  # Secondary grouping variables must be available at primary level
  missing_secondary_keys <- setdiff(secondary_grp, primary_grp)

  if (length(missing_secondary_keys) > 0L) {
    stop(
      "`secondary_grp` must be a subset of `primary_grp`. Missing from ",
      "`primary_grp`: ",
      paste(missing_secondary_keys, collapse = ", "),
      call. = FALSE
    )
  }

  # Keep only required variables
  vars_to_keep <- unique(c(
    primary_grp,
    secondary_grp,
    target_vars_checked,
    weight_var
  ))

  data_selected <- data |>
    dplyr::select(dplyr::all_of(vars_to_keep))


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

  # 1bis. Helper function for Weighted Mean at Primary level,
  # with a minimum number of valid (non-NA) observations required
  calc_mean_primary <- function(x, w, min_n = min_obs) {
    n_valid <- sum(!is.na(x))

    if (n_valid <= min_n) {
      return(NA_real_)
    }

    calc_mean(x, w)
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
      ~ calc_mean_primary(., if(!is.null(weight_var)) .data[[weight_var]] else NULL)
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
    result[[var]] <- dplyr::coalesce(result[[var]], result[[fallback_col]])
  }

  # 6. Cleanup
  # Remove the fallback columns and return just the Primary keys + Calculated vars
  final_output <- result |>
    dplyr::select(dplyr::all_of(primary_grp),
                  dplyr::all_of(target_vars_checked)) |>
    dplyr::select(-dplyr::matches("SYS02"))

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
#'   that holds the national product codes. Multiple codes are pipe-delimited.
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
        dplyr::select(FADN_code_letter, FADN_code_description, national_code = !!rlang::sym(national_code_col)) %>%
        tidyr::separate_longer_delim(national_code, "|") %>%
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
            FADN_code_description = NA_character_,
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
#'   \item{value_mappings}{Optional categorical code crosswalks with columns
#'     fadn_variable, national_value and fadn_value (dimensionless codes).}
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
      manual_matches   = manual_matches,
      value_mappings = if ("value_mappings" %in% readxl::excel_sheets(dictionary_path)) {
        readxl::read_excel(dictionary_path, sheet = "value_mappings")
      } else {
        NULL
      }
    ),
    class = "fadn_dictionary"
  )
}

#' Validate a conversion dictionary
#'
#' @param dictionary_path Path to the filled Excel workbook.
#' @param raw_tables Named list of raw national FADN data frames.
#' @param id_cols Identifier variables (e.g., ID, YEAR, COUNTRY)
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
#'   Optional \code{value_mappings} recode categorical values after renaming;
#'   unmapped non-missing codes cause an error. Units otherwise remain those
#'   specified by the dictionary's unit conversions.
#'   Numeric \code{manual_matches} mapping to the same FADN variable are summed
#'   by farm-year after each source's unit conversion, including across tables.
#'   Missing numeric values are ignored (an all-missing source group sums to
#'   zero), and absent source columns produce a warning.
#'   Repeated categorical destinations are rejected rather than summed.
#' @param table_config Named list mapping product groups to table/code_col.
#' @param id_cols Character vector. Column name(s) identifying individual farms
#'   (e.g., \code{c("IDENT")}). These columns are preserved as-is in the output
#'   and used as grouping keys during pivoting.
#'
#' @return A single wide data frame in EU FADN format (one row per farm).
#'   Categorical codes are dimensionless; measurement units follow the dictionary.
#'   French purchased grazing-feed values are in EUR: \code{AUTAC + HERAC}
#'   yields \code{IGRFEDCNCTRPUR_V}, and \code{AUTAG + HERAG} yields
#'   \code{IGRFEDCRSPUR_V} when specified in the conversion dictionary.
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

    # Excel may remove zero padding from numeric national product codes.
    tbl[[code_col]] <- sub("^0+(?=[0-9]+$)", "", as.character(tbl[[code_col]]), perl = TRUE)
    grp_prefixes$national_code <- sub("^0+(?=[0-9]+$)", "",
                                     as.character(grp_prefixes$national_code), perl = TRUE)
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

  # ── Build base skeleton: union of all farm-year ids ──
  # Priority: tables referenced in manual_matches (farm register), then product tables
  base_tables <- unique(c(
    if (!is.null(mm)) intersect(unique(mm$national_table), names(raw_tables)) else character(0),
    unname(vapply(table_config, function(x) x$table, character(1)))
  ))

  id_skeleton <- purrr::map_dfr(base_tables, function(tn) {
    tdat <- raw_tables[[tn]]
    if (all(id_cols %in% colnames(tdat))) {
      dplyr::distinct(dplyr::select(tdat, dplyr::all_of(id_cols)))
    } else {
      NULL
    }
  }) |>
    dplyr::distinct()

  if (nrow(id_skeleton) == 0) {
    stop("Could not build farm-year skeleton: no table contains all id_cols.")
  }

  # ── Join all pivoted product tables onto the skeleton ──
  result <- id_skeleton
  for (k in seq_along(pivoted_tables)) {
    result <- dplyr::left_join(result, pivoted_tables[[k]], by = id_cols)
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
        crosswalk <- dict_conversion$value_mappings
        if (!is.null(crosswalk)) {
          crosswalk <- crosswalk[crosswalk$fadn_variable == fadn_var, , drop = FALSE]
          if (nrow(crosswalk) > 0) {
            if (fadn_var %in% names(result)) {
              stop("Cannot aggregate non-numeric mappings for ", fadn_var, ".")
            }
            if (anyDuplicated(as.character(crosswalk$national_value))) {
              stop("Duplicate national codes in value_mappings for ", fadn_var, ".")
            }
            idx <- match(as.character(src_col), as.character(crosswalk$national_value))
            unknown <- unique(as.character(src_col)[!is.na(src_col) & is.na(idx)])
            if (length(unknown) > 0) {
              stop("Unmapped national codes for ", fadn_var, ": ",
                   paste(unknown, collapse = ", "), ".")
            }
            vals <- ft_data[, id_cols, drop = FALSE]
            vals[[fadn_var]] <- crosswalk$fadn_value[idx]
            if (anyDuplicated(vals[id_cols])) {
              stop("Categorical mapping for ", fadn_var, " requires unique farm-year IDs.")
            }
            result <- dplyr::left_join(result, vals, by = id_cols)
            next
          }
        }
        is_char <- is.character(src_col) || is.factor(src_col)

        # Test if supposedly numeric column is actually coercible
        if (!is_char && !is.numeric(src_col)) {
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

        if (fadn_var %in% names(result)) {
          if (is_char || !is.numeric(result[[fadn_var]])) {
            stop("Cannot aggregate non-numeric mappings for ", fadn_var, ".")
          }
          incoming <- tail(make.unique(c(names(result), ".manual_value")), 1L)
          names(vals)[names(vals) == fadn_var] <- incoming
          result <- dplyr::left_join(result, vals, by = id_cols)
          both_missing <- is.na(result[[fadn_var]]) & is.na(result[[incoming]])
          result[[fadn_var]] <- dplyr::coalesce(result[[fadn_var]], 0) +
            dplyr::coalesce(result[[incoming]], 0)
          result[[fadn_var]][both_missing] <- NA_real_
          result[[incoming]] <- NULL
        } else {
          result <- dplyr::left_join(result, vals, by = id_cols)
        }
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


#' Simulate one continuous variable from descriptive statistics
#'
#' When the three quartiles are available, values are drawn by piecewise
#' interpolation through the quantile ladder
#' \code{(0, .25, .5, .75, 1) -> (min, p25, p50, p75, max)} (inverse-CDF
#' sampling). The positive part is interpolated in log space (locally
#' log-normal) because farm-level variables are strongly right-skewed. Exact
#' zero shares implied by the quartiles are preserved. If only mean, sd, min
#' and max are available, a moment-matched gamma (non-negative variables) or a
#' normal distribution is used. In all cases positive values are finally
#' rescaled so that the sample mean matches \code{mean}, then clipped to
#' \code{[min, max]}.
#'
#' @param n Number of values to draw.
#' @param mean,sd,min,max Reported descriptive statistics (same unit as the
#'   variable).
#' @param p25,p50,p75 Reported quartiles (same unit), or \code{NA}.
#' @param p_nonzero Share of non-zero values (0-1), or \code{NA}. Only used
#'   when the quartiles are missing and \code{min == 0}: values are zero with
#'   probability \code{1 - p_nonzero} (the lowest uniform scores) and the
#'   positive part is a gamma matched to the conditional moments
#'   \code{mean / p_nonzero} and \code{(sd^2 + mean^2) / p_nonzero - (mean / p_nonzero)^2}.
#' @param u Optional vector of \code{n} uniform scores in (0, 1). When
#'   supplied, values are obtained by inverse-CDF transformation of \code{u}
#'   (monotone in \code{u}), so that dependence between variables can be
#'   imposed through a copula. When \code{NULL}, independent draws are used.
#' @return Numeric vector of length \code{n}.
#' @noRd
.h_sim_continuous <- function(n, mean, sd, min, max,
                              p25 = NA_real_, p50 = NA_real_, p75 = NA_real_,
                              u = NULL, p_nonzero = NA_real_) {
  if (n == 0) return(numeric(0))
  if (!is.finite(mean) || !is.finite(min) || !is.finite(max)) return(rep(NA_real_, n))
  if (!is.finite(sd) || sd <= 0 || max <= min) {
    return(rep(min(max(mean, min), max), n))
  }

  u_given <- !is.null(u)
  if (u_given) u <- pmin(pmax(u, 1e-9), 1 - 1e-9) else u <- runif(n)

  if (all(is.finite(c(p25, p50, p75)))) {
    probs  <- c(0, 0.25, 0.5, 0.75, 1)
    values <- cummax(c(min, p25, p50, p75, max))
    pos    <- values > 0

    if (min >= 0 && sum(pos) >= 2) {
      first_pos <- which(pos)[1]
      out <- numeric(n)
      below <- rep(FALSE, n)
      if (first_pos > 1) {
        below <- u <= probs[first_pos]
        if (any(below)) {
          out[below] <- approx(probs[seq_len(first_pos)], values[seq_len(first_pos)],
                               xout = u[below], rule = 2)$y
        }
      }
      if (any(!below)) {
        out[!below] <- exp(approx(probs[pos], log(values[pos]),
                                  xout = u[!below], rule = 2)$y)
      }
    } else if (min >= 0 && sum(pos) == 1 && mean > 0) {
      # Zero-inflated: exponential tail above the last zero quantile
      z <- max(probs[!pos])
      f <- 1 - z
      out <- numeric(n)
      tail_idx <- u > z
      if (any(tail_idx)) {
        v <- (u[tail_idx] - z) / f
        out[tail_idx] <- -(mean / f) * log(1 - pmin(v, 1 - 1e-9))
      }
    } else {
      out <- approx(probs, values, xout = u, rule = 2)$y
    }
  } else if (is.finite(p_nonzero) && p_nonzero < 1 && min == 0 && mean > 0) {
    # Zero-inflated gamma: the lowest scores give the zeros
    out <- numeric(n)
    if (p_nonzero > 0) {
      m_pos <- mean / p_nonzero
      v_pos <- (sd^2 + mean^2) / p_nonzero - m_pos^2
      pos <- u > 1 - p_nonzero
      if (any(pos)) {
        if (v_pos > 0) {
          v <- pmin(pmax((u[pos] - (1 - p_nonzero)) / p_nonzero, 1e-9), 1 - 1e-9)
          out[pos] <- qgamma(v, shape = m_pos^2 / v_pos, scale = v_pos / m_pos)
        } else {
          out[pos] <- m_pos
        }
      }
    }
  } else if (min >= 0 && mean > min) {
    m <- mean - min
    out <- min + if (u_given) {
      qgamma(u, shape = (m / sd)^2, scale = sd^2 / m)
    } else {
      rgamma(n, shape = (m / sd)^2, scale = sd^2 / m)
    }
  } else {
    out <- if (u_given) qnorm(u, mean = mean, sd = sd) else rnorm(n, mean = mean, sd = sd)
  }

  # Match the reported mean, then enforce the reported range. With quartiles,
  # only the upper tail (above p75) is rescaled so the quartiles are preserved.
  cur <- base::mean(out)
  if (all(is.finite(c(p25, p50, p75))) && min >= 0 && any(out > p75)) {
    tail_idx <- out > p75
    excess <- sum(out[tail_idx] - p75)
    k <- (mean * n - sum(out[!tail_idx]) - sum(tail_idx) * p75) / excess
    out[tail_idx] <- p75 + max(k, 0) * (out[tail_idx] - p75)
  } else if (min >= 0) {
    if (cur > 0 && mean > 0) out <- out * (mean / cur)
  } else {
    out <- out + (mean - cur)
  }
  pmin(pmax(out, min), max)
}


#' Generate mock FADN data from descriptive statistics
#'
#' Handles three cases:
#'   1. General stats (no grouping) → one block of n rows
#'   2. Stats grouped by one or more factors (e.g., farm_type, country)
#'      → generates rows per group proportionally or with explicit counts
#'
#' @param n Total number of rows to generate.
#' @param continuous_stats Data.frame with columns: variable, mean, sd, min, max,
#'   optionally the quartiles p25, p50, p75 (used to reproduce the shape and
#'   the zero share of the distribution), optionally \code{p_nonzero}
#'   (share of non-zero values, 0-1, used for a zero-inflated gamma when
#'   the quartiles are missing), and optionally grouping columns
#'   (e.g., farm_type, country).
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
        v   <- cont_stats$variable[i]
        get <- function(col, default = NA_real_) {
          if (col %in% names(cont_stats)) as.numeric(cont_stats[[col]][i]) else default
        }
        block[[v]] <- .h_sim_continuous(
          n_block,
          mean = get("mean"), sd = get("sd"),
          min = get("min", -Inf), max = get("max", Inf),
          p25 = get("p25"), p50 = get("p50"), p75 = get("p75"),
          p_nonzero = get("p_nonzero")
        )
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
    weight_col <- intersect(c("count", "weight", "n"), names(group_weights))
    if (length(weight_col) == 0) stop("group_weights must have a 'weight', 'count', or 'n' column.")
    key <- function(d) do.call(paste, c(unname(as.list(d[group_cols])), sep = "\r"))
    w <- as.numeric(group_weights[[weight_col[1]]])[match(key(groups_df), key(group_weights))]
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

    if (group_ns[g] <= 0) next

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


#' Resolve a per-variable dependence parameter
#'
#' @param x \code{NULL}, an unnamed scalar (applied to every variable), or a
#'   named numeric vector (names = variables, optional element
#'   \code{".default"} used for unlisted variables). Non-finite values are
#'   ignored.
#' @param vars Character vector of variable names.
#' @param default Value used when \code{x} gives no information.
#' @return Named numeric vector along \code{vars}.
#' @noRd
.h_dep_param <- function(x, vars, default = 0) {
  out <- stats::setNames(rep(default, length(vars)), vars)
  if (is.null(x) || length(x) == 0) return(out)
  if (is.null(names(x))) {
    if (is.finite(x[1])) out[] <- x[1]
    return(out)
  }
  if (".default" %in% names(x) && is.finite(x[[".default"]])) out[] <- x[[".default"]]
  hit <- intersect(names(x), vars)
  hit <- hit[is.finite(x[hit])]
  out[hit] <- x[hit]
  out
}


#' Product family of FADN variable names (e.g. "CWHTC_TA" -> "CWHTC")
#' @noRd
.h_var_family <- function(v) sub("_[^_]+$", "", sub("_X$", "", v))


#' Factorise a correlation matrix for Gaussian-copula sampling
#'
#' Negative eigenvalues (pairwise estimates need not be positive definite)
#' are clipped and the factor is rescaled to give a unit diagonal. Also
#' returns the first principal component, used as the latent farm-size score.
#' @noRd
.h_cor_factor <- function(R) {
  e <- eigen(R, symmetric = TRUE)
  A <- e$vectors %*% diag(sqrt(pmax(e$values, 1e-6)), nrow(R))
  A <- A / sqrt(rowSums(A^2))
  v1 <- e$vectors[, 1]
  if (sum(v1) < 0) v1 <- -v1
  list(vars = colnames(R), A = A, v1 = v1, s_scale = sqrt(sum((t(A) %*% v1)^2)))
}


#' Draw latent standard-normal scores with a farm-like dependence structure
#'
#' Core variables (those in \code{dep$cor_core}) follow a Gaussian copula with
#' that correlation matrix; the latent size score \code{S} is their first
#' principal component. Every other variable \code{v} is
#' \code{lambda_v S + sqrt(1 - lambda_v^2) (sqrt(r) G_family + sqrt(1 - r) e_v)}
#' with \code{lambda_v = dep$size_loading}, \code{r = dep$family_cor}, a
#' family factor \code{G} shared by variables of the same family
#' (\code{dep$family}, default \code{.h_var_family()}) and idiosyncratic noise.
#'
#' @param n Number of units.
#' @param vars Continuous variable names.
#' @param dep Dependence list (see \code{h_generate_mock_panel()}).
#' @param S Optional size score of length \code{n} (used when there are no
#'   core variables, e.g. for child-table rows inheriting their farm's size).
#' @return List with \code{Z} (n x length(vars) matrix) and \code{S}.
#' @noRd
.h_mock_latent <- function(n, vars, dep, S = NULL) {
  Z <- matrix(0, n, length(vars), dimnames = list(NULL, vars))
  fac <- dep[[".factor"]]
  core <- if (!is.null(fac)) intersect(fac$vars, vars) else character(0)
  if (length(core) > 0) {
    Zc <- matrix(rnorm(n * length(fac$vars)), n, length(fac$vars)) %*% t(fac$A)
    colnames(Zc) <- fac$vars
    Z[, core] <- Zc[, core]
    S <- as.numeric(Zc %*% fac$v1) / fac$s_scale
  } else if (is.null(S)) {
    S <- rnorm(n)
  }

  other <- setdiff(vars, core)
  if (length(other) > 0 && n > 0) {
    lam <- pmax(pmin(.h_dep_param(dep[["size_loading"]], other, 0), 0.99), -0.99)
    r <- dep[["family_cor"]]
    r <- if (is.null(r) || !is.finite(r[1])) 0 else min(max(r[1], 0), 0.99)
    fam <- dep[["family"]]
    fam <- if (is.null(fam)) {
      .h_var_family(other)
    } else if (is.null(names(fam))) {
      rep(fam[1], length(other))
    } else {
      ifelse(other %in% names(fam), unname(fam[other]), .h_var_family(other))
    }
    ufam <- unique(fam)
    G <- matrix(rnorm(n * length(ufam)), n, dimnames = list(NULL, ufam))
    E <- matrix(rnorm(n * length(other)), n, length(other))
    idio <- sqrt(r) * G[, fam, drop = FALSE] + sqrt(1 - r) * E
    Z[, other] <- outer(S, lam) + sweep(idio, 2, sqrt(1 - lam^2), `*`)
  }
  list(Z = Z, S = S)
}


#' Draw a categorical variable from level counts
#'
#' If \code{loading} is finite and levels are numeric, the variable is treated
#' as ordinal and driven by the size score (latent threshold model).
#' @noRd
.h_draw_cat <- function(n, sub, S = NULL, loading = NA_real_) {
  if (n == 0 || nrow(sub) == 0) return(rep(NA_character_, n))
  lev <- as.character(sub$level)
  p <- as.numeric(sub$count)
  p <- p / sum(p)
  num <- suppressWarnings(as.numeric(lev))
  if (is.finite(loading) && !is.null(S) && !anyNA(num)) {
    o <- order(num)
    loading <- min(max(loading, -0.99), 0.99)
    u <- pnorm(loading * S + sqrt(1 - loading^2) * rnorm(n))
    return(lev[o][pmin(findInterval(u, cumsum(p[o])) + 1L, length(lev))])
  }
  sample(lev, n, replace = TRUE, prob = p)
}


#' Adjust categorical copy probability for chance matches on redraw
#' @noRd
.h_cat_copy_probability <- function(p_same, cat_stats) {
  for (v in names(p_same)) {
    counts <- as.numeric(cat_stats$count[cat_stats$variable == v])
    counts <- counts[is.finite(counts) & counts > 0]
    if (length(counts) == 0) next
    probs <- counts / sum(counts)
    chance <- sum(probs^2)
    target <- min(max(p_same[[v]], 0), 1)
    p_same[[v]] <- if (chance >= 1) 0 else max((target - chance) / (1 - chance), 0)
  }
  p_same
}


#' Derive nested classifications (e.g. NUTS2 from NUTS3) from a lookup
#' @noRd
.h_apply_cat_lookup <- function(cats, cat_lookup, cat_stats) {
  if (is.null(cat_lookup) || nrow(cat_lookup) == 0) return(cats)
  for (tv in unique(cat_lookup$to_var)) {
    lk <- cat_lookup[cat_lookup$to_var == tv, , drop = FALSE]
    fv <- lk$from_var[1]
    if (!tv %in% names(cats) || !fv %in% names(cats)) next
    val <- as.character(lk$to_level)[match(cats[[fv]], as.character(lk$from_level))]
    miss <- is.na(val)
    if (any(miss)) {
      val[miss] <- .h_draw_cat(sum(miss), cat_stats[cat_stats$variable == tv, , drop = FALSE])
    }
    cats[[tv]] <- val
  }
  cats
}


#' Generate a mock FADN panel (farms followed over several years)
#'
#' Extends \code{h_generate_mock_data()} with (1) dependence between variables
#' and (2) a rotating panel reproducing national FADN sampling plans, in which
#' most farms stay in the sample from one year to the next.
#'
#' Marginal distributions are those of \code{h_generate_mock_data()} (same
#' descriptive statistics, same per-group/per-year mean matching). Values are
#' obtained by inverse-CDF transformation of latent normal scores built by
#' \code{.h_mock_latent()} (Gaussian copula on core variables, latent farm-size
#' score and product-family factors for the others).
#'
#' Panel dynamics, for each group and year after the first: each farm stays
#' in the sample with probability \code{retention}; leaving farms are
#' replaced by new farms so that the group size stays constant. Latent scores
#' of staying farms follow a stationary AR(1) process
#' \code{z_t = rho z_{t-1} + sqrt(1 - rho^2) eta_t} (per-variable
#' \code{autocorr}, \code{size_autocorr} for the size score). Categorical
#' variables keep their previous level with probability
#' \code{cat_persistence}, and nested classifications are re-derived from
#' \code{cat_lookup} so that they stay consistent (e.g. NUTS2 inside NUTS1).
#' Group membership (e.g. \code{TF8 x COUNTRY}) is kept fixed for each farm.
#'
#' @param n Number of farms per year (all groups together).
#' @param years Integer vector of simulated years.
#' @param continuous_stats,categorical_stats,group_cols,group_weights As in
#'   \code{h_generate_mock_data()}.
#' @param dependence Either one dependence list applied to every group, or a
#'   named list of such lists, named by the values of \code{dependence_by}.
#'   A dependence list may contain (all optional, all dimensionless):
#'   \describe{
#'     \item{retention}{Probability (0-1) that a farm in the sample in year
#'       t is still in it in year t+1. Default 0 (independent cross-sections).}
#'     \item{cor_core}{Correlation matrix (with dimnames) of the latent normal
#'       scores of core continuous variables.}
#'     \item{size_loading}{Correlation of non-core variables with the size
#'       score (scalar, or named vector with optional \code{.default}).}
#'     \item{family_cor}{Latent correlation shared by non-core variables of
#'       the same family (scalar in [0, 1)).}
#'     \item{family}{Optional family of each variable (named character
#'       vector, or one string putting all variables in the same family).}
#'     \item{autocorr}{Year-to-year correlation of latent scores (scalar or
#'       named vector).}
#'     \item{size_autocorr}{Year-to-year correlation of the size score.}
#'     \item{cat_persistence}{Probability (0-1) of keeping the previous level
#'       of a categorical variable (scalar or named vector).}
#'     \item{ordinal_loading}{Named vector: categorical variables with numeric
#'       ordered levels (e.g. economic size class \code{SIZC}) driven by the
#'       size score, with this latent correlation.}
#'   }
#' @param dependence_by Name of the column of \code{group_cols} used to pick
#'   the dependence list (e.g. \code{"COUNTRY"}), or \code{NULL}.
#' @param cat_lookup Optional data.frame with columns \code{from_var},
#'   \code{from_level}, \code{to_var}, \code{to_level} describing nested
#'   classifications, applied in row order.
#' @param id_prefix Prefix of generated farm identifiers.
#' @param id_col,year_col Names of the identifier and year columns.
#' @param seed Random seed.
#' @return A data.frame with one row per farm and year (variables in the
#'   units of the descriptive statistics), plus \code{id_col} and
#'   \code{year_col}. Attribute \code{"latent"}: data.frame
#'   (\code{id_col}, \code{year_col}, \code{.size}) giving the latent
#'   standard-normal size score of each farm-year, used by
#'   \code{h_generate_mock_child_panel()}.
#' @keywords internal
h_generate_mock_panel <- function(n,
                                  years,
                                  continuous_stats = NULL,
                                  categorical_stats = NULL,
                                  group_cols = NULL,
                                  group_weights = NULL,
                                  dependence = NULL,
                                  dependence_by = NULL,
                                  cat_lookup = NULL,
                                  id_prefix = "FARM_",
                                  id_col = "ID",
                                  year_col = "YEAR",
                                  seed = 42) {
  set.seed(seed)
  years <- sort(unique(years))

  no_group <- is.null(group_cols)
  if (no_group) {
    group_cols <- ".group"
    if (!is.null(continuous_stats)) continuous_stats$.group <- 1
    if (!is.null(categorical_stats)) categorical_stats$.group <- 1
  }

  ref_stats <- if (!is.null(continuous_stats)) continuous_stats else categorical_stats
  groups_df <- unique(as.data.frame(ref_stats)[, group_cols, drop = FALSE])
  n_groups <- nrow(groups_df)
  grp_key <- function(d) do.call(paste, c(unname(as.list(d[group_cols])), sep = "\r"))

  if (is.null(group_weights)) {
    w <- rep(1, n_groups)
  } else {
    weight_col <- intersect(c("count", "weight", "n"), names(group_weights))
    if (length(weight_col) == 0) stop("group_weights must have a 'weight', 'count', or 'n' column.")
    w <- as.numeric(group_weights[[weight_col[1]]])[match(grp_key(groups_df), grp_key(group_weights))]
    w[is.na(w)] <- 0
  }
  group_ns <- round(w / sum(w) * n)
  diff <- n - sum(group_ns)
  if (diff != 0) {
    idx <- sample(which(w > 0), abs(diff), replace = abs(diff) > sum(w > 0))
    for (i in idx) group_ns[i] <- max(group_ns[i] + sign(diff), 0)
  }

  get_dep <- function(g) {
    dep <- if (is.null(dependence_by)) {
      dependence
    } else {
      dependence[[as.character(groups_df[[dependence_by]][g])]]
    }
    if (is.null(dep)) dep <- list()
    if (!is.null(dep[["cor_core"]]) && is.null(dep[[".factor"]])) dep[[".factor"]] <- .h_cor_factor(dep[["cor_core"]])
    dep
  }
  scalar <- function(x, default) if (is.null(x) || !is.finite(x[1])) default else x[1]

  next_id <- 0L
  blocks <- list()
  latent <- list()

  for (g in seq_len(n_groups)) {
    ng <- group_ns[g]
    if (ng <= 0) next
    grp_vals <- groups_df[g, , drop = FALSE]
    in_group <- function(st) {
      if (is.null(st)) return(NULL)
      mask <- rep(TRUE, nrow(st))
      for (gc in group_cols) mask <- mask & (st[[gc]] == grp_vals[[gc]])
      as.data.frame(st[mask, , drop = FALSE])
    }
    cont_g <- in_group(continuous_stats)
    cat_g <- in_group(categorical_stats)
    cvars <- if (is.null(cont_g)) character(0) else cont_g$variable
    catvars <- if (is.null(cat_g)) character(0) else unique(cat_g$variable)

    dep <- get_dep(g)
    retention <- min(max(scalar(dep[["retention"]], 0), 0), 1)
    rho <- pmin(pmax(.h_dep_param(dep[["autocorr"]], cvars, 0), 0), 0.999)
    rho_s <- min(max(scalar(dep[["size_autocorr"]], 0), 0), 0.999)
    p_same <- .h_dep_param(dep[["cat_persistence"]], catvars, 0)
    p_same <- .h_cat_copy_probability(p_same, cat_g)
    ord <- .h_dep_param(dep[["ordinal_loading"]], catvars, NA_real_)

    state <- NULL
    for (yr in years) {
      keep <- if (is.null(state)) logical(0) else runif(length(state$ids)) < retention
      n_keep <- sum(keep)
      n_new <- ng - n_keep

      fresh <- .h_mock_latent(n_new, cvars, dep)
      if (n_keep > 0) {
        inn <- .h_mock_latent(n_keep, cvars, dep)
        Zk <- sweep(state$Z[keep, , drop = FALSE], 2, rho, `*`) +
          sweep(inn$Z, 2, sqrt(1 - rho^2), `*`)
        Sk <- rho_s * state$S[keep] + sqrt(1 - rho_s^2) * inn$S
      } else {
        Zk <- fresh$Z[0, , drop = FALSE]
        Sk <- numeric(0)
      }
      Z <- rbind(Zk, fresh$Z)
      S <- c(Sk, fresh$S)
      ids <- c(if (n_keep > 0) state$ids[keep] else integer(0), next_id + seq_len(n_new))
      next_id <- next_id + n_new

      cats <- list()
      for (v in catvars) {
        val <- .h_draw_cat(ng, cat_g[cat_g$variable == v, , drop = FALSE], S, ord[[v]])
        if (n_keep > 0 && !is.null(state$cats[[v]])) {
          prev <- state$cats[[v]][keep]
          stay <- !is.na(prev) & runif(n_keep) < p_same[[v]]
          val[seq_len(n_keep)][stay] <- prev[stay]
        }
        cats[[v]] <- val
      }
      cats <- .h_apply_cat_lookup(cats, cat_lookup, cat_g)

      block <- data.frame(row.names = seq_len(ng))
      for (i in seq_along(cvars)) {
        get <- function(col, default = NA_real_) {
          if (col %in% names(cont_g)) as.numeric(cont_g[[col]][i]) else default
        }
        block[[cvars[i]]] <- .h_sim_continuous(
          ng,
          mean = get("mean"), sd = get("sd"),
          min = get("min", -Inf), max = get("max", Inf),
          p25 = get("p25"), p50 = get("p50"), p75 = get("p75"),
          p_nonzero = get("p_nonzero"),
          u = pnorm(Z[, cvars[i]])
        )
      }
      for (v in catvars) block[[v]] <- cats[[v]]
      for (gc in group_cols) block[[gc]] <- grp_vals[[gc]]
      block[[".id"]] <- ids
      block[[year_col]] <- yr

      blocks[[length(blocks) + 1L]] <- block
      latent[[length(latent) + 1L]] <- data.frame(.id = ids, year = yr, .size = S)
      state <- list(ids = ids, Z = Z, S = S, cats = cats)
    }
  }

  result <- do.call(rbind, blocks)
  rownames(result) <- NULL
  width <- nchar(next_id) + 1
  fmt_id <- function(i) paste0(id_prefix, formatC(i, width = width, flag = "0"))
  result[[id_col]] <- fmt_id(result$.id)
  result$.id <- NULL
  if (no_group) {
    result$.group <- NULL
    group_cols <- character(0)
  }
  result <- result[, c(group_cols, setdiff(names(result), c(group_cols, id_col, year_col)),
                       id_col, year_col)]

  lat <- do.call(rbind, latent)
  lat <- data.frame(fmt_id(lat$.id), lat$year, lat$.size)
  names(lat) <- c(id_col, year_col, ".size")
  attr(result, "latent") <- lat
  result
}


#' Generate a mock child table (several rows per farm) following a farm panel
#'
#' For national FADN formats storing crops, animals or products as several
#' rows per farm (e.g. French RICA \code{veg}, \code{ani}, \code{pan}
#' tables). Rows belong to a farm of \code{parent_latent} and live as long as
#' that farm stays in the sample: rows of staying farms are carried over
#' (AR(1) on latent scores, categorical persistence), new farms receive new
#' rows. The latent score of every row loads on its farm's size score, so
#' that bigger farms have larger areas, herds and outputs in every table.
#'
#' @param parent_latent data.frame with \code{parent_id_col}, \code{year_col}
#'   and \code{.size}, as returned in \code{attr(x, "latent")} by
#'   \code{h_generate_mock_panel()}.
#' @param rows_per_parent Mean number of rows per farm (dimensionless).
#' @param continuous_stats,categorical_stats Ungrouped descriptive statistics
#'   of the table, as in \code{h_generate_mock_data()}.
#' @param dependence Dependence list (see \code{h_generate_mock_panel()};
#'   \code{retention} and \code{cor_core} are not used). By default all
#'   variables of a row belong to one family, so that e.g. area and
#'   production of a crop row are correlated through \code{family_cor}.
#' @param parent_id_col,year_col Names of the farm identifier and year columns.
#' @param min_one_row If \code{TRUE}, every farm gets at least one row
#'   (otherwise rows are assigned to a subset of farms when
#'   \code{rows_per_parent < 1}). Farms receive rows with probability
#'   increasing with their size score.
#' @param seed Random seed.
#' @return A data.frame with \code{parent_id_col}, \code{year_col} and the
#'   simulated variables (units of the descriptive statistics).
#' @keywords internal
h_generate_mock_child_panel <- function(parent_latent,
                                        rows_per_parent,
                                        continuous_stats = NULL,
                                        categorical_stats = NULL,
                                        dependence = NULL,
                                        parent_id_col = "ID",
                                        year_col = "YEAR",
                                        min_one_row = rows_per_parent >= 1,
                                        seed = 42) {
  set.seed(seed)
  dep <- if (is.null(dependence)) list() else dependence
  dep[[".factor"]] <- NULL
  if (is.null(dep[["family"]])) dep[["family"]] <- ".row"
  cont <- if (is.null(continuous_stats)) NULL else as.data.frame(continuous_stats)
  catst <- if (is.null(categorical_stats)) NULL else as.data.frame(categorical_stats)
  cvars <- if (is.null(cont)) character(0) else cont$variable
  catvars <- if (is.null(catst)) character(0) else unique(catst$variable)
  rho <- pmin(pmax(.h_dep_param(dep[["autocorr"]], cvars, 0), 0), 0.999)
  p_same <- .h_dep_param(dep[["cat_persistence"]], catvars, 0)
  if (!is.null(catst)) p_same <- .h_cat_copy_probability(p_same, catst)
  lam_rows <- .h_dep_param(dep[["size_loading"]], ".rows", 0)[[1]]

  years <- sort(unique(parent_latent[[year_col]]))
  state <- NULL
  blocks <- list()
  for (yr in years) {
    par <- parent_latent[parent_latent[[year_col]] == yr, , drop = FALSE]
    S_par <- stats::setNames(par$.size, par[[parent_id_col]])

    keep <- if (is.null(state)) logical(0) else state$parent %in% names(S_par)
    n_keep <- sum(keep)
    new_par <- setdiff(names(S_par), state$parents)
    n_new_rows <- round(rows_per_parent * length(new_par))
    new_rows_par <- character(0)
    if (length(new_par) > 0 && n_new_rows > 0) {
      w <- exp(lam_rows * S_par[new_par])
      new_rows_par <- if (min_one_row && n_new_rows >= length(new_par)) {
        c(new_par, new_par[sample.int(length(new_par), n_new_rows - length(new_par),
                                      replace = TRUE, prob = w)])
      } else {
        new_par[sample.int(length(new_par), min(n_new_rows, length(new_par)), prob = w)]
      }
    }

    fresh <- .h_mock_latent(length(new_rows_par), cvars, dep, S = unname(S_par[new_rows_par]))
    if (n_keep > 0) {
      kept_par <- state$parent[keep]
      inn <- .h_mock_latent(n_keep, cvars, dep, S = unname(S_par[kept_par]))
      Zk <- sweep(state$Z[keep, , drop = FALSE], 2, rho, `*`) +
        sweep(inn$Z, 2, sqrt(1 - rho^2), `*`)
    } else {
      kept_par <- character(0)
      Zk <- fresh$Z[0, , drop = FALSE]
    }
    Z <- rbind(Zk, fresh$Z)
    parent <- c(kept_par, new_rows_par)
    n_rows <- length(parent)

    cats <- list()
    for (v in catvars) {
      val <- .h_draw_cat(n_rows, catst[catst$variable == v, , drop = FALSE])
      if (n_keep > 0 && !is.null(state$cats[[v]])) {
        prev <- state$cats[[v]][keep]
        stay <- !is.na(prev) & runif(n_keep) < p_same[[v]]
        val[seq_len(n_keep)][stay] <- prev[stay]
      }
      cats[[v]] <- val
    }

    block <- data.frame(parent, rep(yr, n_rows), stringsAsFactors = FALSE)
    names(block) <- c(parent_id_col, year_col)
    for (i in seq_along(cvars)) {
      get <- function(col, default = NA_real_) {
        if (col %in% names(cont)) as.numeric(cont[[col]][i]) else default
      }
      block[[cvars[i]]] <- .h_sim_continuous(
        n_rows,
        mean = get("mean"), sd = get("sd"),
        min = get("min", -Inf), max = get("max", Inf),
        p25 = get("p25"), p50 = get("p50"), p75 = get("p75"),
        p_nonzero = get("p_nonzero"),
        u = pnorm(Z[, cvars[i]])
      )
    }
    for (v in catvars) block[[v]] <- cats[[v]]

    blocks[[length(blocks) + 1L]] <- block
    state <- list(parent = parent, parents = names(S_par), Z = Z, cats = cats)
  }
  result <- do.call(rbind, blocks)
  rownames(result) <- NULL
  result
}
