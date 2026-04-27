
# Internal parsers


# FARM CHARACTERISTICS ----

#' Parse and Structure Farm-Level Data from Harmonised FADN Input
#'
#' @description
#' An internal function that extracts and cleans farm-level variables from a
#' harmonised FADN data frame, and harmonises country naming conventions by
#' matching the \code{COUNTRY} column against a reference table. The result
#' is a tidy farm-level dataset enriched with standardised country identifiers.
#'
#' @details
#' ## Step 1 — Variable Selection and Cleaning
#' The following variable groups are selected from \code{df_harmonized}:
#' \itemize{
#'   \item **Identifiers** (\code{id_cols}): farm × year traceability columns.
#'   \item **French administrative identifiers**: \code{PACAGE}, \code{SIRET},
#'     \code{DEPCOM} (where available).
#'   \item **Localisation**: \code{COUNTRY}, \code{NUTS} region codes.
#'   \item **Farm representativeness weight**: \code{SYS02}.
#'   \item **Type of farming**: \code{TF_SUBP4}, \code{TF14}, \code{TF8}
#'     (FADN typology at different aggregation levels).
#'   \item **Economic size class**: \code{SIZC}.
#'   \item **Total output**: \code{SE131} (EUR, FADN Standard Results).
#'   \item **Organic farming certification**: \code{ORGANIC}, recoded as a
#'     logical: \code{TRUE} if certified organic (code \code{2}) or in
#'     conversion (code \code{4}), \code{FALSE} otherwise.
#' }
#'
#' @note
#' \itemize{
#'   \item The PDO (Protected Designation of Origin) certification variable
#'     is intentionally excluded due to poor registration quality in several
#'     national datasets (e.g., absent from the French FADN for 2016–2018).
#'   \item Country name harmonisation uses a best-match strategy: the naming
#'     convention (\code{country_eu}, \code{Country_ISO_3166_1_A3}, or
#'     \code{country_FADN}) with the highest number of matching values in the
#'     \code{COUNTRY} column is selected automatically. A warning is issued
#'     if no match is found.
#'   \item A message listing all recognised FADN variable descriptions present
#'     in the output is printed via \code{\link[base]{message}}.
#' }
#'
#' ## Step 2 — Country Name Harmonisation
#' The \code{COUNTRY} column may follow different naming conventions depending
#' on the FADN data source (national vs. EU FADN). The function identifies the
#' best-matching convention by counting overlaps with
#' \code{data_extra$country_names} and joins the full reference table to enrich
#' the output with standardised country identifiers (ISO 3166-1 A3, EU FADN
#' code, etc.).
#'
#' @param df_harmonized A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing harmonised FADN variables in
#'   wide format. Must include at minimum the columns specified in
#'   \code{id_cols} and \code{COUNTRY}.
#' @param id_cols A \code{character} vector of column names used as farm ×
#'   year identifiers (e.g., \code{c("FADN_ID", "YEAR")}).
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm × year,
#' containing:
#' \describe{
#'   \item{...}{Traceability identifier columns (\code{id_cols}).}
#'   \item{COUNTRY}{\code{character}. Country identifier as provided in the
#'     input data.}
#'   \item{NUTS, NUTS2, NUTS3}{\code{character}. NUTS region codes (where
#'     available).}
#'   \item{SYS02}{\code{numeric}. Farm representativeness weight.}
#'   \item{TF_SUBP4, TF14, TF8}{\code{integer}. Type of farming codes at
#'     different aggregation levels.}
#'   \item{SIZC}{\code{integer}. Economic size class.}
#'   \item{SE131}{\code{numeric}. Total farm output (EUR yr\eqn{^{-1}}).}
#'   \item{ORGANIC}{\code{logical}. \code{TRUE} if the farm is certified
#'     organic or in conversion to organic farming.}
#'   \item{PACAGE, SIRET, DEPCOM}{\code{character}. French administrative
#'     identifiers (where available).}
#'   \item{...}{Additional country reference columns from
#'     \code{data_extra$country_names} (e.g., \code{Country_ISO_3166_1_A3},
#'     \code{country_eu}).}
#' }
#'
#' @seealso
#' \code{\link{.parse_crop_data}}, \code{\link{.parse_livestock_data}},
#' \code{\link{data_4FADN2Footprint}}, \code{\link{data_extra}}
#'
#' @importFrom dplyr select mutate left_join filter pull matches all_of
#' @importFrom tidyr any_of
#'
#' @export
#'
#' @keywords internal

.parse_farm_data <- function(df_harmonized, id_cols) {

  country_ref_table = data_extra$country_names

  # 1. Select and Clean variables --------------------------------------------------
  farm_data <- df_harmonized |>
    dplyr::select(
      dplyr::all_of(id_cols),
      # French Identifiers
      dplyr::matches("PACAGE"),
      dplyr::matches("SIRET"),
      dplyr::matches("DEPCOM"),
      # Farm localisation
      "COUNTRY",
      dplyr::matches("NUTS"),
      # Farm representativeness
      dplyr::matches("SYS02"),
      # Type of farming
      "TF_SUBP4", "TF14", "TF8",
      # Economic size class
      "SIZC",
      # SE131	Total output	in EUR	STANDARD RESULTS
      dplyr::matches("SE131"),
      # SE080	Total livestock units	in Livestock unit (conversion coefficients see RICC 1750)	STANDARD RESULTS
      dplyr::matches("SE080"),
      # SE025	Total Utilised Agricultural Area	in ha	STANDARD RESULTS
      dplyr::matches("SE025"),
      # Farm certification
      # Note: PDO variable excluded due to poor registration (e.g., missing in FR 2016-2018)
      "ORGANIC"
    ) |>
    dplyr::mutate(
      # Encode organic farming: TRUE if code is 2 (certified) or 4 (conversion)
      ORGANIC = ORGANIC %in% c(2, 4),
      # Makes sure NUTS and SIRET variables are character, if available
      dplyr::across(dplyr::matches("SIRET|NUTS"),as.character)
    )

  # 2. Harmonize Country Names --------------------------------------------------
  # We identify which naming convention is used in 'COUNTRY' by checking overlap
  # with the reference table.

  # Calculate intersection counts for each naming convention
  match_counts <- c(
    country_eu  = sum(farm_data$COUNTRY %in% country_ref_table$country_eu),
    Country_ISO_3166_1_A3 = sum(farm_data$COUNTRY %in% country_ref_table$Country_ISO_3166_1_A3),
    country_FADN = sum(farm_data$COUNTRY %in% country_ref_table$country_FADN)
  )

  # Identify the best matching column name in the reference table
  best_match_col <- names(which.max(match_counts))

  if (length(best_match_col) == 0 || max(match_counts) == 0) {
    warning("No matching country naming convention found in 'country_ref_table'. Join skipped.")
    return(farm_data)
  }

  # Join with reference table
  farm_data <- farm_data |>
    dplyr::mutate("{best_match_col}" := COUNTRY) |>
    dplyr::left_join(
      country_ref_table,
      by = best_match_col
    )

  message("\n", paste0(dict_FADN |> filter(var_common %in% colnames(farm_data)) |> pull(DESCRIPTION), collapse = ", "))

  return(farm_data)
}


# CROP DATA ----

#' Parse and Structure Crop-Level Data from Harmonised FADN Input
#'
#' @description
#' An internal function that extracts, reshapes, and enriches crop-level
#' variables from a harmonised FADN data frame into a tidy long format with
#' one row per farm × year × crop category. The function handles:
#' \enumerate{
#'   \item Identification and extraction of crop-specific FADN variables
#'     (area, production, sales) based on recognised crop codes.
#'   \item Enrichment with crop metadata (land use type, species) from
#'     \code{\link{data_extra}}.
#'   \item Imputation of missing yields and production quantities for forage
#'     crops and grasslands using French official statistics (SAA Agreste
#'     2020), matched at NUTS3 (département) level.
#' }
#'
#' @details
#' ## Step 1 — Crop Code Identification
#' Valid crop codes are extracted from \code{data_extra$crops$FADN_code_letter},
#' excluding aggregate codes ending in \code{_X}. Codes are sorted by
#' decreasing length to ensure correct regex matching when codes share a
#' common prefix (e.g., \code{WC} before \code{W}).
#'
#' ## Step 2 — Data Extraction and Reshaping
#' Columns matching recognised crop codes are selected from
#' \code{df_harmonized}, pivoted to long format, and then back to wide format
#' to obtain one row per farm × year × crop. Standard FADN variable suffixes
#' are renamed:
#' \itemize{
#'   \item \code{A or TA} → \code{area_ha}: utilised crop area (ha).
#'   \item \code{PRQ} → \code{prod_t}: production quantity (t).
#'   \item \code{SQ} → \code{sales_t}: sales quantity (t).
#' }
#' Rows with zero values are removed early to reduce memory footprint.
#'
#' ## Step 3 — Metadata and Basic Yield
#' Crop metadata (\code{land_use_type}, \code{species}) are joined from
#' \code{data_extra$crops}. A raw yield (\code{yield = prod_t / area_ha}) is
#' calculated.
#'
#' ## Step 4 — Reference Yields (SAA Agreste 2020)
#' For forage crops and grasslands, production quantities are not recorded in
#' the French FADN (RICA), as specified in the French \emph{Instruction de
#' collecte}. Reference yields are derived from the French official
#' agricultural statistics (SAA Agreste 2020, Chiffres définitifs, 11/2021),
#' averaged by crop type and département (NUTS3). Département codes are
#' harmonised, including overseas territories (DOM).
#'
#' ## Step 5 — Yield and Production Imputation
#' Missing yields are imputed using SAA Agreste reference yields, matched by
#' crop type and NUTS3 département. Missing production quantities are then
#' derived as \eqn{prod_t = area\_ha \times yield}.
#'
#' @note
#' \itemize{
#'   \item Input area variables must be in **hectares** and production
#'     variables in **tonnes**. A message is issued as a reminder.
#'   \item The SAA Agreste yield imputation is currently calibrated for the
#'     French FADN (RICA). Adaptation for other national FADNs or the EU FADN
#'     is a planned development.
#'   \item Reference yields (\code{ref_yields}) are recomputed at each call.
#'     Pre-computation and storage in \code{sysdata.rda} is planned to
#'     improve performance.
#' }
#'
#' @param df_harmonized A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing harmonised FADN variables in
#'   wide format, with column names following the convention
#'   \code{<FADN_code_letter>_<variable>} (e.g., \code{WW_TA},
#'   \code{WW_PRQ}).
#' @param farm_data A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing farm-level metadata, including
#'   at minimum the identifier columns (\code{id_cols}) and optionally
#'   \code{NUTS3} (département code for France) used for yield imputation.
#' @param id_cols A \code{character} vector of column names to be used as
#'   farm × year identifiers (e.g., \code{c("FADN_ID", "YEAR")}).
#'
#' @return A \code{\link[tibble]{tibble}} in tidy format with one row per
#' farm × year × crop category, containing:
#' \describe{
#'   \item{...}{Traceability identifier columns (\code{id_cols}).}
#'   \item{FADN_code_letter}{\code{character}. FADN crop category code.}
#'   \item{area_ha}{\code{numeric}. Utilised crop area (ha).}
#'   \item{prod_t}{\code{numeric}. Crop production quantity (t yr\eqn{^{-1}}),
#'     imputed from SAA Agreste yields where missing.}
#'   \item{sales_t}{\code{numeric}. Crop sales quantity (t yr\eqn{^{-1}}),
#'     where available.}
#'   \item{yield}{\code{numeric}. Crop yield (t ha\eqn{^{-1}}), imputed from
#'     SAA Agreste reference yields where missing.}
#'   \item{land_use_type}{\code{character}. Land use type
#'     (\code{"arable"}, \code{"grassland"}, etc.) from
#'     \code{data_extra$crops}.}
#'   \item{species}{\code{character}. Crop or forage species from
#'     \code{data_extra$crops}.}
#'   \item{...}{Additional crop-specific FADN variables present in
#'     \code{df_harmonized} (e.g., subsidies, variable costs per crop).}
#' }
#'
#' @references
#' Agreste (2021). \emph{Statistiques agricoles annuelles 2020, Chiffres
#' définitifs}. Ministère de l'Agriculture et de la Souveraineté alimentaire,
#' Paris. \url{https://agreste.agriculture.gouv.fr}
#'
#' @seealso
#' \code{\link{.parse_farm_data}}, \code{\link{.parse_livestock_data}},
#' \code{\link{data_4FADN2Footprint}}, \code{\link{data_extra}}
#'
#' @importFrom dplyr select filter mutate rename left_join summarise coalesce
#'   pick where any_of all_of matches join_by
#' @importFrom tidyr pivot_longer pivot_wider extract
#'
#' @export
#'
#' @keywords internal

.parse_crop_data <- function(df_harmonized,
                             farm_data,
                             id_cols
) {

  # 1. Identify Crop Codes --------------------------------------------------
  # Filter out codes ending in _X (aggregates) and sort by length (longest first)
  crop_codes <- data_extra$crops$FADN_code_letter |>
    (\(x) x[!grepl("_X", x)])() |>
    unique() |>
    sort(decreasing = TRUE)
  # Create a regex pattern once
  crop_regex <- paste(crop_codes, collapse = "|")

  message("Make sure that your area variables are in hectare and your production variables are in tonnes.")
  message("Only variables for these crop codes are considered: ",
          paste(crop_codes, collapse = ", "))

  # 2. Main Crop Data Extraction --------------------------------------------
  crop_df <- df_harmonized |>
    # Select ID cols and any columns matching our crop codes
    dplyr::select(dplyr::all_of(id_cols), dplyr::matches(crop_regex),-dplyr::matches("_X$")) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "raw_variable",
      values_to = "value"
    ) |>
    # Early filtering of zeros to reduce memory usage before string ops
    dplyr::filter(value > 0) |>
    # Create code variables
    # Regex explanation:
    # ^(crop_codes): Match the code at start (Group 1)
    # _(.*): Match underscore, then everything else is the variable (Group 2)
    tidyr::extract(
      col = "raw_variable",
      into = c("FADN_code_letter", "variable"),
      regex = paste0("^(", crop_regex, ")_(.*)"),
      convert = FALSE
    ) |>
    # Remove entries that didn't match valid codes (NAs generated by extract)
    dplyr::filter(!is.na(FADN_code_letter) & !is.na(variable)) |>
    # Pivot to wide format: One row per Farm-Year-Crop
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(id_cols, "FADN_code_letter")),
      names_from = variable,
      values_from = value
    ) |>
    # area in hectares for each crop: A or TA
    # production quantity in kg for each crop: PRQ
    # sales quantity in kg for each crop: SQ
    # Ensure expected columns exist
    (\(df) {
      if (!"TA" %in% names(df)) df$TA <- NA_real_
      if (!"A"  %in% names(df)) df$A  <- NA_real_
      if (!"PRQ" %in% names(df)) df$PRQ <- NA_real_
      if (!"SQ"  %in% names(df)) df$SQ  <- NA_real_
      df
    })() |>
    dplyr::mutate(
      area_ha = pmax(as.numeric(TA), as.numeric(A), na.rm = T),
      prod_t  = PRQ,
      sales_t = SQ
    ) |>
    dplyr::select(-dplyr::any_of(c("TA", "A", "PRQ", "SQ")))


  # 3. Enhance with Metadata & Basic Yields ---------------------------------
  crop_df <- crop_df |>
    dplyr::left_join(
      data_extra$crops |>
        dplyr::select(dplyr::any_of(c("FADN_code_letter", "land_use_type", "species", "SAA_Agreste_2020"))),
      by = "FADN_code_letter"
    ) |>
    dplyr::mutate(
      # Calculate raw yield (tonnes / ha)
      yield = prod_t / area_ha
    ) |>
    # Clean rows: remove entries with no valid numeric data or missing area
    dplyr::filter(
      !is.na(area_ha),
      rowSums(dplyr::select(dplyr::pick(dplyr::where(is.numeric)), -dplyr::any_of(id_cols)), na.rm = TRUE) > 0
    )

  # 4. Prepare External Yields (SAA Agreste) --------------------------------
  # for forage crops and for grasslands, no production is registered in RICA, as specified in the French "Instruction de collecte" documentation
  # so we used the French official yield data for 2020 (Statistiques agricoles annuelles 2020, Chiffres définitifs, Agreste, 11/2021)

  ref_yields <- data_extra$yield_SAA_Agreste_2020 |>
    dplyr::mutate(
      Departement = dplyr::case_when(
        Departement == "971 - Guadeloupe" ~ "9A",
        Departement == "972 - Martinique" ~ "9B",
        Departement == "973 - Guyane"     ~ "9C",
        Departement == "974 - La Réunion" ~ "9D",
        TRUE ~ substr(Departement, 1, 2)
      ),
      # Calculate yield (t/ha) from production (100kg/cwt -> t) and area
      # formula: (Vol * 0.1) / Area
      yield_SAA = (Production_seche_volume * 0.1) / Superficie_correspondante_hectare
    ) |>
    dplyr::rename(SAA_Agreste_2020 = Cultures_developpees_5) |>
    # Aggregate average yield by Crop Type and Dept
    dplyr::summarise(
      yield_SAA = mean(yield_SAA, na.rm = TRUE),
      .by = c(SAA_Agreste_2020, Departement)
    )

  # 5. Impute Forage Yields -------------------------------------------------
  # Strategy: Join NUTS3 and SAA yields to the main table.
  # Use coalesce() to fill gaps only where needed.

  final_df <- crop_df |>
    # Attach NUTS3 region from farm_data (if available)
    dplyr::left_join(
      farm_data |>
        dplyr::select(dplyr::all_of(id_cols), dplyr::any_of("NUTS3"))|>
        dplyr::mutate(dplyr::across(dplyr::any_of("NUTS3"),as.character)),
      by = id_cols
    ) |>
    # Attach Reference Yields based on Crop Mapping + NUTS3 (or global average per crop)
    (\(df) {
      if ("NUTS3" %in% colnames(df)) {
        dplyr::left_join(
          df,
          ref_yields,
          by = dplyr::join_by(SAA_Agreste_2020, NUTS3 == Departement)
        )
      } else {
        # Compute global average reference yields per crop
        ref_yields_avg <- ref_yields |>
          dplyr::summarise(
            yield_SAA = mean(yield_SAA, na.rm = TRUE),
            .by = SAA_Agreste_2020
          )

        dplyr::left_join(
          df,
          ref_yields_avg,
          by = "SAA_Agreste_2020"
        )
      } })() |>
    dplyr::mutate(
      # Impute Yield: Keep existing yield, otherwise use SAA yield
      ## TODO: check for French FADN
      yield = dplyr::coalesce(yield, yield_SAA),

      # Impute Production: Keep existing prod, otherwise calc Area * Yield
      prod_t = dplyr::coalesce(prod_t, area_ha * yield)
    ) |>
    # Clean up temporary columns
    dplyr::select(-yield_SAA, -SAA_Agreste_2020, -dplyr::any_of("NUTS3"))

  return(final_df)
}

# HERD DATA ----

#' Parse and Structure Herd-Level Data from Harmonised FADN Input
#'
#' @description
#' An internal function that extracts, reshapes, and enriches livestock herd
#' variables from a harmonised FADN data frame into a tidy format with one row
#' per farm × year × livestock category. The function handles:
#' \enumerate{
#'   \item Identification of valid FADN livestock codes from
#'     \code{data_extra$livestock}.
#'   \item Extraction and pivoting of herd variables from wide to long and
#'     back to wide format.
#'   \item Enrichment with livestock metadata (species, livestock unit
#'     coefficients).
#'   \item Calculation and imputation of average livestock units (\code{ALU})
#'     and observed animal numbers (\code{Qobs}).
#'   \item Imputation of missing sales variables using fallback business rules.
#' }
#'
#' @details
#' ## Step 1 — Livestock Code Identification
#' Valid livestock codes are extracted from \code{data_extra$livestock},
#' retaining only entries with a non-missing \code{livestock_unit_coef}.
#' Codes are sorted by decreasing length to ensure correct regex matching
#' when codes share a common prefix. Aggregate codes ending in \code{_X} are
#' excluded.
#'
#' ## Step 2 — Extraction and Reshaping
#' Columns matching recognised livestock codes are selected from
#' \code{df_harmonized}, pivoted to long format, and the raw variable name
#' is split into \code{FADN_code_letter} and \code{variable} suffix using
#' \code{\link[tidyr]{extract}}. The data are then pivoted back to wide
#' format, yielding one row per farm × year × livestock category. Standard
#' FADN variable suffixes include:
#' \itemize{
#'   \item \code{AN}: average number of animals (head).
#'   \item \code{ALU}: average livestock units.
#'   \item \code{ON}: opening number of animals (head).
#'   \item \code{CN}: closing number of animals (head).
#'   \item \code{SN}, \code{SSN}: total sales number / slaughter sales number
#'     (head).
#'   \item \code{SV}, \code{SSV}: total sales value / slaughter sales value
#'     (EUR).
#'   \item \code{SRN}, \code{SRV}: replacement sales number / value.
#' }
#'
#' ## Step 3 — Metadata Join
#' Livestock metadata (\code{livestock_unit_coef}, \code{species}) are joined
#' from \code{data_extra$livestock} by \code{FADN_code_letter}.
#'
#' ## Step 4 — ALU and Qobs Calculation
#' \itemize{
#'   \item \code{ALU} (Average Livestock Units): retained from the input if
#'     available, otherwise computed as \code{AN × livestock_unit_coef}.
#'   \item \code{Qobs} (Observed number of animals, head): estimated as the
#'     row-wise mean of \code{AN}, \code{ALU / livestock_unit_coef},
#'     \code{ON}, and \code{CN}, ignoring \code{NA} values. This averaging
#'     strategy maximises data coverage across FADN datasets that report
#'     different subsets of these variables.
#' }
#' Records with \code{Qobs == 0} are removed.
#'
#' ## Step 5 — Sales Variable Imputation
#' Missing sales variables are imputed using the following fallback rules:
#' \itemize{
#'   \item \code{SSN}: imputed from \code{SN} if missing (all sales assumed
#'     to be for slaughter).
#'   \item \code{SSV}: imputed from \code{SV} if missing.
#'   \item \code{SRN}, \code{SRV}: set to \code{0} if missing (no replacement
#'     sales assumed).
#' }
#' Columns are created with \code{NA} if absent from the input to avoid
#' errors in \code{\link[dplyr]{coalesce}}.
#'
#' @note
#' Input livestock variables must be expressed in the units described in the
#' FADN data dictionary (either livestock units or number of animals). A
#' message listing all recognised livestock codes is printed at runtime.
#'
#' @param df_harmonized A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing harmonised FADN variables in
#'   wide format, with column names following the convention
#'   \code{<FADN_code_letter>_<variable>} (e.g., \code{LCOWDAIR_AN},
#'   \code{LPIGS_ALU}).
#' @param id_cols A \code{character} vector of column names used as farm ×
#'   year identifiers (e.g., \code{c("FADN_ID", "YEAR")}).
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm × year ×
#' livestock category, containing:
#' \describe{
#'   \item{...}{Traceability identifier columns (\code{id_cols}).}
#'   \item{FADN_code_letter}{\code{character}. FADN livestock category code.}
#'   \item{species}{\code{character}. Livestock species from
#'     \code{data_extra$livestock}.}
#'   \item{livestock_unit_coef}{\code{numeric}. Livestock unit conversion
#'     coefficient.}
#'   \item{AN}{\code{numeric}. Average number of animals (head).}
#'   \item{ALU}{\code{numeric}. Average livestock units (LU).}
#'   \item{Qobs}{\code{numeric}. Observed number of animals (head), averaged
#'     across available stock variables.}
#'   \item{ON, CN}{\code{numeric}. Opening and closing number of animals
#'     (head), where available.}
#'   \item{SSN, SSV}{\code{numeric}. Slaughter sales: number (head) and value
#'     (EUR), imputed from total sales if missing.}
#'   \item{SRN, SRV}{\code{numeric}. Replacement sales: number (head) and
#'     value (EUR), defaulting to \code{0} if missing.}
#' }
#'
#' @seealso
#' \code{\link{.parse_farm_data}}, \code{\link{.parse_crop_data}},
#' \code{\link{data_4FADN2Footprint}}, \code{\link{data_extra}}
#'
#' @importFrom dplyr select filter mutate left_join coalesce pull all_of
#'   matches case_when
#' @importFrom tidyr pivot_longer pivot_wider extract
#'
#' @export
#'
#' @keywords internal
#'
.parse_herd_data <- function(df_harmonized, id_cols) {

  # 1. Identify Herd Codes -------------------------------------------------------

  herd_codes <- data_extra$livestock |>
    dplyr::filter(!is.na(livestock_unit_coef)) |>
    dplyr::pull(FADN_code_letter) |>
    unique() |>
    sort(decreasing = TRUE)

  herd_regex <- paste(herd_codes, collapse = "|")

  # User feedback
  message("Make sure that your livestock variables are in the same units as those described in the dictionary (i.e., either livestock unit or number of animals).\n")
  message("Only variables for these livestock codes are considered: ",
          paste0(herd_codes,collapse = ", "))

  # 2. Extraction & Pivoting ------------------------------------------------
  # Filter, Pivot Long, and Split Code/Variable in one pass using tidyr::extract
  long_data <- df_harmonized |>
    dplyr::select(dplyr::all_of(id_cols),
                  dplyr::matches(herd_regex),
                  -dplyr::matches("_X$")) |>
    # Pivot longer to handle all herd variables at once
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "raw_variable",
      values_to = "value"
    ) |>
    # Efficiently split "A_AN" into "A" and "AN".
    # Regex explanation:
    # ^(herdcodes): Match the code at start (Group 1)
    # _(.*): Match underscore, then everything else is the variable (Group 2)
    tidyr::extract(
      col = "raw_variable",
      into = c("FADN_code_letter", "variable"),
      regex = paste0("^(", herd_regex, ")_(.*)"),
      convert = FALSE
    ) |>
    # Remove entries that didn't match valid codes (NAs generated by extract)
    dplyr::filter(!is.na(FADN_code_letter) & !is.na(variable))

  # 4. Reshape & Metadata Join ----------------------------------------------
  wide_data <- long_data |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(id_cols, "FADN_code_letter")),
      names_from = "variable",
      values_from = "value"
    ) |>
    dplyr::left_join(
      data_extra$livestock |>
        dplyr::select(FADN_code_letter,livestock_unit_coef,species),
      by = "FADN_code_letter")

  # 5. Calculations & Imputations -------------------------------------------
  processed_data <- wide_data |>
    # Ensure ALU column exists (filled with NA if absent)
    (\(df) {
      if (!"ALU" %in% names(df)) df$ALU <- NA_real_
      df
    })() |>
    # Calculate ALU where missing
    dplyr::mutate(
      ALU = dplyr::if_else(is.na(ALU) & !is.na(AN),
                           AN * livestock_unit_coef,
                           ALU)
    ) |>
    dplyr::mutate(
      temp_alu_calc = ALU / livestock_unit_coef
    ) |>
    # Ensure ON and CN columns exist (filled with AN if absent)
    (\(df) {
      if (!"ON" %in% names(df)) df$ON <- df$AN
      df
    })() |>
    (\(df) {
      if (!"CN" %in% names(df)) df$CN <- df$AN
      df
    })() |>
    # Calculate Qobs (Observed Quantity of animals in number of head)
    # Mean of Average Number, Computed ALU, Opening, and Closing Numbers
    dplyr::mutate(
      Qobs = rowMeans(
        cbind(AN,temp_alu_calc, ON, CN),
        na.rm = TRUE
      )) |>
    dplyr::select(-temp_alu_calc) |>
    # Filter out empty records (Qobs > 0)
    dplyr::filter(Qobs > 0)

  # 6. Final Imputations (Business Logic) -----------------------------------
  # If sales variables are missing, assume all sales are for slaughter.
  # We use check if columns exist, if not create them with NA, then coalesce.

  # Ensure columns exist to avoid errors in coalesce
  cols_needed <- c("SSN", "SN", "SSV", "SV", "SRN", "SRV")
  for (col in cols_needed) {
    if (!col %in% names(processed_data)) processed_data[[col]] <- NA_real_
  }

  final_data <- processed_data |>
    dplyr::mutate(
      SSN = dplyr::coalesce(SSN, SN), # If SSN missing, take SN
      SSV = dplyr::coalesce(SSV, SV), # If SSV missing, take SV
      SRN = dplyr::coalesce(SRN, 0),  # If SRN missing, assume 0
      SRV = dplyr::coalesce(SRV, 0)   # If SRV missing, assume 0
    )


  return(final_data)
}

# INPUT DATA ----

#' Parse and Structure Farm Input Data from Harmonised FADN Input
#'
#' @description
#' An internal function that extracts, cleans, and enriches farm-level input
#' variables (purchased feeds, fertilisers, pesticides, fuels, and electricity)
#' from a harmonised FADN data frame. Fuel quantities are estimated from
#' expenditure values and country- or EU-level fuel prices where direct
#' quantity records are unavailable.
#'
#' @details
#' ## Step 1 — Variable Selection and Renaming
#' Core FADN input variables are selected and renamed to human-readable names:
#' \itemize{
#'   \item **Purchased feeds** (EUR): concentrates and roughages for grazing
#'     livestock (\code{IGRFEDCNCTRPUR_V}, \code{IGRFEDCRSPUR_V}),
#'     concentrates for pigs (\code{IPIGFEDPUR_V}) and poultry
#'     (\code{IPLTRFEDPUR_V}).
#'   \item **Mineral fertiliser quantity**: nitrogen inputs
#'     (\code{INUSE_Q}, kg N).
#'   \item **Pesticides expenditure** (\code{IPROT_V}, EUR).
#'   \item **Energy expenditure**: motor fuel (\code{IFULS_V}), heating fuel
#'     (\code{IHFULS_V}), and electricity (\code{IELE_V}) in EUR.
#'   \item **Fuel quantities** (litres, optional): \code{IFULS_Q} and
#'     \code{IHFULS_Q} are included where available, otherwise set to
#'     \code{NA}.
#' }
#'
#' ## Step 2 — Zero-Row Filtering
#' Rows where all numeric non-identifier variables are zero or \code{NA} are
#' removed before the fuel price join to reduce memory usage.
#'
#' ## Step 3 — Fuel Price Preparation
#' Country- and year-specific fuel prices are sourced from \code{ref_fuel_wob}:
#' \itemize{
#'   \item \code{PFUEL_ROAD}: price of road/motor fuel (EUR L\eqn{^{-1}}).
#'   \item \code{PFUEL_HEAT}: price of heating fuel (EUR L\eqn{^{-1}}).
#' }
#' EU-27 average prices (mean across all countries by year) are computed as a
#' fallback for country × year combinations with missing price data.
#'
#' ## Step 4 — Quantity Imputation and Energy Conversion
#' \itemize{
#'   \item **Fuel prices**: country-specific price is used where available;
#'     EU-27 average price is used as fallback via
#'     \code{\link[dplyr]{coalesce}}.
#'   \item **Fuel quantities** (litres): observed \code{IFULS_Q} /
#'     \code{IHFULS_Q} are used where available; otherwise estimated as
#'     expenditure divided by the resolved fuel price
#'     (\code{diesel_v / final_price_road}).
#'   \item **Energy content conversion**: fuel quantities are converted to
#'     megajoules using a fixed factor of 37.9 MJ L\eqn{^{-1}} (appropriate
#'     for diesel and light heating fuel).
#' }
#' Intermediate price columns (\code{PFUEL_*}, \code{final_price_*}) are
#' dropped from the output.
#'
#' @param df_harmonized A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing harmonised FADN variables in
#'   wide format. Must include at minimum the columns specified in
#'   \code{id_cols}, \code{COUNTRY}, \code{YEAR}, and the core input
#'   expenditure variables listed above.
#' @param id_cols A \code{character} vector of column names used as farm ×
#'   year identifiers (e.g., \code{c("FADN_ID", "YEAR")}).
#'
#' @return A \code{\link[tibble]{tibble}} with one row per farm × year,
#' containing:
#' \describe{
#'   \item{...}{Traceability identifier columns (\code{id_cols}).}
#'   \item{feed_purch_concent_graz}{\code{numeric}. Purchased concentrate
#'     feed expenditure for grazing livestock (EUR yr\eqn{^{-1}}).}
#'   \item{feed_purch_rough_graz}{\code{numeric}. Purchased roughage
#'     expenditure for grazing livestock (EUR yr\eqn{^{-1}}).}
#'   \item{feed_purch_concent_pigs}{\code{numeric}. Purchased concentrate
#'     feed expenditure for pigs (EUR yr\eqn{^{-1}}).}
#'   \item{feed_purch_concent_poultry}{\code{numeric}. Purchased concentrate
#'     feed expenditure for poultry (EUR yr\eqn{^{-1}}).}
#'   \item{N_min_ferti_Q}{\code{numeric}. Mineral nitrogen fertiliser
#'     quantity (kg N yr\eqn{^{-1}}).}
#'   \item{pesticides_v}{\code{numeric}. Pesticide expenditure
#'     (EUR yr\eqn{^{-1}}).}
#'   \item{diesel_v, heating_fuels_v, elec_V}{\code{numeric}. Energy
#'     expenditure for motor fuel, heating fuel, and electricity
#'     (EUR yr\eqn{^{-1}}).}
#'   \item{diesel_l}{\code{numeric}. Motor fuel consumption (L yr\eqn{^{-1}}),
#'     observed or imputed from expenditure and fuel price.}
#'   \item{heating_fuels_l}{\code{numeric}. Heating fuel consumption
#'     (L yr\eqn{^{-1}}), observed or imputed.}
#'   \item{diesel_MJ}{\code{numeric}. Motor fuel consumption in energy terms
#'     (MJ yr\eqn{^{-1}}), computed as \code{diesel_l × 37.9}.}
#'   \item{heating_fuels_MJ}{\code{numeric}. Heating fuel consumption in
#'     energy terms (MJ yr\eqn{^{-1}}), computed as
#'     \code{heating_fuels_l × 37.9}.}
#' }
#'
#' @references
#' World Oil & Biofuels (WOB) database. Fuel price series used for quantity
#' imputation, accessed via \code{ref_fuel_wob}.
#'
#' @seealso
#' \code{\link{.parse_farm_data}}, \code{\link{.parse_crop_data}},
#' \code{\link{.parse_herd_data}}, \code{\link{data_4FADN2Footprint}},
#' \code{\link{data_extra}}
#'
#' @importFrom dplyr select filter mutate left_join coalesce starts_with
#'   any_of all_of pick where summarise
#'
#' @concept data-preparation
#' @export
#'
#' @keywords internal
#'
.parse_input_data <- function(df_harmonized, id_cols) {

  ref_fuel_data = ref_fuel_wob

  # 1. Select and Rename Core Variables -------------------------------------
  # Using any_of for optional Quantity columns so the code doesn't break if they are missing
  df_base <- df_harmonized |>
    dplyr::select(
      dplyr::all_of(id_cols),
      # Core Values
      feed_purch_concent_graz = "IGRFEDCNCTRPUR_V", # Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value	in EUR
      feed_purch_rough_graz   = "IGRFEDCRSPUR_V", # Purchased coarse fodder for grazing stock (equines, ruminants) Value	in EUR
      feed_purch_concent_pigs = "IPIGFEDPUR_V", # Purchased feedstuffs for pigs Value	in EUR
      feed_purch_concent_poultry = "IPLTRFEDPUR_V", # Purchased feedstuffs for poultry and other small animals Value	in EUR
      N_min_ferti_Q    = "INUSE_Q",   # Quantity of N used in mineral fertilisers Quantity	in tonnes
      pesticides_v = "IPROT_V",   # Crop protection products Value	in EUR
      diesel_v  = "IFULS_V",   # Motor fuels and lubricants Value	in EUR
      heating_fuels_v = "IHFULS_V", # Heating fuels Value	in EUR
      elec_V    = "IELE_V",  # Farming overheads. Electricity. Value	in EUR
    # Optional Quantities (may or may not exist in raw data)
    dplyr::any_of(c(
      diesel_l = "IFULS_Q",
      heating_fuels_l = "IHFULS_Q"
    ))
    ) |>
    ## create variable if they do not exist
    dplyr::mutate(
      diesel_l = (if ("IFULS_Q" %in% names(df_harmonized)) diesel_l else NA_real_),
      heating_fuels_l = (if ("IHFULS_Q" %in% names(df_harmonized)) heating_fuels_l else NA_real_))

  # 2. Filter Zero Rows -----------------------------------------------------
  # Remove rows where all numeric value variables are zero/NA.
  # We do this before the expensive join.
  # Note: logic excludes IDs.
  df_filtered <- df_base |>
    dplyr::filter(
      rowSums(
        dplyr::select(
          dplyr::pick(where(is.numeric)),
          -dplyr::any_of(id_cols)
        ),
        na.rm = TRUE
      ) > 0
    )

  # 3. Fuel Price Preparation (WOB Data) ------------------------------------
  # Prepare Country-Specific prices
  prices_country <- ref_fuel_data |>
    dplyr::select(COUNTRY, YEAR, PFUEL_ROAD, PFUEL_HEAT) |>
    dplyr::mutate(YEAR = as.character(YEAR))

  # Prepare EU Fallback prices (Resolution for TODO regarding NAs)
  # We use this if specific country data is missing
  prices_eu <- ref_fuel_data |>
    dplyr::summarise(
      PFUEL_ROAD_EU = mean(PFUEL_ROAD, na.rm = T),
      PFUEL_HEAT_EU = mean(PFUEL_HEAT, na.rm = T),
      .by = YEAR) |>
    dplyr::mutate(YEAR = as.character(YEAR))

  # 4. Calculation and logic application ------------------------------------
  df_final <- df_filtered |>
    # A. Join Prices
    dplyr::left_join(prices_country, by = c("COUNTRY", "YEAR")) |>
    dplyr::left_join(prices_eu, by = "YEAR") |>
    # B. Compute/Coalesce Quantities
    dplyr::mutate(
      # PRICE: Use Country price, EU‑27 fallback for missing price
      final_price_road = dplyr::coalesce(PFUEL_ROAD, PFUEL_ROAD_EU),
      final_price_heat = dplyr::coalesce(PFUEL_HEAT, PFUEL_HEAT_EU),

      # QUANTITY:
      # 1. If raw quantity (IFULS_Q) exists, use it.
      # 2. Else, calculate Value / Price.
      diesel_l = dplyr::coalesce(diesel_l, diesel_v / final_price_road),
      heating_fuels_l = dplyr::coalesce(heating_fuels_l, heating_fuels_v / final_price_heat),

      # C. Energy Content Conversion
      # 1 liter diesel = 37.9 MJ
      diesel_MJ = diesel_l * 37.9,
      heating_fuels_MJ = heating_fuels_l * 37.9
    ) |>
    # D. Cleanup
    ## Remove price columns
    dplyr::select(
      -dplyr::starts_with("PFUEL"),
      -dplyr::starts_with("final_price")
    )

  return(df_final)
}

# OUTPUT DATA ----

#' Parse and Structure Farm Output Data from Harmonised FADN Input
#'
#' @description
#' An internal function that extracts, structures, and enriches farm output
#' variables across four output categories from harmonised FADN data:
#' \enumerate{
#'   \item **Crop outputs**: production quantities, sales, and yields per
#'     crop × farm × year.
#'   \item **Animal products** (milk, eggs, wool): quantities and yields
#'     linked to their producing herd categories.
#'   \item **Meat outputs**: live-weight production and sales estimates for
#'     animals sold for slaughter.
#'   \item **Living animal outputs**: rearing sales (animals sold for
#'     reproduction or further fattening).
#' }
#'
#' @details
#' ## Step 1 — Metadata Setup
#' Valid animal product output codes are extracted from
#' \code{data_extra$output}, filtering out entries with missing
#' \code{output} labels. A regex pattern is built from these codes (sorted
#' by decreasing length) for column matching.
#'
#' ## Step 2 — Crop Outputs
#' Sourced directly from the pre-parsed \code{crop_data} object. Standard
#' FADN crop variables are mapped as:
#' \itemize{
#'   \item \code{SV} → \code{sales_e}: crop sales value (EUR).
#'   \item \code{TO} → \code{output_e}: total crop output value (EUR).
#'   \item \code{prod_t}, \code{sales_t}, \code{yield}: carried over from
#'     \code{crop_data}.
#' }
#' Rows with zero production and zero sales value are excluded.
#'
#' ## Step 3 — Other Animal Products (Milk, Eggs, Wool)
#' Animal product variables are extracted from \code{df_harmonized} by
#' matching recognised output codes, pivoted to long format, split into
#' \code{FADN_code_letter} and \code{variable} suffix via
#' \code{\link[tidyr]{extract}}, and pivoted back to wide format. Standard
#' FADN suffixes are renamed:
#' \itemize{
#'   \item \code{PRQ} → \code{prod_t}: production quantity (t).
#'   \item \code{SQ}  → \code{sales_t}: sales quantity (t).
#'   \item \code{SV}  → \code{sales_e}: sales value (EUR).
#'   \item \code{TO}  → \code{output_e}: total output value (EUR).
#' }
#' Output metadata (\code{output}, \code{species}) are joined from
#' \code{data_extra$output}. Producer animals are linked via a
#' one-to-many mapping table (\code{from_livestock_category} in
#' \code{data_extra$output}, semicolon-delimited), joined against
#' \code{herd_data} to retrieve observed animal numbers (\code{Qobs}).
#' Yield is computed as \code{prod_t / Qobs}.
#'
#' ## Step 4 — Meat Outputs
#' Sourced from \code{herd_data}. Animals sold for slaughter
#' (\code{SSN}) are assigned a live-weight production quantity using
#' default live weights from \code{data_extra$livestock}:
#' \deqn{\text{prod\_t} = \text{SSN} \times \frac{\text{live\_weight\_kg}}{1000}}
#' Output values (\code{output_e}) are pro-rated from total output
#' (\code{TO}) proportionally to slaughter sales value (\code{SSV}) over
#' total sales value (\code{SV}). Meat output type is categorised by
#' species and livestock code:
#' \itemize{
#'   \item \code{LBOV1} → \code{meat_veal}
#'   \item \code{LCOWDAIR} → \code{meat_cull_cow}
#'   \item cattle → \code{meat_beef}; swine → \code{meat_pork};
#'     poultry → \code{meat_chicken}; others → \code{meat}.
#' }
#' Multiple yield metrics are computed (per animal sold, per observed
#' animal).
#'
#' ## Step 5 — Living Animal Outputs (Rearing Sales)
#' Animals sold for reproduction or further fattening (\code{SRN},
#' \code{SRV}) are extracted from \code{herd_data}. Output values are
#' pro-rated from \code{TO} proportionally to rearing sales value over
#' total sales value.
#'
#' @param df_harmonized A \code{\link[tibble]{tibble}} or
#'   \code{\link[base]{data.frame}} containing harmonised FADN variables in
#'   wide format. Must include \code{id_cols} columns and any animal product
#'   output variables matching recognised FADN output codes.
#' @param crop_data A \code{\link[tibble]{tibble}} as returned by
#'   \code{\link{.parse_crop_data}}, containing one row per farm × year ×
#'   crop with at minimum \code{FADN_code_letter}, \code{species},
#'   \code{prod_t}, \code{sales_t}, \code{yield}, \code{SV}, and \code{TO}.
#' @param herd_data A \code{\link[tibble]{tibble}} as returned by
#'   \code{\link{.parse_herd_data}}, containing one row per farm × year ×
#'   livestock category with at minimum \code{FADN_code_letter},
#'   \code{species}, \code{Qobs}, \code{SSN}, \code{SSV}, \code{SRN},
#'   \code{SRV}, \code{SV}, and \code{TO}.
#' @param id_cols A \code{character} vector of column names used as farm ×
#'   year identifiers (e.g., \code{c("FADN_ID", "YEAR")}).
#'
#' @return A named \code{list} of four \code{\link[tibble]{tibble}}s:
#' \describe{
#'   \item{\code{crop}}{One row per farm × year × crop. Columns:
#'     \code{id_cols}, \code{FADN_code_letter}, \code{output},
#'     \code{species}, \code{prod_t} (t), \code{sales_t} (t),
#'     \code{sales_e} (EUR), \code{output_e} (EUR), \code{yield}
#'     (t ha\eqn{^{-1}}).}
#'   \item{\code{other_herd_products}}{One row per farm × year × output
#'     product × producing livestock category. Columns: \code{id_cols},
#'     \code{FADN_code_letter_output}, \code{output}, \code{species},
#'     \code{prod_t} (t), \code{sales_t} (t), \code{sales_e} (EUR),
#'     \code{output_e} (EUR), \code{FADN_code_letter} (producing livestock
#'     category), \code{Qobs} (head), \code{yield} (t head\eqn{^{-1}}).}
#'   \item{\code{meat}}{One row per farm × year × livestock category.
#'     Columns: \code{id_cols}, \code{FADN_code_letter}, \code{output},
#'     \code{species}, \code{sales_nb} (head), \code{prod_t} (t),
#'     \code{sales_t} (t), \code{sales_e} (EUR), \code{output_e} (EUR),
#'     \code{yield_t_sales_nb} (t head\eqn{^{-1}}),
#'     \code{prop_sales_Qobs}, \code{yield_t_Qobs}
#'     (t head\eqn{^{-1}}), \code{yield_e_Qobs} (EUR head\eqn{^{-1}}).}
#'   \item{\code{living_animals}}{One row per farm × year × livestock
#'     category. Columns: \code{id_cols}, \code{FADN_code_letter},
#'     \code{output}, \code{species}, \code{prod_nb} (head),
#'     \code{sales_nb} (head), \code{sales_e} (EUR),
#'     \code{output_e} (EUR).}
#' }
#'
#' @seealso
#' \code{\link{.parse_crop_data}}, \code{\link{.parse_herd_data}},
#' \code{\link{.parse_farm_data}}, \code{\link{new_FADN2Footprint}},
#' \code{\link{data_extra}}
#'
#' @importFrom dplyr select filter mutate left_join rename coalesce
#'   case_when if_else all_of matches starts_with pull
#' @importFrom tidyr pivot_longer pivot_wider extract separate_longer_delim
#'
#' @export
#'
#' @keywords internal
#'
.parse_output_data <- function(df_harmonized, crop_data, herd_data, id_cols) {

  # 1. Setup Metadata -------------------------------------------------------
  # Extract valid output codes for animal products
  output_meta <- data_extra$output |>
    dplyr::filter(!is.na(output))

  output_codes <- sort(unique(output_meta$FADN_code_letter), decreasing = TRUE)
  output_regex <- paste(output_codes, collapse = "|")

  # Informative message (kept from original, but shorter)
  message(
    "Production variables must use dictionary units (EUR or tonnes).\n",
    "Animal products considered: ", paste(output_codes, collapse = ", "),
    "\nCrop production and slaughtered animals are included."
  )

  # 2. Crops Output ---------------------------------------------------------
  output_crops <- crop_data |>
    dplyr::mutate(
      output = "crop",
      sales_e = SV,
      output_e = TO
    ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter, output, species,
      prod_t, sales_t, sales_e, output_e, yield
    ) |>
    dplyr::filter(prod_t > 0 | sales_e > 0)

  # 3. Other Animal Products (Milk, Wool, Eggs) -----------------------------
  # Strategy: Pivot Long -> Clean Code -> Pivot Wide -> Join Meta -> Join Herd
  output_herd_products <- df_harmonized |>
    dplyr::select(dplyr::all_of(id_cols), dplyr::matches(output_regex), -dplyr::matches("_X$")) |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(id_cols),
      names_to = "raw_var",
      values_to = "value"
    ) |>
    # Create code variables
    # Regex explanation:
    # ^(output_regex): Match the code at start (Group 1)
    # _(.*): Match underscore, then everything else is the variable (Group 2)
    tidyr::extract(
      col = raw_var,
      into = c("FADN_code_letter", "variable"),
      regex = paste0("^(", output_regex, ")_(.*)"),
      convert = FALSE
    ) |>
    # Remove entries that didn't match valid codes (NAs generated by extract)
    dplyr::filter(!is.na(FADN_code_letter) & !is.na(variable)) |>
    # Pivot back to columns
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(id_cols, "FADN_code_letter")),
      names_from = variable,
      values_from = value
    ) |>
    # Add metadata
    dplyr::left_join(
      output_meta |> dplyr::select(FADN_code_letter, output, species),
      by = "FADN_code_letter"
    ) |>
    # Standardize column names
    dplyr::rename(
      prod_t = PRQ,
      sales_t = SQ,
      sales_e = SV,
      output_e = TO
    ) |>
    # filter for production or sales
    dplyr::filter(prod_t > 0 | sales_e > 0)|>
    # Prepare mapping for Producer Animals (One-to-Many handling)
    # Output codes (e.g., Milk) map to Livestock codes (e.g., Dairy Cows)
    (\(x) {
      products_df <- x

      # Prepare mapping table
      map_livestock <- output_meta |>
        dplyr::select(FADN_code_letter,from_livestock_category) %>%
        dplyr::filter(!is.na(from_livestock_category)) %>%
        tidyr::separate_longer_delim(from_livestock_category,";")

      # Prepare herd data for joining
      herd_subset <- herd_data |>
        dplyr::select(dplyr::all_of(id_cols), FADN_code_letter, Qobs) |>
        dplyr::rename(from_livestock_category = FADN_code_letter)|>
        dplyr::left_join(
          map_livestock,
          by = "from_livestock_category", # livestock code
          relationship = "many-to-many"
        )

      # Join Products -> Mapping -> Herd Size
      products_df |>
        dplyr::left_join(
          herd_subset,
          by = c(id_cols, "FADN_code_letter") # output code
        )
    })() |>
    # Calculate Yields
    dplyr::mutate(yield = prod_t / Qobs) |>
    # Clean up structure
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter_output = FADN_code_letter,
      output, species, prod_t, sales_t, sales_e, output_e,
      FADN_code_letter = from_livestock_category,
      Qobs, yield
    )


  # 4. Meat Output ----------------------------------------------------------

  # Default animal weights
  #UNFCCC_data$table3As2
  livestock_weights <- data_extra$livestock |>
    dplyr::select(FADN_code_letter, live_weight_kg) |>
    dplyr::filter(!is.na(live_weight_kg))

  # Animals sold for slaughter
  output_herd_meat <- herd_data |>
    dplyr::left_join(livestock_weights, by = "FADN_code_letter") |>
    dplyr::mutate(
      # Categorize Output
      output = dplyr::case_when(
        FADN_code_letter == "LBOV1" ~ "meat_veal",
        FADN_code_letter == "LCOWDAIR" ~ "meat_cull_cow",
        species == "cattle" ~ "meat_beef",
        species == "swine" ~ "meat_pork",
        species == "poultry" ~ "meat_chicken",
        TRUE ~ "meat"
      ),
      # Meat Logic
      ## estimate number of animals sold for slaughter
      sales_nb = dplyr::coalesce(SSN, 0),
      ## estimate live weight meat quantity
      prod_t   = sales_nb * (live_weight_kg / 1000), # kg to tonnes
      sales_t  = prod_t,
      sales_e  = dplyr::coalesce(SSV, 0),

      # Handle Division by Zero for Output Value pro-rating
      # Logic: output_e = TO * (Sales_Value_Slaughter / Total_Sales_Value)
      output_e = dplyr::if_else(
        SV > 0,
        TO * (dplyr::coalesce(SSV, 0) / SV),
        0
      ),

      # Yields
      yield_t_sales_nb = live_weight_kg / 1000,
      prop_sales_Qobs  = dplyr::if_else(Qobs > 0, sales_nb / Qobs, 0),
      yield_t_Qobs     = dplyr::if_else(Qobs > 0, sales_t / Qobs, 0),
      yield_e_Qobs     = dplyr::if_else(Qobs > 0, sales_e / Qobs, 0)
    ) |>
    # Clean
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter, output, species,
      sales_nb, prod_t, sales_t, sales_e, output_e,
      yield_t_sales_nb, prop_sales_Qobs, yield_t_Qobs, yield_e_Qobs
    ) |>
    dplyr::filter(prod_t > 0 | sales_e > 0)


  # 5. Living Animals (Rearing) ---------------------------------------------
  output_herd_animals <- herd_data |>
    dplyr::mutate(
      output = "living_animals",
      prod_nb = dplyr::coalesce(SRN, 0), # Number of animals
      sales_nb = prod_nb,
      sales_e = dplyr::coalesce(SRV, 0),
      # Pro-rate Total Output based on Rearing Sales vs Total Sales
      output_e = dplyr::if_else(
        SV > 0,
        TO * (dplyr::coalesce(SRV, 0) / SV),
        0
      )
    ) |>
    dplyr::select(
      dplyr::all_of(id_cols),
      FADN_code_letter, output, species,
      prod_nb, sales_nb, sales_e, output_e
    ) |>
    dplyr::filter(prod_nb > 0 | sales_e > 0)


  # 6. Return List ----------------------------------------------------------
  list(
    crop = output_crops,
    living_animals = output_herd_animals,
    meat = output_herd_meat,
    other_herd_products = output_herd_products
  )
}

