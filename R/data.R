# Mock data ----
#' Create a mock (non-confidential) FADN dataset for testing FADN2Footprint
#'
#' @description
#' This script builds a shareable *mock* (anonymised / perturbed) dataset from
#' confidential raw FADN microdata so that users can test the
#' \pkg{FADN2Footprint} workflow without accessing real farm records.
#'
#' The script:
#' \enumerate{
#'   \item Loads the confidential dataset and a variable dictionary.
#'   \item Drops direct identifiers and keeps only variables required by the package.
#'   \item Pre-processes categorical variables by collapsing rare levels into \code{"Other"}.
#'   \item Pre-processes continuous variables via 1\% / 99\% winsorisation (clipping extremes).
#'   \item Draws a stratified weighted sample (20\%) by \code{NUTS0}, \code{YEAR} and \code{TF_GEN1},
#'         excluding strata with too few farms.
#'   \item Adds random noise to continuous variables (Gaussian noise with SD = 10\% of IQR),
#'         optionally enforcing non-negativity for variables whose minimum is 0 in the raw data.
#'   \item Produces a basic validation report comparing summary statistics and selected categorical
#'         distributions between the real and mock datasets.
#'   \item Saves \code{mock_data} as an internal package dataset using \code{usethis::use_data()}.
#' }
#'
#' @details
#' ## Inputs (local / confidential)
#' \itemize{
#'   \item \code{FADN_raw_data.RData}: Must contain raw FADN microdata.
#'   \item \code{inst/scripts/dictionary_FADN.R}: Must create \code{dict_FADN} with (at least)
#'         the columns \code{var_common}, \code{type} (values \code{"categorical"} or \code{"continuous"}),
#'         and \code{needed_in_FADN2Footprint} (e.g., \code{"T"} for required variables).
#' }
#'
#' ## Outputs
#' \itemize{
#'   \item \code{mock_data}: A data.frame/tibble saved into the package data using
#'         \code{usethis::use_data(mock_data, overwrite = TRUE)}.
#'   \item \code{validation_report}: A list (created in-memory) with:
#'         \itemize{
#'           \item \code{summary_comparison}: mean and SD comparison for up to 10 continuous variables.
#'           \item \code{categorical_comparison}: proportional tables for up to 5 categorical variables.
#'         }
#' }
#'
#' @section Confidentiality / disclosure control:
#' This script is a *utility* to reduce disclosure risk, but it does **not** guarantee anonymisation.
#' In particular, users should review:
#' \itemize{
#'   \item the identifier drop list (\code{ID}, \code{PACAGE}, \code{SIRET}, \code{NUTS3});
#'   \item rare-category threshold (\eqn{\le 3} occurrences are collapsed to \code{"Other"});
#'   \item winsorisation limits (1st and 99th percentiles);
#'   \item noise magnitude (10\% of IQR);
#'   \item sampling layers and minimum stratum size (\code{n_farms/5 >= 3}).
#' }
#'
#' @section Dependencies:
#' Requires \pkg{dplyr}, \pkg{tidyr}, and \pkg{usethis}.
#'
#' @section Side effects:
#' \itemize{
#'   \item Loads a local \code{.RData} file.
#'   \item Sources \code{dictionary_FADN.R}.
#'   \item Writes/updates a package dataset via \code{usethis::use_data()}.
#' }
#'
#' @seealso
#' \code{\link[usethis]{use_data}}
#'
#' @docType data
#' @keywords datasets
#' @name mock_data
#' @keywords internal
#'
"mock_data"

# Mock French data ----
#' RICA database
#'
#' A Fictitious RICA database (each column is generated based on the mean and the standard deviation of the 2020 original RICA database)
#'
#' @docType data
#' @name rica_fict
#'
#' @format A list of four dataframes: rica, veg, ani, pan
#'
#' @source Ministère De L’Agriculture - SSP, 2021. Réseau d’Information Comptable Agricole - 2020. https://doi.org/10.34724/CASD.66.4159.V1
#'
#' @keywords internal
#' @keywords datasets
#' @usage data(rica_fict)
#' @examples
#' data(rica_fict)
#' str(rica_fict)
"rica_fict"

# FADN averages ----

#' FADN-derived Regional Averages for Livestock Parameters
#'
#' A named list of internal reference datasets aggregated from farm-level FADN
#' data. Each element contains regional averages at the NUTS2 level, with
#' country-level and EU-level fallbacks applied to handle missing observations.
#' This dataset is used internally by the \code{FADN2Footprint} package to
#' supply default parameter values when farm-level data are insufficient.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{sales_shares}{A data frame with one row per NUTS2 region containing
#'     the average share of slaughter sales (\code{_share_SSN}) and rearing
#'     sales (\code{_share_SRN}) over total livestock sales (\code{SN}), for
#'     each livestock category. See Details for the imputation hierarchy.
#'     Variables include:
#'     \describe{
#'       \item{NUTS2}{NUTS2 regional code (character).}
#'       \item{COUNTRY}{Country code (character).}
#'       \item{[PREFIX]_share_SSN}{Weighted average share of slaughter sales,
#'         bounded in \eqn{[0, 1]}.}
#'       \item{[PREFIX]_share_SRN}{Weighted average share of rearing sales,
#'         bounded in \eqn{[0, 1]}.}
#'     }
#'   }
#'   \item{nuts2_GE}{A data frame with one row per NUTS2 region x livestock
#'     category containing the mean and standard deviation of daily gross energy
#'     intake per animal, derived from \code{f_herd_feed()}. Variables include:
#'     \describe{
#'       \item{FADN_code_letter}{Livestock category code as used in FADN
#'         (character).}
#'       \item{NUTS2}{NUTS2 regional code (character).}
#'       \item{COUNTRY}{Country code (character).}
#'       \item{mean_GE_MJ_anim}{Mean daily gross energy intake per animal
#'         (MJ animal\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{sd_GE_MJ_anim}{Standard deviation of daily gross energy intake
#'         per animal (MJ animal\eqn{^{-1}} day\eqn{^{-1}}).}
#'     }
#'   }
#' }
#'
#' @details
#' Both datasets follow the same three-step geographic imputation hierarchy to
#' handle missing values:
#' \enumerate{
#'   \item Weighted mean at \strong{NUTS2} level (primary).
#'   \item Weighted mean at \strong{country} level if the NUTS2 value is
#'     \code{NA}.
#'   \item Unweighted mean at \strong{EU} level if both NUTS2 and country
#'     values are \code{NA}.
#' }
#'
#' \strong{sales_shares} — Shares are computed at farm level as:
#' \deqn{\text{share\_SSN} = \frac{\text{SSN}}{\text{SN}}, \quad
#'       \text{share\_SRN} = \frac{\text{SRN}}{\text{SN}}}
#' and set to \code{NA} when \code{SN = 0}. Aggregation uses FADN farm
#' weights (\code{WF}).
#'
#' \strong{nuts2_GE} — Gross energy intake is derived from
#' \code{\link{f_herd_feed}} and expressed as a daily value by dividing the
#' annual estimate by 365:
#' \deqn{GE_{\text{day}} = \frac{GE_{\text{year}}}{365}}
#'
#' @note This is an internal package dataset produced during the
#'   \code{FADN2Footprint} data preparation pipeline. It is not intended for
#'   direct use by end users but can be accessed via
#'   \code{FADN2Footprint:::FADN_averages}.
#'
#' @seealso
#'   \code{\link{h_average_practices}},
#'   \code{\link{f_herd_feed}}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Access the full list
#' FADN2Footprint:::FADN_averages
#'
#' # Access livestock sales shares by NUTS2
#' FADN2Footprint:::FADN_averages$sales_shares
#'
#' # Access gross energy averages by NUTS2 and livestock category
#' FADN2Footprint:::FADN_averages$nuts2_GE
#' }
"FADN_averages"




# Data extra ----
#' Extra reference tables and parameters used by FADN2Footprint
#'
#' `data_extra` is an **internal** auxiliary database distributed with the
#' **FADN2Footprint** package. It is a *named list* of tables providing:
#' \itemize{
#'  \item crop, livestock and product mapping keys (FADN ↔ RICA ↔ other nomenclatures),
#'  \item French reference values for fertilization and pesticide indicators,
#'  \item grassland yields and feed intake / composition reference data,
#'  \item and default parameters for GHG and biodiversity-impact calculations.
#' }
#'
#'
#' @format A named list of 15 tibbles:
#' \describe{
#'   \item{\code{country_names}}{
#'     A data frame mapping various country codes.}
#'
#'   \item{\code{French_FQS}}{
#'     French Food Quality Schemes (FQS) lookup table used to link
#'     survey/FQS information to RICA variables.
#'     Source: Corre, T., Pomeon, T., Regolo, J. (2023). \emph{Méthode de sirétisation ODR.}}
#'
#'   \item{\code{crops}}{
#'     Crop mapping table used to harmonise crop codes and attach attributes
#'     needed for footprint calculations and reporting.
#'     }
#'
#'   \item{\code{PKGC_N_ferti}}{
#'     National average mineral and organic nitrogen fertilisation values (France),
#'     derived from PKGC 2017.
#'     Source: Ministère de l'Agriculture - SSP (2019). \emph{Pratiques culturales sur
#'     les grandes cultures - 2017}. \doi{10.34724/CASD.56.3033.V1}
#'   }
#'
#'   \item{\code{PKGC_N_ferti_org_thresholds}}{
#'     Minimum/maximum thresholds for organic fertilisation (France),
#'     estimated from PKGC 2017.
#'     Source: Ministère de l'Agriculture - SSP (2019). \emph{Pratiques culturales sur
#'     les grandes cultures - 2017}. \doi{10.34724/CASD.56.3033.V1}
#'   }
#'
#'   \item{\code{IFT_ref}}{
#'     Reference Treatment Frequency Index (IFT/TFI) averages (France), from PKGC 2017.
#'     Source: Ministère de l'Agriculture - SSP (2019). \emph{Pratiques culturales sur
#'     les grandes cultures - 2017}. \doi{10.34724/CASD.56.3033.V1}
#'   }
#'
#'   \item{\code{livestock}}{
#'     Livestock mapping table and parameters used to harmonise livestock
#'     categories and connect them to IPCC reporting categories.
#'     }
#'
#'   \item{\code{yield_SAA_Agreste_2020}}{
#'     NUTS3-level average yields from the French annual agricultural statistics (SAA),
#'     for grasslands and forages, year 2020.
#'     Source: Agreste (2020). \emph{Statistique agricole annuelle (SAA) - 2020 - Départements.}
#'   }
#'
#'   \item{\code{Sailley_2021_feed_flows}}{
#'     Estimated feed consumption by animal production sector in France in 2015
#'     (in \eqn{\times 1000} t of DM85).
#'     Source: Sailley, M. et al. (2021). \emph{Quantifier et segmenter les flux de matières
#'     premières utilisées en France par l’alimentation animale}. INRAE Productions Animales 34,
#'     273--292. \doi{10.20870/productions-animales.2021.34.4.5396}
#'   }
#'
#'   \item{\code{AROPAJ_France}}{
#'     Total dry matter intake (DMI) for livestock categories used for feeding/allocation keys.
#'     Source: Jayet, P.-A. et al. (2023). \emph{The European agro-economic model AROPAj (report)}.
#'     INRAE-PSAE. \doi{10.17180/nxw3-3537}
#'   }
#'
#'   \item{\code{feed_table_all_as_DM}}{
#'     Feed nutritional composition table expressed on a dry matter (DM) basis, used to
#'     build feed intake and allocation keys.
#'     Source: Tran, G. (2002). \emph{Tables of composition and nutritional values of feed materials}.
#'     INRA/CIRAD/AFZ.
#'   }
#'
#'   \item{\code{output}}{
#'     Mapping table for animal products aligning FADN product codes with species and the
#'     originating livestock category.
#'   }
#'
#'   \item{\code{BVIAS_var_constant}}{
#'     Constants used in the Biodiversity Value Impact model (BVIAS)
#'     to convert practices into biodiversity contributions.
#'     Source: Huet, S. et al. (2025). \emph{Estimating Biodiversity Impact from Agricultural Statistics:
#'     An Application to Food Quality Schemes in France}. SSRN. \doi{10.2139/ssrn.5217233}
#'   }
#'
#'   \item{\code{BVIAS_var_weight}}{
#'     Aggregation weights of agricultural practices used in the Biodiversity Value Impact model (BVIAS).
#'     Source: Huet, S. et al. (2025). \emph{Estimating Biodiversity Impact from Agricultural Statistics:
#'     An Application to Food Quality Schemes in France}. SSRN. \doi{10.2139/ssrn.5217233}
#'   }
#'
#'   \item{\code{IPCC_default_values}}{
#'     Default values used to estimate GHG emissions from managed soils and manure management.
#'     Sources include: Gavrilova, O. et al. (2019). Chapter 10 in the \emph{2019 Refinement to the 2006
#'     IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4 (AFOLU)}; and IPCC (2019),
#'     \emph{Emissions from livestock and manure management} (2019 Refinement).
#'   }
#' }
#'
#' @details
#' This dataset is intended for internal package use (lookups/parameters). Its structure
#' may evolve as mappings and parameter sources are extended.
#' The object is built from the Excel workbook
#' `inst/ext_data/data_extra_FADN.xlsx` (one sheet per list element).
#'
#' @source Built from `inst/ext_data/data_extra_FADN.xlsx` (package internal file), according to:
#' \itemize{
#'   \item \strong{French_FQS}: Corre, T., et al., 2023. Méthode de sirétisation ODR.
#'   \item \strong{BVIAS_var_constant, BVIAS_var_weight}: Huet, S., Diallo, A., Regolo, J., Ihasusta, A., Arnaud, L., Bellassen, V., 2025. Estimating Biodiversity Impact from Agricultural Statistics: An Application to Food Quality Schemes in France. \doi{10.2139/ssrn.5217233}.
#'   \item \strong{PKGC_N_ferti, PKGC_N_ferti_org_thresholds, IFT_ref}: Ministère De L’Agriculture - SSP, 2019. Pratiques culturales sur les grandes cultures - 2017. \doi{10.34724/CASD.56.3033.V1}.
#'   \item \strong{yield_SAA_Agreste_2020}: Agreste, 2020. Statistique agricole annuelle (SAA).
#'   \item \strong{Sailley_2021_feed_flows}: Sailley, M., et al., 2021. Quantifier et segmenter les flux de matières premières utilisées en France par l’alimentation animale. INRAE Productions Animales. \doi{10.20870/productions-animales.2021.34.4.5396}.
#'   \item \strong{AROPAJ_France}: Jayet, P.-A., et al., 2023. The European agro-economic model AROPAj (report). INRAE-PSAE. \doi{10.17180/nxw3-3537}.
#'   \item \strong{feed_table_all_as_DM}: Tran, G., 2002. Tables of composition and nutritional values of feed materials INRA CIRAD AFZ.
#'   \item \strong{IPCC_default_values}: Gavrilova, O., et al., 2019. Chapter 10: Emissions from livestock and manure management. In: 2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 4: AFOLU. and Hergoualc’h, K., et al., 2019. Chapter 11: N2O emissions from managed soils, and CO2 emissions from lime and urea application. In: 2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 4: AFOLU.
#' }
#'
#' @references
#'
#' Corre, T., Pomeon, T., Regolo, J., (2023). *Méthode de sirétisation ODR.*
#'
#' Gavrilova, O., Leip, A., Dong, H., MacDonald, J.D., Gomez Bravo, C.A., Amon, B., et al. (2019).
#' *Chapter 10: Emissions from livestock and manure management*. In:
#' *2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4 (AFOLU)*. IPCC.
#'
#' Hergoualc’h, K., Akiyama, H., Bernoux, M., Chirinda, N., del Prado, A., Kasimir, Å., et al. (2019).
#' *Chapter 11: N2O emissions from managed soils, and CO2 emissions from lime and urea application*. In:
#' *2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4 (AFOLU)*. IPCC.
#'
#' Huet, S., Diallo, A., Regolo, J., Ihasusta, A., Arnaud, L., Bellassen, V. (2025).
#' Estimating Biodiversity Impact from Agricultural Statistics: An Application to Food Quality Schemes in France.
#' \doi{10.2139/ssrn.5217233}
#'
#' Jayet, P.-A., et al. (2023). *The European agro-economic model AROPAj (report)*.
#' INRAE-PSAE. \doi{10.17180/nxw3-3537}
#'
#' Ministère de l’Agriculture - SSP (2019). *Pratiques culturales sur les grandes cultures - 2017*.
#' CASD. \doi{10.34724/CASD.56.3033.V1}
#'
#' Sailley, M., Cordier, C., Courtonne, J.-Y., Duflot, B., Cadudal, F., Perrot, C., Brion, A.,
#' Baumont, R. (2021). Quantifier et segmenter les flux de matières premières utilisées en France
#' par l’alimentation animale. *INRAE Productions Animales*, 34, 273–292.
#' \doi{10.20870/productions-animales.2021.34.4.5396}
#'
#' Tran, G. (2002). INRA / CIRAD / AFZ. *Tables of composition and nutritional values of feed materials*.
#'
#'
#' @source internal
#' @docType data
#' @keywords datasets
#' @name data_extra
#'
#' @examples
#' # Inspect available tables
#' names(data_extra)
#'
#' # Quick structure check
#' str(data_extra$crops)
#' str(data_extra$livestock)
"data_extra"

# Dictionary ----
#' FADN2Footprint variable dictionary (`dict_FADN`)
#'
#' A crosswalk table used to **map user-provided FADN-like datasets** (national FADN/RICA extracts,
#' EU-FADN extracts, or custom harmonized datasets) to the internal variable names expected by
#' **FADN2Footprint**.
#'
#' The recommended first step for a new user is to **fill the `var_user` column** so the package can
#' automatically select and rename the appropriate variables from the user dataset.
#'
#' @usage data(dict_FADN)
#'
#' @format A `data.frame` (or tibble) with the following columns:
#' \describe{
#'   \item{var_pre2014}{Character. Internal variable name for
#'   **pre-2014** FADN coding where applicable. `NA` if not applicable.}
#'
#'   \item{var_common}{Character. Internal variable name that is **stable/common across years**
#'   (preferred reference when it exists).}
#'
#'   \item{var_post2014}{Character. Internal variable name for
#'   **post-2014** FADN coding where applicable. `NA` if not applicable.}
#'
#'   \item{DESCRIPTION}{Character. Human-readable description of the variable (meaning, unit,
#'   scope notes).}
#'
#'   \item{Comment}{Character. Implementation notes (e.g., known caveats, recommended defaults,
#'   derivation hints, quality flags).}
#'
#'   \item{Group}{Character. Logical grouping of FADN data (e.g., general information, crops, livestock,
#'   inputs, standard results, labor, subsidies, etc.).}
#'
#'   \item{needed_in_FADN2Footprint}{Logical. Whether the variable is required to run the core
#'   pipeline for at least one footprint/performance module.}
#'
#'   \item{french_RICA_variable}{Character. Original French RICA/FADN variable code/name when a
#'   direct correspondence exists. `NA` otherwise.}
#'
#'   \item{created_RICA_variable}{Logical (or Character). Indicates the variable is
#'   **not directly available** in source French FADN but **must be derived** (e.g., computed from other columns,
#'   pivoting the French RICA crop table longer, etc.).}
#'
#'   \item{var_user}{Character. **To be filled by the user** with the column name in the user dataset
#'   corresponding to the internal variable (`var_common` and `DESCRIPTION`).
#'   Leave `NA` if unavailable.}
#' }
#'
#' @details
#' ### How this dictionary is used
#' The package relies on internal variable names (FADN common variable names, `var_common`).
#' Since source datasets differ across countries, years, and extract formats, `dict_FADN` serves as a mapping layer.
#'
#' ### Typical workflow
#' \enumerate{
#'   \item Extract a template of the dictionary using \code{\link{get_dictionary_template}}.
#'   This ensures you are working with the correct structure.
#'
#'   \item Fill the \code{var_user} column with your dataset's column names for the corresponding variables.
#'   \emph{Tip: Filter for \code{needed_in_FADN2Footprint == TRUE} to prioritize essential mappings.}
#'
#'   \item Build an S4 object of class \code{FADN2Footprint} with your updated dictionary.
#'   This step validates the mapping and standardizes the input data:
#'   \preformatted{
#'   my_object <- FADN2Footprint::data_4FADN2Footprint(
#'      df = FADN_18,
#'      id_cols = c("ID", "COUNTRY", "YEAR"),
#'      var_dict = my_updated_dictionary
#'   )
#'   }
#'
#'   \item Run the specific assessment modules (e.g., GHG emissions, biodiversity, economics)
#'   on \code{my_object} to generate the final output metrics.
#' }
#'
#' ### Pre-2014 vs Post-2014
#' Some FADN variable codings changed around the 2014 methodology revision. When this happens, the
#' dictionary provides:
#' - `var_pre2014` and/or `var_post2014` (coding-specific internal names), and/or
#' - `var_common` when a stable internal representation exists.
#'
#' ### Created variables
#' `created_RICA_variable` are variable names that are **not directly available** in source French FADN
#' but **must be derived** (e.g., computed from other columns, pivoting the French RICA crop table longer, etc.).
#' For instance, in the French RICA, the crop table has a crop code and a area variable. This table is pivot longer, and
#' the `created_RICA_variable` name for wheat area is `111_SUPER3`, combining the wheat code `111`
#' to the area variable name `SUPER3`. The same approach is implemented for the livestock table.
#'
#'
#' @examples
#' \dontrun{
#' ## 1) Get the template dictionary shipped with the package
#' dict_tpl <- FADN2Footprint::get_dictionary_template()
#'
#' ## 2) Fill `var_user` with your dataset's column names
#' ## Option 1: manual fill in R (example only: replace right-hand side with your real column names)
#' my_updated_dictionary <- dict_tpl
#' my_updated_dictionary$var_user[my_updated_dictionary$var_common == "ID"] <- "IDENT"
#' my_updated_dictionary$var_user[my_updated_dictionary$var_common == "NUTS3"] <- "CDEPT"
#' my_updated_dictionary$var_user[my_updated_dictionary$var_common == "YEAR"] <- "MILEX"
#'
#' ## Option 2: manual fill in a spreadsheet editor
#'
#' ## 3) Build the S4 object using your dataset + updated dictionary
#' my_object <- FADN2Footprint::data_4FADN2Footprint(
#'   df       = my_FADN_data,
#'   id_cols  = c("ID", "COUNTRY", "YEAR"),
#'   var_dict = my_updated_dictionary
#' )
#' }
#'
#' @source internal
#' @docType data
#' @name dict_FADN
#' @keywords datasets dictionary mapping
#'
"dict_FADN"

# EUROSTAT ----
#' EUROSTAT input prices for feed and other agricultural inputs (€/tonne)
#'
#' Harmonised input price time series extracted from a custom EUROSTAT spreadsheet
#' and transformed to support **FADN2Footprint** costing and allocation steps (e.g.,
#' feed cost proxies or price-based allocation keys).
#'
#' The dataset is built by reading selected sheets from an EUROSTAT Excel export,
#' extracting yearly prices (2014–2024) by country, mapping country labels to ISO3
#' codes using \code{data_extra$country_names}, and standardising units from
#' \(€ / 100\ kg\) to \(€ / tonne\).
#'
#' Missing values are imputed by the (country) mean over available years within the
#' processed table.
#'
#' @docType data
#' @usage data(EUROSTAT_input_price)
#'
#' @format
#' A tibble/data frame where each row is a (country, input) observation with one
#' column per year.
#'
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{2014}...\code{2024}}{Annual price in \emph{euros per tonne} (numeric).}
#'   \item{\code{EUROSTAT_feedstuff}}{Input product name (sanitised, underscores) extracted from the sheet title cell.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read the input/product label from cell \code{C7} of each selected sheet.
#'   \item Read country-by-year prices from range \code{A9:W37}.
#'   \item Keep \code{TIME} plus year columns 2014–2024; drop header rows.
#'   \item Convert ":" to \code{NA} and coerce to numeric; multiply by 10 to convert
#'   from \(€ / 100\ kg\) to \(€ / tonne\).
#'   \item Join to \code{data_extra$country_names} to obtain ISO3 country codes.
#'   \item Impute remaining missing numeric values by the mean (within each numeric column).
#' }
#'
#' @source
#' EUROSTAT custom Excel export stored in the package at
#' \code{inst/ext_data/EUROSTAT_prices/EUROSTAT_apri_ap_ina__custom_16862102_spreadsheet.xlsx}
#' (multiple sheets).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_input_price)
#' str(EUROSTAT_input_price)
#' # Example: subset one input and inspect the time series
#' subset(EUROSTAT_input_price, EUROSTAT_feedstuff == unique(EUROSTAT_feedstuff)[1])
"EUROSTAT_input_price"

#' EUROSTAT natural gas prices (€/GJ, annual mean from semester data)
#'
#' Country-level natural gas price time series derived from EUROSTAT dataset
#' \code{NRG_PC_203}. Values are extracted from a custom EUROSTAT Excel export
#' and processed for use in **FADN2Footprint** (e.g., energy cost proxies and
#' footprint accounting steps requiring gas price assumptions).
#'
#' The source table provides prices per semester (S1, S2). This dataset aggregates
#' semester values into an annual mean price per country and year.
#'
#' Units are **euros per gigajoule** (\(€ / GJ\)), reported in **gross
#' calorific value (GCV)** terms, as indicated in the extraction script.
#'
#' @docType data
#' @usage data(EUROSTAT_gaz_price)
#'
#' @format
#' A tibble/data frame with one row per country-year combination and 3 columns:
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{euro_GJ}}{Annual mean gas price in \(€ / GJ\) (numeric), computed as the mean of S1 and S2 when available.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read range \code{A12:AK50} from \code{Sheet 16} of the custom EUROSTAT export.
#'   \item Drop non-data header rows (e.g. \code{"GEO (Labels)"}).
#'   \item Convert price columns to numeric (suppressing coercion warnings).
#'   \item Map country labels to ISO3 codes using \code{data_extra$country_names}.
#'   \item Pivot semester columns (e.g. \code{YYYY-S1}, \code{YYYY-S2}) to long format.
#'   \item Compute annual means by country and year.
#' }
#'
#' @source
#' EUROSTAT dataset \emph{Natural gas prices} \code{NRG_PC_203}. \doi{10.2908/NRG_PC_203}
#' (processed via a custom Excel export stored in the package).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_gaz_price)
#' str(EUROSTAT_gaz_price)
#' head(EUROSTAT_gaz_price)
"EUROSTAT_gaz_price"

#' EUROSTAT electricity prices (€/kWh, annual mean from semester data)
#'
#' Country-level electricity price time series derived from an EUROSTAT export of
#' dataset \code{NRG_PC_205}. Values are extracted from a custom Excel spreadsheet
#' bundled with the package and processed for use in **FADN2Footprint** (e.g.,
#' energy cost proxies and footprint accounting steps requiring electricity price
#' assumptions).
#'
#' The source table provides prices per semester (S1, S2). This dataset aggregates
#' semester values into an annual mean electricity price per country and year.
#'
#' Units are **euros per kilowatt-hour** (\(€ / kWh\)).
#'
#' @docType data
#' @usage data(EUROSTAT_elec_price)
#'
#' @format
#' A tibble/data frame with one row per country-year combination and 3 columns:
#' \describe{
#'   \item{\code{Country_ISO_3166_1_A3}}{Country code (ISO 3166-1 alpha-3).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{euro_kWh}}{Annual mean electricity price in \(€ / kWh\) (numeric), computed as the mean of S1 and S2 when available.}
#' }
#'
#' @details
#' Processing steps (as implemented in the data-creation script):
#' \enumerate{
#'   \item Read range \code{A12:AK56} from \code{Sheet 7} of the custom EUROSTAT export.
#'   \item Drop non-data header rows (e.g. \code{"GEO (Labels)"}).
#'   \item Convert price columns to numeric (suppressing coercion warnings).
#'   \item Map country labels to ISO3 codes using \code{data_extra$country_names}.
#'   \item Pivot semester columns (e.g. \code{YYYY-S1}, \code{YYYY-S2}) to long format.
#'   \item Compute annual means by country and year.
#' }
#'
#' @source
#' EUROSTAT dataset \emph{Electricity prices} \code{NRG_PC_205}
#' (processed via a custom Excel export stored in the package at
#' \code{inst/ext_data/EUROSTAT_prices/EUROSTAT_nrg_pc_205__custom_17738899_spreadsheet.xlsx}).
#'
#' @keywords datasets
#'
#' @examples
#' data(EUROSTAT_elec_price)
#' str(EUROSTAT_elec_price)
#' head(EUROSTAT_elec_price)
"EUROSTAT_elec_price"



# EF electricity ----
#' GHG emission factors for electricity consumption
#'
#' A dataset containing greenhouse gas emission factors
#' for electricity by country and year, produced by the JRC.
#'
#' @format a data frame (tibble) avec X lignes et 3 variables :
#' \describe{
#'   \item{COUNTRY}{Country code}
#'   \item{YEAR}{Years from 1990 to 2021}
#'   \item{EF_elec}{CoM emission factors for national electricity for EU member states, Iceland and Norway: Activity-based (IPCC) approach, CO2 emissions in tonnes CO2/MWh}
#' }
#' @source Bastos, Joana; Monforti-Ferrario, Fabio; Melica, Giulia (2024):
#' GHG Emission Factors for Electricity Consumption. European Commission,
#' Joint Research Centre (JRC) [Dataset] PID: \url{http://data.europa.eu/89h/919df040-0252-4e4e-ad82-c054896e1641}
#' @usage data(electricity_factors)
#' @docType data
#' @keywords datasets
#'
"EF_electricity"

# UNFCCC National Inventories ----
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
#' @docType data
#' @keywords datasets
#' @usage data(UNFCCC_data)
#'
"UNFCCC_data"

# Global Warming Potential ----
#' Reference Global Warming Potentials used for conversion to CO2 equivalents
#'
#' `GWP` is an **internal** auxiliary vector distributed with the
#' **FADN2Footprint** package.
#'
#' @format A named vector.
#'
#' @details
#' This dataset is intended for internal package use.
#'
#' @source Built from Table 8.A.1 in https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_Chapter08_FINAL.pdf
#'
#' @references
#'
#' Myhre, G., et al. (2013). *Anthropogenic and natural radiative forcing*, in:
#' *Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovernmental Panel on Climate Change*. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA.
#'
#' @keywords internal
#' @keywords datasets
#'
#' @docType data
#'
#' @examples
#' # Inspect vector
#' data(GWP)
#' GWP
#'
"GWP"

# Reference fuel prices ----
#' Weekly Oil Bulletin fuel prices (without taxes), by country and year
#'
#' Reference fuel price time series derived from the European Commission
#' **Weekly Oil Bulletin (WOB)** historical files, processed for use in
#' **FADN2Footprint** (e.g., cost proxies for energy use and related footprint
#' accounting).
#'
#' The dataset provides annual average prices **without taxes** (WOT) for:
#' \itemize{
#'   \item automotive gas oil (road diesel),
#'   \item heating gas oil.
#' }
#'
#' Coverage is **2004–2022** (as constructed by the data-creation script):
#' \itemize{
#'   \item 2005–2022: extracted from \code{Oil_Bulletin_Prices_History.xlsx}
#'   (sheets \code{wotax_ctr_road} and \code{wotax_ctr_heat}).
#'   \item 2004: reconstructed from one Excel file per country (weekly observations),
#'   aggregated to annual means and converted to \(€ / L\).
#' }
#'
#' Country identifiers from WOB (\code{country_eu}, e.g. \code{"EL"} for Greece) are
#' mapped to the package's FADN country codes (\code{COUNTRY}) using
#' \code{data_extra$country_names}. Special cases are handled (\code{GR -> EL},
#' \code{GB -> UK}) to match Eurostat/WOB conventions in older files.
#'
#' @docType data
#' @usage data(ref_fuel_wob)
#'
#' @format
#' A data frame with one row per country-year and the following columns:
#' \describe{
#'   \item{\code{COUNTRY}}{Country code as used in FADN inputs (\code{country_FADN}).}
#'   \item{\code{country_eu}}{EU/WOB country code (e.g. \code{FR}, \code{DE}, \code{EL}).}
#'   \item{\code{YEAR}}{Year (character).}
#'   \item{\code{PFUEL_ROAD}}{Automotive gas oil price without tax, in \(€ / L\) (annual mean).}
#'   \item{\code{PFUEL_HEAT}}{Heating gas oil price without tax, in \(€ / L\) (annual mean).}
#' }
#'
#' @details
#' Data-processing notes (as implemented in the script):
#' \enumerate{
#'   \item Zeros in the 2005+ history tables are treated as missing values (\code{NA}).
#'   \item The 2005–2022 tables are reshaped from wide (years as columns) to long
#'   (one row per country-year).
#'   \item For 2004, weekly observations are filtered to year 2004, averaged by
#'   country, and converted from \(€ / 1000\ L\) to \(€ / L\)
#'   via division by 1000.
#'   \item The final dataset is the row-bind of the 2004 and 2005–2022 blocks,
#'   sorted by \code{COUNTRY} and \code{YEAR}.
#' }
#'
#' @source
#' European Commission, DG Energy, Weekly Oil Bulletin (WOB), historical price files
#' (without taxes), as downloaded on **2022-11-04** and stored locally in the
#' project raw-data folders (see script paths \code{data_raw/ref_fuel/...}).
#'
#' @keywords datasets
#'
#' @examples
#' data(ref_fuel_wob)
#' str(ref_fuel_wob)
#' head(ref_fuel_wob)
"ref_fuel_wob"


