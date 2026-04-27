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
#' @docType data
#' @keywords datasets dictionary mapping
#' @name dict_FADN
"dict_FADN"


library(readxl)
library(dplyr)

# Path to your Excel file
file_path <- "inst/ext_data/dict_FADN.xls"

# Read with all columns as text
dict_FADN <- readxl::read_excel(
  file_path,
  sheet = "dict_conversion"
)

# export data
usethis::use_data(dict_FADN, overwrite = T)

rm(file_path)
