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
#' @keywords internal
#' @keywords datasets
#'
#' @examples
#' # Inspect available tables
#' names(data_extra)
#'
#' # Quick structure check
#' str(data_extra$crops)
#' str(data_extra$livestock)
"data_extra"

library(readxl)
library(dplyr)

# Path to your Excel file
file_path <- "inst/ext_data/data_extra_FADN.xlsx"

# Get sheet names
sheets <- readxl::excel_sheets(file_path)

# Read all sheets into a named list
data_extra <- lapply(sheets, function(sheet) {
 readxl::read_excel(path = file_path, sheet = sheet)
})

# Name the list elements with the sheet names
names(data_extra) <- sheets

rm(file_path,sheets)

# mutate variable type
data_extra$French_FQS <- data_extra$French_FQS %>%
  dplyr::mutate(RICA_var_code = as.character(RICA_var_code))

# export data
usethis::use_data(data_extra, overwrite = T)
