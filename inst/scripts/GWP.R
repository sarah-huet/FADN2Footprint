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
#' @examples
#' # Inspect vector
#' data(GWP)
#' GWP
#'
"GWP"

GWP <- c(
  28,
  265)
names(GWP) <- c(
  "CH4",
  "N20"
)

# export data
usethis::use_data(GWP, overwrite = T)
