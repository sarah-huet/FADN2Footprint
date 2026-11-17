# MCF inputs may be fractions or percentages; output values are percentages.
normalize_unfccc_mcf_percent <- function(country_iso3, value) {
  if (!is.numeric(value)) {
    stop("MCF values must be numeric.")
  }
  if (length(country_iso3) != length(value)) {
    stop("Country codes and MCF values must have the same length.")
  }

  # DEU reports MCFs as fractions; NLD mixes fractions and percentages.
  # Convert only fractional values from these sources and leave other values
  # and countries in their reported percentage units.
  fractional_value <- country_iso3 %in% c("DEU", "NLD") &
    is.finite(value) & value >= 0 & value <= 1
  value[fractional_value] <- value[fractional_value] * 100
  value
}
