#' Compare package estimates to literature reference values
#'
#' @param estimates A data.frame with columns: indicator, product, estimate
#' @param references A data.frame with columns: indicator,	unit,	product,	area,	lit_midpoint,	lit_min,	lit_max, tolerance_pct,	reference
#' @return A data.frame with comparison results and a pass/fail column
#' @export
validate_against_literature <- function(
    estimates,
    references
) {

  comp <- merge(references, estimates, by = c("indicator", "unit", "product"), all.x = TRUE)

  comp$lit_midpoint  <- dplyr::coalesce(comp$lit_midpoint, (comp$lit_min + comp$lit_max) / 2)
  comp$abs_deviation <- abs(comp$estimate - comp$lit_midpoint)
  comp$rel_deviation_pct <- round(comp$abs_deviation / comp$lit_midpoint * 100, 1)

  comp$in_lit_range  <- comp$estimate >= comp$lit_min & comp$estimate <= comp$lit_max
  #comp$within_tolerance <- comp$rel_deviation_pct <= comp$tolerance_pct
  comp$within_tolerance <- (
    # above the minimal lit value / 2
    comp$estimate >= pmin(comp$lit_midpoint, comp$lit_min, comp$lit_max, na.rm = T) / 2
    # below the maximum lit value * 2
    & comp$estimate <= pmax(comp$lit_midpoint, comp$lit_min, comp$lit_max, na.rm = T) * 2
  )

  comp$status <- dplyr::case_when(
    #is.na(comp$estimate)    ~ "\U274C missing",
    is.na(comp$estimate)    ~ "\U26A0\UFE0F missing",
    comp$in_lit_range       ~ "\U2705 in range",
    comp$within_tolerance       ~ "\U2705 in range",
    #comp$within_tolerance   ~ "\U26A0\UFE0F within tolerance",
    TRUE                    ~ "\U274C out of range"
  )

  comp[order(comp$status, comp$indicator, comp$unit, comp$product), ]
}
