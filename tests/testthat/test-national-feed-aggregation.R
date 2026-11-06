feed_conversion_dictionary <- function() {
  dict <- h_read_dictionary(testthat::test_path(
    "..", "..", "vignettes", "dictionary_conversion_filled_FRA.xlsx"
  ))
  dict$manual_matches <- subset(
    dict$manual_matches,
    national_variable %in% c("AUTAC", "HERAC", "AUTAG", "HERAG")
  )
  dict
}

test_that("French feed purchases sum into two EUR columns by farm-year", {
  dict <- feed_conversion_dictionary()
  raw <- list(rica = data.frame(
    IDENT = c(1L, 1L, 2L), YEAR = c(2018L, 2019L, 2018L),
    AUTAC = c(10, 20, 0), HERAC = c(30, 40, 0),
    AUTAG = c(1, 2, 0), HERAG = c(3, 4, 0)
  ))
  result <- h_pivot_national_fadn(
    raw, dict, list(), id_cols = c("IDENT", "YEAR")
  )
  expect_named(result, c(
    "IDENT", "YEAR", "IGRFEDCNCTRPUR_V", "IGRFEDCRSPUR_V"
  ))
  expect_identical(result[c("IDENT", "YEAR")], raw$rica[c("IDENT", "YEAR")])
  expect_equal(result$IGRFEDCNCTRPUR_V, c(40, 60, 0))
  expect_equal(result$IGRFEDCRSPUR_V, c(4, 6, 0))
  dict$manual_matches <- dict$manual_matches[
    rev(seq_len(nrow(dict$manual_matches))), ]
  reversed <- h_pivot_national_fadn(
    raw, dict, list(), id_cols = c("IDENT", "YEAR")
  )
  expect_equal(reversed[names(result)], result)
})

test_that("feed aggregation retains missing-value and conversion rules", {
  dict <- feed_conversion_dictionary()
  dict$manual_matches$unit_conversion[
    dict$manual_matches$national_variable == "AUTAC"
  ] <- 1000
  raw <- list(rica = data.frame(
    IDENT = c(1L, 1L, 2L),
    AUTAC = c(1, NA, NA), HERAC = c(2, 3, NA),
    AUTAG = c(NA, 4, NA), HERAG = c(5, NA, NA)
  ))
  result <- h_pivot_national_fadn(raw, dict, list())
  expect_identical(result$IDENT, c(1L, 2L))
  expect_equal(result$IGRFEDCNCTRPUR_V, c(1005, 0))
  expect_equal(result$IGRFEDCRSPUR_V, c(9, 0))
  raw$rica$HERAC <- NULL
  expect_warning(
    partial <- h_pivot_national_fadn(raw, dict, list()),
    "HERAC (rica)", fixed = TRUE
  )
  expect_equal(partial$IGRFEDCNCTRPUR_V, c(1000, 0))
  raw$rica$HERAC <- NA_real_
  all_missing_source <- h_pivot_national_fadn(raw, dict, list())
  expect_equal(all_missing_source$IGRFEDCNCTRPUR_V, c(1000, 0))
})

test_that("numeric destinations aggregate across tables but not categories", {
  dict <- feed_conversion_dictionary()
  dict$manual_matches <- subset(
    dict$manual_matches, national_variable %in% c("AUTAC", "HERAC")
  )
  dict$manual_matches$national_table[
    dict$manual_matches$national_variable == "HERAC"
  ] <- "other"
  raw <- list(
    rica = data.frame(IDENT = 1:2, AUTAC = c(10, 20)),
    other = data.frame(IDENT = c(2L, 1L), HERAC = c(4, 3))
  )
  result <- h_pivot_national_fadn(raw, dict, list())
  expect_named(result, c("IDENT", "IGRFEDCNCTRPUR_V"))
  expect_equal(result$IGRFEDCNCTRPUR_V, c(13, 24))
  raw$other$HERAC <- c("category-a", "category-b")
  expect_error(
    h_pivot_national_fadn(raw, dict, list()),
    "Cannot aggregate non-numeric mappings for IGRFEDCNCTRPUR_V.",
    fixed = TRUE
  )
  raw$other <- data.frame(IDENT = 2L, HERAC = 4)
  partial <- h_pivot_national_fadn(raw, dict, list())
  expect_equal(partial$IGRFEDCNCTRPUR_V, c(10, 24))
})
