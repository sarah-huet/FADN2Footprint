test_that("categorical crosswalks recode national values without changing IDs", {
  dict <- list(
    variable_pattern = data.frame(pattern_national = character(), pattern_FADN = character()),
    category_pattern = data.frame(FADN_code_letter = character(), national_code = character()),
    manual_matches = data.frame(national_table = "rica", national_variable = "OTEXE",
                               fadn_variable = "TF14"),
    value_mappings = data.frame(fadn_variable = "TF14", national_value = c(15, 46),
                                fadn_value = c(15, 46))
  )
  raw <- list(rica = data.frame(IDENT = 1:2, OTEXE = c(15, 46)))
  out <- h_pivot_national_fadn(raw, dict, table_config = list())
  expect_identical(out$IDENT, 1:2)
  expect_equal(out$TF14, c(15, 46))
  expect_named(out, c("IDENT", "TF14"))

  raw$rica$OTEXE[2] <- 999
  expect_error(h_pivot_national_fadn(raw, dict, list()), "Unmapped national codes for TF14: 999", fixed = TRUE)
  dict$value_mappings <- rbind(dict$value_mappings, dict$value_mappings[1, ])
  expect_error(h_pivot_national_fadn(raw, dict, list()), "Duplicate national codes", fixed = TRUE)
})

test_that("GAM groups 46/47 with 45 only for prediction and only before 2017", {
  data(mock_data)
  row <- mock_data$fadn_fict[1, , drop = FALSE]
  row$COUNTRY <- "FRA"
  row$YEAR <- 2016
  row$TF14 <- 45
  row$SIZC <- 8
  row$SE295 <- 1000
  reference <- suppressMessages(data_4FADN2Footprint(row))
  for (code in c(46, 47)) {
    row$TF14 <- code
    object <- suppressMessages(data_4FADN2Footprint(row))
    expect_equal(object@input, reference@input)
    expect_equal(as.numeric(as.character(object@farm$TF14)), code)
  }
  row$TF14 <- 999
  expect_error(suppressMessages(data_4FADN2Footprint(row)),
               "Unsupported N-fertiliser GAM levels for TF14: 999", fixed = TRUE)
  row$SE295 <- 0
  expect_s4_class(suppressMessages(data_4FADN2Footprint(row)), "FADN2Footprint")
  row$SE295 <- 1000
  row$YEAR <- 2020
  expect_s4_class(suppressMessages(data_4FADN2Footprint(row)), "FADN2Footprint")
})

test_that("zero-padded French product codes match numeric dictionary codes", {
  dict <- list(
    variable_pattern = data.frame(Group = "products", pattern_national = "Q",
                                 pattern_FADN = "PRQ", unit_conversion = 1),
    category_pattern = data.frame(Group = "products", national_code = "21",
                                  FADN_code_letter = "PMLKCOW", allocation_key = 1),
    manual_matches = NULL
  )
  raw <- list(pan = data.frame(IDENT = 1, CODE7 = "021", Q = 3))
  out <- h_pivot_national_fadn(raw, dict,
                              list(products = list(table = "pan", code_col = "CODE7")))
  expect_equal(out$PMLKCOW_PRQ, 3)
  expect_named(out, c("IDENT", "PMLKCOW_PRQ"))
})
