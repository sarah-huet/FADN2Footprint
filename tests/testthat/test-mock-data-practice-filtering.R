test_that("about 90% of EU mock farms pass infer_practices", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  expect_true("SE295" %in% names(mock_farms))
  expect_true(all(c("ACHEN", "OTEXE") %in% names(mock_data$rica_fict$rica)))

  object <- data_4FADN2Footprint(mock_farms)
  inferred <- infer_practices(object, overwrite = TRUE)
  pass_rate <- nrow(inferred@farm) / nrow(mock_farms)

  expect_gt(nrow(mock_farms), 0L)
  expect_gt(nrow(inferred@farm), 0L)
  expect_lt(nrow(inferred@farm), nrow(mock_farms))
  expect_equal(pass_rate, 0.9, tolerance = 0.01)
})

test_that("pre-2017 N fertilizer quantity inference uses the saved GAM", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  early_row <- mock_farms[
    which(as.numeric(mock_farms$YEAR) < 2017)[1], ,
    drop = FALSE
  ]

  object <- data_4FADN2Footprint(early_row)

  expect_true("N_min_ferti_Q" %in% names(object@input))
  expect_true(any(is.finite(object@input$N_min_ferti_Q)))
})

test_that("farm parsing accepts inputs without optional altitude", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  mock_farms$ALTITUDE <- NULL

  object <- data_4FADN2Footprint(mock_farms)

  expect_s4_class(object, "FADN2Footprint")
  expect_false("ALTITUDE" %in% names(object@farm))
})

test_that("herding practices handle a farm with no recorded feed", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  row <- which(as.numeric(mock_farms$YEAR) >= 2017)[1]
  object <- data_4FADN2Footprint(mock_farms[row, , drop = FALSE])

  practices <- f_herding_practices(object, overwrite = TRUE)

  expect_true("total_t_DM" %in% names(practices))
  expect_true(all(is.finite(practices$total_t_DM)))
})

test_that("cattle pseudoherd calculations allow absent livestock categories", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  row <- which(
    as.character(mock_farms$TF14) == "45" &
      as.numeric(mock_farms$YEAR) >= 2017
  )[1]
  object <- data_4FADN2Footprint(mock_farms[row, , drop = FALSE])

  pseudoherd <- f_pseudoherd_cattle(object)

  expect_true(all(c("rearing_param", "pseudoherd") %in% names(pseudoherd)))
  expect_true(all(c("FADN_code_letter", "Qeq_meat") %in%
    names(pseudoherd$pseudoherd)))
})

test_that("swine pseudoherd calculations allow absent livestock categories", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  row <- which(as.numeric(mock_farms$YEAR) >= 2017)[1]
  object <- data_4FADN2Footprint(mock_farms[row, , drop = FALSE])

  pseudoherd <- f_pseudoherd_swine(object)

  expect_true(all(c("rearing_param", "pseudoherd") %in% names(pseudoherd)))
  expect_true(all(c("FADN_code_letter", "Qeq_meat") %in%
    names(pseudoherd$pseudoherd)))
})

test_that("French mock Excel files (year/table) have ~90% farms passing", {
  dir <- system.file("ext_data", "mock_FADN_FRA", package = "FADN2Footprint")
  years <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  expect_gt(length(years), 0L)

  tabs <- c("rica", "veg", "ani", "pan")
  yearly <- lapply(years, function(y) {
    files <- file.path(dir, y, paste0(tabs, ".xlsx"))
    expect_true(all(file.exists(files)))
    stats::setNames(lapply(files, readxl::read_excel), tabs)
  })
  names(yearly) <- years
  expect_true(all(vapply(yearly, function(x) "ACHEN" %in% names(x$rica), logical(1))))
  expect_true(all(vapply(yearly, function(x) "OTEXE" %in% names(x$rica), logical(1))))

  dict_path <- testthat::test_path(
    "..", "..", "vignettes", "dictionary_conversion_filled_FRA.xlsx"
  )
  skip_if_not(file.exists(dict_path))

  converted <- suppressMessages(h_pivot_national_fadn(
    raw_tables = h_stack_yearly_tables(yearly, year_col = "YEAR"),
    dict_conversion = h_read_dictionary(dict_path),
    id_cols = c("IDENT", "YEAR"),
    table_config = list(
      CROPS = list(table = "veg", code_col = "CODE3"),
      LIVESTOCK = list(table = "ani", code_col = "CODE6"),
      `ANIMAL PRODUCTS AND SERVICES` = list(table = "pan", code_col = "CODE7")
    )
  ))
  converted$COUNTRY <- "FRA"
  data("dict_FADN", package = "FADN2Footprint")
  inferred <- suppressMessages(infer_practices(
    data_4FADN2Footprint(converted, var_dict = dict_FADN,
                         id_cols = c("ID", "YEAR", "COUNTRY")),
    overwrite = TRUE
  ))
  expect_equal(nrow(inferred@farm) / nrow(converted), 0.9, tolerance = 0.02)
})
