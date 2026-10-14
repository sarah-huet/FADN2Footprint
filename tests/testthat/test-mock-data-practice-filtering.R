test_that("about 90% of EU mock farms pass infer_practices", {
  data(mock_data)
  mock_farms <- mock_data$fadn_fict

  object <- data_4FADN2Footprint(mock_farms)
  inferred <- infer_practices(object, overwrite = TRUE)
  pass_rate <- nrow(inferred@farm) / nrow(mock_farms)

  expect_gt(nrow(mock_farms), 0L)
  expect_gt(nrow(inferred@farm), 0L)
  expect_lt(nrow(inferred@farm), nrow(mock_farms))
  expect_equal(pass_rate, 0.9, tolerance = 0.01)
})

test_that("EU mock Excel files are one per country and year and ~90% pass", {
  dir <- system.file("ext_data", "mock_FADN_EU", package = "FADN2Footprint")
  files <- list.files(dir, pattern = "\\.xlsx$", full.names = TRUE)
  expect_gt(length(files), 0L)
  expect_true(all(grepl("^FADN_[A-Z]{3}_[0-9]{4}\\.xlsx$", basename(files))))

  df <- dplyr::bind_rows(lapply(files, readxl::read_excel))
  expect_equal(
    nrow(unique(df[c("COUNTRY", "YEAR")])), length(files)
  )

  inferred <- suppressMessages(
    infer_practices(data_4FADN2Footprint(df), overwrite = TRUE)
  )
  expect_equal(nrow(inferred@farm) / nrow(df), 0.9, tolerance = 0.02)
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

  dict_path <- "../../vignettes/dictionary_conversion_filled_FRA.xlsx"
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
