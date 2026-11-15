source(testthat::test_path(
  "..", "..", "inst", "scripts", "EUROSTAT_jsonstat_helpers.R"
), local = TRUE)

test_that("JSON-stat sparse observations decode to their dimension labels", {
  js <- list(
    id = c("geo", "time"),
    size = c(2L, 2L),
    dimension = list(
      geo = list(category = list(index = list(BE = 0L, FR = 1L))),
      time = list(category = list(index = list("2020" = 0L, "2021" = 1L)))
    ),
    value = list("0" = 1.5, "3" = 2.5),
    status = list("3" = "e")
  )

  result <- parse_jsonstat(js)

  expect_named(result, c("geo", "time", "value", "flag"))
  expect_identical(result$geo, c("BE", "FR"))
  expect_identical(result$time, c("2020", "2021"))
  expect_equal(result$value, c(1.5, 2.5))
  expect_identical(result$flag, c(NA_character_, "e"))
})

test_that("JSON-stat decoding varies the last dimension fastest", {
  js <- list(
    id = c("geo", "time"),
    size = c(2L, 3L),
    dimension = list(
      geo = list(category = list(index = list(BE = 0L, FR = 1L))),
      time = list(category = list(index = list(
        "2020" = 0L, "2021" = 1L, "2022" = 2L
      )))
    ),
    value = list("1" = 12, "3" = 20, "5" = 22),
    status = list("3" = "e")
  )

  result <- parse_jsonstat(js)
  expect_identical(result$geo, c("BE", "FR", "FR"))
  expect_identical(result$time, c("2021", "2020", "2022"))
  expect_equal(result$value, c(12, 20, 22))
  expect_identical(result$flag, c(NA_character_, "e", NA_character_))
})

test_that("JSON-stat decoding retains a reported zero without expanding gaps", {
  js <- list(
    id = "geo",
    size = 2L,
    dimension = list(
      geo = list(category = list(index = list(BE = 0L, FR = 1L)))
    ),
    value = list("1" = 0),
    status = list()
  )

  result <- parse_jsonstat(js)

  expect_equal(nrow(result), 1L)
  expect_identical(result$geo, "FR")
  expect_identical(result$value, 0)
  expect_true(is.na(result$flag))
})

test_that("Eurostat population references average reported periods by category", {
  bovine <- tibble::tribble(
    ~freq, ~month, ~animals, ~unit, ~geo, ~time, ~value,
    "A", "M05_M06", "A2000", "THS_HD", "FR", "2020", 1,
    "A", "M11_M12", "A2000", "THS_HD", "FR", "2020", 3,
    "A", "M05_M06", "A2300F", "THS_HD", "FR", "2020", 0.4,
    "A", "M11_M12", "A2300F", "THS_HD", "FR", "2020", 0.6
  )
  pigs <- tibble::tribble(
    ~freq, ~month, ~animals, ~unit, ~geo, ~time, ~value,
    "A", "M04", "A3100", "THS_HD", "FR", "2020", 1,
    "A", "M11_M12", "A3100", "THS_HD", "FR", "2020", 3
  )
  sheep <- tibble::tribble(
    ~freq, ~month, ~animals, ~unit, ~geo, ~time, ~value,
    "A", "M05_M06", "A4100", "THS_HD", "FR", "2020", 0.2,
    "A", "M11_M12", "A4100", "THS_HD", "FR", "2020", 0.4,
    "A", "M05_M06", "A4200", "THS_HD", "FR", "2020", 0.1,
    "A", "M11_M12", "A4200", "THS_HD", "FR", "2020", 0.3
  )
  country_names <- data.frame(
    country_eu = "FR",
    Country_ISO_3166_1_A3 = "FRA"
  )

  result <- build_eurostat_livestock_pop(
    bovine,
    pigs,
    sheep,
    country_names
  )

  expect_equal(nrow(result), 6L)
  expect_equal(
    result$population_heads[match(
      c("cattle", "dairy_cattle", "non_dairy_cattle", "swine", "sheep", "goat"),
      result$livcat
    )],
    c(2000, 500, 1500, 2000, 300, 200)
  )
  expect_true(all(result$n_periods == 2L))
  expect_false(anyDuplicated(result[c("iso3", "YEAR", "livcat")]) > 0)
})
