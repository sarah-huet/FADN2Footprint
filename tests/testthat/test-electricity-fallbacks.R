test_that("electricity prices and emission factors use reference-data fallbacks", {
  estimate <- getFromNamespace(
    ".estimate_electricity_activity",
    "FADN2Footprint"
  )

  activity <- data.frame(
    ID = c("matched", "missing-country", "missing-year"),
    Country_ISO_3166_1_A3 = c("FRA", "XXX", "NLD"),
    YEAR = c("2020", "2020", "2010"),
    elec_V = c(100, 60, 30)
  )
  prices <- data.frame(
    Country_ISO_3166_1_A3 = c("FRA", "DEU"),
    YEAR = c("2020", "2020"),
    euro_kWh = c(0.2, 0.4)
  )
  emission_factors <- data.frame(
    Country_ISO_3166_1_A3 = c("FRA", "DEU", "DEU"),
    YEAR = c("2020", "2020", "2021"),
    EF_elec = c(0.5, 0.7, 0.9)
  )

  result <- estimate(activity, prices, emission_factors)

  expect_named(
    result,
    c(
      "ID", "Country_ISO_3166_1_A3", "YEAR", "elec_V",
      "euro_kWh", "elec_kWh", "EF_elec", "farm_ghg_elec_kgCO2e"
    )
  )
  expect_equal(result$euro_kWh, c(0.2, 0.3, 0.3))
  expect_equal(result$elec_kWh, c(500, 200, 100))
  expect_equal(result$EF_elec, c(0.5, 0.6, 0.7))
  expect_equal(result$farm_ghg_elec_kgCO2e, c(250, 120, 70))
  expect_false(anyNA(result))
})

test_that("electricity fallback errors when reference values are all missing", {
  estimate <- getFromNamespace(
    ".estimate_electricity_activity",
    "FADN2Footprint"
  )
  activity <- data.frame(
    ID = "farm",
    Country_ISO_3166_1_A3 = "XXX",
    YEAR = "2020",
    elec_V = 100
  )
  prices <- data.frame(
    Country_ISO_3166_1_A3 = "FRA",
    YEAR = "2020",
    euro_kWh = NA_real_
  )
  emission_factors <- data.frame(
    Country_ISO_3166_1_A3 = "FRA",
    YEAR = "2020",
    EF_elec = 0.5
  )

  expect_error(
    estimate(activity, prices, emission_factors),
    "No finite reference values are available for EUROSTAT electricity prices."
  )
  expect_error(
    estimate(
      activity,
      transform(prices, euro_kWh = 0.2),
      transform(emission_factors, EF_elec = NA_real_)
    ),
    "No finite reference values are available for electricity emission factors."
  )
})
