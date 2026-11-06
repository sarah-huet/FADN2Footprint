test_that("CRT reference tables preserve country and inventory-year keys", {
  load(
    testthat::test_path("..", "..", "data", "UNFCCC_data.rda"),
    envir = environment()
  )

  expect_true(all(vapply(
    UNFCCC_data,
    function(table) is.character(table$YEAR),
    logical(1)
  )))
  expect_equal(
    anyDuplicated(UNFCCC_data$EF_managed_soils[c("Country_ISO_3166_1_A3", "YEAR")]),
    0L
  )
  expect_equal(
    anyDuplicated(UNFCCC_data$EF_fuel[c("Country_ISO_3166_1_A3", "YEAR")]),
    0L
  )
  expect_equal(
    anyDuplicated(
      UNFCCC_data$table3Bb[c("Country_ISO_3166_1_A3", "YEAR", "MMS")]
    ),
    0L
  )
  expect_equal(
    anyDuplicated(
      UNFCCC_data$table3Bas2[
        c(
          "Country_ISO_3166_1_A3", "YEAR", "Animal_cat",
          "climate_region", "MMS", "UNFCCC_cat"
        )
      ]
    ),
    0L
  )
})

test_that("CRT emission factors retain their reported inventory year", {
  load(
    testthat::test_path("..", "..", "data", "UNFCCC_data.rda"),
    envir = environment()
  )

  manure_factors <- subset(
    UNFCCC_data$table3Bb,
    Country_ISO_3166_1_A3 == "AUT" &
      YEAR %in% c("2017", "2018") &
      MMS == "mms_liquid_system"
  )
  manure_factors <- manure_factors[order(manure_factors$YEAR), ]
  expect_equal(manure_factors$YEAR, c("2017", "2018"))
  expect_equal(
    manure_factors$EF3,
    c(0.003712444, 0.003760381),
    tolerance = 1e-6
  )

  soil_factors <- subset(
    UNFCCC_data$EF_managed_soils,
    Country_ISO_3166_1_A3 == "AUT" & YEAR %in% c("2017", "2018")
  )
  soil_factors <- soil_factors[order(soil_factors$YEAR), ]
  expect_equal(
    soil_factors$EF_PRP,
    c(0.003757558, 0.003762700),
    tolerance = 1e-6
  )

  fuel_factors <- subset(
    UNFCCC_data$EF_fuel,
    Country_ISO_3166_1_A3 == "HUN" & YEAR == "1987"
  )
  expect_equal(nrow(fuel_factors), 1L)
  expect_equal(
    unname(unlist(fuel_factors[c("EF_fuel_CO2", "EF_fuel_CH4", "EF_fuel_N2O")])),
    c(73.78438, 6.422031, 1.964642),
    tolerance = 1e-6
  )
})
