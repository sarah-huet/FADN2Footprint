source(
  testthat::test_path("..", "..", "inst", "scripts", "unfccc_mcf_units.R"),
  local = TRUE
)

test_that("MCF ingestion normalizes only fractional DEU and NLD values", {
  country_iso3 <- c("DEU", "NLD", "NLD", "FRA", "FRA", "DEU")
  mcf <- c(0.14444368, 0.17, 95, 0.47, 17.836615736, NA_real_)

  expect_equal(
    normalize_unfccc_mcf_percent(country_iso3, mcf),
    c(14.444368, 17, 95, 0.47, 17.836615736, NA_real_)
  )
})

test_that("inventory MCF values reproduce reported dairy-cattle CH4", {
  table3Bas1 <- UNFCCC_data$table3Bas1
  table3Bas2 <- UNFCCC_data$table3Bas2

  mcf_for <- function(country_iso3, animal_cat) {
    rows <- table3Bas2[
      table3Bas2$Country_ISO_3166_1_A3 == country_iso3 &
        table3Bas2$YEAR == "2020" &
        table3Bas2$Animal_cat == animal_cat &
        table3Bas2$MMS == "mms_liquid_system" &
        table3Bas2$climate_region == "Cool",
      ,
      drop = FALSE
    ]
    expect_equal(nrow(rows), 1L)
    rows$MCF
  }

  expect_equal(mcf_for("DEU", "dairy_cattle"), 14.444368)
  expect_equal(mcf_for("NLD", "mature_dairy_cattle"), 17)
  expect_equal(mcf_for("FRA", "dairy_cattle"), 17.836615736)

  reconstruct_ch4_kt <- function(country_iso3, animal_cat) {
    activity <- table3Bas1[
      table3Bas1$Country_ISO_3166_1_A3 == country_iso3 &
        table3Bas1$YEAR == "2020" &
        table3Bas1$Animal_cat == animal_cat,
      ,
      drop = FALSE
    ]
    factors <- table3Bas2[
      table3Bas2$Country_ISO_3166_1_A3 == country_iso3 &
        table3Bas2$YEAR == "2020" &
        table3Bas2$Animal_cat == animal_cat,
      ,
      drop = FALSE
    ]
    factors <- unique(factors[c("MMS", "climate_region", "MCF", "AWMS")])

    activity[["VS(2)_daily_excretion_(average)"]][1] * 365 *
      activity[["CH4_producing_potential_(Bo)(2)_(average)"]][1] * 0.67 *
      sum((factors$MCF / 100) * (factors$AWMS / 100), na.rm = TRUE) *
      activity$Population__size[1] / 1000
  }

  reconstructed <- c(
    DEU = reconstruct_ch4_kt("DEU", "dairy_cattle"),
    FRA = reconstruct_ch4_kt("FRA", "dairy_cattle")
  )
  reported <- c(DEU = 93.8330014, FRA = 84.1294227974913)

  expect_equal(reconstructed, reported, tolerance = 0.001)
})
