test_that("EF3 defaults cover every CRT manure-management system", {
  resolve <- getFromNamespace("h_resolve_manure_ef3", "FADN2Footprint")
  expected <- c(
    mms_anaerobic_lagoon = 0,
    mms_burned_for_fuel_or_as_waste = 0,
    mms_composting = 0.005,
    mms_daily_spread = 0,
    mms_deep_bedding = 0.07,
    mms_digesters = 0.0006,
    mms_dry_lot = 0.02,
    mms_liquid_system = 0.005,
    mms_other = 0.01,
    mms_pasture_range_and_paddock = 0,
    mms_pit_storage = 0.002,
    mms_solid_storage = 0.01
  )
  systems <- sort(unique(UNFCCC_data$table3Bas2$MMS))
  expect_setequal(systems, names(expected))

  for (animal in c("cattle", "swine", "sheep", "goats", "horse", "poultry")) {
    activity <- data.frame(
      MMS = systems, species = animal, AWMS = 1, EF3 = NA_real_
    )
    result <- resolve(activity)
    factors <- unname(expected[systems])
    if (animal == "poultry") {
      factors[systems %in% c("mms_deep_bedding", "mms_solid_storage")] <- 0.001
    }
    expect_named(result, names(activity))
    expect_equal(nrow(result), nrow(activity))
    expect_equal(result$EF3, factors)
    expect_true(all(is.finite(result$EF3)))
    expect_true(all(result$EF3 >= 0))
    expect_equal(
      sum(100 * result$AWMS * result$EF3 * 44 / 28),
      sum(100 * factors * 44 / 28)
    )
  }
})

test_that("reported EF3 values take precedence without multiplying rows", {
  resolve <- getFromNamespace("h_resolve_manure_ef3", "FADN2Footprint")
  activity <- data.frame(
    YEAR = c("2020", "2021", "2022"),
    MMS = c("mms_solid_storage", "mms_solid_storage", "unlisted"),
    species = "cattle", AWMS = 1, EF3 = c(0, 0.003, 0.004)
  )
  expect_identical(resolve(activity), activity)
  expect_equal(nrow(resolve(activity[c(1, 1, 2), ])), 3L)
})

test_that("missing EF3 cannot silently omit positive manure allocations", {
  resolve <- getFromNamespace("h_resolve_manure_ef3", "FADN2Footprint")
  activity <- data.frame(
    MMS = "unlisted", species = "cattle", AWMS = 1, EF3 = NA_real_
  )
  expect_error(
    resolve(activity),
    "No finite EF3 for positive manure allocation: cattle/unlisted.",
    fixed = TRUE
  )
  activity$AWMS <- 0
  expect_true(is.na(resolve(activity)$EF3))
  activity$AWMS <- 1
  activity$MMS <- "mms_dry_lot"
  expect_equal(resolve(activity)$EF3, 0.02)
  activity <- activity[FALSE, ]
  expect_identical(resolve(activity), activity)
})

test_that("manure emissions include fallback factors for missing reported IEFs", {
  load(testthat::test_path("..", "..", "data", "mock_data.rda"),
       envir = environment())
  herd <- mock_data$fadn_fict[1, c("ID", "YEAR", "COUNTRY")]
  herd$COUNTRY <- "FRA"
  herd$YEAR <- "2018"
  herd$FADN_code_letter <- "LCOWDAIR"
  herd$species <- "cattle"
  herd$Qobs <- 1
  object <- methods::new(
    "FADN2Footprint", herd = herd,
    traceability = list(id_cols = c("ID", "YEAR", "COUNTRY"))
  )
  excretion <- herd[c("ID", "YEAR", "COUNTRY", "FADN_code_letter")]
  excretion$Nex_kgN_anim_y <- 100
  reference <- UNFCCC_data
  reference$table3Bb$EF3 <- NA_real_
  testthat::local_mocked_bindings(
    f_n_excr = function(...) list(cattle = excretion),
    UNFCCC_data = reference, .package = "FADN2Footprint"
  )
  allocation <- subset(
    reference$table3Bas2,
    Country_ISO_3166_1_A3 == "FRA" & YEAR == "2018" &
      UNFCCC_cat == "dairy_cattle" & AWMS > 0
  )
  factors <- c(
    mms_anaerobic_lagoon = 0, mms_burned_for_fuel_or_as_waste = 0,
    mms_composting = 0.005, mms_daily_spread = 0,
    mms_deep_bedding = 0.07, mms_digesters = 0.0006,
    mms_dry_lot = 0.02, mms_liquid_system = 0.005,
    mms_other = 0.01, mms_pasture_range_and_paddock = 0,
    mms_pit_storage = 0.002, mms_solid_storage = 0.01
  )
  nitrogen_share <- allocation$Population__size * allocation$AWMS
  nitrogen_share <- nitrogen_share / sum(nitrogen_share)
  expected <- 100 * sum(nitrogen_share * factors[allocation$MMS]) *
    44 / 28 * GWP[["N20"]]
  result <- f_GHGE_n2o_manure(object, overwrite = TRUE)
  expect_gt(nrow(allocation), 0L)
  expect_true(any(allocation$MMS %in% c(
    "mms_deep_bedding", "mms_dry_lot", "mms_pit_storage",
    "mms_solid_storage"
  )))
  expect_equal(nrow(result), 1L)
  expect_equal(result$N2O_D_MM_kgCO2e_livcat, expected)
  expect_true(is.finite(result$N2O_D_MM_kgCO2e_livcat))
})
