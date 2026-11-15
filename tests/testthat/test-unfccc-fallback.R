fallback_reference <- data.frame(
  FADN_code_letter = c("A", "A", "A", "A"),
  Country_ISO_3166_1_A3 = c("FRA", "FRA", "DEU", "ITA"),
  YEAR = c("2018", "2019", "2018", "2018"),
  value = c(1, 3, 10, NA)
)

test_that("scalar fallback uses country-year, country mean, then overall mean", {
  fallback <- getFromNamespace("h_unfccc_fallback", "FADN2Footprint")
  data <- data.frame(
    id = 1:5,
    FADN_code_letter = c("A", "A", "A", "B", "A"),
    Country_ISO_3166_1_A3 = c("FRA", "FRA", "ITA", "FRA", "FRA"),
    YEAR = c("2018", "2030", "2018", "2018", "2018")
  )
  result <- fallback(data, fallback_reference, values = "value",
                     category = "FADN_code_letter")
  expect_equal(nrow(result), nrow(data))
  expect_equal(result$id, data$id)
  expect_equal(result$value, c(1, 2, 14 / 3, NA, 1))
})

test_that("scalar fallback without category uses the country then overall mean", {
  fallback <- getFromNamespace("h_unfccc_fallback", "FADN2Footprint")
  data <- data.frame(
    Country_ISO_3166_1_A3 = c("FRA", "DEU", "ESP"),
    YEAR = c("2025", "2018", "2018")
  )
  reference <- fallback_reference[-1]
  reference$other <- c(NA, 2, NA, NA)
  result <- fallback(data, reference, values = c("value", "other"))
  expect_equal(result$value, c(2, 10, 14 / 3))
  expect_equal(result$other, c(2, 2, 2))
})

test_that("scalar fallback rejects ambiguous reference rows", {
  fallback <- getFromNamespace("h_unfccc_fallback", "FADN2Footprint")
  reference <- fallback_reference[c(1, 1), ]
  expect_error(
    fallback(fallback_reference[1, 1:3], reference, values = "value",
             category = "FADN_code_letter"),
    "`reference` must have one row per FADN_code_letter x Country_ISO_3166_1_A3 x YEAR.",
    fixed = TRUE
  )
})

test_that("share fallback keeps distributions summing to one", {
  fallback <- getFromNamespace("h_unfccc_fallback_shares", "FADN2Footprint")
  reference <- data.frame(
    FADN_code_letter = "A",
    Country_ISO_3166_1_A3 = c("FRA", "FRA", "FRA", "DEU"),
    YEAR = c("2018", "2018", "2019", "2018"),
    MMS = c("x", "y", "x", "y"),
    AWMS = c(0.6, 0.4, 1, 1)
  )
  data <- data.frame(
    id = 1:4,
    FADN_code_letter = c("A", "A", "A", "B"),
    Country_ISO_3166_1_A3 = c("FRA", "FRA", "ITA", "FRA"),
    YEAR = c("2018", "2030", "2018", "2018")
  )
  result <- fallback(data, reference, share = "AWMS", component = "MMS",
                     category = "FADN_code_letter")
  expect_equal(sort(unique(result$id)), data$id)
  share <- function(i, mms) result$AWMS[result$id == i & result$MMS == mms]
  expect_equal(c(share(1, "x"), share(1, "y")), c(0.6, 0.4))
  expect_equal(c(share(2, "x"), share(2, "y")), c(0.8, 0.2))
  expect_equal(c(share(3, "x"), share(3, "y")), c(1.6, 1.4) / 3)
  expect_equal(sum(result$id == 4), 1L)
  expect_true(is.na(result$MMS[result$id == 4]))
  totals <- tapply(result$AWMS, result$id, sum)
  expect_equal(as.vector(totals[1:3]), c(1, 1, 1))
})

test_that("UNFCCC AWMS shares sum to one per FADN category and country-year", {
  shares_fn <- getFromNamespace("h_unfccc_awms_shares", "FADN2Footprint")
  shares <- shares_fn(UNFCCC_data$table3Bas2)
  expect_false(anyNA(shares$FADN_code_letter))
  expect_true(all(shares$AWMS > 0 & shares$AWMS <= 1 + 1e-9))
  totals <- stats::aggregate(
    AWMS ~ FADN_code_letter + Country_ISO_3166_1_A3 + YEAR,
    data = shares, FUN = sum
  )
  expect_true(all(abs(totals$AWMS - 1) < 1e-9))
})

test_that("manure N2O uses country averages for years without CRT data", {
  load(testthat::test_path("..", "..", "data", "mock_data.rda"),
       envir = environment())
  herd <- mock_data$fadn_fict[1, c("ID", "YEAR", "COUNTRY")]
  herd$COUNTRY <- "FRA"
  herd$YEAR <- "2099"
  herd$FADN_code_letter <- "LCOWDAIR"
  herd$species <- "cattle"
  herd$Qobs <- 1
  object <- methods::new(
    "FADN2Footprint", herd = herd,
    traceability = list(id_cols = c("ID", "YEAR", "COUNTRY"))
  )
  excretion <- herd[c("ID", "YEAR", "COUNTRY", "FADN_code_letter")]
  excretion$Nex_kgN_anim_y <- 100
  testthat::local_mocked_bindings(
    f_n_excr = function(...) list(cattle = excretion),
    .package = "FADN2Footprint"
  )
  result <- f_GHGE_n2o_manure(object, overwrite = TRUE)
  expect_equal(nrow(result), 1L)
  expect_true(is.finite(result$N2O_D_MM_kgCO2e_livcat))
  expect_gt(result$N2O_D_MM_kgCO2e_livcat, 0)
})
