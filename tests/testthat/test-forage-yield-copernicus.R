test_that("forage yields use the annual Copernicus regional references", {
  id_cols <- c("ID", "YEAR", "COUNTRY")
  crop_codes <- c("CFODMZ", "CFODOTH", "CGRSTMP", "CGRSXRG")
  farm_ids <- c(
    "nuts3_maize",
    "nuts3_artificial",
    "nuts3_temporary",
    "nuts3_permanent",
    "nuts2_fallback",
    "observed_yield"
  )

  df_harmonized <- tibble::tibble(
    ID = farm_ids,
    YEAR = rep("2004", length(farm_ids)),
    COUNTRY = rep("AUT", length(farm_ids))
  )
  for (code in crop_codes) {
    df_harmonized[[paste0(code, "_A")]] <- NA_real_
    df_harmonized[[paste0(code, "_PRQ")]] <- NA_real_
    df_harmonized[[paste0(code, "_SQ")]] <- NA_real_
  }
  for (i in seq_along(crop_codes)) {
    df_harmonized[[paste0(crop_codes[[i]], "_A")]][[i]] <- 2
    df_harmonized[[paste0(crop_codes[[i]], "_PRQ")]][[i]] <- 0
  }
  df_harmonized$CFODMZ_A[[5]] <- 2
  df_harmonized$CFODMZ_PRQ[[5]] <- 0
  df_harmonized$CFODMZ_A[[6]] <- 2
  df_harmonized$CFODMZ_PRQ[[6]] <- 20

  farm_data <- tibble::tibble(
    ID = farm_ids,
    YEAR = rep("2004", length(farm_ids)),
    COUNTRY = rep("AUT", length(farm_ids)),
    NUTS3 = c("AT111", "AT111", "AT111", "AT111", "AT999", "AT111"),
    NUTS2 = rep("AT11", length(farm_ids))
  )

  parse_crop_data <- get(
    ".parse_crop_data",
    envir = asNamespace("FADN2Footprint")
  )
  parsed <- suppressMessages(
    parse_crop_data(df_harmonized, farm_data, id_cols)
  )
  parsed <- dplyr::arrange(parsed, ID)

  expected_yields <- c(
    nuts3_maize = 11.1739515347891,
    nuts3_artificial = 4.62722859346797,
    nuts3_temporary = 3.70520016383227,
    nuts3_permanent = 2.51718167643529,
    nuts2_fallback = 10.7611011437003,
    observed_yield = 10
  )

  expect_equal(
    parsed$yield,
    unname(expected_yields[parsed$ID]),
    tolerance = 1e-10
  )
  expect_equal(parsed$prod_t[parsed$ID == "nuts3_maize"], 2 * expected_yields[["nuts3_maize"]])
  expect_equal(parsed$prod_t[parsed$ID == "observed_yield"], 20)
  expect_true(all(parsed$default_forage_yield[parsed$ID != "observed_yield"]))
  expect_false(parsed$default_forage_yield[parsed$ID == "observed_yield"])
})
