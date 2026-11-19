test_that("herd Qobs prefers AN and falls back to the ALU-derived count", {
  livestock <- FADN2Footprint:::data_extra$livestock
  livestock_unit_coef <- livestock$livestock_unit_coef[
    match("LCOWDAIR", livestock$FADN_code_letter)
  ]
  country <- FADN2Footprint:::data_extra$country_names$country_FADN[[1]]

  herd_data <- data.frame(
    ID = c("AN-present", "AN-missing"),
    YEAR = c("2020", "2020"),
    COUNTRY = c(country, country),
    NUTS2 = c("FR10", "FR10"),
    TF14 = c(1, 1),
    LCOWDAIR_AN = c(10, NA_real_),
    LCOWDAIR_ALU = c(30, 25) * livestock_unit_coef
  )

  parsed <- suppressMessages(
    FADN2Footprint:::.parse_herd_data(
      herd_data,
      id_cols = c("ID", "YEAR", "COUNTRY")
    )
  )

  expect_equal(
    parsed$Qobs[match("AN-present", parsed$ID)],
    10
  )
  expect_equal(
    parsed$Qobs[match("AN-missing", parsed$ID)],
    25
  )

  herd_data_alu_only <- herd_data[
    herd_data$ID == "AN-missing",
    setdiff(names(herd_data), "LCOWDAIR_AN"),
    drop = FALSE
  ]
  parsed_alu_only <- suppressMessages(
    FADN2Footprint:::.parse_herd_data(
      herd_data_alu_only,
      id_cols = c("ID", "YEAR", "COUNTRY")
    )
  )

  expect_equal(parsed_alu_only$Qobs, 25)
})
