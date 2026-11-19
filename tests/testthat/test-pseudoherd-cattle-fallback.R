test_that("cattle pseudoherd falls back when aggregate flow rates are missing", {
  data("mock_data", package = "FADN2Footprint")
  mock_farms <- mock_data$fadn_fict
  row <- which(
    as.character(mock_farms$TF14) == "45" &
      as.numeric(mock_farms$YEAR) >= 2017
  )[1]
  object <- data_4FADN2Footprint(mock_farms[row, , drop = FALSE])

  flow_vars <- c("ON", "CN", "PN", "SN", "SSN", "SRN")
  object@herd <- object@herd |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(flow_vars),
      ~ dplyr::if_else(species == "cattle", 0, .x)
    ))

  pseudoherd <- f_pseudoherd_cattle(object)
  meat_with_observed_animals <- pseudoherd$pseudoherd |>
    dplyr::filter(
      species == "cattle",
      Qobs_meat > 0,
      !grepl("_(rearing|slaughter)$", FADN_code_letter)
    )

  expect_gt(nrow(meat_with_observed_animals), 0)
  expect_false(anyNA(meat_with_observed_animals$Q_max))
})
