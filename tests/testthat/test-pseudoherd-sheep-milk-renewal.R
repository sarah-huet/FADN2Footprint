make_sheep_pseudoherd_object <- function(milk_prod_t) {
  data("mock_data", package = "FADN2Footprint")
  row <- which(as.numeric(mock_data$fadn_fict$YEAR) >= 2017)[1]
  object <- data_4FADN2Footprint(mock_data$fadn_fict[row, , drop = FALSE])
  id_cols <- object@traceability$id_cols
  farm_ids <- object@farm |>
    dplyr::select(tidyselect::all_of(id_cols))

  object@herd <- object@herd |>
    dplyr::filter(species != "sheep") |>
    dplyr::bind_rows(
    dplyr::mutate(
      farm_ids,
      FADN_code_letter = "LEWEBRE",
      species = "sheep",
      Qobs = 10,
      ON = 10,
      CN = 10,
      PN = 0,
      SN = 10
    ),
    dplyr::mutate(
      farm_ids,
      FADN_code_letter = "LSHEPOTH",
      species = "sheep",
      Qobs = 20,
      ON = 20,
      CN = 20,
      PN = 0,
      SN = 10
    )
    )
  object@output$other_herd_products <- object@output$other_herd_products |>
    dplyr::filter(FADN_code_letter_output != "PMLKSHEP") |>
    dplyr::bind_rows(
      dplyr::mutate(
        farm_ids,
        FADN_code_letter_output = "PMLKSHEP",
        prod_t = milk_prod_t
      )
    )

  object
}

test_that("sheep pseudoherd reserves ewe-renewal animals for milk", {
  object <- make_sheep_pseudoherd_object(milk_prod_t = 0.255)
  result <- f_pseudoherd_sheep(object)

  milk_ewe <- result$pseudoherd |>
    dplyr::filter(FADN_code_letter == "LEWEBRE") |>
    dplyr::pull(Qeq_milk)
  other_milk <- result$pseudoherd |>
    dplyr::filter(FADN_code_letter == "LSHEPOTH") |>
    dplyr::pull(Qeq_milk)
  params <- result$rearing_param
  expected_other_milk <- params$rt_LSHEPOTH * milk_ewe / params$rt_LEWEBRE

  expect_equal(milk_ewe, 1)
  expect_equal(
    other_milk,
    round(expected_other_milk, 2)
  )

  remaining_others <- max(20 - expected_other_milk, 0)
  remaining_breeders <- 10 - milk_ewe
  expect_equal(
    result$pseudoherd |>
      dplyr::filter(FADN_code_letter == "LSHEPOTH") |>
      dplyr::pull(Qeq_meat),
    round(max(
      remaining_others,
      params$rt_LSHEPOTH * remaining_breeders * params$offspring_LEWEBRE
    ), 2)
  )
})

test_that("sheep without milk production have no milk pseudoherd", {
  object <- make_sheep_pseudoherd_object(milk_prod_t = 0)
  result <- f_pseudoherd_sheep(object)

  expect_true(all(result$pseudoherd$Qeq_milk == 0))
  expect_true(all(is.finite(result$pseudoherd$Qeq_meat)))
})
