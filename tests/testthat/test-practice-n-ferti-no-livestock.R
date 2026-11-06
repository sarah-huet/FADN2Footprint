test_that("farms without livestock receive the no-manure organic-N average", {
  data_extra <- get("data_extra", envir = asNamespace("FADN2Footprint"))
  no_manure_reference <- data_extra$PKGC_N_ferti_org |>
    dplyr::filter(
      EPK_ferti_crop_name == "Ble_tendre",
      BIO == 0,
      Norg_int == FALSE
    ) |>
    dplyr::pull(N_org)

  object <- methods::new(
    "FADN2Footprint",
    farm = data.frame(
      ID = "farm_without_livestock",
      YEAR = "2020",
      COUNTRY = "FRA",
      ORGANIC = FALSE
    ),
    crop = data.frame(
      ID = "farm_without_livestock",
      YEAR = "2020",
      COUNTRY = "FRA",
      FADN_code_letter = "CWHTC",
      area_ha = 1
    ),
    input = data.frame(
      ID = "farm_without_livestock",
      YEAR = "2020",
      COUNTRY = "FRA",
      N_min_ferti_Q = 0
    ),
    practices = list(
      crops = list(N_ferti = NULL),
      herding = list(
        N_excretion = list(data.frame(
          ID = "farm_without_livestock",
          YEAR = "2020",
          COUNTRY = "FRA",
          Nex_kgN_livcat_y = 0
        ))
      )
    ),
    traceability = list(id_cols = c("ID", "YEAR", "COUNTRY"))
  )

  result <- suppressMessages(f_n_ferti(object))

  expect_length(no_manure_reference, 1L)
  expect_equal(result$N_ferti_org, no_manure_reference)
  expect_equal(result$N_ferti, no_manure_reference)
  expect_named(
    result,
    c(
      "ID", "YEAR", "COUNTRY", "ORGANIC", "FADN_code_letter",
      "area_ha", "N_min_ferti_Q", "N_ferti", "N_ferti_min", "N_ferti_org"
    )
  )
})
