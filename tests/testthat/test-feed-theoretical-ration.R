test_that("disaggregated theoretical ration sums to the livestock-category total", {
  object <- methods::new(
    "FADN2Footprint",
    farm = data.frame(
      ID = "farm",
      Country_ISO_3166_1_A3 = "FRA",
      YEAR = "2020"
    ),
    herd = data.frame(
      ID = "farm",
      FADN_code_letter = "LCOWDAIR",
      species = "cattle",
      livestock_unit_coef = 1,
      Qobs = 1
    ),
    traceability = list(id_cols = "ID")
  )
  ration <- f_feed_theo_ration(object)[["th_feed_ration"]]

  grouping_cols <- c(object@traceability$id_cols, "FADN_code_letter")

  totals <- ration |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) |>
    dplyr::summarise(
      aggregated = dplyr::first(th_total_DMI_t_livcat_y),
      disaggregated = sum(th_DMI_t_livcat_y, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(aggregated))

  expect_gt(nrow(totals), 0L)
  expect_equal(totals$disaggregated, totals$aggregated, tolerance = 1e-8)
})
