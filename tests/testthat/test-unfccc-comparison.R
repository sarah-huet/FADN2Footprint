source(testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"), local = TRUE)

test_that("catalogue covers every requested comparison", {
  catalogue <- h_unfccc_catalogue()
  expect_equal(nrow(catalogue), 72L)
  expect_false(anyDuplicated(catalogue$metric_id) > 0)
  expect_true(all(c("soil_activity_organic", "soil_n2o_organic",
                    "soil_total", "manure_indirect_n2o") %in% catalogue$metric_id))
  expect_false(any(c(
    "soil_activity_animal_manure", "soil_n2o_animal_manure",
    "soil_activity_other_organic", "soil_n2o_other_organic"
  ) %in% catalogue$metric_id))
  expect_setequal(catalogue$sheet, c("Table3", "Table3.A", "Table3.B(a)",
    "Table3.B(b)", "Table3.D", "Table3.F", "Table4.B", "Table4.C"))
})

test_that("livestock categories use explicit CRT code mappings", {
  catalogue <- h_unfccc_catalogue()
  expected_codes <- c(
    population_cattle = "3.A.1",
    population_dairy_cattle = "3.A.1.a",
    population_non_dairy_cattle = "3.A.1.b",
    population_sheep = "3.A.2",
    population_swine = "3.A.3",
    population_poultry = "3.A.4.g"
  )
  actual_codes <- setNames(
    catalogue$category_code[
      match(names(expected_codes), catalogue$metric_id)
    ],
    names(expected_codes)
  )

  expect_identical(actual_codes, expected_codes)
  sheep_codes <- catalogue$category_code[
    grepl("_sheep$", catalogue$metric_id)
  ]
  expect_true(length(sheep_codes) > 0L)
  expect_true(all(grepl("\\.2$", sheep_codes)))
})

test_that("poultry population matches the CRT poultry category", {
  metric <- h_unfccc_catalogue() |>
    dplyr::filter(metric_id == "population_poultry")
  crt <- data.frame(
    iso3 = "FRA", year = 2018, sheet = "Table3.A",
    variable = "ACTIVITY DATA AND OTHER RELATED INFORMATION - Population size (1)",
    category_code = c("3.A.4.g", "3.A.4"),
    category = c("3.A.4.g. Poultry", "3.A.4. Other livestock"),
    value = c(123, 456), flag = NA_character_,
    unit_expected = "(1000s)", unit_found = "(1000s)", unit_check = "OK"
  )

  result <- h_unfccc_inventory(crt, metric)
  expect_equal(result$crt_value, 123000)
  expect_equal(result$crt_status, "ok")
})

test_that("sheep matching checks the CRT category label as well as its code", {
  sheep_metric <- h_unfccc_catalogue() |>
    dplyr::filter(metric_id == "population_sheep")
  crt <- data.frame(
    iso3 = "FRA",
    year = 2018,
    sheet = "Table3.A",
    variable = paste(
      "ACTIVITY DATA AND OTHER RELATED INFORMATION -",
      "Population size (1)"
    ),
    category_code = c("3.A.2", "3.A.4"),
    category = c("3.A.2. Sheep", "3.A.4. Other livestock"),
    value = c(7100, 303700),
    flag = NA_character_,
    unit_expected = "(1000s)",
    unit_found = "(1000s)",
    unit_check = "OK"
  )

  result <- h_unfccc_inventory(crt, sheep_metric)
  expect_equal(result$crt_value, 7100000)
  expect_equal(result$crt_status, "ok")

  # A sheep code paired with an Other livestock label is not a valid match.
  mismatched_label <- crt[2, , drop = FALSE]
  mismatched_label$category_code <- "3.A.2"
  mismatched_label$category <- "3.A.2. Other livestock"
  expect_equal(
    nrow(h_unfccc_inventory(mismatched_label, sheep_metric)),
    0L
  )
})

test_that("missing values and zero denominators remain explicit", {
  expect_true(is.na(h_unfccc_sum(c(1, NA_real_))))
  expect_equal(h_unfccc_sum(c(1, 2)), 3)
  expect_true(is.na(h_unfccc_sum(numeric())))
  expect_true(is.na(h_unfccc_ratio(1, 0)))
  expect_equal(h_unfccc_ratio(4, 2), 2)
})

test_that("coverage can be extracted from the constructor object", {
  data(mock_data, package = "FADN2Footprint")
  raw <- subset(mock_data$fadn_fict, YEAR == 2018 & TF14 %in% c(15, 45))
  object <- suppressMessages(data_4FADN2Footprint(raw))
  coverage <- h_unfccc_fadn(object, h_unfccc_catalogue(), coverage_only = TRUE)

  herd <- dplyr::left_join(
    object@herd,
    object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols), SYS02),
    by = object@traceability$id_cols,
    relationship = "many-to-one"
  )
  expect_equal(
    sum(coverage$f2f_value[coverage$metric_id == "population_cattle"]),
    sum(herd$Qobs[herd$species == "cattle"] *
          herd$SYS02[herd$species == "cattle"])
  )
  expect_true(all(grepl("^(population_|area_)", coverage$metric_id)))
  expect_true(all(coverage$f2f_status == "ok"))
})

test_that("coverage bars independently retain raw and GHG comparisons", {
  raw <- tibble::tibble(
    iso3 = c("FRA", "ITA"), YEAR = "2018",
    metric_id = "population_cattle", f2f_value = c(80, 20),
    f2f_status = "ok"
  )
  processed <- tibble::tibble(
    iso3 = "FRA", YEAR = "2018", metric_id = "population_cattle",
    f2f_value = 40, f2f_status = "ok"
  )
  inventory <- tibble::tibble(
    iso3 = c("FRA", "ITA"), YEAR = "2018",
    metric_id = "population_cattle", crt_value = c(100, 0),
    crt_status = "ok"
  )
  bars <- h_unfccc_coverage_plot_data(raw, processed, inventory)
  expect_setequal(bars$estimate, c("UNFCCC CRT", "Raw FADN", "FADN2Footprint"))
  expect_equal(
    bars$coverage_percent[match(c("UNFCCC CRT", "Raw FADN", "FADN2Footprint"),
                                bars$estimate)],
    c(100, 80, 40)
  )
  expect_true(all(bars$iso3 == "FRA"))
  processed$f2f_status <- "missing_f2f_value"
  bars <- h_unfccc_coverage_plot_data(raw, processed, inventory)
  expect_setequal(bars$estimate, c("UNFCCC CRT", "Raw FADN"))
})

test_that("Eurostat and poultry coverage use the matching CRT baseline", {
  raw <- tibble::tibble(
    iso3 = "FRA", YEAR = "2018",
    metric_id = c("population_cattle", "population_poultry", "area_cropland"),
    f2f_value = c(80, 60, 30), f2f_status = "ok"
  )
  processed <- dplyr::mutate(raw, f2f_value = f2f_value / 2)
  inventory <- tibble::tibble(
    iso3 = "FRA", YEAR = "2018",
    metric_id = raw$metric_id, crt_value = c(100, 200, 300),
    crt_status = "ok"
  )
  eurostat <- tibble::tibble(
    iso3 = "FRA", YEAR = 2018, livcat = c("cattle", "poultry", "sheep"),
    population_heads = c(120, 300, NA_real_)
  )

  bars <- h_unfccc_coverage_plot_data(raw, processed, inventory, eurostat)
  expect_equal(
    bars$coverage_percent[bars$estimate == "Eurostat"],
    c(120, 150)
  )
  expect_equal(
    bars$coverage_percent[bars$metric_id == "Poultry" & bars$estimate == "Raw FADN"],
    30
  )
  expect_false(any(bars$metric_id == "Cropland" & bars$estimate == "Eurostat"))

  inventory$crt_value[inventory$metric_id == "population_poultry"] <- 0
  bars <- h_unfccc_coverage_plot_data(raw, processed, inventory, eurostat)
  expect_false(any(bars$metric_id == "Poultry"))
  expect_error(
    h_unfccc_coverage_plot_data(raw, processed, inventory, dplyr::bind_rows(eurostat, eurostat)),
    "unique"
  )
})

test_that("CRT matching does not add parent and child rows or erase IE", {
  catalogue <- h_unfccc_catalogue()
  metric <- catalogue[catalogue$metric_id == "population_cattle", ]
  crt <- data.frame(iso3 = "FRA", year = 2020, sheet = "Table3.A",
    category_code = c("3.A.1", "3.A.1.a", "3.A.1.b"),
    category = c("3.A.1. Cattle", "3.A.1.a. Dairy cattle", "3.A.1.b. Non-dairy cattle"),
    variable = "ACTIVITY DATA AND OTHER RELATED INFORMATION - Population size (1)",
    value = c(3, 1, 2), flag = NA_character_, unit_expected = "(1000s)",
    unit_found = "(1000s)", unit_check = "OK")
  out <- h_unfccc_inventory(crt, metric)
  expect_equal(out$crt_value, 3000)
  expect_equal(out$crt_status, "ok")
  crt$value[1] <- NA_real_; crt$flag[1] <- "IE"
  out <- h_unfccc_inventory(crt, metric)
  expect_true(is.na(out$crt_value))
  expect_equal(out$crt_flag, "IE")
  crt$value[1] <- 3; crt$flag[1] <- NA_character_
  crt$unit_found[1] <- "(kt)"; crt$unit_check[1] <- "MISMATCH"
  expect_true(is.na(h_unfccc_inventory(crt, metric)$crt_value))
  crt$unit_found[1] <- "(1000s)"; crt$unit_check[1] <- "OK"
  crt$unit_check[1] <- "UNIT_MISMATCH"
  expect_equal(h_unfccc_inventory(crt, metric)$crt_status, "unit_mismatch")
  crt$unit_check[1] <- "OK"
  crt <- rbind(crt, transform(crt[1, ], value = 4))
  expect_equal(h_unfccc_inventory(crt, metric)$crt_status, "ambiguous")
  crt <- crt[1, ]
  crt$category_code <- NA_character_
  crt$category <- "Cattle"
  expect_equal(h_unfccc_inventory(crt, metric)$crt_value, 3000)
})

test_that("CRT units are converted into the comparison units", {
  catalogue <- h_unfccc_catalogue() |>
    dplyr::filter(metric_id %in% c(
      "soil_activity_direct",
      "production_wheat",
      "soil_total"
    ))
  crt <- data.frame(
    iso3 = "FRA",
    year = 2020,
    sheet = c("Table3.D", "Table3.F", "Table3"),
    variable = c(
      "ACTIVITY DATA - Value",
      "Crop production",
      "Total GHG emissions"
    ),
    category_code = c("3.D.1", "3.F.1.a", "3.D"),
    category = c(
      "3.D.1. Direct N2O emissions from managed soils",
      "3.F.1.a. Wheat",
      "3.D. Agricultural soils"
    ),
    value = c(0.5, 1, 2),
    flag = NA_character_,
    unit_expected = c("(kt N)", "(kt)", "(kt CO2 equivalent)"),
    unit_found = c("(kt N)", "(kt)", "(kt CO2 equivalent)"),
    unit_check = "OK"
  )

  out <- h_unfccc_inventory(crt, catalogue)
  expect_equal(
    out$crt_value[match(c(
      "soil_activity_direct",
      "production_wheat",
      "soil_total"
    ), out$metric_id)],
    c(500, 1000, 2)
  )
  expect_true(all(out$crt_status == "ok"))
})

test_that("soil diagnostic return preserves default emissions and units", {
  data(mock_data, package = "FADN2Footprint")
  raw <- subset(mock_data$fadn_fict, YEAR == 2018 & TF14 %in% c(15, 45))
  object <- suppressMessages(infer_practices(data_4FADN2Footprint(raw)))
  eligible <- object@farm |>
    dplyr::semi_join(object@crop, by = object@traceability$id_cols) |>
    dplyr::semi_join(object@herd, by = object@traceability$id_cols)
  expect_gt(nrow(eligible), 0L)
  farm_id <- eligible$ID[1]
  object <- m_prune_farms(object, ID == farm_id)
  expect_equal(nrow(object@farm), 1L)
  expect_gt(nrow(object@crop), 0L)
  expect_gt(nrow(object@herd), 0L)
  detailed <- suppressMessages(f_GHGE_n2o_msoils(object, return_components = TRUE))
  usual <- suppressMessages(f_GHGE_n2o_msoils(object))
  expect_equal(detailed$emissions, usual)
  expect_named(detailed, c("emissions", "activity", "direct", "indirect"))
  expect_equal(detailed$activity$F_SN,
    detailed$activity$N_ferti_min * detailed$activity$area_ha)
  expect_equal(detailed$direct$N2O_d * GWP[["N20"]], usual$N2O_d_kgCO2e)

  manure <- suppressMessages(f_GHGE_n2o_manure(object, return_components = TRUE))
  usual_manure <- suppressMessages(f_GHGE_n2o_manure(object))
  expect_equal(manure$emissions, usual_manure)
  expect_named(manure, c("emissions", "direct", "volatilisation", "leaching"))
  expect_equal(manure$volatilisation$N2O_G_mm * GWP[["N20"]],
               usual_manure$N2O_G_mm_kgCO2e_livcat)
  expect_equal(manure$leaching$N2O_L_mm * GWP[["N20"]],
               usual_manure$N2O_L_mm_kgCO2e_livcat)

  methane <- suppressMessages(f_GHGE_ch4_manure(object))
  f2f <- suppressMessages(h_unfccc_fadn(object, h_unfccc_catalogue()))
  expected <- dplyr::left_join(
    methane,
    manure$emissions |>
      dplyr::select(
        dplyr::all_of(object@traceability$id_cols),
        FADN_code_letter,
        dplyr::ends_with("_livcat")
      ),
    by = c(object@traceability$id_cols, "FADN_code_letter"),
    relationship = "one-to-one"
  ) |>
    dplyr::mutate(
      livcat = dplyr::case_when(
        FADN_code_letter == "LCOWDAIR" ~ "dairy_cattle",
        species == "cattle" ~ "non_dairy_cattle",
        TRUE ~ species
      ),
      manure_total = CH4_MM_kgCO2e_livcat +
        N2O_D_MM_kgCO2e_livcat
    ) |>
    dplyr::left_join(
      object@farm |>
        dplyr::select(
          dplyr::all_of(object@traceability$id_cols),
          SYS02
        ),
      by = object@traceability$id_cols,
      relationship = "many-to-one"
    ) |>
    dplyr::summarise(
      expected = sum(manure_total * SYS02) / 1e6,
      .by = livcat
    ) |>
    dplyr::mutate(metric_id = paste0("manure_total_", livcat))
  actual <- f2f |>
    dplyr::filter(metric_id %in% expected$metric_id) |>
    dplyr::arrange(metric_id)
  expected <- expected |>
    dplyr::arrange(metric_id)
  expect_equal(actual$metric_id, expected$metric_id)
  expect_equal(actual$f2f_value, expected$expected)

  ids <- object@traceability$id_cols
  weighted_sum <- function(x, values) {
    weights <- dplyr::left_join(
      x,
      object@farm |> dplyr::select(dplyr::all_of(ids), SYS02),
      by = ids,
      relationship = "many-to-one"
    )$SYS02
    sum(values * weights)
  }
  indirect <- f2f[f2f$metric_id == "manure_indirect_n2o", ]
  expect_equal(nrow(indirect), 1L)
  expect_equal(indirect$f2f_status, "ok")
  expect_equal(
    indirect$f2f_value,
    weighted_sum(
      usual_manure,
      usual_manure$N2O_G_mm_kgCO2e_livcat +
        usual_manure$N2O_L_mm_kgCO2e_livcat
    ) / GWP[["N20"]] / 1e6
  )
  soil_total <- f2f[f2f$metric_id == "soil_total", ]
  expect_equal(nrow(soil_total), 1L)
  expect_equal(soil_total$f2f_status, "ok")
  expect_equal(
    soil_total$f2f_value,
    weighted_sum(
      usual,
      usual$N2O_d_kgCO2e + usual$N2O_ATD_kgCO2e + usual$N2O_L_kgCO2e
    ) / 1e6
  )
})
