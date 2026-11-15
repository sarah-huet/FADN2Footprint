source(testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"), local = TRUE)

selected_workbook <- testthat::test_path("..", "..", "inst", "validation", "CRT_vs_F2F.xlsx")

test_that("selected rows define ordered report sections and exact base families", {
  selected <- h_unfccc_selected(selected_workbook)
  expect_equal(nrow(selected), 12L)
  expect_equal(selected$workbook_row, 2:13)
  expect_equal(selected$title[c(1, 2, 12)],
    c("Livestock population", "Land area", "Soil Indirect N2O emissions"))
  expect_setequal(selected$metric_ids[[2]], c("area_cropland", "area_grassland"))
  expect_setequal(selected$metric_ids[[5]], paste0("soil_activity_",
    c("inorganic", "organic", "grazing", "residues")))
  expect_equal(selected$metric_ids[[10]], "manure_indirect_n2o")
  expect_setequal(selected$metric_ids[[11]], paste0("soil_n2o_",
    c("direct", "inorganic", "organic", "grazing", "residues")))
  expect_setequal(selected$metric_ids[[12]], paste0("soil_n2o_",
    c("indirect", "deposition", "leaching")))
})

test_that("section pages separate sheets, units and intensity bases", {
  catalogue <- h_unfccc_catalogue()
  data <- catalogue |> dplyr::filter(metric_id %in%
    c("area_cropland", "area_grassland", "enteric_ch4_cattle"))
  data$f2f_value <- 1
  data$f2f_rescaled <- 2
  data <- h_unfccc_estimates(data)
  intensity <- data[data$metric_id == "enteric_ch4_cattle", ]
  intensity$metric_id <- "enteric_ch4_cattle_per_head"
  intensity$unit <- "kg CH4/head/year"
  intensity$comparison_basis <- "Per-head livestock emissions"
  data <- dplyr::bind_rows(data, intensity)
  selected <- h_unfccc_selected(selected_workbook)
  area_pages <- h_unfccc_section_pages(data, selected$metric_ids[[2]])
  expect_length(area_pages, 2L)
  expect_setequal(vapply(area_pages, function(x) x$sheet[1], character(1)),
    c("Table4.B", "Table4.C"))
  emission_pages <- h_unfccc_section_pages(data, selected$metric_ids[[7]])
  expect_length(emission_pages, 2L)
  expect_equal(vapply(emission_pages, function(x) x$unit[1], character(1)),
    c("kt CH4/year", "kg CH4/head/year"))
  expect_false(any(vapply(emission_pages, function(x)
    any(grepl("^area_", x$metric_id)), logical(1))))
  expect_error(h_unfccc_section_pages(data, "unknown_metric"),
    "No comparison rows match the selected metrics")
})

test_that("section plots preserve selected estimates and unavailable tiles", {
  data <- tibble::tibble(
    metric_id = "enteric_ch4_cattle", sheet = "Table3.A",
    unit = "kt CH4/year", estimate = "rescaled",
    comparison_basis = "Population-rescaled totals",
    iso3 = c("ITA", "FRA", "FRA", "ITA"),
    YEAR = c("2021", "2020", "2021", "2020"),
    comparison_value = c(120, NA, 5, 80), crt_value = c(100, 100, 0, 100),
    f2f_value = c(999, 999, 999, 999),
    f2f_status = "ok", crt_status = "ok"
  )
  scatter <- h_unfccc_scatter_plot(data)
  expect_equal(scatter$data$plot_value, c(120, 5, 80))
  expect_equal(rlang::as_label(scatter$mapping$colour), "iso3")
  expect_equal(rlang::as_label(scatter$mapping$shape), "YEAR")
  expect_equal(levels(scatter$data$YEAR), c("2020", "2021"))
  page <- dplyr::distinct(data, metric_id, sheet, unit, estimate, comparison_basis)
  heatmap <- h_unfccc_country_year_plot(data, page)
  order <- match(paste(data$iso3, data$YEAR),
    paste(heatmap$data$iso3, heatmap$data$YEAR))
  expect_equal(heatmap$data$percentage_error[order], c(20, NA, NA, -20))
  expect_equal(heatmap$data$tile_label[order], c("20.0%", "", "", "-20.0%"))
  expect_equal(levels(heatmap$data$iso3), c("FRA", "ITA"))
  expect_equal(levels(heatmap$data$YEAR), c("2020", "2021"))
  expect_s3_class(ggplot2::ggplot_build(scatter), "ggplot_built")
  expect_s3_class(ggplot2::ggplot_build(heatmap), "ggplot_built")
  incomplete <- h_unfccc_country_year_plot(data[-3, ], page)
  expect_equal(nrow(incomplete$data), 4L)
  missing_tile <- subset(incomplete$data, iso3 == "FRA" & YEAR == "2021")
  expect_true(is.na(missing_tile$percentage_error))
  expect_equal(missing_tile$tile_label, "")
  data$comparison_value <- NA_real_
  unavailable <- h_unfccc_scatter_plot(data)
  expect_equal(nrow(unavailable$data), 0L)
  expect_s3_class(ggplot2::ggplot_build(unavailable), "ggplot_built")
})
