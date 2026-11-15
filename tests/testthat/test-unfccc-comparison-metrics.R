source(testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"), local = TRUE)

test_that("metrics use CRT as reference and percentage units", {
  comparison <- tibble::tibble(
    metric_id = "population_cattle", sheet = "Table3.A",
    category_label = "Cattle", unit = "heads",
    f2f_value = c(110, 180, 360), crt_value = c(100, 200, 300),
    f2f_status = "ok", crt_status = "ok"
  )
  out <- h_unfccc_metrics(comparison)
  expect_equal(out$MPE, 20 / 3)
  expect_equal(out$MAPE, 40 / 3)
  expect_equal(out$R_squared, 1 - 4100 / 20000)
  expect_equal(out$n_pairs, 3L)
  expect_equal(out$n_percentage_pairs, 3L)
  expect_equal(out$estimate, "raw")
  expect_equal(out$unit, "heads")
  expect_equal(out$r_squared_status, "ok")

  comparison$f2f_value <- 2 * comparison$crt_value
  expect_equal(h_unfccc_metrics(comparison)$R_squared, -6)
})

test_that("metrics match the documented sums with signed reference denominators", {
  comparison <- tibble::tibble(
    metric_id = "formula_check", sheet = "Table3",
    category_label = "Formula check", unit = "kt CO2-eq/year",
    f2f_value = c(-80, 180, 360), crt_value = c(-100, 200, 300),
    f2f_status = "ok", crt_status = "ok"
  )
  prediction <- comparison$f2f_value
  reference <- comparison$crt_value
  n <- length(reference)
  out <- h_unfccc_metrics(comparison)
  expect_equal(out$MPE, 100 / n * sum((prediction - reference) / reference))
  expect_equal(out$MAPE, 100 / n * sum(abs(prediction - reference) / reference))
  expect_equal(out$R_squared,
    1 - sum((reference - prediction)^2) / sum((reference - mean(reference))^2))
})

test_that("invalid and proxy pairs are excluded while zero references remain explicit", {
  comparison <- tibble::tibble(
    metric_id = "area_cropland", sheet = "Table4.B",
    category_label = "Total cropland", unit = "ha",
    f2f_value = c(0, 110, 500, NA, Inf, 300),
    crt_value = c(0, 100, 200, 100, 100, 300),
    f2f_status = c("ok", "ok", "proxy_not_equivalent", "ok", "ok", "ok"),
    crt_status = c("ok", "ok", "ok", "ok", "ok", "notation_key")
  )
  out <- h_unfccc_metrics(comparison)
  expect_equal(out$n_pairs, 2L)
  expect_equal(out$n_excluded, 4L)
  expect_equal(out$n_zero_reference, 1L)
  expect_equal(out$n_percentage_pairs, 1L)
  expect_equal(out$MPE, 10)
  expect_equal(out$MAPE, 10)
  expect_equal(out$R_squared, 0.98)

  comparison$f2f_status <- "unavailable"
  out <- h_unfccc_metrics(comparison)
  expect_equal(out$n_pairs, 0L)
  expect_true(all(is.na(out[c("MPE", "MAPE", "R_squared")])))
  expect_equal(out$r_squared_status, "insufficient_pairs")
  expect_equal(out$percentage_status, "no_nonzero_reference")
})

test_that("R squared is undefined for single pairs and constant references", {
  comparison <- tibble::tibble(
    metric_id = "population_cattle", sheet = "Table3.A",
    category_label = "Cattle", unit = "heads",
    f2f_value = c(0, 1), crt_value = c(0, 0),
    f2f_status = "ok", crt_status = "ok"
  )
  out <- h_unfccc_metrics(comparison)
  expect_true(is.na(out$R_squared))
  expect_true(is.na(out$MPE))
  expect_true(is.na(out$MAPE))
  expect_equal(out$n_zero_reference, 2L)
  expect_equal(out$r_squared_status, "constant_reference")
  expect_equal(out$percentage_status, "no_nonzero_reference")

  out <- h_unfccc_metrics(comparison[1, ])
  expect_true(is.na(out$R_squared))
  expect_equal(out$r_squared_status, "insufficient_pairs")
})

test_that("zero R squared is outside the satisfactory criterion", {
  metrics <- tibble::tibble(
    metric_id = "population_cattle",
    sheet = "Table3.A",
    estimate = "raw",
    MPE = 0,
    MAPE = 0,
    R_squared = 0,
    mpe_p_value = 1,
    n_pairs = 2L,
    n_percentage_pairs = 2L
  )

  plot <- h_unfccc_metric_plot(metrics)
  rating <- plot$data$rating[plot$data$statistic == "R_squared"]
  expect_equal(rating, "Outside criterion")
})

test_that("only coverage variables use raw totals in rescaled metrics", {
  comparison <- tibble::tibble(
    metric_id = rep(c("enteric_total_cattle", "area_cropland"), each = 3),
    sheet = rep(c("Table3.A", "Table4.B"), each = 3),
    category_label = rep(c("Cattle", "Total cropland"), each = 3),
    unit = rep(c("heads", "ha"), each = 3),
    f2f_value = c(110, 180, 360, 100, 200, 300),
    f2f_rescaled = c(100, 200, 300, NA, NA, NA),
    crt_value = rep(c(100, 200, 300), 2),
    f2f_status = "ok", crt_status = "ok"
  )
  out <- h_unfccc_metrics(comparison, rescale = TRUE)
  expect_equal(nrow(out), 2L)
  expect_equal(anyDuplicated(out[c("metric_id", "estimate")]), 0L)
  cattle <- subset(out, metric_id == "enteric_total_cattle" & estimate == "rescaled")
  expect_equal(cattle$MPE, 0)
  expect_equal(cattle$MAPE, 0)
  expect_equal(cattle$R_squared, 1)
  area <- subset(out, metric_id == "area_cropland" & estimate == "raw")
  expect_equal(area$n_pairs, 3L)
  expect_equal(area$R_squared, 1)

  comparison$f2f_rescaled <- NA_real_
  out <- h_unfccc_metrics(comparison, rescale = TRUE)
  cattle <- subset(out, metric_id == "enteric_total_cattle")
  expect_equal(cattle$n_pairs, 0L)
  expect_true(is.na(cattle$MAPE))
})

test_that("comparison retains raw coverage outputs alongside selected metrics", {
  helpers <- new.env(parent = environment())
  source(testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"),
         local = helpers)
  # Isolate report wiring from the scientific calculation routines.
  helpers$h_unfccc_fadn <- function(object, catalogue) tibble::tibble(
    iso3 = "FRA", YEAR = c("2020", "2021"), metric_id = "population_cattle",
    f2f_value = c(110, 180), f2f_status = "ok"
  )
  helpers$h_unfccc_inventory <- function(crt, catalogue) tibble::tibble(
    iso3 = "FRA", YEAR = c("2020", "2021"), metric_id = "population_cattle",
    crt_value = c(100, 200), crt_status = "ok"
  )
  object <- methods::new("FADN2Footprint", farm = data.frame(
    Country_ISO_3166_1_A3 = "FRA", YEAR = c("2020", "2021")
  ))
  out <- helpers$h_unfccc_compare(object, NULL, rescale = TRUE)
  expect_named(out, c("catalogue", "comparison", "metrics"))
  expect_equal(nrow(out$comparison), 2L * nrow(out$catalogue))
  expect_equal(nrow(out$metrics), nrow(out$catalogue))
  cattle <- subset(out$comparison, metric_id == "population_cattle")
  expect_equal(cattle$difference, c(10, -20))
  expect_equal(cattle$relative_difference, c(0.1, -0.1))
  expect_equal(out$metrics, helpers$h_unfccc_metrics(out$comparison, rescale = TRUE))
})

test_that("Eurostat population fills only missing CRT scaling references", {
  helpers <- new.env(parent = environment())
  source(
    testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"),
    local = helpers
  )
  helpers$h_unfccc_fadn <- function(object, catalogue) tibble::tibble(
    iso3 = "FRA",
    YEAR = rep(c("2020", "2021"), 2),
    metric_id = rep(c("population_cattle", "enteric_total_cattle"), each = 2),
    f2f_value = c(200, 200, 300, 300),
    f2f_status = "ok"
  )
  helpers$h_unfccc_inventory <- function(crt, catalogue) tibble::tibble(
    iso3 = "FRA",
    YEAR = c("2020", "2021", "2020", "2021"),
    metric_id = c(
      "population_cattle", "population_cattle",
      "enteric_total_cattle", "enteric_total_cattle"
    ),
    crt_value = c(NA_real_, 100, 200, 100),
    crt_status = c("missing_crt", "ok", "ok", "ok")
  )
  object <- methods::new("FADN2Footprint", farm = data.frame(
    Country_ISO_3166_1_A3 = "FRA", YEAR = c("2020", "2021")
  ))
  eurostat_population <- tibble::tibble(
    iso3 = "FRA",
    YEAR = "2020",
    livcat = "cattle",
    population_heads = 400
  )

  out <- helpers$h_unfccc_compare(
    object,
    NULL,
    rescale = TRUE,
    eurostat_population = eurostat_population
  )
  emissions <- subset(
    out$comparison,
    metric_id == "enteric_total_cattle"
  )
  missing_population <- subset(
    out$comparison,
    metric_id == "population_cattle" & YEAR == "2020"
  )

  expect_equal(emissions$f2f_rescaled, c(600, 150))
  expect_equal(
    emissions$population_reference_source,
    c("Eurostat", "UNFCCC CRT")
  )
  expect_equal(missing_population$population_reference_value, 400)
  expect_equal(missing_population$population_reference_source, "Eurostat")
  expect_equal(missing_population$comparison_status, "missing_crt")
})

test_that("MPE t-test uses individual valid percentage errors against zero", {
  comparison <- tibble::tibble(
    metric_id = "population_cattle", sheet = "Table3.A",
    category_label = "Cattle", unit = "heads",
    f2f_value = c(110, 180, 360, 999, 999, 0),
    crt_value = c(100, 200, 300, 100, 100, 0),
    f2f_status = c("ok", "ok", "ok", "proxy_not_equivalent", "ok", "ok"),
    crt_status = c("ok", "ok", "ok", "ok", "notation_key", "ok")
  )
  expected <- stats::t.test(c(10, -10, 20), mu = 0, alternative = "two.sided")
  out <- h_unfccc_metrics(comparison)
  expect_equal(out$mpe_p_value, expected$p.value)
  expect_equal(out$mpe_t_statistic, unname(expected$statistic))
  expect_equal(out$mpe_df, unname(expected$parameter))
  expect_equal(out$mpe_ci_lower, expected$conf.int[1])
  expect_equal(out$mpe_ci_upper, expected$conf.int[2])
  expect_equal(out$mpe_test_status, "ok")

  comparison$f2f_rescaled <- comparison$crt_value * c(1.2, 0.8, 1.4, 1, 1, 1)
  comparison$metric_id <- "enteric_total_cattle"
  out <- subset(h_unfccc_metrics(comparison, rescale = TRUE), estimate == "rescaled")
  expected <- stats::t.test(c(20, -20, 40), mu = 0)
  expect_equal(out$mpe_p_value, expected$p.value)
  expect_equal(out$mpe_ci_lower, expected$conf.int[1])

  out <- h_unfccc_metrics(comparison[1, ])
  expect_true(is.na(out$mpe_p_value))
  expect_equal(out$mpe_test_status, "insufficient_pairs")
  comparison$f2f_value <- comparison$crt_value
  out <- h_unfccc_metrics(comparison)
  expect_true(is.na(out$mpe_p_value))
  expect_equal(out$mpe_test_status, "constant_errors")

  comparison$f2f_value <- comparison$crt_value * (1 + .Machine$double.eps)
  out <- h_unfccc_metrics(comparison)
  expect_true(is.na(out$mpe_p_value))
  expect_equal(out$mpe_test_status, "constant_errors")
})

test_that("metric heatmaps apply exact acceptability boundaries and retain missing rows", {
  metrics <- tibble::tibble(
    metric_id = paste0("metric_", 1:6), sheet = "Table3",
    estimate = "raw", unit = "kt CO2-eq/year",
    MPE = c(-10, 10, 0, 20, 30, NA),
    MAPE = c(19.9, 20, 50, 50.1, -1, NA),
    R_squared = c(1, 0.51, 0.5, 0, -0.1, NA),
    mpe_p_value = c(0.051, 0.05, 0.01, 0.9, NA, NA),
    n_pairs = c(3L, 3L, 3L, 3L, 3L, 0L),
    n_percentage_pairs = c(3L, 3L, 3L, 3L, 3L, 0L)
  )
  plot <- h_unfccc_metric_plot(metrics)
  expect_s3_class(plot, "ggplot")
  expect_equal(nrow(plot$data), 24L)
  expect_equal(subset(plot$data, statistic == "MAPE")$rating,
    c("Meets criterion", "Intermediate", "Intermediate", "Outside criterion",
      "Unavailable", "Unavailable"))
  expect_equal(subset(plot$data, statistic == "R_squared")$rating,
    c("Meets criterion", "Meets criterion", "Intermediate", "Outside criterion",
      "Outside criterion", "Unavailable"))
  expect_equal(subset(plot$data, statistic == "mpe_p_value")$rating,
    c("Meets criterion", "Outside criterion", "Outside criterion",
      "Meets criterion", "Unavailable", "Unavailable"))
  expect_equal(subset(plot$data, statistic == "MPE")$rating,
    subset(plot$data, statistic == "mpe_p_value")$rating)
  expect_true(all(nzchar(plot$data$label)))
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
  expect_equal(nrow(h_unfccc_metric_plot(metrics[6, ])$data), 4L)
})

test_that("report differences, metrics and captions share the rescaled estimates", {
  helpers <- new.env(parent = environment())
  source(testthat::test_path("..", "..", "inst", "validation", "compare_UNFCCC.R"),
         local = helpers)
  ids <- c("population_cattle", "population_swine", "area_cropland",
           "area_grassland", "enteric_total_cattle", "soil_total",
           "production_wheat", "gross_energy_cattle", "manure_indirect_n2o")
  f2f <- tibble::tibble(
    iso3 = "FRA", YEAR = rep(c("2020", "2021"), length(ids)),
    metric_id = rep(ids, each = 2),
    f2f_value = c(50, 100, 10, 20, 10, 20, 20, 40,
                  40, 60, 40, 60, 40, 60, 100, 200, 40, 60),
    f2f_status = "ok",
    f2f_species_values = rep(list(NULL), 18))
  f2f$f2f_species_values[17:18] <- list(c(cattle = 10, swine = 30),
                                                      c(cattle = 20, swine = 40))
  inventory <- f2f |>
    dplyr::transmute(iso3, YEAR, metric_id,
      crt_value = c(100, 200, 30, 60, 20, 40, 40, 80,
                    100, 200, 100, 200, 100, 200, 100, 200, 110, 160),
      crt_status = "ok")
  helpers$h_unfccc_fadn <- function(object, catalogue) f2f
  helpers$h_unfccc_inventory <- function(crt, catalogue) inventory
  object <- methods::new("FADN2Footprint", farm = data.frame(
    Country_ISO_3166_1_A3 = "FRA", YEAR = c("2020", "2021")))
  out <- helpers$h_unfccc_compare(object, NULL)
  for (id in c("enteric_total_cattle", "soil_total", "production_wheat")) {
    pairs <- subset(out$comparison, metric_id == id)
    summary <- subset(out$metrics, metric_id == id)
    expect_equal(pairs$comparison_value, c(80, 120))
    expect_equal(pairs$difference, c(-20, -80))
    expect_equal(pairs$relative_difference, c(-0.2, -0.4))
    expect_equal(summary$MPE, -30)
    expect_equal(summary$MAPE, 30)
    expect_equal(summary$R_squared, 1 - 6800 / 5000)
    expect_equal(summary$estimate, "rescaled")
    plot <- helpers$h_unfccc_metric_plot(summary)
    expect_match(plot$labels$caption, "F2F rescaled by")
  }
  manure <- subset(out$comparison, metric_id == "manure_indirect_n2o")
  expect_equal(manure$f2f_rescaled, c(110, 160))
  expect_equal(manure$difference, c(0, 0))
  expect_match(manure$comparison_basis, "species-specific", all = TRUE)
  energy <- subset(out$comparison, metric_id == "gross_energy_cattle")
  expect_equal(energy$comparison_value, energy$f2f_value)
  expect_equal(energy$estimate, rep("per_head", 2))
  expect_match(energy$comparison_basis, "no coverage rescaling", all = TRUE)
  coverage <- subset(out$comparison, family == "area" | component == "population")
  expect_equal(coverage$comparison_value, coverage$f2f_value)
  expect_true(all(coverage$estimate == "raw"))
  expect_true(all(is.na(coverage$f2f_rescaled[coverage$family == "area"])))
  expect_error(helpers$h_unfccc_compare(object, NULL, rescale = FALSE),
               "require coverage rescaling")

  report <- readLines(testthat::test_path("..", "..", "inst", "validation",
                                        "validation_report_UNFCCC.qmd"))
  chunk <- function(label) {
    start <- match(paste0("```{r ", label, "}"), report)
    end <- start + match("```", report[(start + 1L):length(report)])
    parse(text = report[(start + 1L):(end - 1L)])
  }
  page <- subset(out$metrics, metric_id == "soil_total")
  heatmap <- helpers$h_unfccc_country_year_plot(out$comparison, page)
  expect_equal(heatmap$data$percentage_error, c(-20, -40))
  expect_match(heatmap$labels$caption, "rescaled by CRT/F2F.*area ratio")
  expect_s3_class(ggplot2::ggplot_build(heatmap), "ggplot_built")

  soil_plot <- helpers$h_unfccc_scatter_plot(
    subset(out$comparison, metric_id == "soil_total")
  )
  expect_equal(soil_plot$data$plot_value, c(80, 120))
  expect_match(soil_plot$labels$caption, "rescaled by CRT/F2F.*area ratio")

  plots <- list()
  headings <- character()
  plot_env <- new.env(parent = helpers)
  plot_env$comparison <- out$comparison
  plot_env$metrics <- out$metrics
  plot_env$selected <- helpers$h_unfccc_selected(testthat::test_path(
    "..", "..", "inst", "validation", "CRT_vs_F2F.xlsx"
  ))
  plot_env$print <- function(x) {
    plots[[length(plots) + 1L]] <<- x
    invisible(x)
  }
  plot_env$cat <- function(..., sep = " ") {
    headings <<- c(headings, paste(..., sep = sep, collapse = sep))
    invisible(NULL)
  }
  eval(chunk("selected_reports"), envir = plot_env)
  section_headings <- headings[startsWith(headings, "\n\n## ")]
  expect_equal(section_headings,
    paste0("\n\n## ", plot_env$selected$title, "\n\n"))
  expect_equal(length(plots) %% 3L, 0L)
  expect_true(all(vapply(plots[seq(1L, length(plots), by = 3L)],
    function(x) "plot_value" %in% names(x$data), logical(1))))
  expect_true(all(vapply(plots[seq(2L, length(plots), by = 3L)],
    function(x) "percentage_error" %in% names(x$data), logical(1))))
  expect_true(all(vapply(plots[seq(3L, length(plots), by = 3L)],
    function(x) "statistic" %in% names(x$data), logical(1))))
  expect_true(all(vapply(plots, function(x) nzchar(x$labels$caption), logical(1))))

  # Incomplete species coverage must not produce a partial indirect total.
  f2f$f2f_species_values[[17]] <- c(cattle = 10, swine = 30, sheep = 1)
  missing_species <- helpers$h_unfccc_compare(object, NULL)
  manure <- subset(missing_species$comparison, metric_id == "manure_indirect_n2o")
  expect_equal(manure$f2f_rescaled, c(NA_real_, 160))
  f2f$f2f_species_values[[17]] <- c(cattle = 10, swine = 30, sheep = 0)
  zero_species <- helpers$h_unfccc_compare(object, NULL)
  manure <- subset(zero_species$comparison, metric_id == "manure_indirect_n2o")
  expect_equal(manure$f2f_rescaled, c(110, 160))
  inventory$crt_status[inventory$metric_id == "population_cattle"] <- "notation_key"
  inventory$crt_value[inventory$metric_id == "area_grassland"] <- 0
  f2f$f2f_value[f2f$metric_id == "area_grassland"] <- 0
  f2f$f2f_value[f2f$metric_id == "area_cropland"] <- 0
  out <- helpers$h_unfccc_compare(object, NULL)
  affected <- subset(out$comparison,
    metric_id %in% c("manure_indirect_n2o", "enteric_total_cattle",
                     "soil_total", "production_wheat"))
  expect_true(all(is.na(affected$comparison_value)))
  expect_true(all(is.na(affected$difference)))
  expect_true(all(affected$comparison_status == "missing_rescaling_factor_or_value"))
  affected_metrics <- subset(out$metrics, metric_id %in% affected$metric_id)
  expect_true(all(affected_metrics$n_pairs == 0L))
  expect_true(all(is.na(affected_metrics$MAPE)))
})
