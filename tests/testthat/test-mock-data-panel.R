cont_stats <- data.frame(
  variable = c("A_TA", "A_PRQ", "B_V"),
  mean = c(50, 300, 1000), sd = c(40, 250, 800),
  min = c(0, 0, 0), max = c(400, 3000, 9000)
)
cat_stats <- data.frame(
  variable = c(rep("NUTS3", 3), rep("NUTS2", 2), rep("SIZC", 3)),
  level = c("FR101", "FR102", "FR211", "FR10", "FR21", "5", "6", "7"),
  count = c(10, 10, 20, 20, 20, 30, 20, 10)
)
lookup <- data.frame(
  from_var = "NUTS3", from_level = c("FR101", "FR102", "FR211"),
  to_var = "NUTS2", to_level = c("FR10", "FR10", "FR21")
)
core <- matrix(c(1, 0.8, 0.8, 1), 2, dimnames = list(c("A_TA", "A_PRQ"), c("A_TA", "A_PRQ")))

gen <- function(dep, n = 2000, years = 2018:2020, ...) {
  h_generate_mock_panel(n = n, years = years, continuous_stats = cont_stats,
                        categorical_stats = cat_stats, dependence = dep,
                        cat_lookup = lookup, seed = 1, ...)
}
retained_share <- function(x, y0, y1) mean(x$ID[x$YEAR == y0] %in% x$ID[x$YEAR == y1])

test_that("h_generate_mock_panel returns one row per farm-year with expected structure", {
  x <- gen(list(retention = 0.9))
  expect_s3_class(x, "data.frame")
  expect_true(all(c("ID", "YEAR", cont_stats$variable, "NUTS3", "NUTS2", "SIZC") %in% names(x)))
  expect_equal(as.vector(table(x$YEAR)), rep(2000L, 3))
  expect_false(anyDuplicated(x[c("ID", "YEAR")]) > 0)
  expect_false(anyNA(x[cont_stats$variable]))
  expect_true(all(as.matrix(x[cont_stats$variable]) >= 0))
  lat <- attr(x, "latent")
  expect_equal(nrow(lat), nrow(x))
  expect_named(lat, c("ID", "YEAR", ".size"))
})

test_that("panel retention follows the sampling-plan retention rate", {
  expect_equal(retained_share(gen(list(retention = 1)), 2018, 2019), 1)
  expect_equal(retained_share(gen(list(retention = 0)), 2018, 2019), 0)
  expect_equal(retained_share(gen(list(retention = 0.8), n = 5000), 2019, 2020),
               0.8, tolerance = 0.03)
  # default (no dependence) keeps independent cross-sections
  expect_equal(retained_share(gen(NULL), 2018, 2019), 0)
})

test_that("Gaussian copula reproduces the core rank correlation", {
  x <- gen(list(cor_core = core), n = 5000, years = 2020)
  rs <- stats::cor(x$A_TA, x$A_PRQ, method = "spearman")
  expect_gt(rs, 0.7)
  # non-core variable without size loading stays independent
  expect_lt(abs(stats::cor(x$A_TA, x$B_V, method = "spearman")), 0.06)
})

test_that("size loading correlates non-core variables with farm size", {
  x <- gen(list(cor_core = core, size_loading = c(.default = 0.7)), n = 5000, years = 2020)
  expect_gt(stats::cor(x$A_TA, x$B_V, method = "spearman"), 0.4)
})

test_that("staying farms keep persistent values and categories", {
  x <- gen(list(retention = 1, autocorr = 0.95, cat_persistence = 1,
                size_autocorr = 0.95), n = 3000, years = 2019:2020)
  a <- x[x$YEAR == 2019, ]; b <- x[x$YEAR == 2020, ]
  b <- b[match(a$ID, b$ID), ]
  expect_gt(stats::cor(a$B_V, b$B_V, method = "spearman"), 0.85)
  expect_true(all(a$NUTS3 == b$NUTS3))

  y <- gen(list(retention = 1), n = 3000, years = 2019:2020)
  a <- y[y$YEAR == 2019, ]; b <- y[y$YEAR == 2020, ]
  b <- b[match(a$ID, b$ID), ]
  expect_lt(abs(stats::cor(a$B_V, b$B_V, method = "spearman")), 0.06)
})

test_that("categorical persistence accounts for chance matches on redraw", {
  cat_stats <- data.frame(variable = "CAT", level = c("a", "b"), count = c(1, 1))
  farms <- h_generate_mock_panel(
    n = 10000, years = 2019:2020, categorical_stats = cat_stats,
    dependence = list(retention = 1, cat_persistence = 0.6), seed = 12
  )
  a <- farms[farms$YEAR == 2019, ]
  b <- farms[farms$YEAR == 2020, ]
  b <- b[match(a$ID, b$ID), ]
  expect_equal(mean(a$CAT == b$CAT), 0.6, tolerance = 0.02)

  parent_latent <- data.frame(
    ID = rep(sprintf("F%05d", seq_len(10000)), each = 2),
    YEAR = rep(2019:2020, times = 10000),
    .size = 0
  )
  children <- h_generate_mock_child_panel(
    parent_latent, rows_per_parent = 1, categorical_stats = cat_stats,
    dependence = list(cat_persistence = 0.6), seed = 12
  )
  a <- children[children$YEAR == 2019, ]
  b <- children[children$YEAR == 2020, ]
  b <- b[match(a$ID, b$ID), ]
  expect_equal(mean(a$CAT == b$CAT), 0.6, tolerance = 0.02)
})

test_that("nested classifications are derived consistently", {
  x <- gen(list(retention = 0.5, cat_persistence = 0.5), n = 500)
  expect_true(all(x$NUTS2 == lookup$to_level[match(x$NUTS3, lookup$from_level)]))
})

test_that("ordinal categorical variables follow the size score", {
  x <- gen(list(cor_core = core, ordinal_loading = c(SIZC = 0.8)), n = 5000, years = 2020)
  expect_gt(stats::cor(as.numeric(x$SIZC), x$A_TA, method = "spearman"), 0.4)
  expect_setequal(unique(x$SIZC), c("5", "6", "7"))
})

test_that("grouped generation uses group-specific dependence", {
  cs <- rbind(transform(cont_stats, COUNTRY = "AAA"), transform(cont_stats, COUNTRY = "BBB"))
  x <- h_generate_mock_panel(
    n = 2000, years = 2018:2019, continuous_stats = cs,
    group_cols = "COUNTRY", dependence_by = "COUNTRY",
    dependence = list(AAA = list(retention = 1), BBB = list(retention = 0)), seed = 3
  )
  expect_equal(retained_share(x[x$COUNTRY == "AAA", ], 2018, 2019), 1)
  expect_equal(retained_share(x[x$COUNTRY == "BBB", ], 2018, 2019), 0)
  expect_true(all(tapply(x$ID, x$ID, length) <= 2))
})

test_that("child panel rows follow their farm through the panel", {
  farms <- gen(list(retention = 0.7, cor_core = core), n = 300, years = 2018:2020)
  lat <- attr(farms, "latent")
  rows <- h_generate_mock_child_panel(
    lat, rows_per_parent = 3,
    continuous_stats = cont_stats[1:2, ],
    categorical_stats = data.frame(variable = "CODE", level = c("1", "2"), count = c(1, 1)),
    dependence = list(size_loading = c(.default = 0.6), family_cor = 0.8,
                      autocorr = 0.9, cat_persistence = 1),
    seed = 2
  )
  key <- paste(rows$ID, rows$YEAR)
  expect_true(all(key %in% paste(lat$ID, lat$YEAR)))
  expect_true(all(paste(lat$ID, lat$YEAR) %in% key))
  # rows per farm are stable while the farm stays in the sample
  n_by <- table(key)
  per_farm <- tapply(as.vector(n_by), sub(" .*", "", names(n_by)), function(z) length(unique(z)))
  expect_true(all(per_farm == 1))
  expect_equal(nrow(rows) / nrow(lat), 3, tolerance = 0.1)
  # row variables share a family factor (area vs production of a row)
  expect_gt(stats::cor(rows$A_TA, rows$A_PRQ, method = "spearman"), 0.5)

  fewer <- h_generate_mock_child_panel(lat, rows_per_parent = 0.3,
                                       continuous_stats = cont_stats[1, ], seed = 2)
  expect_false(anyDuplicated(fewer[c("ID", "YEAR")]) > 0)
  expect_lt(nrow(fewer), nrow(lat))
})

test_that(".h_sim_continuous keeps its random stream when no uniforms are given", {
  set.seed(10)
  a <- .h_sim_continuous(100, mean = 5, sd = 3, min = 0, max = 50)
  set.seed(10)
  invisible(runif(100))
  b <- 0 + rgamma(100, shape = (5 / 3)^2, scale = 9 / 5)
  b <- pmin(pmax(b * (5 / mean(b)), 0), 50)
  expect_equal(a, b)

  u <- c(0, 0.25, 0.5, 0.75, 1)
  v <- .h_sim_continuous(5, mean = 5, sd = 3, min = 0, max = 50, u = u)
  expect_true(all(is.finite(v)))
  expect_false(is.unsorted(v))
})

test_that(".h_sim_continuous zero-inflates gamma marginals with p_nonzero", {
  u <- (seq_len(10000) - 0.5) / 10000
  x <- .h_sim_continuous(10000, mean = 100, sd = 150, min = 0, max = 5000,
                         u = u, p_nonzero = 0.3)
  expect_equal(mean(x == 0), 0.7, tolerance = 0.001)
  expect_equal(mean(x), 100, tolerance = 1e-6)
  # zeros are given to the lowest scores (monotone in u)
  expect_true(all(x[u <= 0.7] == 0))
  expect_true(all(x[u > 0.7] > 0))
  expect_false(is.unsorted(x))
  # no non-zero share -> all zeros; NA -> unchanged gamma behaviour
  expect_true(all(.h_sim_continuous(100, 100, 150, 0, 5000, u = u[1:100], p_nonzero = 0) == 0))
  expect_identical(
    .h_sim_continuous(100, 100, 150, 0, 5000, u = u[1:100]),
    .h_sim_continuous(100, 100, 150, 0, 5000, u = u[1:100], p_nonzero = NA_real_)
  )
})

test_that("h_generate_mock_panel uses p_nonzero so that zeros go to small farms", {
  cs <- cont_stats
  cs$p_nonzero <- c(NA, NA, 0.4)
  x <- h_generate_mock_panel(n = 5000, years = 2020, continuous_stats = cs,
                             dependence = list(size_loading = c(.default = 0.8)), seed = 1)
  expect_equal(mean(x$B_V == 0), 0.6, tolerance = 0.03)
  expect_equal(mean(x$A_TA == 0), 0)
  s <- attr(x, "latent")$.size
  expect_gt(mean(s[x$B_V > 0]), mean(s[x$B_V == 0]))
})
