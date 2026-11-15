load_unfccc <- function(env) {
  load(testthat::test_path("..", "..", "data", "UNFCCC_data.rda"), envir = env)
  env$UNFCCC_data
}

test_that("livestock tables are mapped to FADN categories", {
  unfccc <- load_unfccc(environment())
  fadn_codes <- data_extra$livestock$FADN_code_letter
  for (table in c("table3As2", "table3Bas1", "table3Bas2")) {
    x <- unfccc[[table]]
    expect_true(all(c("Animal_cat", "species", "fadn_match",
                      "FADN_code_letter", "match_level") %in% names(x)))
    expect_false("UNFCCC_cat" %in% names(x))
    mapped <- x[!is.na(x$FADN_code_letter), ]
    expect_true(all(mapped$FADN_code_letter %in% fadn_codes))
    expect_true(all(mapped$match_level %in% 1:3))
    # only the closest match level is kept per FADN category x country x year
    levels <- unique(mapped[c("FADN_code_letter", "Country_ISO_3166_1_A3",
                              "YEAR", "match_level")])
    expect_equal(
      anyDuplicated(levels[c("FADN_code_letter", "Country_ISO_3166_1_A3",
                             "YEAR")]),
      0L
    )
  }
})

test_that("DNK 2018 swine sub-categories replace the swine aggregate", {
  # Regression: the aggregate swine row was kept with its uncoded children
  # (sows, fattening pigs, weaners) and double counted in weighted means.
  s1 <- load_unfccc(environment())$table3Bas1
  swine <- s1[s1$Country_ISO_3166_1_A3 == "DNK" & s1$YEAR == "2018" &
                s1$species %in% "swine", ]
  expect_setequal(unique(swine$Animal_cat),
                  c("sows", "fattening_pigs", "weaners"))
  expect_equal(unique(swine$Animal_cat[swine$FADN_code_letter == "LSOWBRE"]),
               "sows")
  expect_equal(unique(swine$Animal_cat[swine$FADN_code_letter == "LPIGLET"]),
               "weaners")
  expect_setequal(
    swine$FADN_code_letter[swine$Animal_cat == "fattening_pigs"],
    c("LPIGFAT", "LPIGOTH")
  )
  expect_false("LPIG" %in% swine$FADN_code_letter)
  expect_equal(
    sum(unique(swine[c("Animal_cat", "Population__size")])$Population__size),
    1045.165 + 5345.416 + 6390.667,
    tolerance = 1e-6
  )
})

test_that("dairy and non-dairy cattle map to distinct FADN categories", {
  s1 <- load_unfccc(environment())$table3Bas1
  expect_true(all(s1$FADN_code_letter[s1$Animal_cat == "dairy_cattle"] ==
                    "LCOWDAIR"))
  non_dairy <- s1[s1$Animal_cat == "non_dairy_cattle", ]
  expect_gt(nrow(non_dairy), 0L)
  expect_false("LCOWDAIR" %in% non_dairy$FADN_code_letter)
  expect_false(any(c("LBUFDAIRPRS", "LCOWBUFDAIR_X") %in%
                     non_dairy$FADN_code_letter))
  dnk <- non_dairy[non_dairy$Country_ISO_3166_1_A3 == "DNK" &
                     non_dairy$YEAR == "2018", ]
  expect_true(all(c("LBOV1", "LHEIFBRE", "LCOWOTH") %in%
                    dnk$FADN_code_letter))
  expect_true(all(dnk$match_level == 2L))
})
