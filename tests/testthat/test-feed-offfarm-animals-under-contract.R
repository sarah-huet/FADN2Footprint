# Tests for f_feed_offfarm_animals_under_contract()
#
# The function imputes purchased feed values (EUR) for farms rearing animals
# under contract, from the weighted ratio of purchased feed value to animal
# sales value observed on farms without contracts
# (per feed variable x species group x country x year, with
# country and all-farms fallbacks).

make_object <- function(input, herd) {
  methods::new(
    "FADN2Footprint",
    input = input,
    herd = herd,
    traceability = list(id_cols = c("ID", "YEAR", "COUNTRY"))
  )
}

test_that("imputes purchased feed value from contract sales using the reference ratio", {
  input <- data.frame(
    ID = c("ref", "contract"),
    YEAR = "2020",
    COUNTRY = "FRA",
    feed_purch_concent_poultry = c(1000, 0),
    sales_contract_poultry = c(0, 50000)
  )
  herd <- data.frame(
    ID = c("ref", "contract"),
    YEAR = "2020",
    COUNTRY = "FRA",
    FADN_code_letter = "LPLTRBROYL",
    species = "poultry",
    Qobs = c(10000, 20000),
    SV = c(5000, 0)
  )
  object <- make_object(input, herd)

  res <- suppressMessages(f_feed_offfarm_animals_under_contract(object))

  # only the contract farm receives an imputed value
  expect_equal(nrow(res), 1L)
  expect_equal(res$ID, "contract")
  expect_equal(res$feed_var, "feed_purch_concent_poultry")
  # reference ratio = 1000 / 5000 = 0.2 EUR feed per EUR animal sales
  # imputed value = 0.2 * 50000 = 10000 EUR
  expect_equal(res$feed_purch_value_contract, 10000)
  # output structure and units sanity: EUR values must be positive
  expect_named(
    res,
    c("ID", "YEAR", "COUNTRY", "feed_var", "feed_purch_value_contract")
  )
  expect_true(all(res$feed_purch_value_contract > 0))
  expect_false(anyNA(res$feed_purch_value_contract))
})

test_that("falls back to the country-level ratio when no reference farm exists for the year", {
  # reference farm observed in 2019 only; contract farm in 2020
  input <- data.frame(
    ID = c("ref", "contract"),
    YEAR = c("2019", "2020"),
    COUNTRY = "FRA",
    feed_purch_concent_pigs = c(2000, 0),
    sales_contract_pigs = c(0, 10000)
  )
  herd <- data.frame(
    ID = c("ref", "contract"),
    YEAR = c("2019", "2020"),
    COUNTRY = "FRA",
    FADN_code_letter = "LPIGFAT",
    species = "swine",
    Qobs = c(500, 800),
    SV = c(4000, 0)
  )
  object <- make_object(input, herd)

  res <- suppressMessages(f_feed_offfarm_animals_under_contract(object))

  # country-level ratio = 2000 / 4000 = 0.5 -> imputed value = 5000 EUR
  expect_equal(nrow(res), 1L)
  expect_equal(res$ID, "contract")
  expect_equal(res$feed_purch_value_contract, 5000)
})

test_that("returns an empty table when no contract sales variable is present", {
  input <- data.frame(
    ID = "a",
    YEAR = "2020",
    COUNTRY = "FRA",
    feed_purch_concent_pigs = 500
  )
  herd <- data.frame(
    ID = "a",
    YEAR = "2020",
    COUNTRY = "FRA",
    FADN_code_letter = "LPIGFAT",
    species = "swine",
    Qobs = 100,
    SV = 2000
  )
  object <- make_object(input, herd)

  expect_message(
    res <- f_feed_offfarm_animals_under_contract(object),
    "No animals-under-contract sales variable"
  )
  expect_equal(nrow(res), 0L)
  expect_named(
    res,
    c("ID", "YEAR", "COUNTRY", "feed_var", "feed_purch_value_contract")
  )
})

test_that("returns an empty table when no farm has animals under contract", {
  input <- data.frame(
    ID = "a",
    YEAR = "2020",
    COUNTRY = "FRA",
    feed_purch_concent_poultry = 500,
    sales_contract_poultry = 0
  )
  herd <- data.frame(
    ID = "a",
    YEAR = "2020",
    COUNTRY = "FRA",
    FADN_code_letter = "LHENSLAY",
    species = "poultry",
    Qobs = 100,
    SV = 2000
  )
  object <- make_object(input, herd)

  expect_message(
    res <- f_feed_offfarm_animals_under_contract(object),
    "No farm with animals under contract"
  )
  expect_equal(nrow(res), 0L)
})

test_that("rejects objects that are not FADN2Footprint", {
  expect_error(
    f_feed_offfarm_animals_under_contract(data.frame()),
    "must be a valid 'FADN2Footprint' object"
  )
})
