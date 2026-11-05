#' Create a mock (non-confidential) FADN dataset for testing FADN2Footprint
#'
#' @description
#' This script builds shareable *mock* FADN datasets (EU-FADN format and French
#' RICA format) from aggregated descriptive statistics, so that users can test
#' the \pkg{FADN2Footprint} workflow without accessing real farm records.
#'
#' The script:
#' \enumerate{
#'   \item Loads marginal descriptive statistics (mean/sd/min/max and, for the
#'         French data, quartiles; level counts for categorical variables).
#'   \item Loads aggregated dependence statistics computed from confidential
#'         microdata by \code{inst/scripts/mock_data_dependence_stats.R}
#'         (correlations, latent farm-size loadings, year-to-year persistence,
#'         national sample retention rates, nested classifications).
#'   \item Simulates a rotating farm panel over \code{2010:2020} with
#'         \code{h_generate_mock_panel()}: variables are correlated through a
#'         Gaussian copula (core variables), a latent farm-size score and
#'         product-family factors; each year a share of farms (country-specific
#'         retention of the FADN sampling plan) stays in the sample with
#'         persistent values and categories, and leaving farms are replaced.
#'         French crop/animal/product tables follow their farm through the
#'         panel (\code{h_generate_mock_child_panel()}).
#'   \item Applies consistency rules (crop variables without area, organic
#'         farms without mineral fertiliser, farmer age increasing by one year
#'         per year).
#'   \item Uses \code{infer_practices()} to retain whole farms so that about
#'         90\% of farm-years pass the practice inference.
#'   \item Saves \code{mock_data} with \code{usethis::use_data()} and writes
#'         Excel files (one per country and year for EU; one folder per year
#'         and one file per table for France).
#' }
#'
#' @details
#' ## Inputs
#' \itemize{
#'   \item \code{inst/ext_data/French_FADN_desc_stats.xlsx},
#'         \code{inst/ext_data/EU_FADN_2016_2018_desc_stats.xlsx}: marginal
#'         descriptive statistics.
#'   \item \code{inst/ext_data/mock_dependence_stats_EU_17_21.RData}:
#'         aggregated dependence statistics (\code{mock_dependence}). French RICA
#'         variable names cannot be matched to EU-FADN names, so the French mock
#'         uses the French median parameters (size loading, autocorrelation,
#'         categorical persistence) and the French retention rate.
#' }
#'
#' ## Outputs
#' \itemize{
#'   \item \code{mock_data}: list with \code{fadn_fict} (EU format, one row per
#'         farm and year) and \code{rica_fict} (French tables, last year).
#'   \item \code{inst/ext_data/mock_FADN_EU}, \code{inst/ext_data/mock_FADN_FRA}.
#' }
#'
#' @section Confidentiality / disclosure control:
#' Only aggregated statistics computed on at least 50 farms (or farm pairs)
#' are used; no record of the confidential data is copied into the mock.
#'
#' @section Dependencies:
#' Requires \pkg{dplyr}, \pkg{readxl}, \pkg{openxlsx} and \pkg{usethis}.
#'
#' @seealso
#' \code{\link[usethis]{use_data}}
#'
#' @keywords internal
#'
"mock_data"

# Shared settings and helpers ----

mock_years <- 2010:2020
target_pass_rate <- 0.9

# Aggregated dependence statistics (see inst/scripts/mock_data_dependence_stats.R)
load("inst/ext_data/mock_dependence_stats_EU_17_21.RData")

# Dependence parameters of one country for h_generate_mock_panel(). Countries
# without microdata fall back to the median over countries (no copula).
build_dependence <- function(cty, md = mock_dependence) {
  one <- function(df, col) {
    x <- df[[col]][df$COUNTRY == cty]
    if (length(x) == 1 && is.finite(x)) x else stats::median(df[[col]], na.rm = TRUE)
  }
  named <- function(df, col) {
    d <- df[df$COUNTRY == cty & is.finite(df[[col]]), , drop = FALSE]
    stats::setNames(d[[col]], d$variable)
  }
  list(
    retention = one(md$retention, "retention"),
    cor_core = md$cor_core[[cty]],
    size_loading = c(.default = one(md$summary, "median_size_loading"),
                     named(md$size_loading, "loading")),
    family_cor = one(md$family_cor, "family_cor"),
    autocorr = c(.default = one(md$summary, "median_autocorr"), named(md$autocorr, "rho")),
    size_autocorr = one(md$size_autocorr, "rho"),
    cat_persistence = c(.default = one(md$summary, "median_cat_persistence"),
                        named(md$cat_persistence, "p_same")),
    ordinal_loading = c(SIZC = one(md$ordinal_loading, "loading"))
  )
}

# Keep whole farms (so that panels are not broken) such that ~target_pass_rate
# of the retained farm-years pass infer_practices(). Fully valid farms are
# kept first, then farms by decreasing share of valid years (ties in random
# order). `all_rows` and `valid_rows` are data.frames with ID and YEAR.
select_panel_farms <- function(all_rows, valid_rows, target_pass_rate, seed) {
  set.seed(seed)
  ok <- paste(all_rows$ID, all_rows$YEAR) %in% paste(valid_rows$ID, valid_rows$YEAR)
  n_rows <- tapply(ok, all_rows$ID, length)
  n_ok <- tapply(ok, all_rows$ID, sum)
  fully_valid <- names(n_rows)[n_ok == n_rows]
  others <- setdiff(names(n_rows), fully_valid)
  others <- others[order(-(n_ok[others] / n_rows[others]), stats::runif(length(others)))]
  valid_cum <- sum(n_ok[fully_valid]) + c(0, cumsum(n_ok[others]))
  total_cum <- sum(n_rows[fully_valid]) + c(0, cumsum(n_rows[others]))
  k <- which.min(abs(valid_cum / total_cum - target_pass_rate))
  if (abs(valid_cum[k] / total_cum[k] - target_pass_rate) > 0.01) {
    stop("Cannot reach a ", target_pass_rate, " pass rate by selecting whole farms.")
  }
  c(fully_valid, others[seq_len(k - 1)])
}

# Share of the farms of year t still in the sample in year t+1
panel_retention <- function(ids, years) {
  yrs <- sort(unique(years))
  a <- unlist(lapply(yrs[-length(yrs)], function(y) ids[years == y] %in% ids[years == y + 1]))
  mean(a)
}

# French FADN data ----

## Upload desc stats ----
library(readxl)
file_path = "inst/ext_data/French_FADN_desc_stats.xlsx"

sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(s) read_excel(file_path, sheet = s))
names(data_list) <- sheets

## Generate Mock data ----

# Standardise the descriptive statistics sheets: continuous variables are
# simulated from mean/sd/min/max and the quartiles (P25, Median, P75);
# categorical variables are sampled from the observed level frequencies.
prep_cont <- function(df) {
  df |>
    dplyr::mutate(
      variable = Variable,
      mean = as.numeric(Mean),
      sd   = as.numeric(SD),
      min  = as.numeric(Min),
      p25  = as.numeric(P25),
      p50  = as.numeric(Median),
      p75  = as.numeric(P75),
      max  = as.numeric(Max)
    )
}
prep_cat <- function(df) {
  df |>
    dplyr::filter(!is.na(N)) |>
    dplyr::mutate(variable = Variable,
                  level = as.character(Category),
                  count = as.numeric(N))
}

# Number of records of each table in the real data (N / share of total)
table_size <- function(cat_df) {
  stats::median(as.numeric(cat_df$N) / (as.numeric(cat_df$`N/total`) / 100),
                na.rm = TRUE)
}

# Farms per year before the infer_practices() selection
n_farms <- 300L
tables <- c(rica = "rica20", veg = "veg20", ani = "ani20", pan = "pan20")

# Each table keeps its real size relative to the farm table (e.g. several
# crop rows per farm in `veg`)
size_rica <- table_size(data_list$rica20_cat)

# RICA variable names cannot be matched to the EU-FADN names of the
# dependence statistics: use the French median parameters. Farmer age is not
# driven by farm size (it is aged deterministically below); the economic size
# class CDEXE is ordinal and follows the latent farm size like SIZC.
# ACHEN (fertiliser purchases, EUR) is the RICA equivalent of SE295 (see the
# conversion dictionary): it is missing from the French descriptive
# statistics, so its marginal and dependence come from the French EU-FADN
# SE295 statistics.
dep_fra <- build_dependence("FRA")
dep_rica <- list(
  retention = dep_fra$retention,
  size_loading = c(.default = dep_fra$size_loading[[".default"]], AGECH = 0,
                   ACHEN = dep_fra$size_loading[["SE295"]]),
  family_cor = dep_fra$family_cor,
  autocorr = c(dep_fra$autocorr[".default"], ACHEN = dep_fra$autocorr[["SE295"]]),
  size_autocorr = dep_fra$size_autocorr,
  cat_persistence = dep_fra$cat_persistence[".default"],
  ordinal_loading = c(CDEXE = dep_fra$ordinal_loading[["SIZC"]])
)

rica_cont <- prep_cont(data_list$rica20_cont)
if (!"ACHEN" %in% rica_cont$variable) {
  achen <- mock_dependence$marginals_country
  achen <- achen[achen$COUNTRY == "FRA" & achen$variable == "SE295", , drop = FALSE]
  stopifnot(nrow(achen) == 1)
  achen$variable <- "ACHEN"
  rica_cont <- dplyr::bind_rows(rica_cont, achen[c("variable", "mean", "sd", "min",
                                                   "p25", "p50", "p75", "max")])
}

rica_panel <- h_generate_mock_panel(
  n = n_farms, years = mock_years,
  continuous_stats  = rica_cont,
  categorical_stats = prep_cat(data_list$rica20_cat),
  dependence = dep_rica, id_col = "IDENT", year_col = "YEAR", seed = 42
)
farm_latent <- attr(rica_panel, "latent")
attr(rica_panel, "latent") <- NULL

# Farmers get one year older each year they stay in the sample
rica_panel <- rica_panel[order(rica_panel$IDENT, rica_panel$YEAR), ]
first_year <- ave(rica_panel$YEAR, rica_panel$IDENT, FUN = min)
first_age <- ave(rica_panel$AGECH, rica_panel$IDENT, FUN = function(x) x[1])
rica_panel$AGECH <- pmin(first_age + rica_panel$YEAR - first_year,
                         max(as.numeric(data_list$rica20_cont$Max[data_list$rica20_cont$Variable == "AGECH"])))

panel_tabs <- c(
  list(rica = rica_panel),
  lapply(seq_along(tables)[-1], function(i) {
    tb <- tables[[i]]
    h_generate_mock_child_panel(
      farm_latent,
      rows_per_parent = table_size(data_list[[paste0(tb, "_cat")]]) / size_rica,
      continuous_stats  = prep_cont(data_list[[paste0(tb, "_cont")]]),
      categorical_stats = prep_cat(data_list[[paste0(tb, "_cat")]]),
      dependence = dep_rica[c("size_loading", "family_cor", "autocorr", "cat_persistence")],
      parent_id_col = "IDENT", year_col = "YEAR", seed = 42 + i
    )
  }) |> stats::setNames(names(tables)[-1])
)

split_years <- function(tabs) {
  out <- lapply(mock_years, function(y) {
    lapply(tabs, function(df) {
      df <- df[df$YEAR == y, , drop = FALSE]
      df[, c("IDENT", setdiff(names(df), c("IDENT", "YEAR"))), drop = FALSE]
    })
  })
  stats::setNames(out, mock_years)
}

# Use the A2 conversion + infer_practices() as the source of truth for
# eligibility, then keep whole farms to reach the target pass rate.
data("dict_FADN", package = "FADN2Footprint")
dict_conv <- h_read_dictionary("vignettes/dictionary_conversion_filled_FRA.xlsx")
converted <- h_pivot_national_fadn(
  raw_tables = h_stack_yearly_tables(split_years(panel_tabs), year_col = "YEAR"),
  dict_conversion = dict_conv,
  id_cols = c("IDENT", "YEAR"),
  table_config = list(
    CROPS = list(table = "veg", code_col = "CODE3"),
    LIVESTOCK = list(table = "ani", code_col = "CODE6"),
    `ANIMAL PRODUCTS AND SERVICES` = list(table = "pan", code_col = "CODE7")
  )
)
converted$COUNTRY <- "FRA"
ready <- data_4FADN2Footprint(converted, var_dict = dict_FADN,
                              id_cols = c("ID", "YEAR", "COUNTRY")) |>
  infer_practices(overwrite = TRUE)

keep_fr <- select_panel_farms(converted[c("ID", "YEAR")], ready@farm[c("ID", "YEAR")],
                              target_pass_rate, seed = 42)
panel_tabs <- lapply(panel_tabs, function(df) df[df$IDENT %in% keep_fr, , drop = FALSE])
rica_fict_years <- split_years(panel_tabs)
rica_fict <- rica_fict_years[[as.character(max(mock_years))]]

message(
  length(keep_fr), " French mock farms (", nrow(panel_tabs$rica), " farm-years) retained; ",
  "retention ", round(panel_retention(panel_tabs$rica$IDENT, panel_tabs$rica$YEAR), 3),
  " (target ", round(dep_rica$retention, 3), ")."
)



# EU FADN mock data ----

## Upload desc stats ----
library(readxl)
file_path = "inst/ext_data/EU_FADN_2016_2018_desc_stats.xlsx"

sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(s) suppressMessages(read_excel(file_path, sheet = s)))
names(data_list) <- sheets

# Variables required by .parse_input_data() but missing from the sheet (e.g.
# SE295) come from the aggregated marginals exported with the dependence
# statistics (inst/scripts/mock_data_dependence_stats.R).
extra_marg <- mock_dependence$marginals
extra_marg <- extra_marg[!extra_marg$variable %in% data_list$continuous_vars$variable, , drop = FALSE]
extra_marg$TF8 <- methods::as(extra_marg$TF8, class(data_list$continuous_vars$TF8))
extra_marg <- dplyr::semi_join(extra_marg, data_list$continuous_vars, by = c("TF8", "COUNTRY"))
data_list$continuous_vars <- dplyr::bind_rows(data_list$continuous_vars, extra_marg)

# Zero mass: share of non-zero values per TF8 x COUNTRY (the sheet has no
# quartiles, so a plain gamma would never give exact zeros)
nz <- mock_dependence$nonzero_share
nz$TF8 <- methods::as(nz$TF8, class(data_list$continuous_vars$TF8))
data_list$continuous_vars <- dplyr::left_join(data_list$continuous_vars, nz,
                                              by = c("TF8", "COUNTRY", "variable"))

# Farms per year before the infer_practices() selection (about a third of
# the generated farm-years pass, see below).
n_per_year <- 1000L

# The EU sheets only report mean/sd/min/max (no quartiles), so distributions
# are moment-matched (gamma for non-negative variables) within each
# TF8 x COUNTRY group. Groups are weighted by their number of real farms.
group_cols <- c("TF8", "COUNTRY")
group_weights <- data_list$categorical_vars |>
  dplyr::group_by(variable, TF8, COUNTRY) |>
  dplyr::summarise(n = sum(count), .groups = "drop") |>
  dplyr::group_by(TF8, COUNTRY) |>
  dplyr::summarise(weight = max(n), .groups = "drop") |>
  as.data.frame()

# Country-specific dependence: copula, size loadings, persistence and
# retention rate of the national FADN sample
eu_countries <- unique(data_list$continuous_vars$COUNTRY)
eu_dependence <- stats::setNames(lapply(eu_countries, build_dependence), eu_countries)

## Generate mock data ----
fadn_fict <- h_generate_mock_panel(
  n = n_per_year,
  years = mock_years,
  continuous_stats = data_list$continuous_vars,
  categorical_stats = data_list$categorical_vars,
  group_cols    = group_cols,
  group_weights = group_weights,
  dependence    = eu_dependence,
  dependence_by = "COUNTRY",
  cat_lookup    = mock_dependence$cat_lookup,
  seed = 42
)
attr(fadn_fict, "latent") <- NULL

# Independent sampling of every crop variable can create crop records with
# production but no area, as well as crop types without practice references.
# Keep the major cereal and forage types used by the validated workflow.
practice_crop_codes <- c(
  "CWHTC", "CWHTD", "CBRL", "COAT", "CMZ", "CRYE", "CRICE",
  "CGRSXRG", "CRG"
)
crop_codes <- unique(data_extra$crops$FADN_code_letter)

for (code in crop_codes) {
  crop_cols <- grep(paste0("^", code, "_"), names(fadn_fict), value = TRUE)
  if (length(crop_cols) == 0) {
    next
  }

  if (!code %in% practice_crop_codes) {
    for (col in crop_cols) {
      if (is.numeric(fadn_fict[[col]])) {
        fadn_fict[[col]][] <- 0
      }
    }
    next
  }

  area_cols <- intersect(paste0(code, c("_TA", "_A")), names(fadn_fict))
  if (length(area_cols) == 0) {
    for (col in crop_cols) {
      if (is.numeric(fadn_fict[[col]])) {
        fadn_fict[[col]][] <- 0
      }
    }
    next
  }

  area_ha <- Reduce(pmax, c(fadn_fict[area_cols], list(na.rm = TRUE)))
  no_area <- is.na(area_ha) | area_ha <= 0
  for (col in crop_cols) {
    if (is.numeric(fadn_fict[[col]])) {
      fadn_fict[[col]][no_area] <- 0
    }
  }
}

# Certification and mineral-fertilizer inputs are otherwise sampled
# independently, which creates impossible organic-farm combinations.
organic_farms <- as.character(fadn_fict$ORGANIC) %in% c("2", "4")
fadn_fict$INUSE_Q[organic_farms] <- 0

# Use infer_practices() as the source of truth for mock eligibility. Keep
# whole farms (panels stay complete) so that ~10% of farm-years fail.
candidate_n <- nrow(fadn_fict)
practice_ready <- data_4FADN2Footprint(fadn_fict) |>
  infer_practices(overwrite = TRUE)
keep_eu <- select_panel_farms(fadn_fict[c("ID", "YEAR")],
                              practice_ready@farm[c("ID", "YEAR")],
                              target_pass_rate, seed = 42)
valid_rows <- paste(practice_ready@farm$ID, practice_ready@farm$YEAR)

fadn_fict <- fadn_fict[fadn_fict$ID %in% keep_eu, , drop = FALSE]
rownames(fadn_fict) <- NULL

if (nrow(fadn_fict) == 0) {
  stop("No EU mock farms were retained; refusing to save empty mock data.")
}

pass_rate <- mean(paste(fadn_fict$ID, fadn_fict$YEAR) %in% valid_rows)
message(
  nrow(fadn_fict), " of ", candidate_n, " generated EU mock farm-years retained (",
  length(keep_eu), " farms); ", round(pass_rate * 100, 1), "% pass infer_practices()."
)
retention_check <- do.call(rbind, lapply(split(fadn_fict, fadn_fict$COUNTRY), function(d) {
  cty <- d$COUNTRY[1]
  data.frame(COUNTRY = cty, mock = panel_retention(d$ID, d$YEAR),
             target = eu_dependence[[cty]]$retention)
}))
print(retention_check)

# export data ----

mock_data <- list(
  'rica_fict' = rica_fict,
  'fadn_fict' = fadn_fict
)

usethis::use_data(mock_data, overwrite = T)

## Excel files ----
# EU set: one file per country and year, all in one folder
eu_dir <- "inst/ext_data/mock_FADN_EU"
unlink(eu_dir, recursive = TRUE)
dir.create(eu_dir, recursive = TRUE)
for (cty in unique(fadn_fict$COUNTRY)) {
  for (yr in unique(fadn_fict$YEAR[fadn_fict$COUNTRY == cty])) {
    openxlsx::write.xlsx(
      fadn_fict[fadn_fict$COUNTRY == cty & fadn_fict$YEAR == yr, , drop = FALSE],
      file.path(eu_dir, paste0("FADN_", cty, "_", yr, ".xlsx"))
    )
  }
}

# French set: one subfolder per year, one file per table
fr_dir <- "inst/ext_data/mock_FADN_FRA"
unlink(fr_dir, recursive = TRUE)
for (yr in names(rica_fict_years)) {
  dir.create(file.path(fr_dir, yr), recursive = TRUE)
  for (tb in names(rica_fict_years[[yr]])) {
    openxlsx::write.xlsx(rica_fict_years[[yr]][[tb]],
                         file.path(fr_dir, yr, paste0(tb, ".xlsx")))
  }
}
