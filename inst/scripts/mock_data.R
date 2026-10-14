#' Create a mock (non-confidential) FADN dataset for testing FADN2Footprint
#'
#' @description
#' This script builds a shareable *mock* (anonymised / perturbed) dataset from
#' confidential raw FADN microdata so that users can test the
#' \pkg{FADN2Footprint} workflow without accessing real farm records.
#'
#' The script:
#' \enumerate{
#'   \item Loads the confidential dataset and a variable dictionary.
#'   \item Drops direct identifiers and keeps only variables required by the package.
#'   \item Pre-processes categorical variables by collapsing rare levels into \code{"Other"}.
#'   \item Pre-processes continuous variables via 1\% / 99\% winsorisation (clipping extremes).
#'   \item Draws a stratified weighted sample (20\%) by \code{NUTS0}, \code{YEAR} and \code{TF_GEN1},
#'         excluding strata with too few farms.
#'   \item Adds random noise to continuous variables (Gaussian noise with SD = 10\% of IQR),
#'         optionally enforcing non-negativity for variables whose minimum is 0 in the raw data.
#'   \item Produces a basic validation report comparing summary statistics and selected categorical
#'         distributions between the real and mock datasets.
#'   \item Saves \code{mock_data} as an internal package dataset using \code{usethis::use_data()}.
#' }
#'
#' @details
#' ## Inputs (local / confidential)
#' \itemize{
#'   \item \code{FADN_raw_data.RData}: Must contain raw FADN microdata.
#'   \item \code{inst/scripts/dictionary_FADN.R}: Must create \code{dict_FADN} with (at least)
#'         the columns \code{var_common}, \code{type} (values \code{"categorical"} or \code{"continuous"}),
#'         and \code{needed_in_FADN2Footprint} (e.g., \code{"T"} for required variables).
#' }
#'
#' ## Outputs
#' \itemize{
#'   \item \code{mock_data}: A data.frame/tibble saved into the package data using
#'         \code{usethis::use_data(mock_data, overwrite = TRUE)}.
#'   \item \code{validation_report}: A list (created in-memory) with:
#'         \itemize{
#'           \item \code{summary_comparison}: mean and SD comparison for up to 10 continuous variables.
#'           \item \code{categorical_comparison}: proportional tables for up to 5 categorical variables.
#'         }
#' }
#'
#' @section Confidentiality / disclosure control:
#' This script is a *utility* to reduce disclosure risk, but it does **not** guarantee anonymisation.
#' In particular, users should review:
#' \itemize{
#'   \item the identifier drop list (\code{ID}, \code{PACAGE}, \code{SIRET}, \code{NUTS3});
#'   \item rare-category threshold (\eqn{\le 3} occurrences are collapsed to \code{"Other"});
#'   \item winsorisation limits (1st and 99th percentiles);
#'   \item noise magnitude (10\% of IQR);
#'   \item sampling layers and minimum stratum size (\code{n_farms/5 >= 3}).
#' }
#'
#' @section Dependencies:
#' Requires \pkg{dplyr}, \pkg{tidyr}, and \pkg{usethis}.
#'
#' @section Side effects:
#' \itemize{
#'   \item Loads a local \code{.RData} file.
#'   \item Sources \code{dictionary_FADN.R}.
#'   \item Writes/updates a package dataset via \code{usethis::use_data()}.
#' }
#'
#' @seealso
#' \code{\link[usethis]{use_data}}
#'
#' @keywords internal
#'
"mock_data"

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

n_farms <- 500
tables <- c(rica = "rica20", veg = "veg20", ani = "ani20", pan = "pan20")

# Each table keeps its real size relative to the farm table (e.g. several
# crop rows per farm in `veg`)
size_rica <- table_size(data_list$rica20_cat)

rica_fict <- lapply(tables, function(tb) {
  h_generate_mock_data(
    n = round(n_farms * table_size(data_list[[paste0(tb, "_cat")]]) / size_rica),
    seed = 42,
    continuous_stats  = prep_cont(data_list[[paste0(tb, "_cont")]]),
    categorical_stats = prep_cat(data_list[[paste0(tb, "_cat")]])
  )
})

# add common identifier
set.seed(42)

n <- nrow(rica_fict$rica)

# Create a pool of unique farm IDs
id_pool <- paste0("FARM_", formatC(seq_len(n), width = nchar(n) + 1, flag = "0"))

rica_fict <- lapply(rica_fict, function(df) {
  nrows <- nrow(df)

  if (nrows == n) {
    # One row per farm (e.g., rica table) → assign all IDs
    df[["IDENT"]] <- id_pool
  } else if (nrows > n) {
    # Multiple rows per farm (e.g., veg, ani, pan) → every farm appears at
    # least once, remaining rows sampled with replacement
    df[["IDENT"]] <- c(id_pool, sample(id_pool, size = nrows - n, replace = TRUE))
  } else {
    # Fewer rows than farms → sample without replacement
    df[["IDENT"]] <- sample(id_pool, size = nrows, replace = FALSE)
  }

  # Move ID to first column
  df[, c("IDENT", setdiff(names(df), "IDENT"))]
})



# EU FADN mock data ----

## Upload desc stats ----
library(readxl)
file_path = "inst/ext_data/EU_FADN_2016_2018_desc_stats.xlsx"

sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(s) suppressMessages(read_excel(file_path, sheet = s)))
names(data_list) <- sheets

# Generate enough candidates to retain a useful practice-ready sample after
# running the same validation used by the analysis workflow.
n <- 10000L

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

## Generate mock data ----
fadn_fict <- h_generate_mock_data(
  n = n,
  seed = 42,
  continuous_stats = data_list$continuous_vars,
  categorical_stats = data_list$categorical_vars,
  group_cols    = group_cols,
  group_weights = group_weights
) |>
  # add identifier
  dplyr::mutate(
    ID = paste0("FARM_", formatC(seq_len(n), width = nchar(n) + 1, flag = "0")),
    YEAR = 2020)

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

# Use infer_practices() as the source of truth for mock eligibility. Retain
# enough invalid farms to make approximately 10% of the final sample fail.
candidate_n <- nrow(fadn_fict)
practice_ready <- data_4FADN2Footprint(fadn_fict) |>
  infer_practices(overwrite = TRUE)
valid_ids <- practice_ready@farm |>
  dplyr::select(ID)
invalid_ids <- practice_ready@traceability$discarded_farms |>
  dplyr::select(ID) |>
  dplyr::distinct()

target_pass_rate <- 0.9
n_invalid_to_keep <- round(nrow(valid_ids) * (1 - target_pass_rate) / target_pass_rate)
if (nrow(invalid_ids) < n_invalid_to_keep) {
  stop("Not enough invalid candidate farms to produce a mock with a 90% pass rate.")
}

set.seed(42)
retained_invalid_ids <- invalid_ids |>
  dplyr::slice_sample(n = n_invalid_to_keep)
retained_ids <- dplyr::bind_rows(valid_ids, retained_invalid_ids)

fadn_fict <- fadn_fict |>
  dplyr::semi_join(retained_ids, by = "ID")

if (nrow(fadn_fict) == 0) {
  stop("No EU mock farms were retained; refusing to save empty mock data.")
}

pass_rate <- nrow(valid_ids) / nrow(fadn_fict)
message(
  nrow(fadn_fict), " of ", candidate_n, " generated EU mock farms retained; ",
  sum(fadn_fict$ID %in% valid_ids$ID), " (",
  round(pass_rate * 100, 1), "%) pass infer_practices()."
)

# export data ----

mock_data <- list(
  'rica_fict' = rica_fict,
  'fadn_fict' = fadn_fict
)

usethis::use_data(mock_data, overwrite = T)
