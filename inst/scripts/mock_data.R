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

rica_fict <- list(

  rica = h_generate_mock_data(
    n = 500,
    seed = 42,
    continuous_stats = data_list$rica20_cont |>
      dplyr::mutate(
        variable = Variable,
        mean = as.numeric(Mean),
        sd = as.numeric(SD),
        min = as.numeric(Min),
        max = as.numeric(Max)
      ),
    categorical_stats = data_list$rica20_cat |>
      dplyr::filter(!is.na(N)) |>
      dplyr::mutate(variable = Variable,
                    level = Category,
                    count = as.numeric(N))
  ),

  veg  = h_generate_mock_data(
    n = 500,
    seed = 42,
    continuous_stats = data_list$veg20_cont |>
      dplyr::mutate(
        variable = Variable,
        mean = as.numeric(Mean),
        sd = as.numeric(SD),
        min = as.numeric(Min),
        max = as.numeric(Max)
      ),
    categorical_stats = data_list$veg20_cat |>
      dplyr::filter(!is.na(N)) |>
      dplyr::mutate(variable = Variable,
                    level = Category,
                    count = as.numeric(N))
  ),

  ani  = h_generate_mock_data(
    n = 500,
    seed = 42,
    continuous_stats = data_list$ani20_cont |>
      dplyr::mutate(
        variable = Variable,
        mean = as.numeric(Mean),
        sd = as.numeric(SD),
        min = as.numeric(Min),
        max = as.numeric(Max)
      ),
    categorical_stats = data_list$ani20_cat |>
      dplyr::filter(!is.na(N)) |>
      dplyr::mutate(variable = Variable,
                    level = Category,
                    count = as.numeric(N))
  ),

  pan  = h_generate_mock_data(
    n = 500,
    seed = 42,
    continuous_stats = data_list$pan20_cont |>
      dplyr::mutate(
        variable = Variable,
        mean = as.numeric(Mean),
        sd = as.numeric(SD),
        min = as.numeric(Min),
        max = as.numeric(Max)
      ),
    categorical_stats = data_list$pan20_cat |>
      dplyr::filter(!is.na(N)) |>
      dplyr::mutate(variable = Variable,
                    level = Category,
                    count = as.numeric(N))
  )
)

# add common identifier


set.seed(42)

# Infer n from first table if not provided
n <- nrow(rica_fict[[1]])

# Create a pool of unique farm IDs
id_pool <- paste0("FARM_", formatC(seq_len(n), width = nchar(n) + 1, flag = "0"))

rica_fict <- lapply(rica_fict, function(df) {
  nrows <- nrow(df)

  if (nrows == n) {
    # One row per farm (e.g., rica table) → assign all IDs
    df[["IDENT"]] <- id_pool
  } else if (nrows > n) {
    # Multiple rows per farm (e.g., veg, ani, pan) → sample with replacement
    df[["IDENT"]] <- sample(id_pool, size = nrows, replace = TRUE)
  } else {
    # Fewer rows than farms → sample without replacement
    df[["IDENT"]] <- sample(id_pool, size = nrows, replace = FALSE)
  }

  # Move ID to first column
  df <- df[, c("IDENT", setdiff(names(df), "IDENT"))]

})



# EU FADN mock data ----

## Upload desc stats ----
library(readxl)
file_path = "inst/ext_data/EU_FADN_2016_2018_desc_stats.xlsx"

sheets <- excel_sheets(file_path)
data_list <- lapply(sheets, function(s) read_excel(file_path, sheet = s))
names(data_list) <- sheets

# choose a sample size
n <- 5000

## Generate mock data ----
fadn_fict <- h_generate_mock_data(
  n = n,
  seed = 42,
  continuous_stats = data_list$continuous_vars,
  categorical_stats = data_list$categorical_vars,
  group_cols    = c("TF8", "COUNTRY")
) |>
  # add identifier
  dplyr::mutate(
    ID = paste0("FARM_", formatC(seq_len(n), width = nchar(n) + 1, flag = "0")),
    YEAR = 2020)

# export data ----

mock_data <- list(
  'rica_fict' = rica_fict,
  'fadn_fict' = fadn_fict
)

usethis::use_data(mock_data, overwrite = T)

