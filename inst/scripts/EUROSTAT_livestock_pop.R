# Build the Eurostat livestock-population reference used when CRT populations
# are unavailable for coverage rescaling. Run from the package root.

required <- c(
  "jsonlite",
  "dplyr",
  "tidyr",
  "tibble",
  "usethis"
)
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing)) {
  stop("Install required packages: ", paste(missing, collapse = ", "))
}

if (!file.exists("DESCRIPTION")) {
  stop("Run this script from the package root.")
}

source(file.path("inst", "scripts", "EUROSTAT_jsonstat_helpers.R"))
data_environment <- new.env(parent = emptyenv())
load(file.path("data", "data_extra.rda"), envir = data_environment)
if (!exists("data_extra", envir = data_environment, inherits = FALSE)) {
  stop("The package country-code reference data are missing.")
}

dataset_codes <- c(
  bovine = "apro_mt_lscatl",
  pigs = "apro_mt_lspig",
  sheep = "apro_mt_lssheep"
)

download_jsonstat <- function(dataset_code) {
  url <- paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/",
    dataset_code
  )

  raw_text <- if (requireNamespace("httr2", quietly = TRUE)) {
    httr2::request(url) |>
      httr2::req_perform() |>
      httr2::resp_body_string()
  } else {
    paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }
  parsed <- jsonlite::fromJSON(raw_text, simplifyVector = FALSE)
  if (!identical(parsed$class, "dataset")) {
    stop("Eurostat returned an invalid response for ", dataset_code, ".")
  }
  parsed
}

# Keep downloads in memory; do not read from or write to the restricted data_raw folder.
eurostat_json <- lapply(dataset_codes, download_jsonstat)
eurostat_tables <- lapply(eurostat_json, parse_jsonstat)

EUROSTAT_livestock_pop <- build_eurostat_livestock_pop(
  bovine = eurostat_tables$bovine,
  pigs = eurostat_tables$pigs,
  sheep = eurostat_tables$sheep,
  country_names = data_environment$data_extra$country_names
)

usethis::use_data(EUROSTAT_livestock_pop, overwrite = TRUE, compress = "xz")

print(dplyr::count(EUROSTAT_livestock_pop, livcat))
