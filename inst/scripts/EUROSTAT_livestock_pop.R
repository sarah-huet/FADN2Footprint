#https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/apro_mt_lscatl?format=JSON&sinceTimePeriod=2000&geo=EU&geo=EU27_2020&geo=EU28&geo=EU27_2007&geo=EU25&geo=EU15&geo=BE&geo=BG&geo=CZ&geo=DK&geo=DE&geo=EE&geo=IE&geo=EL&geo=ES&geo=FR&geo=HR&geo=IT&geo=CY&geo=LV&geo=LT&geo=LU&geo=HU&geo=MT&geo=NL&geo=AT&geo=PL&geo=PT&geo=RO&geo=SI&geo=SK&geo=FI&geo=SE&geo=IS&geo=CH&geo=UK&geo=BA&geo=ME&geo=MK&geo=AL&geo=RS&geo=TR&geo=UA&geo=XK&unit=THS_HD&month=M05_M06&month=M11_M12&animals=A2000&animals=A2010&animals=A2010B&animals=A2010C&animals=A2020&animals=A2030&animals=A2110C&animals=A2120&animals=A2130&animals=A2210C&animals=A2220&animals=A2220B&animals=A2220C&animals=A2230&animals=A2230_2330&animals=A2230B&animals=A2230C&animals=A2300&animals=A2300F&animals=A2300G&animals=A2400&animals=A2410&animals=A2420&lang=EN

# Build internal FADN2Footprint sales coefficients from Eurostat slaughterings.
# Run from the package root (this file lives in inst/scripts/).

# 1. Packages --------------------------------------------------------------
devtools::load_all()
required <- c("jsonlite", "dplyr", "tidyr", "purrr", "tibble", "usethis", "FADN2Footprint")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(purrr)
  library(tibble); library(usethis); library(FADN2Footprint)
})

# 2. Download + generic JSON-stat 2.0 parser -------------------------------

url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/apro_mt_lscatl?format=JSON&sinceTimePeriod=2000&geo=EU&geo=EU27_2020&geo=EU28&geo=EU27_2007&geo=EU25&geo=EU15&geo=BE&geo=BG&geo=CZ&geo=DK&geo=DE&geo=EE&geo=IE&geo=EL&geo=ES&geo=FR&geo=HR&geo=IT&geo=CY&geo=LV&geo=LT&geo=LU&geo=HU&geo=MT&geo=NL&geo=AT&geo=PL&geo=PT&geo=RO&geo=SI&geo=SK&geo=FI&geo=SE&geo=IS&geo=CH&geo=UK&geo=BA&geo=ME&geo=MK&geo=AL&geo=RS&geo=TR&geo=UA&geo=XK&unit=THS_HD&month=M05_M06&month=M11_M12&animals=A2000&animals=A2010&animals=A2010B&animals=A2010C&animals=A2020&animals=A2030&animals=A2110C&animals=A2120&animals=A2130&animals=A2210C&animals=A2220&animals=A2220B&animals=A2220C&animals=A2230&animals=A2230_2330&animals=A2230B&animals=A2230C&animals=A2300&animals=A2300F&animals=A2300G&animals=A2400&animals=A2410&animals=A2420&lang=EN"
# The API expects each vector member as a repeated parameter; verify the URL.
raw_dir <- file.path("data_raw")
#dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
raw_path <- file.path(raw_dir, "apro_mt_lscatl.json")
if (requireNamespace("httr2", quietly = TRUE)) {
  raw_text <- httr2::request(url) |> httr2::req_perform() |> httr2::resp_body_string()
} else {
  raw_text <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
writeLines(raw_text, raw_path, useBytes = TRUE)
js <- jsonlite::fromJSON(raw_text, simplifyVector = FALSE)

ids <- unlist(js$id, use.names = FALSE)

# Category codes ordered by their 0-based index
cats <- lapply(ids, function(d) {
  idx <- unlist(js$dimension[[d]]$category$index, use.names = TRUE)
  names(idx)[order(as.numeric(idx))]
})
names(cats) <- ids

# expand.grid varies its FIRST column fastest -> give it the dims reversed
grid <- do.call(expand.grid,
                c(rev(cats), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
grid <- grid[, ids, drop = FALSE]          # back to JSON-stat order

stopifnot(nrow(grid) == prod(unlist(js$size)))

# Sparse values: keys are 0-based positions
stopifnot(!is.null(names(js$value)))       # fails if names were dropped
grid$value <- NA_real_
grid$value[as.integer(names(js$value)) + 1L] <-
  vapply(js$value, as.numeric, numeric(1))

grid$flag <- NA_character_
if (length(js$status)) {
  grid$flag[as.integer(names(js$status)) + 1L] <-
    vapply(js$status, as.character, character(1))
}

eurostat_raw <- grid |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(heads = value * 1000, year = as.integer(time))

stopifnot(nrow(eurostat_raw) == length(js$value))   # 15,819 for your file


# 3. Eurostat geo -> ISO3 harmonisation ------------------------------------

eurostat_long <- left_join(
  eurostat_raw,
  data_extra$country_name |>
    dplyr::select(Country_ISO_3166_1_A3, country_eu),
                           by = join_by("geo" == "country_eu")) |>
                filter(!is.na(Country_ISO_3166_1_A3))

stopifnot(!anyNA(eurostat_long$Country_ISO_3166_1_A3))

# 4. Load FADN and adapt long/wide livestock sales -------------------------
load("../FADN2Footprint/data_raw/FADN_16_18.RData")
fadn_data = FADN_16_18 |>
    select(ID, COUNTRY, YEAR, TF14, SYS02, dplyr::matches("SN$|SSN$|SRN$")) |>
    dplyr::left_join(data_extra$country_name |> dplyr::select(Country_ISO_3166_1_A3, country_FADN),
             by = join_by("COUNTRY" == "country_FADN"))

# ADAPTER BLOCK: accepts long FADN_code_letter/SN columns or wide <code>_SN,
# <code>_SSN, <code>_SRN columns. Adjust only this block for a local schema.
sn_cols <- grep("_SN$", names(fadn_data), value = TRUE)
codes <- sub("_SN$", "", sn_cols)

fadn_long <- fadn_data |>
  pivot_longer(
    dplyr::matches("SN$|SSN$|SRN$"),
    names_to      = c("FADN_code_letter", ".value"),
    names_pattern = "(.*)_(SN|SSN|SRN)$"
  )

# 5. Non-overlapping mapping (verify labels below against API metadata). ----
# B3100 = Pig; B4100 = Sheep; B4200 = Goat. Broad *_ patterns
# are intentionally explicit here and should be reviewed if FADN codes change.
animals_labels <- tibble::tibble(
  animals  = names(js$dimension$animals$category$label),
  label = unlist(js$dimension$animals$category$label, use.names = FALSE)
)
map_tbl <- tribble(
  ~FADN_code_letter, ~eurostat_animals,
  "LBOV1", "A2010",
  "LBOV1_2F", "A2220", "LBOV1_2M", "A2120",
  "LBOV2", "A2130",
  "LCOWDAIR", "A2300F", "LCOWOTH", "A2300G", "LBUFDAIRPRS", "A2400",
  "LHEIFBRE", "A2220C", "LHEIFFAT", "A2220B",
  "LHEIFBRE", "A2230C", "LHEIFFAT", "A2230B"#,
  #"LPIGLET", "B3100", "LPIGFAT", "B3100", "LSOWBRE", "B3100", "LPIGOTH", "B3100",
  #"LEWEBRE", "B4100", "LSHEPOTH", "B4100",
  #"LGOATBRE", "B4200", "LGOATOTH", "B4200",
  #"LEQD", "B5000",
  #"LPLTRBROYL", "B7000", "LPLTROTH", "B7000", "LHENSLAY", "B7000",
  #"LRABBIT", "B8000", "LRABBRE", "B8000", "LRABOTH", "B8000"
)
# Unmatched codes are excluded rather than silently assigned to a animals category.
# In particular, the B5000 horse/ass/mule category is not mapped without a
# confirmed FADN livestock code; inspect names before adding one.
fadn_long <- left_join(fadn_long, map_tbl, by = "FADN_code_letter")

