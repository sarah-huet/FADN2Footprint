#https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/apro_mt_pann?format=JSON&sinceTimePeriod=2000&geo=BE&geo=BG&geo=CZ&geo=DK&geo=DE&geo=EE&geo=IE&geo=EL&geo=ES&geo=FR&geo=HR&geo=IT&geo=CY&geo=LV&geo=LT&geo=LU&geo=HU&geo=MT&geo=NL&geo=AT&geo=PL&geo=PT&geo=RO&geo=SI&geo=SK&geo=FI&geo=SE&geo=IS&geo=CH&geo=UK&geo=BA&geo=ME&geo=MK&geo=AL&geo=RS&geo=TR&geo=XK&unit=THS_HD&meatitem=SLAUGHT&meatitem=SLAUGHT_OTH&meat=B1000&meat=B1100&meat=B1110&meat=B1120&meat=B1200&meat=B1210&meat=B1210_1220&meat=B1220&meat=B1230&meat=B1240&meat=B3100&meat=B4000&meat=B4100&meat=B4110&meat=B4120&meat=B4190&meat=B4200&meat=B5000&meat=B7000&meat=B7100&meat=B7110&meat=B7120&meat=B7200&meat=B7300&meat=B7410&meat=B7420&meat=B7430&meat=B7490&meat=B8000&lang=EN

# Build internal FADN2Footprint sales coefficients from Eurostat slaughterings.
# Run from the package root (this file lives in inst/scripts/).

# 1. Packages --------------------------------------------------------------
required <- c("jsonlite", "dplyr", "tidyr", "purrr", "tibble", "usethis", "FADN2Footprint")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages: ", paste(missing, collapse = ", "))
suppressPackageStartupMessages({
  library(jsonlite); library(dplyr); library(tidyr); library(purrr)
  library(tibble); library(usethis); library(FADN2Footprint)
})

# 2. Download + generic JSON-stat 2.0 parser -------------------------------

url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/apro_mt_pann?format=JSON&sinceTimePeriod=2000&geo=BE&geo=BG&geo=CZ&geo=DK&geo=DE&geo=EE&geo=IE&geo=EL&geo=ES&geo=FR&geo=HR&geo=IT&geo=CY&geo=LV&geo=LT&geo=LU&geo=HU&geo=MT&geo=NL&geo=AT&geo=PL&geo=PT&geo=RO&geo=SI&geo=SK&geo=FI&geo=SE&geo=IS&geo=CH&geo=UK&geo=BA&geo=ME&geo=MK&geo=AL&geo=RS&geo=TR&geo=XK&unit=THS_HD&meatitem=SLAUGHT&meatitem=SLAUGHT_OTH&meat=B1000&meat=B1100&meat=B1110&meat=B1120&meat=B1200&meat=B1210&meat=B1210_1220&meat=B1220&meat=B1230&meat=B1240&meat=B3100&meat=B4000&meat=B4100&meat=B4110&meat=B4120&meat=B4190&meat=B4200&meat=B5000&meat=B7000&meat=B7100&meat=B7110&meat=B7120&meat=B7200&meat=B7300&meat=B7410&meat=B7420&meat=B7430&meat=B7490&meat=B8000&lang=EN"             
# The API expects each vector member as a repeated parameter; verify the URL.
raw_dir <- file.path("data_raw")
#dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
raw_path <- file.path(raw_dir, "apro_mt_pann.json")
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

eurostat_long <- left_join(eurostat_raw, data_extra$country_name |> dplyr::select(Country_ISO_3166_1_A3, country_eu),
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
# B3100 = Pigmeat; B4100 = Sheepmeat; B4200 = Goat meat. Broad *_ patterns
# are intentionally explicit here and should be reviewed if FADN codes change.
meat_labels <- tibble::tibble(
  meat  = names(js$dimension$meat$category$label),
  label = unlist(js$dimension$meat$category$label, use.names = FALSE)
)
map_tbl <- tribble(
  ~FADN_code_letter, ~eurostat_meat,
  "LBOV1", "B1100",
  "LBOV1_2F", "B1120", "LBOV1_2M", "B1120",
  "LBOV2", "B1220",
  "LCOWDAIR", "B1230", "LCOWOTH", "B1230",
  "LHEIFBRE", "B1240", "LHEIFFAT", "B1240",
  "LPIGLET", "B3100", "LPIGFAT", "B3100", "LSOWBRE", "B3100", "LPIGOTH", "B3100",
  "LEWEBRE", "B4100", "LSHEPOTH", "B4100",
  "LGOATBRE", "B4200", "LGOATOTH", "B4200",
  "LPLTRBROYL", "B7000", "LPLTROTH", "B7000", "LHENSLAY", "B7000",
  "LRABBIT", "B8000", "LRABOTH", "B8000"
)
# Unmatched codes are excluded rather than silently assigned to a meat category.
# In particular, the B5000 horse/ass/mule category is not mapped without a
# confirmed FADN livestock code; inspect names before adding one.
fadn_long <- left_join(fadn_long, map_tbl, by = "FADN_code_letter")

# 6. Estimate exact-fit coefficients and documented fallbacks ----------------
## sum sales by country/year/meat to compare with Eurostat totals. This is used to
## compute the ratio of Eurostat slaughterings to FADN sales   
fadn_sum <- fadn_long |>
  filter(!is.na(eurostat_meat)) |>
  summarise(SN = sum(SN * SYS02, na.rm = TRUE),
         .by = c(Country_ISO_3166_1_A3, YEAR, TF14, eurostat_meat, FADN_code_letter)) |>
  left_join(eurostat_long |>
                select(Country_ISO_3166_1_A3, YEAR = year, eurostat_meat = meat, T = heads),
            by = c("Country_ISO_3166_1_A3", "YEAR", "eurostat_meat")) |>
    mutate(SN_sum_eurostat_meat = sum(SN, na.rm = TRUE),
           .by = c("Country_ISO_3166_1_A3", "YEAR", "TF14", "eurostat_meat")) |>
    mutate(SN_share_FADN_code_letter = SN / SN_sum_eurostat_meat) |>
    mutate(ratio = if_else(!is.na(SN) & !is.na(T), T * SN_share_FADN_code_letter / SN, NA_real_),
           ratio = pmin(ratio, 1),  # cap at 1 to avoid infeasible coefficients
           flag = case_when(is.na(T) ~ "no_target",
                            is.na(SN) | SN <= 0 ~ "no_fadn",
                            TRUE ~ "ok")) |>
    mutate(avrg_ratio = mean(ratio, na.rm = TRUE),
           .by = c("YEAR", "FADN_code_letter")) |>
    mutate(ratio = ifelse(is.na(ratio), avrg_ratio, ratio),
           share_SSN = if_else(!is.na(ratio), pmin(pmax(ratio, 0), 1), NA_real_),
           share_SRN = 1 - share_SSN)


# 7. Save ------------
EUROSTAT_slaughter <- fadn_sum 

usethis::use_data(EUROSTAT_slaughter, overwrite = T)
