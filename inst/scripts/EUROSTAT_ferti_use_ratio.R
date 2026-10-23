# =============================================================================
# FADN2Footprint - Download nitrogen fertilizer prices
# Sources: Eurostat, FAOSTAT, World Bank (Pink Sheet)
# =============================================================================

# ---- 0. Packages -------------------------------------------------------------
pkgs <- c(
    "eurostat",
    #"FAOSTAT",
    "readxl",
    "dplyr",
    "tidyr",
    "stringr",
    "readr",
    "httr",
    "janitor"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
    install.packages(to_install)
}
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- file.path("data", "raw", "fertilizer_prices")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. EUROSTAT
#    apri_ap_crpouta : Absolute prices of agricultural products/inputs
#    apri_pi20_outa  : Price indices of agricultural inputs (2020 = 100)

#    apri_pi_ina : Price indices of the means of agricultural production, input - annual data
#    apri_aip_ft : Purchase prices for fertilisers (expressed in nutritive content)
# =============================================================================

# 1a. Absolute prices of inputs (fertilisers)
#abs_raw <- eurostat::get_eurostat(
#    id = "apri_aip_ft",
#    time_format = "num",
#    cache = FALSE
#)
#
## Inspect product codes to select N fertilisers
## (labels allow filtering on text rather than relying on fragile codes)
#abs_lab <- eurostat::label_eurostat(abs_raw, fix_duplicated = TRUE)
##abs_lab <- janitor::clean_names(abs_lab)
#
#n_abs <- abs_lab |>
#    dplyr::filter(
#        am_item == "Nitrogenous fertilisers",
#        p_adj == "Real index"
#    )
#
#readr::write_csv(
#    n_abs,
#    file.path(out_dir, "eurostat_n_fertilizer_prices_abs.csv")
#)

# 1b. Price indices of inputs (nitrogen fertilisers)
idx_raw <- eurostat::get_eurostat(
    "apri_pi_ina",
    time_format = "num",
    cache = FALSE
)

idx_lab <- eurostat::label_eurostat(idx_raw, fix_duplicated = TRUE) |>
    janitor::clean_names()

# am_item == "Nitrogenous fertilisers"
# Real index
# Nominal index: "The Harmonised Consumer Price Index (HICP) is used to deflate the nominal price indices."
## https://ec.europa.eu/eurostat/cache/metadata/en/apri_pi_esms.htm#stat_processDisseminated
## as FADN record on fertiliser expenditure are not deflated, we will use the real index

n_idx <- idx_lab |>
    dplyr::filter(
        am_item == "Nitrogenous fertilisers",
        p_adj == "Real index",
        grepl("Index,", unit)
    ) |>
    # add country code mapping
    dplyr::left_join(
        data_extra$country_names |>
            dplyr::select(country_name, country_FADN) |>
            tidyr::separate_longer_delim(country_name, ";"),
        by = c("geo" = "country_name")
    )

#n_idx |> tibble::view()

# "Index, 2000=100"
# "Index, 2005=100"
# "Index, 2010=100"
# "Index, 2015=100"
# "Index, 2020=100"

# We then estimate the ratio of nitrogen fertiliser use (use on expenditure) for 2020
# based on our test FADN dataset (EU_17_21)
load(
    "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_17_21.RData"
)
devtools::load_all(getwd())
# INUSE_Q		H_SC_3031_Q	Quantity of N used in mineral fertilisers Quantity
# SE295	SE295	SE295	Purchased fertilisers and soil improvers (excluding those used for forests).
# SE296		SE296	Fertiliser. Quantity of N in mineral fertilisers used

#my_FADN_data_sub |>
#    dplyr::filter(YEAR == 2020) |>
#    ggplot2::ggplot(ggplot2::aes(x = INUSE_Q, y = SE295)) +
#    ggplot2::geom_point() +
#    ggplot2::facet_wrap(~COUNTRY)

# to estimate the base ratio, we calculate the five year average around 2020
n_ratio_2020 <- my_FADN_data_sub |>
    dplyr::filter(YEAR %in% 2018:2022) |>
    dplyr::filter(SE295 > 0) |>
    dplyr::mutate(
        n_ratio_2020 = dplyr::coalesce(dplyr::coalesce(INUSE_Q, 0) / SE295, NA)
    ) |>
    # we estimate this ratio for each country
    dplyr::summarise(
        n_ratio_2020 = mean(n_ratio_2020, na.rm = TRUE),
        .by = c(COUNTRY)
    )

# We estimate other years based on the 2020 ratio
n_ratio_base_idx <- n_ratio_2020 |>
    # 2015 ratio based on 2020 ratio
    # 2015 ratio = 2020 ratio * (index 2015 for year 2015 / index 2015 for year 2020)
    # add 2015 index for year 2020
    dplyr::left_join(
        n_idx |>
            dplyr::filter(time_period == 2020, unit == "Index, 2015=100") |>
            dplyr::select(country_FADN, values),
        by = c("COUNTRY" = "country_FADN")
    ) |>
    dplyr::rename(index_2015_y2020 = values) |>
    # replace NAs with average index
    dplyr::mutate(
        index_2015_y2020 = dplyr::if_else(
            is.na(index_2015_y2020),
            mean(index_2015_y2020, na.rm = TRUE),
            index_2015_y2020
        )
    ) |>
    # estimate 2015 ratio based on 2020 ratio and index for each country
    dplyr::mutate(
        n_ratio_2015 = n_ratio_2020 *
            100 /
            index_2015_y2020
    ) |>
    # 2010 ratio based on 2015 ratio
    # 2010 ratio = 2015 ratio * (index 2010 for year 2010 / index 2010 for year 2015)
    # add 2010 index for year 2015
    dplyr::left_join(
        n_idx |>
            dplyr::filter(time_period == 2015, unit == "Index, 2010=100") |>
            dplyr::select(country_FADN, values),
        by = c("COUNTRY" = "country_FADN")
    ) |>
    dplyr::rename(index_2010_y2015 = values) |>
    # replace NAs with average index for 2010
    dplyr::mutate(
        index_2010_y2015 = dplyr::if_else(
            is.na(index_2010_y2015),
            mean(index_2010_y2015, na.rm = TRUE),
            index_2010_y2015
        )
    ) |>
    # estimate 2010 ratio based on 2015 ratio and index for each country
    dplyr::mutate(
        n_ratio_2010 = n_ratio_2015 *
            100 /
            index_2010_y2015
    ) |>
    # 2005 ratio based on 2010 ratio
    # 2005 ratio = 2010 ratio * (index 2005 for year 2005 / index 2005 for year 2010)
    # add 2005 index for year 2010
    dplyr::left_join(
        n_idx |>
            dplyr::filter(time_period == 2010, unit == "Index, 2005=100") |>
            dplyr::select(country_FADN, values),
        by = c("COUNTRY" = "country_FADN")
    ) |>
    dplyr::rename(index_2005_y2010 = values) |>
    # replace NAs with average index for 2005
    dplyr::mutate(
        index_2005_y2010 = dplyr::if_else(
            is.na(index_2005_y2010),
            mean(index_2005_y2010, na.rm = TRUE),
            index_2005_y2010
        )
    ) |>
    # estimate 2005 ratio based on 2010 ratio and index for each country
    dplyr::mutate(
        n_ratio_2005 = n_ratio_2010 *
            100 /
            index_2005_y2010
    ) |>
    # 2000 ratio based on 2005 ratio
    # 2000 ratio = 2005 ratio * (index 2000 for year 2000 / index 2000 for year 2005)
    # add 2000 index for year 2005
    dplyr::left_join(
        n_idx |>
            dplyr::filter(time_period == 2005, unit == "Index, 2000=100") |>
            dplyr::select(country_FADN, values),
        by = c("COUNTRY" = "country_FADN")
    ) |>
    dplyr::rename(index_2000_y2005 = values) |>
    # replace NAs with average index for 2000
    dplyr::mutate(
        index_2000_y2005 = dplyr::if_else(
            is.na(index_2000_y2005),
            mean(index_2000_y2005, na.rm = TRUE),
            index_2000_y2005
        )
    ) |>
    # estimate 2000 ratio based on 2005 ratio and index for each country
    dplyr::mutate(
        n_ratio_2000 = n_ratio_2005 *
            100 /
            index_2000_y2005
    )

# estimate all years based on available indices and ratios
n_ratio_all_years <- n_idx |>
    # add base ratios
    dplyr::left_join(
        n_ratio_base_idx |>
            dplyr::select(COUNTRY, dplyr::starts_with("n_ratio_")),
        by = c("country_FADN" = "COUNTRY")
    ) |>
    # infer ratios using the index for each country
    # "Index, 2000=100"
    # "Index, 2005=100"
    # "Index, 2010=100"
    # "Index, 2015=100"
    # "Index, 2020=100"
    dplyr::mutate(
        n_ratio = dplyr::case_when(
            unit == "Index, 2020=100" ~ n_ratio_2020 *
                values /
                100,
            unit == "Index, 2015=100" ~ n_ratio_2015 *
                values /
                100,
            unit == "Index, 2010=100" ~ n_ratio_2010 *
                values /
                100,
            unit == "Index, 2005=100" ~ n_ratio_2005 *
                values /
                100,
            unit == "Index, 2000=100" ~ n_ratio_2000 *
                values /
                100
        )
    )

EUROSTAT_ferti_use_ratio <- n_ratio_all_years |>
    dplyr::summarise(
        n_ratio = mean(n_ratio, na.rm = TRUE),
        .by = c(country_FADN, time_period)
    )


usethis::use_data(EUROSTAT_ferti_use_ratio, overwrite = TRUE)

# =============================================================================
# 2. FAOSTAT
#    Domain "RFB" = Fertilizers by Product (includes trade value & quantity)
#    Unit price proxy = import value (1000 USD) / import quantity (tonnes)
# =============================================================================

## Download bulk file for the domain (more robust than API for big pulls)
#fao_raw <- FAOSTAT::get_faostat_bulk(code = "RFB", data_folder = out_dir)
#fao <- janitor::clean_names(fao_raw)
#
## N fertiliser products (urea, AN, CAN, AS, etc.)
#n_items <- fao |>
#    dplyr::filter(
#        stringr::str_detect(
#            stringr::str_to_lower(item),
#            "urea|ammonium nitrate|ammonium sulphate|calcium ammonium|nitrogen"
#        ),
#        stringr::str_detect(
#            stringr::str_to_lower(element),
#            "import (quantity|value)"
#        )
#    )
#
## Compute unit value (USD per tonne) from import value & quantity
#unit_values <- n_items |>
#    dplyr::select(area, area_code_m49, item, element, year, value) |>
#    dplyr::mutate(
#        element = dplyr::case_when(
#            stringr::str_detect(
#                stringr::str_to_lower(element),
#                "quantity"
#            ) ~ "qty_t",
#            stringr::str_detect(
#                stringr::str_to_lower(element),
#                "value"
#            ) ~ "value_kusd"
#        )
#    ) |>
#    tidyr::pivot_wider(names_from = element, values_from = value) |>
#    dplyr::mutate(unit_value_usd_per_t = (value_kusd * 1000) / qty_t) |>
#    dplyr::filter(is.finite(unit_value_usd_per_t))
#
#readr::write_csv(
#    unit_values,
#    file.path(out_dir, "faostat_n_fertilizer_unit_values.csv")
#)
#unit_values
#
#
## =============================================================================
## 3. WORLD BANK - Commodity Price Data (Pink Sheet), monthly
##    The URL contains a hash that changes at each release; the script
##    tries the current known URL and reports clearly on failure.
##    Check: https://www.worldbank.org/en/research/commodity-markets
## =============================================================================
#
#url <- paste0(
#    "https://thedocs.worldbank.org/en/doc/74e8be41ceb20fa0da750cda2f6b9e4e-0050012026/related/CMO-Historical-Data-Annual.xlsx"
#)
#
#dest <- file.path(out_dir, "WB_CMO_Historical_Data_Annual.xlsx")
#
#resp <- tryCatch(
#    httr::GET(
#        url,
#        httr::write_disk(dest, overwrite = TRUE),
#        httr::timeout(120)
#    ),
#    error = function(e) NULL
#)
#if (is.null(resp) || httr::status_code(resp) != 200) {
#    warning(
#        "World Bank download failed. Get the current link from ",
#        "https://www.worldbank.org/en/research/commodity-markets ",
#        "and update `url`."
#    )
#    return(NULL)
#}
#
## Sheet "Annual Indices (Real)": header rows at the top, column names on row 6
#raw <- readxl::read_excel(
#    dest,
#    sheet = "Annual Indices (Real)",
#    col_names = FALSE
#)
##raw |> tibble::view()
#
## find row with the first year 1960
#start_row <- which(raw[[1]] == 1960)[1]
#
## aggregate rows above the start row to preserve any header information if needed
#header_info <- raw[1:(start_row - 1), ]
### among header info table, detect column with fertilisers
#for (col in seq_len(ncol(header_info))) {
#    if (
#        any(
#            stringr::str_detect(header_info[[col]], "Fertilizers|Fertilisers"),
#            na.rm = TRUE
#        )
#    ) {
#        ferti_col <- col
#        break
#    }
#}
#
#if (!is.na(start_row) & !is.null(ferti_col)) {
#    wb <- raw[start_row:nrow(raw), c(1, ferti_col)]
#}
#
#names(wb) <- c("YEAR", "ferti_index_real")
#
#wb <- wb |>
#    dplyr::mutate(dplyr::across(-c(YEAR), as.numeric)) |>
#    tidyr::drop_na()
#
