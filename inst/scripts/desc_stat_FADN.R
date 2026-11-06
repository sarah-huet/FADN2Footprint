# Script for generating descriptive statistics for FADN data

library(dplyr)
library(readxl)

# Path to data
wd_path <- "C:/users/srhuet/OneDrive/Research/GitHub/FADN2Footprint"
file_path <- file.path(wd_path, "data_raw/raw_FADN_EU_04_21.RData")

# Load the data
load(file_path)

# Filter FADN data needed in F2F
dict_path <- file.path(wd_path, "inst/ext_data/dict_FADN_2026.xlsx")
dict_FADN <- readxl::read_excel(
    dict_path,
    sheet = "dict_FADN"
)

# Filter for variables needed in F2F
dict_FADN_needed <- dict_FADN |>
    dplyr::filter(needed_in_FADN2Footprint == "y")

# Retrieve continous variable names
cont_vars <- dict_FADN_needed |>
    dplyr::filter(type == "continuous") |>
    pull(var_common)

# Retrieve categorical variable names
cat_vars <- dict_FADN_needed |>
    dplyr::filter(type == "categorical") |>
    # remove ID
    dplyr::filter(var_common != "ID") |>
    pull(var_common)

# Select only the columns from the FADN data that are needed in F2F
FADN_needed <- my_FADN_data |>
    dplyr::select(ID, dplyr::matches(dict_FADN_needed$var_common))

# the categorical variables give the grouping
# for each group, Estimate number of farms, mean, sd, and quantiles for continuous variables
desc_stats <- FADN_needed |>
    dplyr::summarise(
        n_farms = dplyr::n(),
        dplyr::across(
            dplyr::matches(cont_vars),
            list(
                mean = ~ mean(.x, na.rm = TRUE),
                sd = ~ sd(.x, na.rm = TRUE),
                q0 = ~ quantile(.x, 0.0, na.rm = TRUE),
                q10 = ~ quantile(.x, 0.10, na.rm = TRUE),
                q25 = ~ quantile(.x, 0.25, na.rm = TRUE),
                q50 = ~ quantile(.x, 0.50, na.rm = TRUE),
                q75 = ~ quantile(.x, 0.75, na.rm = TRUE),
                q90 = ~ quantile(.x, 0.90, na.rm = TRUE),
                q100 = ~ quantile(.x, 1.0, na.rm = TRUE)
            ),
            .names = "{col}_{fn}"
        ),
        .by = COUNTRY
    )

# save
save_path <- file.path(wd_path, "inst/ext_data/desc_stats_FADN_EU_04_21.RData")
save(desc_stats, file = save_path)
