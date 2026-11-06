# A script to generate the S4 object and save it

# Uncomment the lines below when using a locally installed version of the package:
# devtools::install_local("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/", dependencies = TRUE, force = TRUE)
#devtools::document("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")
devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

#library(FADN2Footprint)

# =========================================
# Load FADN raw data (2016-2018) ----
# =========================================

#file_path <- "C:/Users/srhuet/OneDrive/Research/Data/FADN/FADN_LAMASUS_2024"
#file_list <- list.files(file_path)
#file_list <- file_list[grepl(".csv", file_list)]
#
#my_FADN_data <- tibble::tibble()
#
#for (file in file_list) {
#        tmp <- read.csv(paste0(file_path, "/", file))
#
#        my_FADN_data <- dplyr::bind_rows(
#                my_FADN_data,
#                tmp
#        )
#
#        cat(file, " added to FADN table.\n")
#}
#
#cat("FADN dataset loaded:", nrow(my_FADN_data), "farm-year observations\n")
## FADN dataset loaded: 1453096 farm-year observations
#cat(
#        "Countries covered:",
#        paste(unique(my_FADN_data$COUNTRY), collapse = ", "),
#        "\n"
#)
## Countries covered: BEL, BGR, CYP, CZE, DAN, DEU, ELL, ESP, EST, FRA, HRV, HUN, IRE, ITA, LTU, LUX, LVA, MLT, NED, OST, POL, POR, ROU, SUO, SVE, SVK, SVN, UKI
#cat(
#        "Years covered:",
#        paste(sort(unique(my_FADN_data$YEAR)), collapse = ", "),
#        "\n"
#)
## Years covered: 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021
#save(
#        list = "my_FADN_data",
#        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_04_21.RData"
#)
#

# =========================================
# Create subsets ----
# =========================================
#load(
#        "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_04_21.RData"
#)

#my_FADN_data <- my_FADN_data |>
#        dplyr::filter(YEAR >= 2017)
#
#save(
#        list = "my_FADN_data",
#        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_17_21.RData"
#)

#my_FADN_data <- my_FADN_data |>
#        dplyr::filter(YEAR >= 2010 & COUNTRY %in% c("FRA", "ITA", "DEU"))
#
#save(
#        list = "my_FADN_data",
#        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_10_21_FRA_ITA_DEU.RData"
#)

#my_FADN_data <- my_FADN_data |>
#        dplyr::filter(YEAR >= 2017 & COUNTRY %in% c("FRA", "ITA", "DEU"))
#
#save(
#        list = "my_FADN_data",
#        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_17_21_FRA_ITA_DEU.RData"
#)

# =========================================
# Create the FADN2Footprint S4 object ----
# =========================================

load(
        "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_17_21_FRA_ITA_DEU.RData"
)
##my_FADN_data <- FADN_16_18
#
cat("Create the FADN2Footprint S4 object\n")

my_object <- data_4FADN2Footprint(
        df = my_FADN_data,
        id_cols = c("ID", "YEAR", "COUNTRY"),
        var_dict = FADN2Footprint::dict_FADN
)
#
## We keep only arable and grass land use types (see`data_extra$crops`). We remove other land uses, fallows and rented land (22.82% of initial crop x farm x year observations) and store them in `object@traceability$other_land_use`
## We remove 36231 observations (1.59% of initial crop x farm x year observations) that had no area nor production. We store them in `object@traceability$removed_crops`.
## Data for 408919 farms have been gathered into a FADN2Footprint object.
#
save(
        list = "my_object",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_17_21_FRA_ITA_DEU_obj.RData"
)

# =========================================
# Infer the practices ----
# =========================================

#load(
#        "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_17_21_obj.RData"
#)

cat("Infer the practices")

my_object_w_practices <- FADN2Footprint::infer_practices(
        my_object,
        overwrite = FALSE
)

# 188128 observations have no livestock. They receive the average national value as a standard value (see Methods).
# 23403 observations have not enough N excretion from their livestock. They receive the minimum organic N threshold as a standard value (see Methods).
# 138541 observations have too much N excretion from their livestock. They receive the maximum organic N threshold as a standard value (see Methods).
# 175460 observations have enough and not too much N excretion from their livestock. They spread the total N excreted from their livestock on-farm (see Methods).

# nrow(my_object_GHGE@traceability$discarded_farms) / (nrow(my_object_GHGE@farm) + nrow(my_object_GHGE@traceability$discarded_farms))
# 47464 out of 408919 (12%) observations with non valid practices are discarded from the S4 object slots (see object@traceability$discarded_farms$problem).

save(
        list = "my_object_w_practices",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_17_21_FRA_ITA_DEU_obj_practices.RData"
)

# =========================================
# Compute the GHGE ----
# =========================================

cat("Compute the GHGE")
#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_17_21_FRA_ITA_DEU_obj_practices.RData")

my_object_GHGE <- FADN2Footprint::compute_footprint_ghg(
        my_object_w_practices,
        overwrite = FALSE
)

save(
        list = "my_object_GHGE",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_17_21_FRA_ITA_DEU_obj_GHGE.RData"
)
