# A script to generate the S4 object and save it


# Uncomment the lines below when using a locally installed version of the package:
# devtools::install_local("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/", dependencies = TRUE, force = TRUE)
# library(FADN2Footprint)

#devtools::document("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")
devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

# Load FADN raw data (2016-2018) ----

load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_FRA_18_21.RData")


# Create the FADN2Footprint S4 object ----
my_object <- data_4FADN2Footprint(
        df       = my_FADN_data,
        id_cols  = c("ID", "YEAR", "COUNTRY"),
        var_dict = FADN2Footprint::dict_FADN
)

save(
        list = "my_object",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj.RData"
)
# Infer the practices ----

#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj.RData")

my_object_w_practices <- FADN2Footprint::infer_practices(my_object, overwrite = F)

save(
     list = "my_object_w_practices",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData"
)

# Compute the GHGE ----

#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData")

my_object_GHGE <- FADN2Footprint::compute_footprint_ghg(my_object_w_practices, overwrite = F)

save(
     list = "my_object_GHGE",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_GHGE.RData"
)
