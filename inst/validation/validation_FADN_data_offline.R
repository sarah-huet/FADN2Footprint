# A script to generate the S4 object and save it

# Uncomment the lines below when using a locally installed version of the package:
# devtools::install_local("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/", dependencies = TRUE, force = TRUE)
# library(FADN2Footprint)

#devtools::document("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")
devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

# Load FADN raw data for France (2018-2021) ----

#load(
#     "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_FRA_18_21.RData"
#)
#
#
## Create the FADN2Footprint S4 object ----
#my_object <- data_4FADN2Footprint(
#     df = my_FADN_data,
#     id_cols = c("ID", "YEAR", "COUNTRY"),
#     var_dict = FADN2Footprint::dict_FADN
#)
#
#save(
#     list = "my_object",
#     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj.RData"
#)
# Infer the practices ----

load(
     "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj.RData"
)

my_object_w_practices <- FADN2Footprint::infer_practices(
     my_object,
     overwrite = F
)

#13428 observations have no livestock. They receive the average national value as a standard value (see Methods).
#1036 observations have not enough N excretion from their livestock. They receive the minimum organic N threshold as a standard value (see Methods).
#12456 observations have too much N excretion from their livestock. They receive the maximum organic N threshold as a standard value (see Methods).
#14052 observations have enough and not too much N excretion from their livestock. They spread the total N excreted from their livestock on-farm (see Methods).

#3106 out of 29735 (10%) observations with non valid practices are discarded from the S4 object slots (see object@traceability$discarded_farms$problem).

save(
     list = "my_object_w_practices",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData"
)

# Compute the GHGE ----

#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData")

my_object_GHGE <- FADN2Footprint::compute_footprint_ghg(
     my_object_w_practices,
     overwrite = F
)

save(
     list = "my_object_GHGE",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_GHGE.RData"
)

##############################################################
# Load FADN raw data for all member states (2016-2018) ----

load(
     "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_16_18.RData"
)


# Create the FADN2Footprint S4 object ----
my_object <- data_4FADN2Footprint(
     df = FADN_16_18,
     id_cols = c("ID", "YEAR", "COUNTRY"),
     var_dict = FADN2Footprint::dict_FADN
)

# We keep only arable and grass land use types (see`data_extra$crops`). We remove other land uses, fallows and rented land (0% of initial crop x farm x year observations) and store them in `object@traceability$other_land_use`
# We remove 51401 observations (8.77% of initial crop x farm x year observations) that had no area nor production. We store them in `object@traceability$removed_crops`.

save(
     list = "my_object",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_16_18_obj.RData"
)
# Infer the practices ----

load(
     "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_16_18_obj.RData"
)

my_object_w_practices <- FADN2Footprint::infer_practices(
     my_object,
     overwrite = F
)

# 78372 observations have no livestock. They receive the average national value as a standard value (see Methods).
# 10887 observations have not enough N excretion from their livestock. They receive the minimum organic N threshold as a standard value (see Methods).
# 43814 observations have too much N excretion from their livestock. They receive the maximum organic N threshold as a standard value (see Methods).
# 49743 observations have enough and not too much N excretion from their livestock. They spread the total N excreted from their livestock on-farm (see Methods).

# 36077 out of 168757 (21%) observations with non valid practices are discarded from the S4 object slots (see object@traceability$discarded_farms$problem).

save(
     list = "my_object_w_practices",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_16_18_obj_practices.RData"
)

# Compute the GHGE ----

#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData")

my_object_GHGE <- FADN2Footprint::compute_footprint_ghg(
     my_object_w_practices,
     overwrite = F
)

save(
     list = "my_object_GHGE",
     file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_EU_16_18_obj_GHGE.RData"
)
