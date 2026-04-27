# A script to generate the S4 object and save it


# Uncomment the lines below when using a locally installed version of the package:
# devtools::install_local("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/", dependencies = TRUE, force = TRUE)
#devtools::document("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")
#devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

library(FADN2Footprint)

# Load FADN raw data (2016-2018) ----
file_path = "//abel/perso_vbellassen/backup_documents/donnees_agricoles/FADN/LAMASUS_extract/"
file_list = list.files(file_path)

my_FADN_data = tibble::tibble()

for (file in file_list) {

        tmp = read.csv(paste0(file_path,file))

        my_FADN_data <- bind_rows(
                my_FADN_data,
                tmp
        )

        cat(file," added to FADN table.\n")

}

cat("FADN dataset loaded:", nrow(my_FADN_data), "farm-year observations\n")
cat("Countries covered:", paste(unique(my_FADN_data$COUNTRY), collapse = ", "), "\n")
cat("Years covered:", paste(sort(unique(my_FADN_data$YEAR)), collapse = ", "), "\n")

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
