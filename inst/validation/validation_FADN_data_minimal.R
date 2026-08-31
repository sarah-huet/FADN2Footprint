# A script to generate the S4 object and save it


# Uncomment the lines below when using a locally installed version of the package:
# devtools::install_local("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/", dependencies = TRUE, force = TRUE)
#devtools::document("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")
devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

#library(FADN2Footprint)

# Load FADN raw data (2016-2018) ----
file_path = "//abel/perso_vbellassen/backup_documents/donnees_agricoles/FADN/LAMASUS_extract/"
file_list = list.files(file_path)
file_list = file_list[grepl(".csv", file_list)]

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

save(        list = "my_FADN_data",
             file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_FRA_18_21.RData"
)

# Filter minimum viable data to make the package work ----

# Crop regex
crop_codes <- data_extra$crops$FADN_code_letter |>
        # Filter out codes
        ## ending in _X (pre-2014 versions)
        ## beginning with CTOT (aggregates)
        (\(x) x[!grepl("_X$|^CTOT", x)])() |>
        unique() |>
        sort(decreasing = TRUE)
# Create a regex pattern once
crop_regex <- paste(crop_codes, collapse = "|")

# Herd regex
herd_codes <- data_extra$livestock |>
        dplyr::filter(!is.na(livestock_unit_coef)) |>
        dplyr::pull(FADN_code_letter) |>
        unique() |>
        sort(decreasing = TRUE)

herd_regex <- paste(herd_codes, collapse = "|")

# Livestock product regex
output_meta <- data_extra$output |>
        dplyr::filter(!is.na(output))
output_codes <- sort(unique(output_meta$FADN_code_letter), decreasing = TRUE)
output_regex <- paste(output_codes, collapse = "|")

# Filter dictionary
Indication_of_requested_data_to_access_2026 <- read_excel("C:/Users/srhuet/OneDrive/Research/Data/FADN/Indication_of_requested_data_to_access_2026.xlsx",
                                                          sheet = "Selection of variables")

dict_min1 <- Indication_of_requested_data_to_access_2026 |>
        dplyr::filter(
                `COMMON name` %in% c(

                        "ID", "YEAR", "COUNTRY",

                        # Farm characteristics
                        "NUTS2",
                        # Farm representativeness
                        "SYS02",
                        # Type of farming
                        "TF_SUBP4", "TF14", "TF8",
                        # Economic size class
                        "SIZC",
                        # SE131	Total output	in EUR	STANDARD RESULTS
                        "SE131",
                        # SE080	Total livestock units	in Livestock unit (conversion coefficients see RICC 1750)	STANDARD RESULTS
                        "SE080",
                        # SE025	Total Utilised Agricultural Area	in ha	STANDARD RESULTS
                        "SE025",
                        # Total area under glass
                        #"CTOTUG"),
                        # Farm certification
                        # Note: PDO variable excluded due to poor registration (e.g., missing in FR 2016-2018)
                        "ORGANIC",

                        # Crop variables
                        paste0(crop_codes, "_TA"),
                        paste0(crop_codes, "_A"),
                        paste0(crop_codes, "_PRQ"),
                        paste0(crop_codes, "_SQ"),
                        paste0(crop_codes, "_SV"),
                        paste0(crop_codes, "_TO"),

                        # Livestock
                        paste0(herd_codes, "_AN"),
                        paste0(herd_codes, "_ALU"),
                        paste0(herd_codes, "_ON"),
                        paste0(herd_codes, "_CN"),
                        paste0(herd_codes, "_PN"),
                        paste0(herd_codes, "_SN"),
                        paste0(herd_codes, "_SSN"),
                        paste0(herd_codes, "_SRN"),
                        paste0(herd_codes, "_SV"),
                        paste0(herd_codes, "_SSV"),
                        paste0(herd_codes, "_SRV"),
                        paste0(herd_codes, "_TO"),

                        # Inputs
                        "IGRFEDCNCTRPUR_V", # Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value	in EUR
                        "IGRFEDCRSPUR_V", # Purchased coarse fodder for grazing stock (equines, ruminants) Value	in EUR
                        "IPIGFEDPUR_V", # Purchased feedstuffs for pigs Value	in EUR
                        "IPLTRFEDPUR_V", # Purchased feedstuffs for poultry and other small animals Value	in EUR
                        "INUSE_Q",   # Quantity of N used in mineral fertilisers Quantity	in tonnes
                        "IPROT_V",   # Crop protection products Value	in EUR
                        "IFULS_V",   # Motor fuels and lubricants Value	in EUR
                        "IHFULS_V", # Heating fuels Value	in EUR
                        "IELE_V",  # Farming overheads. Electricity. Value	in EUR

                        # Outputs
                        paste0(output_codes, "_PRQ"),
                        paste0(output_codes, "_SQ"),
                        paste0(output_codes, "_SV"),
                        paste0(output_codes, "_TO")

                )
        )

length(dict_min1$`COMMON name`)




dict_min2 <- Indication_of_requested_data_to_access_2026 |>
        dplyr::filter(
                grepl(
                        paste0(c(

                                "ID", "YEAR", "COUNTRY",

                                # Farm characteristics
                                "NUTS2",
                                # Farm representativeness
                                "SYS02",
                                # Type of farming
                                "TF_SUBP4", "TF14", "TF8",
                                # Economic size class
                                "SIZC",
                                # SE131	Total output	in EUR	STANDARD RESULTS
                                "SE131",
                                # SE080	Total livestock units	in Livestock unit (conversion coefficients see RICC 1750)	STANDARD RESULTS
                                "SE080",
                                # SE025	Total Utilised Agricultural Area	in ha	STANDARD RESULTS
                                "SE025",
                                # Total area under glass
                                #"CTOTUG"),
                                # Farm certification
                                # Note: PDO variable excluded due to poor registration (e.g., missing in FR 2016-2018)
                                "ORGANIC",

                                # Crop variables
                                paste0(crop_codes, "_TA"),
                                paste0(crop_codes, "_A"),
                                paste0(crop_codes, "_PRQ"),
                                paste0(crop_codes, "_SQ"),
                                paste0(crop_codes, "_SV"),
                                paste0(crop_codes, "_TO"),

                                # Livestock
                                paste0(herd_codes, "_AN"),
                                paste0(herd_codes, "_ALU"),
                                paste0(herd_codes, "_ON"),
                                paste0(herd_codes, "_CN"),
                                paste0(herd_codes, "_PN"),
                                paste0(herd_codes, "_SN"),
                                paste0(herd_codes, "_SSN"),
                                paste0(herd_codes, "_SRN"),
                                paste0(herd_codes, "_SV"),
                                paste0(herd_codes, "_SSV"),
                                paste0(herd_codes, "_SRV"),
                                paste0(herd_codes, "_TO"),

                                # Inputs
                                "IGRFEDCNCTRPUR_V", # Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value	in EUR
                                "IGRFEDCRSPUR_V", # Purchased coarse fodder for grazing stock (equines, ruminants) Value	in EUR
                                "IPIGFEDPUR_V", # Purchased feedstuffs for pigs Value	in EUR
                                "IPLTRFEDPUR_V", # Purchased feedstuffs for poultry and other small animals Value	in EUR
                                "INUSE_Q",   # Quantity of N used in mineral fertilisers Quantity	in tonnes
                                "IPROT_V",   # Crop protection products Value	in EUR
                                "IFULS_V",   # Motor fuels and lubricants Value	in EUR
                                "IHFULS_V", # Heating fuels Value	in EUR
                                "IELE_V",  # Farming overheads. Electricity. Value	in EUR

                                # Outputs
                                paste0(output_codes, "_PRQ"),
                                paste0(output_codes, "_SQ"),
                                paste0(output_codes, "_SV"),
                                paste0(output_codes, "_TO")

                        ), collapse = "|"),
                        `COMMON name`
                )
        )

length(dict_min2$`COMMON name`)




# Filter data
load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_FRA_18_21.RData")

my_FADN_data_min1 <- my_FADN_data |>
        dplyr::select(dplyr::matches(dict_min1$`COMMON name`))

dim(my_FADN_data_min1)

my_FADN_data_min2 <- my_FADN_data |>
        dplyr::select(
                "ID", "YEAR", "COUNTRY",

                # Farm characteristics
                "NUTS2",
                # Farm representativeness
                dplyr::matches("SYS02"),
                # Type of farming
                "TF_SUBP4", "TF14", "TF8",
                # Economic size class
                "SIZC",
                # SE131	Total output	in EUR	STANDARD RESULTS
                dplyr::matches("SE131"),
                # SE080	Total livestock units	in Livestock unit (conversion coefficients see RICC 1750)	STANDARD RESULTS
                dplyr::matches("SE080"),
                # SE025	Total Utilised Agricultural Area	in ha	STANDARD RESULTS
                dplyr::matches("SE025"),
                # Total area under glass
                #dplyr::matches("CTOTUG"),
                # Farm certification
                # Note: PDO variable excluded due to poor registration (e.g., missing in FR 2016-2018)
                "ORGANIC",

                # Crop variables
                dplyr::matches(paste0(crop_codes, "_TA")),
                dplyr::matches(paste0(crop_codes, "_A")),
                dplyr::matches(paste0(crop_codes, "_PRQ")),
                dplyr::matches(paste0(crop_codes, "_SQ")),
                dplyr::matches(paste0(crop_codes, "_SV")),
                dplyr::matches(paste0(crop_codes, "_TO")),

                # Livestock
                dplyr::matches(paste0(herd_codes, "_AN")),
                dplyr::matches(paste0(herd_codes, "_ALU")),
                dplyr::matches(paste0(herd_codes, "_ON")),
                dplyr::matches(paste0(herd_codes, "_CN")),
                dplyr::matches(paste0(herd_codes, "_PN")),
                dplyr::matches(paste0(herd_codes, "_SN")),
                dplyr::matches(paste0(herd_codes, "_SSN")),
                dplyr::matches(paste0(herd_codes, "_SRN")),
                dplyr::matches(paste0(herd_codes, "_SV")),
                dplyr::matches(paste0(herd_codes, "_SSV")),
                dplyr::matches(paste0(herd_codes, "_SRV")),
                dplyr::matches(paste0(herd_codes, "_TO")),

                # Inputs
                "IGRFEDCNCTRPUR_V", # Purchased concentrated feedstuffs for grazing stock (equines, ruminants) Value	in EUR
                "IGRFEDCRSPUR_V", # Purchased coarse fodder for grazing stock (equines, ruminants) Value	in EUR
                "IPIGFEDPUR_V", # Purchased feedstuffs for pigs Value	in EUR
                "IPLTRFEDPUR_V", # Purchased feedstuffs for poultry and other small animals Value	in EUR
                "INUSE_Q",   # Quantity of N used in mineral fertilisers Quantity	in tonnes
                "IPROT_V",   # Crop protection products Value	in EUR
                "IFULS_V",   # Motor fuels and lubricants Value	in EUR
                "IHFULS_V", # Heating fuels Value	in EUR
                "IELE_V",  # Farming overheads. Electricity. Value	in EUR

                # Outputs
                dplyr::matches(paste0(output_codes, "_PRQ")),
                dplyr::matches(paste0(output_codes, "_SQ")),
                dplyr::matches(paste0(output_codes, "_SV")),
                dplyr::matches(paste0(output_codes, "_TO"))
        )

dim(my_FADN_data_min2)


# Create the FADN2Footprint S4 object ----

cat("Create the FADN2Footprint S4 object")

my_object <- data_4FADN2Footprint(
        df       = my_FADN_data_min1,
        id_cols  = c("ID", "YEAR", "COUNTRY"),
        var_dict = FADN2Footprint::dict_FADN
)

save(
        list = "my_object",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj.RData"
)

# Infer the practices ----
cat("Infer the practices")

my_object_w_practices <- FADN2Footprint::infer_practices(my_object, overwrite = F)

save(
        list = "my_object_w_practices",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData"
)

# Compute the GHGE ----
cat("Compute the GHGE")
#load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_practices.RData")

my_object_GHGE <- FADN2Footprint::compute_footprint_ghg(my_object_w_practices, overwrite = F)

save(
        list = "my_object_GHGE",
        file = "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/FADN_FRA_2018_2021_obj_GHGE.RData"
)
