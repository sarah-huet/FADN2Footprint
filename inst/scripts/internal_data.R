



data_list <- c(  "data_extra",
                 "dict_FADN",
                 "EF_electricity",
                 "EUROSTAT_elec_price",
                 "EUROSTAT_gaz_price",
                 "EUROSTAT_input_price",
                 "GWP",
                 "ref_fuel_wob",
                 "reference_rearing_param",
                 "UNFCCC_data")

file_list = list.files("inst/scripts/")

file_path = paste0("inst/scripts/",file_list)

for (file in file_path) {
  source(file)
}


load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/R/sysdata.rda")

usethis::use_data(
  data_extra,
  dict_FADN,
  FADN_averages,
  EF_electricity,
  EUROSTAT_elec_price,
  EUROSTAT_gaz_price,
  EUROSTAT_input_price,
  GWP,
  ref_fuel_wob,
  reference_rearing_param,
  UNFCCC_data,
  internal = TRUE, overwrite = TRUE)
