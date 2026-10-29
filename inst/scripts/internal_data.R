devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/R/sysdata.rda")

source(
  "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/inst/scripts/data_extra.R"
)

usethis::use_data(
  data_extra,
  EUROSTAT_ferti_use_ratio,
  dict_FADN,
  FADN_averages,
  EF_electricity,
  EUROSTAT_elec_price,
  EUROSTAT_gaz_price,
  EUROSTAT_input_price,
  EUROSTAT_slaughter,
  GWP,
  ref_fuel_wob,
  reference_rearing_param,
  UNFCCC_data,
  internal = TRUE,
  overwrite = TRUE
)
