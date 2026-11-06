devtools::load_all("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/")

load("C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/R/sysdata.rda")

source(
  "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/inst/scripts/GAM_to_infer_Nmin_Q_from_price.R"
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
  N_min_inference_GAM,
  EUROSTAT_slaughter,
  GWP,
  ref_fuel_wob,
  reference_rearing_param,
  UNFCCC_data,
  internal = TRUE,
  overwrite = TRUE
)
