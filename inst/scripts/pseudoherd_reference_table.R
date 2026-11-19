#' Reference rearing parameters
#'
#' Create reference tables with reference values for rearing parameters
#'
#' @docType data
#' @usage data(reference_rearing_param)
#'
#' @format A list of 16 data frames
#'
#' @source <https://tofill.com>
#'
#' @keywords datasets
#' @examples
#' data(reference_rearing_param)
#' str(reference_rearing_param[[1]])
"reference_rearing_param"

# Reference data ----

# ____________________________
load("C:/Users/srhuet/OneDrive/Research/Data/FADN/R_objects/FADN_EU_16_18_obj.RData")
ref_obj <- my_object
# ref_obj = RICA_2020_obj

n_obs_min <- 15
# _____________________________

id_cols <- c("ID", "COUNTRY", "YEAR", "NUTS2")

ref_data <- ref_obj@herd |>
  dplyr::select(dplyr::all_of(ref_obj@traceability$id_cols), FADN_code_letter, Qobs, ON, CN, PN, SN, SSN, SRN) |>
  tidyr::pivot_wider(
    id_cols = dplyr::all_of(ref_obj@traceability$id_cols),
    names_from = FADN_code_letter,
    values_from = c(Qobs, ON, CN, PN, SN, SSN, SRN),
    names_glue = "{FADN_code_letter}_{.value}"
  ) |>
  # replace NAs by zeros
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) |>
  # add missing livestock categories to prevent errors
  cbind(
    expand.grid(
      code = setdiff(
        data_extra$livestock |>
          dplyr::pull(FADN_code_letter),
        unique(ref_obj@herd$FADN_code_letter)
      ),
      suffix = c("Qobs", "ON", "CN", "PN", "SN", "SSN", "SRN")
    ) |>
      dplyr::transmute(name = paste(code, suffix, sep = "_")) |>
      dplyr::mutate(value = 0) |>
      tidyr::pivot_wider(names_from = name, values_from = value)
  ) |>
  # add needed id_cols
  dplyr::left_join(
    ref_obj@farm |>
      dplyr::select(dplyr::all_of(id_cols)),
    by = ref_obj@traceability$id_cols
  )

# Goat flows are not included until a goat rearing-parameter implementation
# defines the livestock categories and flow relationships used by the package.
reference_rearing_param <- list(
  ref_per_NUTS2 = list(cattle = tibble(), swine = tibble(), poultry = tibble(), sheep = tibble()),
  ref_overall = list(cattle = tibble(), swine = tibble(), poultry = tibble(), sheep = tibble())
)

# CATTLE ----
## Estimate rearing parameters for cattle ----


tmp_param_cattle <- FADN2Footprint:::h_herd_cattle_process_init(
  herd_cattle = ref_obj@herd |>
    dplyr::filter(species == "cattle"),
  id_cols = ref_obj@traceability$id_cols
) |>
  dplyr::left_join(
    ref_obj@farm |>
      dplyr::select(dplyr::all_of(id_cols)),
    by = ref_obj@traceability$id_cols
  ) |>
  # remove farms with less than one cattle
  dplyr::filter(
    rowSums(dplyr::across(dplyr::matches("Qobs"), ~ dplyr::coalesce(.x, 0))) >= 1
  ) |>
  FADN2Footprint:::h_herd_cattle_process_mixed_categories()

tmp_param_cattle <- tmp_param_cattle |>
  # estimate observed quantities and times for each rearing stage
  dplyr::mutate(
    # juveniles
    Qobs_j = LBOV1_Qobs,
    rt_j = rt_LBOV1,
    # fattening
    Qobs_f = LBOV1_2M_Qobs + LBOV2_Qobs + LBOV1_2F_fattening_Qobs + LHEIFFAT_Qobs,
    ## LBOV1_2M & LBOV1_2F have at least 1 y.o., LBOV2 & LHEIFFAT have at least 2 y.o.
    rt_f = (rt_LBOV1_2M_slaughter *
      (LBOV1_2M_SSN / (LBOV1_2M_Fout + LBOV1_2F_fattening_Fout)) +
      (1 + rt_LBOV2) *
        ((LBOV1_2M_Fout - LBOV1_2M_SSN) /
          (LBOV1_2M_Fout + LBOV1_2F_fattening_Fout)) +
      rt_LBOV1_2F_fattening_slaughter *
        (LBOV1_2F_SSN / (LBOV1_2M_Fout + LBOV1_2F_fattening_Fout)) +
      (1 + rt_LHEIFFAT) *
        ((LBOV1_2F_fattening_Fout - LBOV1_2F_SSN) /
          (LBOV1_2M_Fout + LBOV1_2F_fattening_Fout))),
    # breeders
    Qobs_b = LBOV1_2F_breeders_Qobs + LHEIFBRE_Qobs + LCOWDAIR_Qobs + LCOWOTH_Qobs,
    rt_b = ((1 + rt_LBOV1_2F_breeders) * LBOV1_2F_breeders_Qobs +
      (2 + rt_LHEIFBRE) * LHEIFBRE_Qobs +
      (2 + rt_LCOWDAIR) * LCOWDAIR_Qobs) / Qobs_b,
    offspring = offspring_b
  ) |>
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches("Qobs|rt_|_prop_|t_1st_calve|offspring")) |>
  tidyr::pivot_longer(
    cols = -all_of(id_cols),
    names_to = "rearing_param",
    values_to = "value"
  )

## Reference values for cattle ----

# Create reference values per NUTS2
tmp_ref_NUTS2 <- h_average_practices(
  data = tmp_param_cattle |>
    dplyr::left_join(
      ref_obj@farm |>
        dplyr::select(dplyr::all_of(id_cols), NUTS2, SYS02),
      by = id_cols
    ),
  target_vars = "value",
  primary_grp = c("COUNTRY", "YEAR", "NUTS2", "rearing_param"),
  secondary_grp = c("COUNTRY", "rearing_param"),
  weight_var = "SYS02"
)

tmp_ref_NUTS2 <- tmp_param_cattle |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  # summarise
  dplyr::summarise(
    median = median(value, na.rm = T),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    .by = c(NUTS2, rearing_param)
  ) |>
  # remove NUTS2 with less than n_obs_min farms
  ## 13 639 observations to 11 303
  # WIP: to validate
  dplyr::filter(n >= n_obs_min)

# Create overall reference values
tmp_ref_all <- tmp_param_cattle |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  dplyr::summarise(
    median = median(value, na.rm = T),
    q1 = as.numeric(quantile(value, 0.25, na.rm = T)),
    q3 = as.numeric(quantile(value, 0.75, na.rm = T)),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    p01 = as.numeric(quantile(value, 0.01, na.rm = T)),
    p05 = as.numeric(quantile(value, 0.05, na.rm = T)),
    p10 = as.numeric(quantile(value, 0.10, na.rm = T)),
    p90 = as.numeric(quantile(value, 0.90, na.rm = T)),
    p95 = as.numeric(quantile(value, 0.95, na.rm = T)),
    p99 = as.numeric(quantile(value, 0.99, na.rm = T)),
    .by = c(rearing_param)
  )


## Threshold for cattle rearing parameters ----
## we define min and max for each rearing parameter regarding parameter distribution

# tmp0 = unique(tmp_param_cattle$rearing_param)[29]
# tmp1 = tmp_param_cattle |> filter(rearing_param == tmp0 & is.finite(value) & value >0)

# hist(log(tmp1 |> pull(value)),1000,main = tmp0)
# abline(v= c(log(tmp_ref_all$p01[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p99[tmp_ref_all$rearing_param == tmp0])), col = "blue")
# abline(v= c(log(tmp_ref_all$p05[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p95[tmp_ref_all$rearing_param == tmp0])), col = "red")
# abline(v= c(log(tmp_ref_all$p10[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p90[tmp_ref_all$rearing_param == tmp0])), col = "green")
#

tmp_ref_all <- tmp_ref_all |>
  dplyr::mutate(
    threshold_down = case_when(
      rearing_param == "rt_LCOWDAIR" ~ p05,
      rearing_param == "rt_LCOWOTH" ~ p10,
      rearing_param == "rt_LBOV2" ~ p10,
      rearing_param == "rt_LHEIFFAT" ~ p10,
      rearing_param == "rt_LBOV1_2M" ~ p10,
      rearing_param == "rt_LHEIFBRE" ~ p05,
      rearing_param == "rt_LBOV1_2F" ~ p05,
      rearing_param == "rt_LBOV1" ~ p05,
      rearing_param == "t_1st_calve_heiffers" ~ p01,
      rearing_param == "offspring_cows" ~ p05,
      rearing_param == "rt_LBOV1_2F_fattening" ~ p05,
      rearing_param == "rt_LBOV1_2F_breeders" ~ p05,
      rearing_param == "t_1st_calve" ~ p05,
      rearing_param == "offspring_b" ~ p01,
      rearing_param == "rt_LBOV1_2F_fattening_rearing" ~ p01,
      rearing_param == "rt_LBOV1_2F_fattening_slaughter" ~ p01,
      rearing_param == "rt_LBOV1_2M_rearing" ~ p01,
      rearing_param == "rt_LBOV1_2M_slaughter" ~ p01,
      rearing_param == "rt_LBOV1_rearing" ~ p01,
      rearing_param == "rt_LBOV1_slaughter" ~ p01,
      rearing_param == "rt_j" ~ p05,
      rearing_param == "rt_f" ~ p01,
      rearing_param == "rt_b" ~ p10,
      rearing_param == "offspring" ~ p01
    ),
    threshold_up = case_when(
      rearing_param == "rt_LCOWDAIR" ~ p99,
      rearing_param == "rt_LCOWOTH" ~ p95,
      rearing_param == "rt_LBOV2" ~ p90,
      rearing_param == "rt_LHEIFFAT" ~ p90,
      rearing_param == "rt_LBOV1_2M" ~ 1,
      rearing_param == "rt_LHEIFBRE" ~ p99,
      rearing_param == "rt_LBOV1_2F" ~ 1,
      rearing_param == "rt_LBOV1" ~ 1,
      rearing_param == "t_1st_calve_heiffers" ~ p95,
      rearing_param == "offspring_cows" ~ p90,
      rearing_param == "rt_LBOV1_2F_fattening" ~ 1,
      rearing_param == "rt_LBOV1_2F_breeders" ~ 1,
      rearing_param == "t_1st_calve" ~ p95,
      rearing_param == "offspring_b" ~ p90,
      rearing_param == "rt_LBOV1_2F_fattening_rearing" ~ 1,
      rearing_param == "rt_LBOV1_2F_fattening_slaughter" ~ 1,
      rearing_param == "rt_LBOV1_2M_rearing" ~ 1,
      rearing_param == "rt_LBOV1_2M_slaughter" ~ p99,
      rearing_param == "rt_LBOV1_rearing" ~ 1,
      rearing_param == "rt_LBOV1_slaughter" ~ 1,
      rearing_param == "rt_j" ~ 1,
      rearing_param == "rt_f" ~ p90,
      rearing_param == "rt_b" ~ p95,
      rearing_param == "offspring" ~ p90
    )
  )

reference_rearing_param$ref_per_NUTS2$cattle <- tmp_ref_NUTS2
reference_rearing_param$ref_overall$cattle <- tmp_ref_all

# SWINE ----
## Estimate rearing parameters for swine ----

tmp_param_swine <- ref_data |>
  # select swine variables
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches(paste0(
    unique(na.omit(data_extra$livestock$FADN_code_letter[data_extra$livestock$species == "swine"])),
    collapse = "|"
  ))) |>
  # remove farms with less than one animal
  dplyr::filter(rowSums(dplyr::across(dplyr::matches("Qobs"), ~ dplyr::coalesce(.x, 0))) >= 1) |>
  # Flows
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LSOWBRE
    Fout_LSOWBRE = LSOWBRE_SN,
    Fin_LSOWBRE = LSOWBRE_CN - LSOWBRE_ON + Fout_LSOWBRE,
    # Flow in LPIGOTH
    Fout_LPIGOTH = LPIGOTH_SN,
    Fin_LPIGOTH = LPIGOTH_CN - LPIGOTH_ON + Fout_LPIGOTH,
    # Flow in LPIGFAT
    Fout_LPIGFAT = LPIGFAT_SN,
    Fin_LPIGFAT = LPIGFAT_CN - LPIGFAT_ON + Fout_LPIGFAT,
    # Flow in LPIGLET
    Fout_LPIGLET = LPIGLET_SN + pmax(0, Fin_LPIGFAT - LPIGFAT_PN) + pmax(0, Fin_LSOWBRE - LSOWBRE_PN) + pmax(0, Fin_LPIGOTH - LPIGOTH_PN),
    Fin_LPIGLET = LPIGLET_CN - LPIGLET_ON + Fout_LPIGLET
  ) |>
  # replace flow values below zero by zeros
  dplyr::mutate(dplyr::across(dplyr::matches("Fin|Fout"), ~ pmax(.x, 0, na.rm = TRUE))) |>
  # rearing parameters
  dplyr::mutate(
    rt_LSOWBRE = LSOWBRE_Qobs / ((Fin_LSOWBRE + Fout_LSOWBRE) / 2),
    rt_LPIGOTH = LPIGOTH_Qobs / ((Fin_LPIGOTH + Fout_LPIGOTH) / 2),
    rt_LPIGFAT = LPIGFAT_Qobs / ((Fin_LPIGFAT + Fout_LPIGFAT) / 2),
    rt_LPIGLET = LPIGLET_Qobs / ((Fin_LPIGLET + Fout_LPIGLET) / 2),
    offspring_LSOWBRE = (Fin_LPIGLET - LPIGLET_PN) / LSOWBRE_Qobs
  ) |>
  # estimate observed quantities and times for each production process step
  dplyr::mutate(
    # juveniles
    Qobs_j = LPIGLET_Qobs,
    rt_j = rt_LPIGLET,
    # fattening
    Qobs_f = LPIGFAT_Qobs,
    rt_f = rt_LPIGFAT,
    # breeders
    Qobs_b = LSOWBRE_Qobs + LPIGOTH_Qobs,
    offspring = offspring_LSOWBRE
  ) |>
  # replace Inf per NAs
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("rt_|t_1st|offspring"),
      ~ ifelse(!is.finite(.x), NA_real_, .x)
    )
  ) |>
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches("Qobs|rt_|offspring")) |>
  tidyr::pivot_longer(
    cols = -all_of(id_cols),
    names_to = "rearing_param",
    values_to = "value"
  )

## Reference values for swine ----

# Create reference values per NUTS2
tmp_ref_NUTS2 <- tmp_param_swine |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  # summarise
  dplyr::summarise(
    median = median(value, na.rm = T),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    .by = c(NUTS2, rearing_param)
  ) |>
  # remove NUTS2 with less than n_obs_min farms
  ## 2750 observations to 2470
  # WIP: to validate
  dplyr::filter(n >= n_obs_min)

# Create overall reference values
tmp_ref_all <- tmp_param_swine |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  dplyr::summarise(
    median = median(value, na.rm = T),
    q1 = as.numeric(quantile(value, 0.25, na.rm = T)),
    q3 = as.numeric(quantile(value, 0.75, na.rm = T)),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    p01 = as.numeric(quantile(value, 0.01, na.rm = T)),
    p05 = as.numeric(quantile(value, 0.05, na.rm = T)),
    p10 = as.numeric(quantile(value, 0.10, na.rm = T)),
    p90 = as.numeric(quantile(value, 0.90, na.rm = T)),
    p95 = as.numeric(quantile(value, 0.95, na.rm = T)),
    p99 = as.numeric(quantile(value, 0.99, na.rm = T)),
    .by = c(rearing_param)
  )

## Threshold for swine rearing parameters ----
## we define min and max for each rearing parameter regarding parameter distribution

# tmp0 = "rt_f" # unique(tmp_param_swine$rearing_param)
# tmp1 = tmp_param_swine |> filter(rearing_param == tmp0 & is.finite(value) & value >0)
# if (nrow(tmp1) >0) {
# hist(log(tmp1 |> pull(value)),1000,main = tmp0)
# abline(v= c(log(tmp_ref_all$p01[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p99[tmp_ref_all$rearing_param == tmp0])), col = "blue")
# abline(v= c(log(tmp_ref_all$p05[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p95[tmp_ref_all$rearing_param == tmp0])), col = "red")
# abline(v= c(log(tmp_ref_all$p10[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p90[tmp_ref_all$rearing_param == tmp0])), col = "green")
# abline(v= c(log(tmp_ref_all$q1[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$q3[tmp_ref_all$rearing_param == tmp0])), col = "grey")
# }

tmp_ref_all <- tmp_ref_all |>
  dplyr::mutate(
    threshold_down = case_when(
      rearing_param == "rt_LSOWBRE" ~ p05,
      rearing_param == "rt_LPIGOTH" ~ q1,
      rearing_param == "rt_LPIGFAT" ~ p05,
      rearing_param == "rt_LPIGLET" ~ p05,
      rearing_param == "offspring_LSOWBRE" ~ p05,
      rearing_param == "rt_j" ~ p05,
      rearing_param == "rt_f" ~ p10,
      rearing_param == "offspring" ~ p05
    ),
    threshold_up = case_when(
      rearing_param == "rt_LSOWBRE" ~ p95,
      rearing_param == "rt_LPIGOTH" ~ p95,
      rearing_param == "rt_LPIGFAT" ~ p95,
      rearing_param == "rt_LPIGLET" ~ p95,
      rearing_param == "offspring_LSOWBRE" ~ p95,
      rearing_param == "rt_j" ~ p95,
      rearing_param == "rt_f" ~ p95,
      rearing_param == "offspring" ~ p95
    )
  )

reference_rearing_param$ref_per_NUTS2$swine <- tmp_ref_NUTS2
reference_rearing_param$ref_overall$swine <- tmp_ref_all

# POULTRY ----

## Estimate rearing parameters for poultry ----

tmp_param_poultry <- ref_data |>
  # select swine variables
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches(paste0(
    unique(na.omit(data_extra$livestock$FADN_code_letter[data_extra$livestock$species == "poultry"])),
    collapse = "|"
  ))) |>
  # remove farms with less than one animal
  dplyr::filter(rowSums(dplyr::across(dplyr::matches("Qobs"), ~ dplyr::coalesce(.x, 0))) >= 1) |>
  # Flows
  ## See Figure in package vignette
  dplyr::mutate(
    # Flow in LHENSLAY
    Fout_LHENSLAY = LHENSLAY_SN,
    Fin_LHENSLAY = LHENSLAY_CN - LHENSLAY_ON + Fout_LHENSLAY,
    # Flow in LPLTRBROYL
    Fout_LPLTRBROYL = LPLTRBROYL_SN,
    Fin_LPLTRBROYL = LPLTRBROYL_CN - LPLTRBROYL_ON + Fout_LPLTRBROYL,
    # Flow in LPLTROTH
    Fout_LPLTROTH = LPLTROTH_SN,
    Fin_LPLTROTH = LPLTROTH_CN - LPLTROTH_ON + Fout_LPLTROTH
  ) |>
  # replace flow values below zero by zeros
  dplyr::mutate(dplyr::across(dplyr::matches("Fin|Fout"), ~ pmax(.x, 0, na.rm = TRUE))) |>
  # rearing parameters
  dplyr::mutate(
    rt_LHENSLAY = LHENSLAY_Qobs / ((Fin_LHENSLAY + Fout_LHENSLAY) / 2),
    rt_LPLTRBROYL = LPLTRBROYL_Qobs / ((Fin_LPLTRBROYL + Fout_LPLTRBROYL) / 2),
    rt_LPLTROTH = LPLTROTH_Qobs / ((Fin_LPLTROTH + Fout_LPLTROTH) / 2)
  ) |>
  # replace Inf per NAs
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("rt_|t_1st|offspring"),
      ~ ifelse(!is.finite(.x), NA_real_, .x)
    )
  ) |>
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches("Qobs|rt_")) |>
  tidyr::pivot_longer(
    cols = -all_of(id_cols),
    names_to = "rearing_param",
    values_to = "value"
  )

## Reference values for poultry ----

# Create reference values per NUTS2
tmp_ref_NUTS2 <- tmp_param_poultry |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  # summarise
  dplyr::summarise(
    median = median(value, na.rm = T),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    .by = c(NUTS2, rearing_param)
  ) |>
  # remove NUTS2 with less than n_obs_min farms
  ## 2750 observations to 2470
  # WIP: to validate
  dplyr::filter(n >= n_obs_min)

# Create overall reference values
tmp_ref_all <- tmp_param_poultry |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  dplyr::summarise(
    median = median(value, na.rm = T),
    q1 = as.numeric(quantile(value, 0.25, na.rm = T)),
    q3 = as.numeric(quantile(value, 0.75, na.rm = T)),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    p01 = as.numeric(quantile(value, 0.01, na.rm = T)),
    p05 = as.numeric(quantile(value, 0.05, na.rm = T)),
    p10 = as.numeric(quantile(value, 0.10, na.rm = T)),
    p90 = as.numeric(quantile(value, 0.90, na.rm = T)),
    p95 = as.numeric(quantile(value, 0.95, na.rm = T)),
    p99 = as.numeric(quantile(value, 0.99, na.rm = T)),
    .by = c(rearing_param)
  )

## Threshold for poultry rearing parameters ----
## we define min and max for each rearing parameter regarding parameter distribution

# tmp0 = "rt_LPLTROTH" # unique(tmp_param_poultry$rearing_param)
# tmp1 = tmp_param_poultry |> filter(rearing_param == tmp0 & is.finite(value) & value >0)
# if (nrow(tmp1) >0) {
# hist(log(tmp1 |> pull(value)),1000,main = tmp0)
# abline(v= c(log(tmp_ref_all$p01[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p99[tmp_ref_all$rearing_param == tmp0])), col = "blue")
# abline(v= c(log(tmp_ref_all$p05[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p95[tmp_ref_all$rearing_param == tmp0])), col = "red")
# abline(v= c(log(tmp_ref_all$p10[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p90[tmp_ref_all$rearing_param == tmp0])), col = "green")
# abline(v= c(log(tmp_ref_all$q1[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$q3[tmp_ref_all$rearing_param == tmp0])), col = "grey")
# }

tmp_ref_all <- tmp_ref_all |>
  dplyr::mutate(
    threshold_down = case_when(
      rearing_param == "rt_LHENSLAY" ~ p05,
      rearing_param == "rt_LPLTRBROYL" ~ p01,
      rearing_param == "rt_LPLTROTH" ~ p10
    ),
    threshold_up = case_when(
      rearing_param == "rt_LHENSLAY" ~ p95,
      rearing_param == "rt_LPLTRBROYL" ~ p95,
      rearing_param == "rt_LPLTROTH" ~ p95
    )
  )

reference_rearing_param$ref_per_NUTS2$poultry <- tmp_ref_NUTS2
reference_rearing_param$ref_overall$poultry <- tmp_ref_all

# SHEEP ----
## Estimate rearing parameters for sheep ----

tmp_param_sheep <- ref_data |>
  # select sheep variables
  dplyr::select(
    dplyr::all_of(id_cols),
    dplyr::matches(paste0(
      unique(na.omit(data_extra$livestock$FADN_code_letter[data_extra$livestock$species == "sheep"])),
      collapse = "|"
    ))
  ) |>
  # remove farms with less than one animal
  dplyr::filter(rowSums(dplyr::across(dplyr::matches("Qobs"), ~ dplyr::coalesce(.x, 0))) >= 1) |>
  ### Flows ----
  ## see diagram in Annex 1 of COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK, 2009. Typology Handbook of agricultural holdings and the standard output (SO) coefficient calculation. (No. RI/CC 1500 rev. 3), COMMUNITY COMMITTEE FOR THE FARM ACCOUNTANCY DATA NETWORK. European Commission, Brussels.
  dplyr::mutate(
    # Flow in LEWEBRE (breeding ewes)
    LEWEBRE_Fout = LEWEBRE_SN,
    LEWEBRE_Fin = LEWEBRE_CN - LEWEBRE_ON + LEWEBRE_Fout,

    # Flow in LSHEPOTH (other sheep: juveniles / fattening)
    LSHEPOTH_Fout = LSHEPOTH_SN + pmax(0, LEWEBRE_Fin - LEWEBRE_PN),
    LSHEPOTH_Fin = LSHEPOTH_CN - LSHEPOTH_ON + LSHEPOTH_Fout
  ) |>
  # replace flow values below zero by zeros
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("Fin|Fout"),
      ~ pmax(.x, 0, na.rm = TRUE)
    )
  ) |>
  ### rearing parameters ----
  ## average number of animals = average livestock unit / livestock unit coefficient
  ## see Commission Regulation (EC) No 1200/2009 of 30 November 2009 implementing Regulation (EC) No 1166/2008 of the European Parliament and of the Council on farm structure surveys and the survey on agricultural production methods, as regards livestock unit coefficients and definitions of the characteristics (Text with EEA relevance), 2009. , OJ L.
  dplyr::mutate(
    rt_LEWEBRE = LEWEBRE_Qobs / ((LEWEBRE_Fin + LEWEBRE_Fout) / 2),
    rt_LSHEPOTH = LSHEPOTH_Qobs / ((LSHEPOTH_Fin + LSHEPOTH_Fout) / 2)
  ) |>
  ### Estimate rearing stage quantities and offspring parameter ----
  ## LSHEPOTH represents a single "others" stage (no fattening/breeders split needed)
  dplyr::mutate(
    offspring_LEWEBRE = (LSHEPOTH_Fin - LSHEPOTH_PN) / LEWEBRE_Qobs
  ) |>
  # replace Inf per NAs
  dplyr::mutate(
    dplyr::across(
      dplyr::matches("rt_|t_1st|offspring"),
      ~ ifelse(!is.finite(.x), NA_real_, .x)
    )
  ) |>
  # estimate observed quantities and times for each rearing stage
  dplyr::mutate(
    # others (juveniles / fattening)
    Qobs_o = LSHEPOTH_Qobs,
    rt_o = rt_LSHEPOTH,
    # breeders
    Qobs_b = LEWEBRE_Qobs,
    rt_b = rt_LEWEBRE,
    offspring = offspring_LEWEBRE
  ) |>
  dplyr::select(dplyr::all_of(id_cols), dplyr::matches("Qobs|rt_|offspring")) |>
  tidyr::pivot_longer(
    cols = -all_of(id_cols),
    names_to = "rearing_param",
    values_to = "value"
  )

## Reference values for sheep ----

# Create reference values per NUTS2
tmp_ref_NUTS2 <- tmp_param_sheep |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  # summarise
  dplyr::summarise(
    median = median(value, na.rm = T),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    .by = c(NUTS2, rearing_param)
  ) |>
  # remove NUTS2 with less than n_obs_min farms
  ## 13 639 observations to 11 303
  # WIP: to validate
  dplyr::filter(n >= n_obs_min)

# Create overall reference values
tmp_ref_all <- tmp_param_sheep |>
  # remove unreliable values
  dplyr::filter(
    value > 0 & is.finite(value)
  ) |>
  dplyr::summarise(
    median = median(value, na.rm = T),
    q1 = as.numeric(quantile(value, 0.25, na.rm = T)),
    q3 = as.numeric(quantile(value, 0.75, na.rm = T)),
    mean = mean(value, na.rm = T),
    sd = sd(value, na.rm = T),
    n = length(value),
    p01 = as.numeric(quantile(value, 0.01, na.rm = T)),
    p05 = as.numeric(quantile(value, 0.05, na.rm = T)),
    p10 = as.numeric(quantile(value, 0.10, na.rm = T)),
    p75 = as.numeric(quantile(value, 0.75, na.rm = T)),
    p90 = as.numeric(quantile(value, 0.90, na.rm = T)),
    p95 = as.numeric(quantile(value, 0.95, na.rm = T)),
    p99 = as.numeric(quantile(value, 0.99, na.rm = T)),
    .by = c(rearing_param)
  )


## Threshold for sheep rearing parameters ----
## we define min and max for each rearing parameter regarding parameter distribution

# tmp0 = "rt_LEWEBRE"
# tmp1 = tmp_param_sheep |> filter(rearing_param == tmp0 & is.finite(value) & value >0)

# hist(log(tmp1 |> pull(value)),1000,main = tmp0)
# abline(v= c(log(tmp_ref_all$p01[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p99[tmp_ref_all$rearing_param == tmp0])), col = "blue")
# abline(v= c(log(tmp_ref_all$p05[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p95[tmp_ref_all$rearing_param == tmp0])), col = "red")
# abline(v= c(log(tmp_ref_all$p10[tmp_ref_all$rearing_param == tmp0]),log(tmp_ref_all$p90[tmp_ref_all$rearing_param == tmp0])), col = "green")


tmp_ref_all <- tmp_ref_all |>
  dplyr::mutate(
    threshold_down = case_when(
      rearing_param == "rt_LEWEBRE" ~ p05,
      rearing_param == "rt_LSHEPOTH" ~ p01,
      rearing_param == "rt_LSHEPOTH_fattening" ~ p01,
      rearing_param == "rt_LSHEPOTH_breeders" ~ p01,
      rearing_param == "offspring_LEWEBRE" ~ p01,
      rearing_param == "rt_o" ~ p01,
      rearing_param == "rt_b" ~ p01,
      rearing_param == "offspring" ~ p01
    ),
    threshold_up = case_when(
      rearing_param == "rt_LEWEBRE" ~ p75,
      rearing_param == "rt_LSHEPOTH" ~ p99,
      rearing_param == "rt_LSHEPOTH_fattening" ~ p99,
      rearing_param == "rt_LSHEPOTH_breeders" ~ p99,
      rearing_param == "offspring_LEWEBRE" ~ p99,
      rearing_param == "rt_o" ~ p99,
      rearing_param == "rt_b" ~ p75,
      rearing_param == "offspring" ~ p99
    )
  )

reference_rearing_param$ref_per_NUTS2$sheep <- tmp_ref_NUTS2
reference_rearing_param$ref_overall$sheep <- tmp_ref_all


# export data
usethis::use_data(reference_rearing_param, overwrite = T)


rm(id_cols, tmp_param_cattle, tmp_param_poultry, tmp_param_swine, tmp_ref_all, tmp_ref_NUTS2)
