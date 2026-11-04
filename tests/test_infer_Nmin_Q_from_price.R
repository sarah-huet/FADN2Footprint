# Script to compare the inference of mineral fertilizer quantity from price with actual registered quantity after 2017

devtools::load_all()

# Load data
load(
    "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_10_21_FRA_ITA_DEU.RData"
)

# Select relevant columns from the loaded data
my_data <- my_FADN_data |>
    dplyr::filter(YEAR >= 2017) |>
    dplyr::select(
        ID,
        COUNTRY,
        YEAR,
        N_min_ferti_Q = "INUSE_Q", # Quantity of N used in mineral fertilisers Quantity	in tonnes
        N_min_ferti_V = "SE295", # SE295	Purchased fertilisers and soil improvers (excluding those used for forests).
    )

plot(my_data$N_min_ferti_Q, my_data$N_min_ferti_V)


# Infer quantity from price for years
my_data_Nmin_Q_pred <- my_data |>
    # fertilisers use quantity where not collected before 2017
    # estimate N_min_ferti_Q using N_min_ferti_V / ratio if quantity not available
    ## add EUROSTAT N fertiliser ratios for estimation of quantity when missing
    dplyr::left_join(
        EUROSTAT_ferti_use_ratio |>
            dplyr::mutate(YEAR = as.numeric(time_period)) |>
            dplyr::select(country_FADN, YEAR, n_ratio),
        by = c("COUNTRY" = "country_FADN", "YEAR")
    ) |>
    dplyr::mutate(
        N_min_ferti_Q_estim_EUROSTAT = N_min_ferti_V * n_ratio
    )

comp <- my_data_Nmin_Q_pred |>
    # compare estimation
    dplyr::mutate(
        N_min_ferti_Q_rel_diff = dplyr::case_when(
            N_min_ferti_V > 0 & N_min_ferti_Q > 0 ~ (N_min_ferti_Q -
                N_min_ferti_Q_estim_EUROSTAT) /
                N_min_ferti_Q,
            N_min_ferti_V > 0 & N_min_ferti_Q == 0 ~ 1,
            N_min_ferti_V == 0 & N_min_ferti_Q == 0 ~ 0,
            .default = NA
        )
    )

#EUROSTAT_ferti_use_ratio |> view()

# Check summary of relative differences for farms post 2017
plot(comp$N_min_ferti_Q, comp$N_min_ferti_Q_estim_EUROSTAT)
abline(a = 0, b = 1, col = "red")

summary(comp$N_min_ferti_Q_rel_diff)
#hist(comp$N_min_ferti_Q_rel_diff)

# our inference overestimate the quantity of nitrogenous mineral fertilisers
# I want to regress the actual quantity on the estimated quantity to quantify the bias
# GAM regression to model the relationship between actual and estimated quantity
library(mgcv)
# GAM regression
gam_reg <- gam(N_min_ferti_Q ~ s(N_min_ferti_Q_estim_EUROSTAT), data = comp)
summary(gam_reg)

# use intercept and slope from the regression to adjust the estimated quantity
comp_adj <- comp |>
  dplyr::mutate(N_min_ferti_Q_estim_adjusted =
    as.numeric(predict(gam_reg, newdata = comp)))

# Check summary of relative differences for farms post 2017
plot(comp_adj$N_min_ferti_Q, comp_adj$N_min_ferti_Q_estim_adjusted, log = c("x", "y"))
abline(a = 0, b = 1, col = "red")


