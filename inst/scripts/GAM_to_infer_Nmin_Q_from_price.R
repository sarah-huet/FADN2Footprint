# Script to compare the inference of mineral fertilizer quantity from price with actual registered quantity after 2017

devtools::load_all()

# Load data
load(
    "C:/Users/srhuet/OneDrive/Research/GitHub/FADN2Footprint/data_raw/raw_FADN_EU_17_21.RData"
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
        TF14,
        SIZC,
        SIZEUR
    ) |>
    dplyr::mutate(
        TF14 = as.factor(TF14),
        SIZC = as.factor(SIZC)
    )

#plot(my_data$N_min_ferti_Q, my_data$N_min_ferti_V)

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

mean_N_min_ferti_Q_estim_EUROSTAT <- mean(
    comp$N_min_ferti_Q_estim_EUROSTAT,
    na.rm = TRUE
)
sd_N_min_ferti_Q_estim_EUROSTAT <- sd(
    comp$N_min_ferti_Q_estim_EUROSTAT,
    na.rm = TRUE
)

# Remove outliers before fitting the GAM regression
comp <- comp |>
    dplyr::filter(
        N_min_ferti_Q_estim_EUROSTAT <=
            mean_N_min_ferti_Q_estim_EUROSTAT +
                3 * sd_N_min_ferti_Q_estim_EUROSTAT
    )

#EUROSTAT_ferti_use_ratio |> view()

# Check summary of relative differences for farms post 2017
#plot(comp$N_min_ferti_Q, comp$N_min_ferti_Q_estim_EUROSTAT)
#abline(a = 0, b = 1, col = "red")

summary(comp$N_min_ferti_Q_rel_diff)
summary(comp$N_min_ferti_Q_estim_EUROSTAT)
#hist(comp$N_min_ferti_Q_rel_diff)

# our inference overestimate the quantity of nitrogenous mineral fertilisers
# I want to regress the actual quantity on the estimated quantity to quantify the bias
# GAM regression to model the relationship between actual and estimated quantity
library(mgcv)
# GAM regression
## fix the smoothing parameter by hand (large sp = very smooth)
gam_reg <- gam(
    N_min_ferti_Q ~ s(N_min_ferti_Q_estim_EUROSTAT, sp = 10) +
        COUNTRY +
        TF14 +
        SIZC,
    family = tw(),
    method = "REML",
    data = comp
)
#gam_reg2 <- gam(
#    N_min_ferti_Q ~ s(N_min_ferti_Q_estim_EUROSTAT, sp = 10) + COUNTRY + TF14,
#    family = tw(),
#    method = "REML",
#    data = comp
#)
#gam_reg <- gam_reg2

#gam_reg3 <- gam(
#    N_min_ferti_Q ~ s(N_min_ferti_Q_estim_EUROSTAT) + TF14,
#    family = tw(),
#    method = "REML",
#    data = comp
#)
#gam_reg <- gam_reg3

#gam_reg4 <- gam(
#    N_min_ferti_Q ~ s(N_min_ferti_Q_estim_EUROSTAT, sp = 10),
#    family = tw(),
#    method = "REML",
#    data = comp
#)
#gam_reg <- gam_reg4

#load('data/N_min_inference_GAM.rda')
#gam_reg <- N_min_inference_GAM

summary(gam_reg)
#gam.check(gam_reg) # k suffisant ? résidus corrects ?
gam_reg$family$getTheta(TRUE) # paramètre p de Tweedie, doit être entre 1 et 2

plot(
    gam_reg,
    pages = 1,
    shade = TRUE,
    trans = exp,
    shift = coef(gam_reg)[1],
    seWithMean = TRUE,
    residuals = FALSE
)


# use intercept and slope from the regression to adjust the estimated quantity
p <- predict(gam_reg, newdata = comp, type = "link", se.fit = TRUE)
comp_adj <- comp |>
    dplyr::mutate(
        N_min_ferti_Q_estim_adjusted = ifelse(N_min_ferti_V > 0, exp(p$fit), 0),
        N_min_ferti_Q_estim_adjusted_lo = ifelse(
            N_min_ferti_V > 0,
            exp(p$fit - 1.96 * p$se.fit),
            0
        ),
        N_min_ferti_Q_estim_adjusted_hi = ifelse(
            N_min_ferti_V > 0,
            exp(p$fit + 1.96 * p$se.fit),
            0
        )
    )


# Check summary of relative differences for farms post 2017
plot(
    comp_adj$N_min_ferti_Q_estim_EUROSTAT,
    comp_adj$N_min_ferti_Q_estim_adjusted
)
abline(a = 0, b = 1, col = "red")
plot(
    comp_adj$N_min_ferti_Q,
    comp_adj$N_min_ferti_Q_estim_adjusted
)
abline(a = 0, b = 1, col = "red")

# Performance metrics
metrics <- function(obs, pred) {
    ok <- is.finite(obs) & is.finite(pred)
    obs <- obs[ok]
    pred <- pred[ok]
    c(
        RMSE = sqrt(mean((obs - pred)^2)),
        MAE = mean(abs(obs - pred)),
        avrg_bias = mean(pred - obs),
        total_rel_biais = (sum(pred) - sum(obs)) / sum(obs),
        cor = cor(obs, pred)
    )
}
rbind(
    EUROSTAT = metrics(
        comp_adj$N_min_ferti_Q,
        comp_adj$N_min_ferti_Q_estim_EUROSTAT
    ),
    adjusted = metrics(
        comp_adj$N_min_ferti_Q,
        comp_adj$N_min_ferti_Q_estim_adjusted
    )
)

# Export ----
N_min_inference_GAM <- gam_reg
usethis::use_data(N_min_inference_GAM, overwrite = TRUE)
