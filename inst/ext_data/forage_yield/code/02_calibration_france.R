# =============================================================================
# 02_calibration_france.R - Step 2: rescale Copernicus DMP to SAA yields (France)
# Data: SAA departmental data: 2010-2025 series (sheets FOU for forage, COP for
#       grain maize area and winter wheat yield) and, for earlier years
#       (saa_fds_years), the yearly FDS files (FDS_FOURRAGE, FDS_DEVELOPPE);
#       step 1 NUTS3 averages (outputs/step1/regional_dmp_nuts3.csv).
# Models, fitted separately for each period: A (2004-2013), B (2014-2016), C (2017+):
#   - green maize (FOU 06): linear regression (OLS) of SAA yield on the maize
#     DMP of the growing season
#   - permanent grassland (FOU 17): same, on the grassland DMP
#   - benchmark: same targets explained by SAA winter wheat yield (COP line 01)
# Yield ratios (all years, SAA only): mean over departement x year of
#   yield 11 / yield 17 and yield 12 / yield 17, keeping departement-years
#   with >= ratio_min_area ha of each of the two types.
# Outputs (outputs/step2/): models.rds, fit metrics (R2, MAE), yield ratios,
# observed vs predicted figures, SAA vs Copernicus area tables and figures;
# outputs/paper/: Figures 2-3 and Tables 2-3 of the working paper.
# Units: SAA yields in q/ha are converted to t/ha (/10). Forage yields are
# treated as dry matter (t DM/ha); wheat yields are at standard moisture.
# =============================================================================
source("00_config.R")
library(ggplot2)

# ---- 1. Read the SAA departmental data --------------------------------------
# Function reading one sheet (FOU or COP) into a long table:
# one row per departement x SAA line x year, with SURF (ha), REND (q/ha), PROD (q).
read_saa <- function(sheet) {
  d <- as.data.table(readxl::read_excel(saa_file, sheet = sheet, skip = 5))
  d <- d[!is.na(LIB_DEP) & grepl("^[0-9]{2}[0-9AB] - ", LIB_DEP)]
  d[, `:=`(dep = substr(LIB_DEP, 1, 3), dep_name = sub("^[^ ]+ - ", "", LIB_DEP),
           line = substr(LIB_SAA, 1, 2))]
  d <- d[!dep %in% c("971", "972", "973", "974", "976")]       # overseas: no DMP
  num <- grep("^(SURF|REND|PROD)_[0-9]{4}$", names(d), value = TRUE)
  d[, (num) := lapply(.SD, function(x) suppressWarnings(as.numeric(x))), .SDcols = num]
  long <- melt(d, id.vars = c("dep", "dep_name", "line", "LIB_SAA"), measure.vars = num)
  long[, `:=`(var = sub("_.*", "", variable), year = as.integer(sub(".*_", "", variable)))]
  dcast(long, dep + dep_name + line + LIB_SAA + year ~ var, value.var = "value")
}
# Same long table from the older yearly FDS files (years saa_fds_years, before
# the 2010-2025 series). Their crop codes (N306_MOD) and indicators (N027_MOD)
# are mapped to the lines of the 2010-2025 series; departement names are taken
# from the 2010-2025 series. NB: in these files, temporary grassland includes
# grassland sown up to 6 years ago (5 years in the 2010-2025 series), so that
# permanent grassland (STH) is smaller (France 2010: 9.5 vs 10.9 Mha).
fds_lines <- list(FOU = c("06" = "3.11.1", "11" = "3.12.1.11", "12" = "3.12.1.12", "17" = "3.12.2", "18" = "3.12"),
                  COP = c("01" = "1.11.1.11.1", "18" = "1.11.6"))
fds_ind <- list(FOU = c(SURF = "050.07", REND = "070.02", PROD = "020.08"),   # forage: dry matter
                COP = c(SURF = "050.08", REND = "070.01", PROD = "020.01"))
read_fds <- function(type, years, dep_names) {
  rbindlist(lapply(years, function(y) {
    f <- saa_fds_file(type, y)
    if (!file.exists(f)) { warning("Missing SAA file ", f); return(NULL) }
    d <- fread(f, sep = ";", encoding = "UTF-8", colClasses = "character")
    setnames(d, sub("^\ufeff", "", names(d)))
    d <- d[FRDOM == "METRO" & DEP != "..." & N306_MOD %in% fds_lines[[type]] & N027_MOD %in% fds_ind[[type]]]
    d[, `:=`(dep = sprintf("%03s", DEP), line = names(fds_lines[[type]])[match(N306_MOD, fds_lines[[type]])],
             var = names(fds_ind[[type]])[match(N027_MOD, fds_ind[[type]])],
             value = suppressWarnings(as.numeric(VALEUR)), year = as.integer(ANNREF))]
    d[, dep := gsub(" ", "0", dep)]
    dcast(d, dep + line + year ~ var, value.var = "value")
  }), fill = TRUE)[dep_names, on = "dep", nomatch = 0][, LIB_SAA := line][]
}
# Read both sources and check that the lines used are present
fou <- read_saa("FOU")
cop <- read_saa("COP")
dep_names <- unique(fou[, .(dep, dep_name)])
fou <- rbind(read_fds("FOU", saa_fds_years, dep_names)[, source := "FDS"], fou[, source := "SAA 2010-2025"], fill = TRUE)
cop <- rbind(read_fds("COP", saa_fds_years, dep_names)[, source := "FDS"], cop[, source := "SAA 2010-2025"], fill = TRUE)
msg("SAA years: ", paste(range(fou$year), collapse = "-"), " (FDS files for ",
    paste(range(fou[source == "FDS", year]), collapse = "-"), ")")
stopifnot(all(c("06", "11", "12", "17", "18") %in% fou$line), all(c("01", "18") %in% cop$line))
msg("SAA lines used: ", to_ascii(paste(unique(fou[source != "FDS" & line %in% c("06","11","12","17","18"), LIB_SAA]), collapse = " | ")),
    " | COP: ", to_ascii(paste(unique(cop[source != "FDS" & line %in% c("01", "18"), LIB_SAA]), collapse = " | ")))

# Yields in t/ha (REND, or PROD / SURF when REND is missing), then one row per
# departement x year with areas and yields of lines 06, 11, 12, 17, 18, plus
# the grain maize area (COP line 18) for the area comparison.
fou[, yield := fifelse(!is.na(REND), REND, fifelse(SURF > 0, PROD / SURF, NA_real_)) / 10]  # t/ha
saa <- dcast(fou[line %in% c("06", "11", "12", "17", "18")], dep + dep_name + year ~ line,
             value.var = c("SURF", "yield"))
saa <- merge(saa, cop[line == "18", .(dep, year, SURF_grain = SURF)], by = c("dep", "year"), all.x = TRUE)
# Winter wheat yield (COP line 01, t/ha), used as an alternative predictor to
# benchmark the Copernicus DMP models (section 5b)
cop[, yield := fifelse(!is.na(REND), REND, fifelse(SURF > 0, PROD / SURF, NA_real_)) / 10]
saa <- merge(saa, cop[line == "01", .(dep, year, yield_wheat = yield)], by = c("dep", "year"), all.x = TRUE)

# ---- 2. Join SAA to the Copernicus DMP of step 1 ----------------------------
# Departements are matched to French NUTS3 regions by name (accents, case and
# punctuation removed), year by year, so the NUTS version of each year is used.
# Unmatched departement-years are listed in outputs/step2/unmatched_departements.csv.
norm <- function(x) gsub("[^a-z]", "", tolower(to_ascii(x)))
dmp3 <- fread(file.path(dir_out1, "regional_dmp_nuts3.csv"), encoding = "UTF-8")[cntr == "FR"]
dmp3 <- dmp3[!grepl("^FRY|^FR9", nuts3)]                       # overseas
dmp3[, key := norm(name)]
saa[, key := norm(dep_name)]
saa[, dep_name := to_ascii(dep_name)]   # ASCII names in all outputs
unmatched <- setdiff(unique(saa[year %in% dmp3$year, paste(year, dep_name)]),
                     merge(saa, unique(dmp3[, .(year, key)]), by = c("year", "key"))[, paste(year, dep_name)])
if (length(unmatched)) {
  warning(length(unmatched), " SAA departement-years without NUTS3 match, e.g.: ",
          paste(head(unmatched, 10), collapse = "; "))
  write_csv(data.table(unmatched), file.path(dir_out2, "unmatched_departements.csv"))
}
# Calibration table: one row per departement x year, with SAA data and DMP of
# maize and grassland pixels; each year is assigned to period A, B or C.
wide_dmp <- dcast(dmp3, year + nuts3 + key ~ forage, value.var = c("dmp", "copernicus_area_ha", "dmp_source"))
dat <- merge(saa, wide_dmp, by = c("year", "key"))
dat[, period := period_of_year(year)]
write_csv(dat, file.path(dir_out2, "calibration_data.csv"))

# ---- 3. Helper functions ----------------------------------------------------
# Fit metrics: R2 (consistent with the OLS fit), R2 weighted by the SAA area,
# MAE unweighted and weighted by the SAA area.
metrics <- function(y, p, w) data.table(n = length(y),
                                        R2 = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
                                        R2_weighted = 1 - sum(w * (y - p)^2) / sum(w * (y - weighted.mean(y, w))^2),
                                        MAE = mean(abs(y - p)), MAE_weighted = weighted.mean(abs(y - p), w))

# Model: simple linear regression (OLS, unweighted) of SAA yield on DMP.
# Previous version, weighted by the SAA area of the forage type in the NUTS3:
# fit_ols <- function(d) list(type = "WLS", model = lm(y ~ x, data = d, weights = w), x_range = range(d$x))
fit_ols <- function(d) list(type = "OLS", model = lm(y ~ x, data = d), x_range = range(d$x))
predict_model <- function(m, x) unname(predict(m$model, newdata = data.frame(x = x)))

# ---- 4. Yield ratios to permanent grassland (SAA only, all years) -----------
# Mean over departement x year (>= 10,000 ha of both types) of the ratio of
# yields; used in step 3 to derive artificial and temporary grass yields.
# Only the 2010-2025 series is used (the older FDS files define temporary and
# permanent grassland differently, see section 1).
ratio_min_area <- 10000   # ha of each of the two types (CLAUDE.md)
ratio_rows <- list()
ratios <- rbindlist(lapply(c(artificial = "11", temporary = "12"), function(ln) {
  d <- saa[year >= 2010 & get(paste0("SURF_", ln)) >= ratio_min_area & SURF_17 >= ratio_min_area &
           get(paste0("yield_", ln)) > 0 & yield_17 > 0,
           .(dep_name, year, ratio = get(paste0("yield_", ln)) / yield_17)]
  ratio_rows[[ln]] <<- d
  data.table(n_departement_years = nrow(d), n_departements = uniqueN(d$dep_name),
             years = paste(range(d$year), collapse = "-"), ratio = mean(d$ratio), sd = sd(d$ratio))
}), idcol = "grass_type")
ratios[, grass_type := paste0(grass_type, "/permanent")]
write_csv(ratios, file.path(dir_out2, "yield_ratios.csv"))
write_csv(rbindlist(ratio_rows, idcol = "line"), file.path(dir_out2, "yield_ratios_data.csv"))
print(ratios)

# ---- 5. Fit the models, separately for each period -------------------------
# Green maize: SAA yield of line 06 vs maize DMP; permanent grass: SAA yield of
# line 17 vs grassland DMP. OLS: yield = a + b x DMP. These models are used in step 3.
# 5b. Benchmark: same targets explained by the SAA winter wheat yield of the
# same departement and year (yield = a + b x wheat yield), compared with the
# DMP model on the same departement-years (those with both predictors).
# model name -> SAA line, DMP column
model_spec <- list(green_maize     = list(line = "06", dmp = "dmp_maize"),
                   permanent_grass = list(line = "17", dmp = "dmp_grass"))
models <- list(); met <- list(); preds <- list()
# Robustness check: the same models fitted on 2004-2016 pooled (periods A and B,
# "AB"), to see whether the metrics of the short period B (3 years) are driven by
# its small sample. These models are reported but not used in step 3.
robust_periods <- list(AB = c("A", "B"))
for (p in c(sort(unique(dat$period)), names(robust_periods))) { #p<-sort(unique(dat$period))[1]
  dp <- if (p %in% names(robust_periods)) dat[period %in% robust_periods[[p]]] else dat[period == p]
  for (k in names(model_spec)) { #k<-"green_maize"
    sp <- model_spec[[k]]
    d <- dp[, .(year, nuts3, dep_name, y = get(paste0("yield_", sp$line)), x = get(sp$dmp),
                w = get(paste0("SURF_", sp$line)), z = get(gsub("dmp", "dmp_source",sp$dmp)))][w > 0 & !is.na(y) & !is.na(x) & z =="NUTS3"]
    m <- fit_ols(d)
    models[[p]][[k]] <- m
    d[, pred := predict_model(m, x)]
    met[[paste(p, k)]] <- cbind(period = p, model = k, metrics(d$y, d$pred, d$w),
                                predictor = "DMP", sample = "all",
                                spec = sprintf("OLS: yield = %.3f + %.3f x DMP", coef(m$model)[1], coef(m$model)[2]))
    preds[[paste(p, k)]] <- cbind(period = p, model = k, predictor = "DMP", sample = "all", d)

    # 5b. benchmark on the departement-years with both DMP and wheat yield
    dc <- merge(d[, -c("pred")], dp[, .(nuts3, year, x_wheat = yield_wheat)], by = c("nuts3", "year"))[!is.na(x_wheat)]
    for (pr in c("DMP", "wheat yield")) {
      db <- copy(dc)
      if (pr == "wheat yield") db[, x := x_wheat]
      mb <- fit_ols(db)
      db[, pred := predict_model(mb, x)]
      met[[paste(p, k, pr, "common")]] <- cbind(period = p, model = k, metrics(db$y, db$pred, db$w),
        predictor = pr, sample = "common",
        spec = sprintf("OLS: yield = %.3f + %.3f x %s", coef(mb$model)[1], coef(mb$model)[2], pr))
      preds[[paste(p, k, pr, "common")]] <- cbind(period = p, model = k, predictor = pr, sample = "common",
                                                    db[, -c("x_wheat")])
    }
  }
}
# ---- 6. Save models (used by step 3), metrics and coefficients --------------
met <- rbindlist(met, fill = TRUE)
preds <- rbindlist(preds, fill = TRUE)
saveRDS(list(models = models[setdiff(names(models), names(robust_periods))], ratios = ratios,
             predict_model = predict_model),
        file.path(dir_out2, "models.rds"))
write_csv(met, file.path(dir_out2, "fit_metrics.csv"))
write_csv(preds, file.path(dir_out2, "fit_observed_vs_predicted.csv"))
coefs <- rbindlist(lapply(names(models), function(p) rbindlist(lapply(names(models[[p]]), function(k) {
  m <- models[[p]][[k]]
  cf <- summary(m$model)$coefficients
  data.table(period = p, model = k, term = c("intercept", "dmp"), estimate = cf[, 1],
             std_error = cf[, 2], t_value = cf[, 3], p_value = cf[, 4])
}))))
write_csv(coefs, file.path(dir_out2, "model_coefficients.csv"))
print(met)

# ---- 7. Figures: quality of the fit ----------------------------------------
# a) observed vs predicted yields; b) SAA yield vs DMP with the fitted curve.
pm <- preds[sample == "all"]   # DMP models used in step 3
gp <- ggplot(pm, aes(pred, y, size = w)) +
  geom_abline(linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.3, colour = "#2C6E91") +
  facet_grid(period ~ model) + scale_size_area(max_size = 2.5, guide = "none") +
  labs(x = "Predicted yield (tDM/ha)", y = "SAA yield (tDM/ha)",
       title = "Observed vs predicted yields by period (point size = SAA area)") +
  theme_bw(10)
ggsave(file.path(dir_out2, "fit_observed_vs_predicted.png"), gp, width = 8, height = 6, dpi = 150)

# c) benchmark: DMP vs wheat yield as predictor, same departement-years,
#    observed vs predicted, with R2 and MAE in each panel
pb <- preds[sample == "common"]
lab <- met[sample == "common", .(period, model, predictor,
                                 label = sprintf(" R2 = %.2f\n MAE = %.2f t/ha\n n = %d", R2, MAE, n))]
gb <- ggplot(pb, aes(pred, y)) +
  geom_abline(linetype = 2, colour = "grey50") +
  geom_point(aes(size = w), alpha = 0.3, colour = "#2C6E91") +
  geom_text(data = lab, aes(x = -Inf, y = Inf, label = label), hjust = 0, vjust = 1.2, size = 3) +
  facet_grid(model + period ~ predictor) + scale_size_area(max_size = 2.5, guide = "none") +
  labs(x = "Predicted yield (tDM/ha)", y = "SAA yield (tDM/ha)",
       title = "Benchmark: Copernicus DMP vs SAA winter wheat yield as predictor (OLS, same departement-years)") +
  theme_bw(10)
ggsave(file.path(dir_out2, "benchmark_dmp_vs_wheat.png"), gb, width = 9, height = 7.5, dpi = 150)

gm <- ggplot(pm, aes(x, y)) +
  geom_point(aes(size = w), alpha = 0.3, colour = "#2C6E91") +
  geom_line(data = pm[order(x)], aes(x, pred), colour = "#C0392B") +
  facet_grid(model ~ period, scales = "free") + scale_size_area(max_size = 2.5, guide = "none") +
  labs(x = "Copernicus DMP, 1 March - 30 September (tDM/ha)", y = "SAA yield (tDM/ha)",
       title = "SAA yield vs Copernicus DMP (growing season), OLS fit (point size = SAA area)") + theme_bw(10)
ggsave(file.path(dir_out2, "fit_curves.png"), gm, width = 9, height = 7, dpi = 150)

# ---- 8. Areas: SAA vs Copernicus masks -------------------------------------
# Per departement x year: SAA area (maize: green + grain; grassland: line 18)
# vs Copernicus mask area from step 1; then national totals per year, and
# scatter / time-series figures.
area <- rbind(
  dat[, .(period, year, nuts3, dep_name, type = "maize (green + grain)",
          saa_ha = SURF_06 + fifelse(is.na(SURF_grain), 0, SURF_grain),
          copernicus_ha = copernicus_area_ha_maize)],
  dat[, .(period, year, nuts3, dep_name, type = "grassland (line 18)",
          saa_ha = SURF_18, copernicus_ha = copernicus_area_ha_grass)])
area[, `:=`(diff_ha = copernicus_ha - saa_ha, ratio = copernicus_ha / saa_ha)]
write_csv(area, file.path(dir_out2, "area_saa_vs_copernicus_nuts3.csv"))
area_sum <- area[, .(saa_total_ha = sum(saa_ha, na.rm = TRUE), copernicus_total_ha = sum(copernicus_ha, na.rm = TRUE),
                     correlation_nuts3 = cor(saa_ha, copernicus_ha, use = "complete.obs"),
                     median_ratio_nuts3 = median(ratio[is.finite(ratio)], na.rm = TRUE)),
                 by = .(type, period, year)]
area_sum[, ratio_total := copernicus_total_ha / saa_total_ha]
write_csv(area_sum, file.path(dir_out2, "area_saa_vs_copernicus_summary.csv"))
print(area_sum)

ga <- ggplot(area, aes(saa_ha / 1000, copernicus_ha / 1000)) +
  geom_abline(linetype = 2, colour = "grey50") + geom_point(alpha = 0.3, colour = "#2C6E91") +
  facet_grid(type ~ period, scales = "free") +
  labs(x = "SAA area (kha)", y = "Copernicus area (kha)",
       title = "Area per departement: SAA vs Copernicus masks") + theme_bw(10)
ggsave(file.path(dir_out2, "area_saa_vs_copernicus_scatter.png"), ga, width = 9, height = 7, dpi = 150)

gt <- ggplot(melt(area_sum, id.vars = c("type", "period", "year"),
                  measure.vars = c("saa_total_ha", "copernicus_total_ha")),
             aes(year, value / 1e6, colour = variable)) +
  geom_line() + geom_point(size = 1) + facet_wrap(~type, scales = "free_y") +
  scale_colour_manual(values = c(saa_total_ha = "#2C6E91", copernicus_total_ha = "#C0392B"),
                      labels = c("SAA", "Copernicus"), name = NULL) +
  labs(x = NULL, y = "Total area, metropolitan France (Mha)",
       title = "Total area: SAA vs Copernicus masks") + theme_bw(10)
ggsave(file.path(dir_out2, "area_saa_vs_copernicus_totals.png"), gt, width = 9, height = 4, dpi = 150)

# ---- 9. Figures and tables of the working paper (outputs/paper/) -----------
# Periods are labelled with the years of French data actually used.
cal_lab <- dat[, .(lab = paste(range(year), collapse = "-")), by = period]
cal_lab <- setNames(cal_lab$lab, cal_lab$period)
for (r in names(robust_periods))
  cal_lab[r] <- paste0(paste(range(dat[period %in% robust_periods[[r]], year]), collapse = "-"), " (robustness)")
lab_period <- function(p) factor(cal_lab[p], levels = cal_lab[c(sort(unique(dat$period)), names(robust_periods))])
lab_forage <- function(k) factor(forage_label[k], levels = forage_label[c("green_maize", "permanent_grass")])

# Table 2: calibration models (DMP, used in step 3) and benchmark (wheat yield,
# same departement-years) per forage type and period
t2 <- merge(coefs[, .(period, model, term, est = sprintf("%.2f (%.2f)", estimate, std_error))][
              , dcast(.SD, period + model ~ term, value.var = "est")],
            met[sample == "all", .(period, model, n, R2, MAE)], by = c("period", "model"))
t2 <- merge(t2, met[sample == "common" & predictor == "wheat yield",
                    .(period, model, n_benchmark = n, R2_benchmark = R2, MAE_benchmark = MAE)],
            by = c("period", "model"))
t2[, `:=`(years = cal_lab[period], model = forage_label[model])]
setnames(t2, c("intercept", "dmp"), c("intercept_se", "slope_se"))
setcolorder(t2, c("model", "period", "years", "intercept_se", "slope_se"))
t2[, ord := match(period, c(sort(unique(dat$period)), names(robust_periods)))]
setorder(t2, model, ord); t2[, ord := NULL]
write_csv(t2, file.path(dir_paper, "table2_calibration_models.csv"))
t2i <- t2[, .(Forage = model, Years = years, n, `Intercept (se)` = intercept_se, `Slope (se)` = slope_se,
              R2 = fmt(R2), MAE = fmt(MAE), `n bench.` = n_benchmark,
              R2b = fmt(R2_benchmark), `MAE bench.` = fmt(MAE_benchmark))]
setnames(t2i, c("R2", "R2b"), c("R\u00b2", "R\u00b2 bench."))
save_table(t2i, "table2_calibration_models", align = c("l", "l", rep("r", 8)))
# Table 3: yield ratios to permanent grassland
write_csv(ratios, file.path(dir_paper, "table3_yield_ratios.csv"))
save_table(ratios[, .(Ratio = sub("/", " / ", grass_type), Mean = fmt(ratio), `Standard deviation` = fmt(sd),
                      `Departements` = n_departements, `Departement-years` = n_departement_years, Years = years)],
           "table3_yield_ratios", width_cm = 13)

# Figure 2: observed yields against the predictor (Copernicus DMP, or SAA wheat
# yield for the benchmark) with the fitted OLS line, on the same
# departement-years; statistics in each panel
pb <- preds[sample == "common"]
row_lab <- function(model, predictor)
  paste0(forage_label[model], "\n", ifelse(predictor == "DMP", "x: Copernicus DMP", "x: wheat yield"))
pb[, `:=`(row = row_lab(model, predictor), col = lab_period(period))]
lb <- met[sample == "common", .(row = row_lab(model, predictor), col = lab_period(period),
                                label = sprintf("R\u00b2 = %.2f\nMAE = %.2f\nn = %d", R2, MAE, n))]
lv <- unique(pb[order(model, predictor), row]); pb[, row := factor(row, lv)]; lb[, row := factor(row, lv)]
f2 <- ggplot(pb, aes(x, y)) +
  geom_point(alpha = 0.35, size = 0.6, colour = pal_cat[1], stroke = 0) +
  geom_line(data = pb[order(x)], aes(x, pred), colour = "black", linewidth = 0.5) +
  geom_text(data = lb, aes(x = -Inf, y = Inf, label = label), hjust = -0.05, vjust = 1.1, size = 2.3, lineheight = 0.9) +
  facet_wrap(~ row + col, ncol = length(cal_lab), scales = "free",
             labeller = function(l) list(paste0(l$row, "\n", l$col))) +
  labs(x = "Predictor: seasonal Copernicus DMP, 1 March - 30 September (t DM/ha), or SAA winter wheat yield (t/ha)",
       y = "Observed yield, SAA (t DM/ha/yr)") + theme_paper()
save_fig(f2, "fig2_fitted_curves", height_cm = 21)

# Figure 3: areas per departement, Copernicus masks vs SAA, statistics per panel
ar <- area[!is.na(saa_ha) & !is.na(copernicus_ha)]
ar[, `:=`(row = ifelse(grepl("maize", type), "Maize\n(SAA green + grain maize)", "Grassland\n(SAA all grassland)"),
          col = lab_period(period))]
la <- ar[, .(label = sprintf("r = %.2f\nCopernicus / SAA = %.2f", cor(saa_ha, copernicus_ha), sum(copernicus_ha) / sum(saa_ha))),
         by = .(row, col)]
f3 <- ggplot(ar, aes(saa_ha / 1000, copernicus_ha / 1000)) +
  geom_abline(linetype = 2, colour = "grey50", linewidth = 0.3) +
  geom_point(alpha = 0.35, size = 0.6, colour = pal_cat[1], stroke = 0) +
  geom_text(data = la, aes(x = -Inf, y = Inf, label = label), hjust = -0.05, vjust = 1.1, size = 2.3, lineheight = 0.9) +
  facet_grid(row ~ col, scales = "free") +
  labs(x = "Area in French agricultural statistics, SAA (thousand ha)",
       y = "Area of the Copernicus mask (thousand ha)") + theme_paper()
save_fig(f3, "fig3_mask_areas", height_cm = 11)

msg("Done: step 2 outputs in ", dir_out2)
