# =============================================================================
# 03_prediction_eu.R - Step 3: yearly forage yields for EU27 + UK since 2004
# Applies the step 2 model of each year's period to the step 1 regional DMP:
#   green maize      <- maize DMP
#   permanent grass  <- grassland DMP
#   artificial grass <- permanent grass x ratio artificial/permanent (step 2)
#   temporary grass  <- permanent grass x ratio temporary/permanent  (step 2)
# Units: tDM/ha/yr. Rows cover every NUTS code of every version used; values
# are blank in years when the code does not exist in the applicable NUTS
# version (UK in 2024: NUTS 2021 codes, see nuts_missing_fallback), and where
# step 1 left the region blank (no valid DMP even after the fallbacks).
# Predictions are capped to plausible bounds (section 1).
# Outputs (outputs/step3/):
#   forage_yields_EU.xlsx      sheets NUTS2 and NUTS3
#   national_summary_NUTS2.xlsx / .csv  (mean, min, max, p5 ... p95 by
#                              country, forage type and year, and all years)
# and outputs/paper/: Figures 1 and 4-6, Tables 1, 4 and 5 of the working paper.
# =============================================================================
source("00_config.R")

# ---- 1. Load the step 2 models and yield ratios -----------------------------
st <- readRDS(file.path(dir_out2, "models.rds"))
models <- st$models; ratios <- st$ratios

# Cap predictions to the range of Smit et al. (2008) for grass, and to p1-p99 of
# Eurostat NUTS2 yields 2000-2024 for green maize (converted to tDM with 65%
# moisture, as recommended in the 2023 Eurostat crop production statistics handbook)
predict_model_capped <- function(m, x, crop) {
  min_yield<-ifelse(crop=="grass",0.1,4.7)
  max_yield<-ifelse(crop=="grass",11,23.9)
  pmin(pmax(st$predict_model(m,x),min_yield),max_yield)
}

ratio_of <- function(g) ratios[grass_type == paste0(g, "/permanent"), ratio]

# ---- 2. Prediction function (used for NUTS3 and NUTS2) ----------------------
# Takes the step 1 table (one row per region x year x forage type), puts maize
# and grass DMP side by side, applies the model of each year's period, flags
# DMP values outside the French calibration range, and derives artificial and
# temporary grass from permanent grass with the SAA ratios.
predict_level <- function(d, id_cols) {
  w <- dcast(d, as.formula(paste(paste(c(id_cols, "year", "nuts_version"), collapse = " + "), "~ forage")),
             value.var = c("dmp", "n_pixels", "dmp_source"))
  w[, period := period_of_year(year)]
  for (p in unique(w$period)) {
    i <- w$period == p
    mm <- models[[p]]$green_maize
    w[i, green_maize := predict_model_capped(mm, dmp_maize,"green_maize")]
    w[i, maize_dmp_outside_FR_range := dmp_maize < mm$x_range[1] | dmp_maize > mm$x_range[2]]
    gm <- models[[p]]$permanent_grass
    x <- w$dmp_grass[i]; ok <- !is.na(x)
    pr <- rep(NA_real_, sum(i))
    if (any(ok)) pr[ok] <- predict_model_capped(gm, x[ok],"grass")
    w[i, permanent_grass := pr]
    w[i, grass_dmp_outside_FR_range := dmp_grass < gm$x_range[1] | dmp_grass > gm$x_range[2]]
  }
  w[, artificial_grass := permanent_grass * ratio_of("artificial")]
  w[, temporary_grass  := permanent_grass * ratio_of("temporary")]
  w
}

# ---- 3. Predict for all EU regions ------------------------------------------
r3 <- fread(file.path(dir_out1, "regional_dmp_nuts3.csv"), encoding = "UTF-8")
r2 <- fread(file.path(dir_out1, "regional_dmp_nuts2.csv"), encoding = "UTF-8")
p3 <- predict_level(r3, c("cntr", "nuts2", "nuts3", "name"))
p2 <- predict_level(r2, c("cntr", "nuts2", "name"))

# ---- 4. One row per NUTS code x year, blanks where the code does not exist --
# (NUTS codes change between versions: a code is blank in the years when it is
# not part of the applicable NUTS version, see CLAUDE.md)
full_grid <- function(p, code) {
  codes <- unique(p[, c(code, "cntr", "name"), with = FALSE], by = code)
  g <- CJ(code = codes[[code]], year = years_all); setnames(g, "code", code)
  g <- merge(g, codes, by = code)
  g[, nuts_version := nuts_version_of_year(year)]
  merge(g, p[, -c("cntr", "name", "nuts_version")], by = c(code, "year"), all.x = TRUE)
}
out_cols <- c("green_maize", "artificial_grass", "temporary_grass", "permanent_grass")
p3 <- full_grid(p3, "nuts3")[, c("cntr", "nuts2", "nuts3", "name", "year", "nuts_version", "period", out_cols,
                                 "n_pixels_maize", "n_pixels_grass", "dmp_source_maize", "dmp_source_grass",
                                 "maize_dmp_outside_FR_range", "grass_dmp_outside_FR_range"), with = FALSE]
p3[, nuts2 := substr(nuts3, 1, 4)]
p2 <- full_grid(p2, "nuts2")[, c("cntr", "nuts2", "name", "year", "nuts_version", "period", out_cols,
                                 "n_pixels_maize", "n_pixels_grass", "dmp_source_maize", "dmp_source_grass",
                                 "maize_dmp_outside_FR_range", "grass_dmp_outside_FR_range"), with = FALSE]
setorder(p3, nuts3, year); setorder(p2, nuts2, year)

# ---- 5. Check for negative predictions (possible with a linear model) -----
neg <- p2[, lapply(.SD, function(x) sum(x < 0, na.rm = TRUE)), .SDcols = out_cols]
if (any(unlist(neg) > 0)) { warning("Negative predicted yields in NUTS2 table (count by type):"); print(neg) }

# ---- 6. Output a): Excel file with NUTS2 and NUTS3 sheets + README ---------
# (also saved as csv)
readme <- data.table(note = c(
  "Estimated regional yearly forage yields, tDM/ha/yr (see CLAUDE.md for the method).",
  "Columns green_maize ... permanent_grass: predicted yields; blank = region absent from the applicable NUTS version, or no retained pixel.",
  sprintf("artificial_grass = permanent_grass x %.3f, temporary_grass = permanent_grass x %.3f (mean SAA yield ratios, see outputs/step2/yield_ratios.csv).",
          ratio_of("artificial"), ratio_of("temporary")),
  sprintf("n_pixels_*: Copernicus DMP pixels with more than %d%% of their area covered by the forage type in the region.", round(100 * mask_min_cover)),
  "dmp_source_*: level whose DMP was used: NUTS3, NUTS2, NUTS1, NUTS0, NEIGHBOUR_GRASS / NEIGHBOUR_MAIZE (national DMP of the neighbouring country, see neighbour_country in 00_config.R).",
  "DMP = sum over the growing season (1 March - 30 September).",
  "Region names are in plain ASCII (accents removed).",
  "UK: absent from NUTS 2024, the UK keeps its NUTS 2021 regions and codes in 2024.",
  "*_dmp_outside_FR_range: TRUE = regional DMP outside the range observed in the French calibration data (extrapolation).",
  "period: A = up to 2013 (DMP 1 km, CLC masks), B = 2014-2016 (DMP 300 m, HRL 2017 masks), C = 2017 onwards (DMP 300 m, year-specific HRL masks).",
  "Models: OLS of SAA yield on DMP, unweighted (see outputs/step2)."))
wb <- openxlsx::createWorkbook()
for (s in list(list("NUTS2", p2), list("NUTS3", p3), list("README", readme))) {
  openxlsx::addWorksheet(wb, s[[1]])
  openxlsx::writeData(wb, s[[1]], s[[2]])
  openxlsx::freezePane(wb, s[[1]], firstRow = TRUE)
}
add_metadata_sheet(wb, "forage_yields_EU.xlsx", list(NUTS2 = p2, NUTS3 = p3))
openxlsx::saveWorkbook(wb, file.path(dir_out3, "forage_yields_EU.xlsx"), overwrite = TRUE)
write_csv(p2, file.path(dir_out3, "forage_yields_EU_NUTS2.csv"))
write_csv(p3, file.path(dir_out3, "forage_yields_EU_NUTS3.csv"))

# ---- 7. Output b): national summary statistics of NUTS2 yearly yields -----
# Per country x forage type: mean, min, max and percentiles of NUTS2 values,
# year by year and all years together.
long2 <- melt(p2, id.vars = c("cntr", "nuts2", "year"), measure.vars = out_cols,
              variable.name = "forage_type", value.name = "yield")[!is.na(yield)]
stats <- function(x) list(n_nuts2 = length(x), mean = mean(x), min = min(x), max = max(x),
                          p5 = quantile(x, .05, names = FALSE), p25 = quantile(x, .25, names = FALSE),
                          p50 = quantile(x, .50, names = FALSE), p75 = quantile(x, .75, names = FALSE),
                          p95 = quantile(x, .95, names = FALSE))
by_year <- long2[, stats(yield), by = .(cntr, forage_type, year)][order(cntr, forage_type, year)]
all_years <- long2[, stats(yield), by = .(cntr, forage_type)][order(cntr, forage_type)]
write_csv(by_year, file.path(dir_out3, "national_summary_NUTS2_by_year.csv"))
write_csv(all_years, file.path(dir_out3, "national_summary_NUTS2_all_years.csv"))
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "by_year");   openxlsx::writeData(wb, "by_year", by_year)
openxlsx::addWorksheet(wb, "all_years"); openxlsx::writeData(wb, "all_years", all_years)
add_metadata_sheet(wb, "national_summary_NUTS2.xlsx", list(by_year = by_year, all_years = all_years))
openxlsx::saveWorkbook(wb, file.path(dir_out3, "national_summary_NUTS2.xlsx"), overwrite = TRUE)

# ---- 8. Figures and tables of the working paper (outputs/paper/) -----------
library(ggplot2)
yrs_c <- years_all[period_of_year(years_all) == "C"]

# Figure 1: workflow (input data in grey, processing steps in white, output in black)
wf <- data.table(
  x = c(1, 1, 1, 1, 2.75, 2.75, 2.75, 4.5, 4.5),
  y = c(3.6, 2.6, 1.9, 0.8, 3.6, 2.4, 0.8, 2.4, 0.8),
  label = c("Land-cover masks\nCORINE Land Cover, HRL\ngrassland and crop types",
            "Copernicus DMP\n10-daily, 1 km (2004-2013)\nand 300 m (2014-2024)",
            "NUTS boundaries\nGISCO, 2003-2024 versions",
            "French statistics (SAA)\ndepartements, 2010-2024",
            "01c: share of grassland and\nmaize in each DMP pixel",
            "01d: seasonal DMP (Mar-Sep)\nof retained pixels, by NUTS3\nand NUTS2, with fallbacks",
            "02: OLS yield ~ DMP by period\n+ ratios of temporary and\nartificial to permanent grass",
            "03: yields of the 4 forage\ntypes, NUTS2 and NUTS3,\n2004-2024",
            "Quality indicators\n(fit, areas, coverage)"),
  type = c(rep("input", 4), rep("step", 3), "output", "output"))
# arrows: start and end points (x0, y0) -> (x1, y1), drawn between box edges
ar <- data.table(x0 = c(1.70, 1.70, 1.70, 2.75, 1.70, 2.75, 3.42, 3.42, 3.42),
                 y0 = c(3.60, 2.60, 1.90, 3.40, 0.80, 2.15, 2.40, 1.00, 0.80),
                 x1 = c(2.08, 2.08, 2.08, 2.75, 2.08, 2.75, 3.97, 3.97, 4.05),
                 y1 = c(3.60, 2.50, 2.28, 2.67, 0.80, 1.07, 2.40, 2.15, 0.80))
f1 <- ggplot() +
  geom_segment(data = ar, aes(x = x0, y = y0, xend = x1, yend = y1),
               arrow = arrow(length = unit(1.5, "mm"), type = "closed"), colour = "grey40", linewidth = 0.3) +
  geom_label(data = wf, aes(x, y, label = label, fill = type, colour = type), size = 2.3, lineheight = 0.9,
             label.r = unit(0, "mm"), label.padding = unit(1.5, "mm"), hjust = 0.5) +
  scale_fill_manual(values = c(input = "grey85", step = "white", output = "grey15"), guide = "none") +
  scale_colour_manual(values = c(input = "black", step = "black", output = "white"), guide = "none") +
  coord_cartesian(xlim = c(0.25, 5.25), ylim = c(0.35, 4.05), expand = FALSE) + theme_void()
save_fig(f1, "fig1_workflow", height_cm = 8)

# Table 1 (data): input data by year - DMP sensor and grid, masks, retained pixels
files <- fread(file.path(dir_out1, "dmp_files_used.csv"), encoding = "UTF-8")
files[, sensor := sub("_V[0-9].*$", "", sub("^.*_GLOBE_", "", id))]
files[, sensor := c(VGT = "SPOT-VGT", PROBAV = "PROBA-V", OLCI = "Sentinel-3 OLCI")[sensor]]
sens <- files[, .(dmp_sensor = paste(unique(sensor), collapse = " + "), n_dekads = .N), by = year]
msk <- fread(file.path(dir_out1, "mask_sources_by_year.csv"), encoding = "UTF-8")
msk <- dcast(msk, year + grid ~ forage, value.var = c("src_type", "src_year_used"))
msk[, `:=`(grass_mask = paste(src_type_grass, src_year_used_grass),
           maize_mask = paste(src_type_maize, src_year_used_maize))]
pix <- dcast(r3[, .(n = sum(n_pixels, na.rm = TRUE)), by = .(year, forage)], year ~ forage, value.var = "n")
setnames(pix, c("grass", "maize"), c("n_pixels_grass", "n_pixels_maize"))
t1 <- Reduce(function(a, b) merge(a, b, by = "year"),
             list(sens, msk[, .(year, dmp_grid = grid, grass_mask, maize_mask)], pix))
t1[, `:=`(period = period_of_year(year), french_statistics = year >= min(saa_fds_years, 2010))]
write_csv(t1, file.path(dir_paper, "table1_inputs_by_year.csv"))
save_table(t1[, .(Year = year, Period = period, `DMP sensor` = dmp_sensor, `DMP grid` = dmp_grid,
                  `Grassland mask` = grass_mask, `Maize mask` = maize_mask,
                  `Grassland pixels` = fmt(n_pixels_grass, 0), `Maize pixels` = fmt(n_pixels_maize, 0),
                  `French statistics` = ifelse(french_statistics, "yes", "no"))],
           "table1_inputs_by_year", align = c("l", "l", "l", "l", "l", "l", "r", "r", "l"))

# Figure 4: number of retained pixels per year (log scale)
f4 <- ggplot(melt(t1, id.vars = "year", measure.vars = c("n_pixels_grass", "n_pixels_maize")),
             aes(year, value, colour = variable)) +
  break_lines() + geom_line(linewidth = 0.6) + geom_point(size = 1.2) + scale_years() +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  scale_colour_manual(values = pal_cat[1:2], labels = c("Grassland", "Maize"), name = NULL) +
  labs(x = NULL, y = "Retained pixels (log scale)") + theme_paper()
save_fig(f4, "fig4_retained_pixels", height_cm = 7, width_cm = 12)

# Table 4: origin of the regional DMP (share of NUTS3-years by fallback level)
t4 <- r3[, .N, by = .(forage, period = period_of_year(year), dmp_source)]
t4[, share := N / sum(N), by = .(forage, period)]
t4 <- dcast(t4, forage + period ~ dmp_source, value.var = "share", fill = 0)
t4[, years := period_label[period]]
write_csv(t4, file.path(dir_paper, "table4_dmp_origin.csv"))
t4i <- t4[, .(Forage = forage_label[forage], Years = years, NUTS3 = pct(NUTS3), NUTS2 = pct(NUTS2), NUTS1 = pct(NUTS1),
              Country = pct(NUTS0),
              `Neighbouring country` = pct((if ("NEIGHBOUR_GRASS" %in% names(t4)) NEIGHBOUR_GRASS else 0) +
                                           (if ("NEIGHBOUR_MAIZE" %in% names(t4)) NEIGHBOUR_MAIZE else 0)))]
save_table(t4i, "table4_dmp_origin", width_cm = 15, align = c("l", "l", rep("r", 5)))

# Yields with area weights (Copernicus mask area of the NUTS2 region and year)
w2 <- dcast(r2[, .(nuts2, year, forage, copernicus_area_ha)], nuts2 + year ~ forage, value.var = "copernicus_area_ha")
setnames(w2, c("grass", "maize"), c("area_grass", "area_maize"))
yl <- merge(p2[, .(cntr, nuts2, year, green_maize, permanent_grass)], w2, by = c("nuts2", "year"))
yl <- rbind(yl[, .(cntr, nuts2, year, forage = "green_maize", yield = green_maize, area = area_maize)],
            yl[, .(cntr, nuts2, year, forage = "permanent_grass", yield = permanent_grass, area = area_grass)])[
              !is.na(yield) & !is.na(area) & area > 0]

# Figure 5: yearly yields, EU and the 5 countries with the largest area of each
# forage in period C; one panel and one colour legend per forage type
top <- yl[year %in% yrs_c, .(area = sum(area)), by = .(forage, cntr)][order(-area), head(.SD, 5), by = forage]
trend <- rbind(yl[, .(cntr = "EU27 + UK", yield = weighted.mean(yield, area)), by = .(forage, year)],
               yl[top, on = .(forage, cntr), nomatch = 0][, .(yield = weighted.mean(yield, area)), by = .(forage, cntr, year)])
trend_panel <- function(k, tag) {
  cty <- top[forage == k, cntr]                                  # by decreasing area
  cols <- c("EU27 + UK" = "black", setNames(pal_cat[seq_along(cty)], cty))
  d <- trend[forage == k][, cntr := factor(cntr, names(cols))]
  ggplot(d, aes(year, yield, colour = cntr, linewidth = cntr == "EU27 + UK")) +
    break_lines() + geom_line() + scale_years() +
    scale_colour_manual(values = cols, name = NULL) +
    scale_linewidth_manual(values = c(`FALSE` = 0.4, `TRUE` = 1), guide = "none") +
    labs(x = NULL, y = "Yield (t DM/ha/yr)", title = paste(tag, forage_label[k])) + theme_paper() +
    theme(plot.title = element_text(size = 8, face = "bold")) +
    guides(colour = guide_legend(nrow = 2))
}
save_side_by_side(list(trend_panel("green_maize", "a)"), trend_panel("permanent_grass", "b)")),
                  "fig5_time_trends", height_cm = 9)
write_csv(trend[, .(forage, cntr, year, yield)], file.path(dir_paper, "fig5_time_trends_data.csv"))

# Figure 6: mean yields of period C by NUTS3 region (NUTS 2021 boundaries).
# Dot: low-confidence regions, i.e. in most years of the period the DMP
# comes from the NUTS1, country or neighbouring country (fewer than min_pixels
# pixels below), or lies outside the French calibration range.
low_src <- c("NUTS1", "NUTS0", "NEIGHBOUR_GRASS", "NEIGHBOUR_MAIZE")
mp <- p3[year %in% yrs_c, .(green_maize = mean(green_maize, na.rm = TRUE),
                            permanent_grass = mean(permanent_grass, na.rm = TRUE),
                            low_green_maize = mean(dmp_source_maize %in% low_src | maize_dmp_outside_FR_range %in% TRUE) > 0.5,
                            low_permanent_grass = mean(dmp_source_grass %in% low_src | grass_dmp_outside_FR_range %in% TRUE) > 0.5),
         by = nuts3]
geo <- st_transform(st_read(file.path(dir_nuts, "NUTS3_EU_2021.gpkg"), quiet = TRUE), 3035)
map_panel <- function(k, tag) {
  g <- merge(geo, mp[, .(nuts3, value = get(k), low = get(paste0("low_", k)))], by.x = "nuts_id", by.y = "nuts3", all.x = TRUE)
  # discrete classes: about 7 rounded breaks within the 2nd-98th percentiles
  br <- pretty(quantile(g$value, c(0.02, 0.98), na.rm = TRUE), n = 7)
  br <- br[br > min(g$value, na.rm = TRUE) & br < max(g$value, na.rm = TRUE)]
  ggplot(g) + geom_sf(aes(fill = value), colour = "white", linewidth = 0.05) +
    geom_sf(data = suppressWarnings(st_point_on_surface(g[g$low %in% TRUE, ])), size = 0.25, colour = "grey15", shape = 16) +
    scale_fill_stepsn(colours = ramp_2col(length(br) + 1), breaks = br, limits = range(g$value, na.rm = TRUE),
                      na.value = "grey80", name = paste0(tag, " ", forage_label[k], "\n(t DM/ha/yr)")) +
    coord_sf(xlim = c(2.5e6, 7.4e6), ylim = c(1.35e6, 5.45e6), expand = FALSE, datum = NA) +
    theme_void(8) + theme(legend.position = "bottom", legend.key.width = unit(1.2, "cm"),
                           legend.title = element_text(hjust = 0.5)) +
    guides(fill = guide_coloursteps(title.position = "top", even.steps = TRUE))
}
save_side_by_side(list(map_panel("green_maize", "a)"), map_panel("permanent_grass", "b)")),
                  "fig6_maps_2017_2024", height_cm = 10.5)


# Table 5: national summary for period C - mean Copernicus area, area-weighted
# mean yield, 5th and 95th percentiles of NUTS2 yearly yields
t5 <- yl[year %in% yrs_c, .(area_kha = sum(area) / uniqueN(year) / 1000, mean = weighted.mean(yield, area),
                            p5 = quantile(yield, .05, names = FALSE), p95 = quantile(yield, .95, names = FALSE),
                            n_nuts2_years = .N), by = .(cntr, forage)]
t5 <- dcast(t5, cntr ~ forage, value.var = c("area_kha", "mean", "p5", "p95", "n_nuts2_years"))
write_csv(t5, file.path(dir_paper, "table5_national_summary_period_C.csv"))
save_table(t5[, .(Country = cntr,
                  `Maize area (kha)` = fmt(area_kha_green_maize, 0), `Maize mean` = fmt(mean_green_maize, 1),
                  `Maize p5` = fmt(p5_green_maize, 1), `Maize p95` = fmt(p95_green_maize, 1),
                  `Grassland area (kha)` = fmt(area_kha_permanent_grass, 0), `Grassland mean` = fmt(mean_permanent_grass, 1),
                  `Grassland p5` = fmt(p5_permanent_grass, 1), `Grassland p95` = fmt(p95_permanent_grass, 1))],
           "table5_national_summary_period_C")

msg("Done: step 3 outputs in ", dir_out3)
