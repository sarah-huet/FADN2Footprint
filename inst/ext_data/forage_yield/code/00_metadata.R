# =============================================================================
# 00_metadata.R - dictionary of the variables written in the outputs
# Used by write_csv() and add_metadata_sheet() (00_functions.R): each CSV gets
# a companion <name>_metadata.csv and each Excel file a "metadata" sheet, with
# the description, unit and source of every column.
# Columns of the dictionary:
#   file        regular expression on the output file name ("" = any file);
#               file-specific entries take precedence over general ones
#   variable    column name, or a regular expression if it starts with "^"
#   description what the variable is; for coded variables with few, non-obvious
#               values, the meaning of each value
#   unit
#   source      where the data come from (raw data set, or script computing it)
# A column missing from the dictionary is written as "UNDOCUMENTED" (with a
# warning): add it here.
# =============================================================================

# ---- Sources ------------------------------------------------------------------
SAA_FOU <- "Agreste SAA 2010-2025, departemental data (SAA_2010-2025_provisoires_donnees_departementales.xlsx), sheet FOU"
SAA_COP <- "Agreste SAA 2010-2025, departemental data (SAA_2010-2025_provisoires_donnees_departementales.xlsx), sheet COP"
DMP     <- "Copernicus Land Monitoring Service, Dry Matter Productivity 10-daily (CDSE: 1 km v2 before 2014, 300 m v2 from 2014)"
MASKS   <- "Copernicus: CORINE Land Cover 100 m (2006, 2012) before the HRL; HRL Grassland 100 m and HRL Crop Types 10 m (2017+)"
NUTS    <- "Eurostat GISCO, NUTS maps (EU-wide, 1:1 million)"
CDSE    <- "Copernicus Data Space Ecosystem, STAC catalogue"
comp    <- function(script, what = NULL) paste0("Computed in ", script, if (!is.null(what)) paste0(" from ", what))

var_dict <- rbindlist(list(
  # ---- Identifiers shared by many files --------------------------------------
  list("", "year", "Year of the data (for DMP: growing season 1 March - 30 September of that year)", "year", "-"),
  list("", "period", "Calibration period. Values: A = up to 2013 (DMP 1 km SPOT-VGT, CLC masks); B = 2014-2016 (DMP 300 m PROBA-V, HRL 2017 masks); C = 2017 onwards (DMP 300 m PROBA-V then Sentinel-3 OLCI from 2019, year-specific HRL masks); AB = 2004-2016 pooled (robustness check in step 2 only, not used for predictions)", "-", comp("00_config.R (period_of_year)")),
  list("", "nuts_version", "NUTS version used for the year (latest version not more recent than the year). Exception: the UK, absent from NUTS 2024, keeps its NUTS 2021 regions and codes in 2024 (nuts_missing_fallback in 00_config.R)", "year of the NUTS version", comp("00_config.R (nuts_version_of_year)")),
  list("", "cntr", "Country code (Eurostat codes: EL = Greece, UK = United Kingdom)", "-", NUTS),
  list("", "nuts2", "NUTS2 region code", "-", NUTS),
  list("", "nuts3", "NUTS3 region code", "-", NUTS),
  list("", "name", "Region name, transliterated to plain ASCII (accents removed)", "-", NUTS),
  list("", "name_utf8", "Region name with its original spelling (UTF-8)", "-", NUTS),
  list("", "forage", "Copernicus mask. Values: grass = grassland; maize = maize (green and grain maize together)", "-", MASKS),
  list("", "dep", "SAA departement code", "-", SAA_FOU),
  list("", "dep_name", "SAA departement name, plain ASCII", "-", SAA_FOU),
  list("", "model", "Forage type modelled. Values: green_maize = SAA line 06; permanent_grass = SAA line 17", "-", comp("02_calibration_france.R")),

  # ---- Step 1 ------------------------------------------------------------------
  list("nuts_versions_missing_countries", "version", "NUTS version", "year of the NUTS version", NUTS),
  list("nuts_versions_missing_countries", "missing_countries", "EU27 + UK countries absent from this NUTS version (blank = none)", "country codes", comp("01a_nuts_maps.R", "the NUTS maps")),
  list("nuts_versions_missing_countries", "filled_from_earlier_version", "Missing countries whose regions are taken from an earlier NUTS version (nuts_missing_fallback in 00_config.R), with that version; e.g. UK (NUTS 2021) in NUTS 2024", "-", comp("01a_nuts_maps.R", "the NUTS maps")),
  list("mask_sources_by_year", "src_type", "Mask source. Values: CLC = CORINE Land Cover; HRL = Copernicus High Resolution Layer (2017+)", "-", comp("00_config.R (mask_source)")),
  list("mask_sources_by_year", "src_year", "Mask year required by the rules (CLC: closest vintage, ties to the most recent; HRL: max(2017, year))", "year", comp("00_config.R (mask_source)")),
  list("mask_sources_by_year", "src_year_used", "Mask year actually used (latest available if the required year is not published)", "year", comp("01c_masks.R", "the CDSE catalogue")),
  list("mask_sources_by_year", "grid", "DMP grid the mask is computed on. Values: 1km = DMP 1 km product (before 2014); 300m = DMP 300 m product (from 2014)", "-", comp("00_functions.R (grid_of_year)")),
  list("mask_sources_by_year", "mask_file", "Path of the mask file (share of each DMP pixel covered by the forage type)", "-", comp("01c_masks.R", MASKS)),
  list("dmp_files_used", "collection", "STAC collection of the DMP product", "-", CDSE),
  list("dmp_files_used", "id", "Identifier of the 10-daily DMP file used", "-", CDSE),
  list("dmp_files_used", "start", "First day of the dekad", "date", CDSE),
  list("dmp_files_used", "end", "Last day of the dekad", "date", CDSE),
  list("dmp_files_used", "days", "Number of days in the dekad", "days", comp("00_functions.R (select_dekads)")),
  list("dmp_files_used", "rt", "Consolidation level of the file kept for the dekad. Values: 0 to 6 = RT0 (near real time) to RT6 (final); 99 = archive product without RT level", "-", CDSE),
  list("regional_dmp", "mask", "Mask file used for the year and forage type", "-", comp("01c_masks.R", MASKS)),
  list("regional_dmp", "n_pixels", "Number of retained DMP pixels in the region (cover share > mask_min_cover, pixel centre in the region)", "pixels", comp("01d_regional_dmp.R", "the masks and NUTS maps")),
  list("regional_dmp", "dmp_source", "Level whose DMP is used: the first one with >= min_pixels retained pixels and a valid DMP. Values: NUTS3, NUTS2, NUTS1, NUTS0 = region itself or its parent region; NEIGHBOUR_GRASS / NEIGHBOUR_MAIZE = national grassland / maize DMP of the neighbouring country (neighbour_country in 00_config.R)", "-", comp("01d_regional_dmp.R")),
  list("regional_dmp", "dmp", "Dry matter productivity over the growing season (1 March - 30 September), after fallback (see dmp_source)", "t DM/ha", comp("01d_regional_dmp.R", DMP)),
  list("regional_dmp", "dmp_own", "Dry matter productivity of the region's own pixels, before fallback (blank if no retained pixel or no valid data)", "t DM/ha", comp("01d_regional_dmp.R", DMP)),
  list("regional_dmp", "copernicus_area_ha", "Area of the forage type in the region according to the Copernicus mask (cover share x pixel area, all pixels)", "ha", comp("01d_regional_dmp.R", MASKS)),
  list("pixels_retained", "^[0-9]{4}$", "Number of retained DMP pixels in the NUTS2 region in that year (cells < min_pixels highlighted in the Excel file)", "pixels", comp("01d_regional_dmp.R", "the masks and NUTS maps")),

  # ---- Step 2 ------------------------------------------------------------------
  list("calibration_data", "key", "Normalised departement name used to match SAA departements with NUTS3 regions", "-", comp("02_calibration_france.R")),
  list("calibration_data", "^SURF_(06|11|12|17|18)$", "SAA area of the line in the suffix. Values of the suffix: 06 = green maize; 11 = artificial grassland; 12 = temporary grassland; 17 = permanent grassland; 18 = all grassland", "ha", SAA_FOU),
  list("calibration_data", "^yield_(06|11|12|17|18)$", "SAA yield of the line in the suffix (REND, or PROD / SURF when REND is missing), converted from q/ha. Suffix: as for SURF_", "t DM/ha", SAA_FOU),
  list("calibration_data", "SURF_grain", "SAA area of grain maize and seed maize (line 18)", "ha", SAA_COP),
  list("calibration_data", "yield_wheat", "SAA yield of winter wheat and spelt (line 01), converted from q/ha (standard humidity of the SAA, not dry matter)", "t/ha", SAA_COP),
  list("calibration_data", "^dmp_(grass|maize)$", "Copernicus DMP over the growing season for the forage type in the suffix (after fallback)", "t DM/ha", comp("01d_regional_dmp.R", DMP)),
  list("calibration_data", "^copernicus_area_ha_(grass|maize)$", "Copernicus mask area of the forage type in the suffix", "ha", comp("01d_regional_dmp.R", MASKS)),
  list("calibration_data", "^dmp_source_(grass|maize)$", "Level whose DMP is used for the forage type in the suffix. Values: NUTS3, NUTS2, NUTS1, NUTS0, NEIGHBOUR_GRASS, NEIGHBOUR_MAIZE (see regional_dmp tables)", "-", comp("01d_regional_dmp.R")),
  list("unmatched_departements", "unmatched", "SAA departement-years without a matching NUTS3 region (year and name)", "-", comp("02_calibration_france.R")),
  list("yield_ratios", "grass_type", "Ratio computed. Values: artificial/permanent = SAA line 11 / line 17; temporary/permanent = line 12 / line 17", "-", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "n_departement_years", "Number of departement x year used (>= 10,000 ha of each of the two grass types)", "count", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "n_departements", "Number of distinct departements used", "count", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "years", "First and last year of the data used", "years", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "ratio", "Mean of the yield ratio over the departement x year used (yield_ratios_data: ratio of that departement-year)", "ratio (t DM/ha per t DM/ha)", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "sd", "Standard deviation of the yield ratio", "ratio", comp("02_calibration_france.R", SAA_FOU)),
  list("yield_ratios", "line", "SAA line of the numerator. Values: 11 = artificial grassland; 12 = temporary grassland", "-", SAA_FOU),
  list("fit_metrics", "n", "Number of departement x year used in the fit", "count", comp("02_calibration_france.R")),
  list("fit_metrics", "R2", "Coefficient of determination (unweighted)", "-", comp("02_calibration_france.R")),
  list("fit_metrics", "R2_weighted", "Coefficient of determination weighted by the SAA area of the forage type", "-", comp("02_calibration_france.R")),
  list("fit_metrics", "MAE", "Mean absolute error of the predicted yield", "t DM/ha", comp("02_calibration_france.R")),
  list("fit_metrics", "MAE_weighted", "Mean absolute error weighted by the SAA area of the forage type", "t DM/ha", comp("02_calibration_france.R")),
  list("fit_metrics|fit_observed", "predictor", "Explanatory variable. Values: DMP = Copernicus DMP over the growing season; wheat yield = SAA winter wheat yield (benchmark)", "-", comp("02_calibration_france.R")),
  list("fit_metrics|fit_observed", "sample", "Departement-years used. Values: all = all with a NUTS3-level DMP (model used in step 3); common = those with both DMP and wheat yield (benchmark)", "-", comp("02_calibration_france.R")),
  list("fit_metrics", "spec", "Fitted equation", "-", comp("02_calibration_france.R")),
  list("fit_observed", "y", "Observed SAA yield", "t DM/ha", SAA_FOU),
  list("fit_observed", "x", "Value of the predictor (DMP: t DM/ha over the growing season; wheat yield: t/ha)", "see description", paste(comp("01d_regional_dmp.R", DMP), "/", SAA_COP)),
  list("fit_observed", "w", "SAA area of the forage type (point size in the figures)", "ha", SAA_FOU),
  list("fit_observed", "z", "Level whose DMP is used (only NUTS3 is kept in the fit)", "-", comp("01d_regional_dmp.R")),
  list("fit_observed", "pred", "Predicted yield", "t DM/ha", comp("02_calibration_france.R")),
  list("model_coefficients", "term", "Coefficient. Values: intercept; dmp = slope on the DMP", "-", comp("02_calibration_france.R")),
  list("model_coefficients", "estimate", "Estimated coefficient (intercept in t DM/ha; slope in t DM/ha of yield per t DM/ha of DMP)", "see description", comp("02_calibration_france.R")),
  list("model_coefficients", "std_error", "Standard error of the coefficient", "same as estimate", comp("02_calibration_france.R")),
  list("model_coefficients", "t_value", "t statistic", "-", comp("02_calibration_france.R")),
  list("model_coefficients", "p_value", "p-value (two-sided)", "-", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "type", "Area compared. Values: maize (green + grain) = SAA FOU line 06 + COP line 18 vs maize mask; grassland (line 18) = SAA FOU line 18 vs grassland mask", "-", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "saa_ha", "SAA area", "ha", paste(SAA_FOU, "/", SAA_COP)),
  list("area_saa_vs_copernicus", "copernicus_ha", "Copernicus mask area", "ha", comp("01d_regional_dmp.R", MASKS)),
  list("area_saa_vs_copernicus", "diff_ha", "Copernicus area minus SAA area", "ha", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "ratio", "Copernicus area / SAA area", "ratio", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "saa_total_ha", "Total SAA area, metropolitan France", "ha", comp("02_calibration_france.R", "SAA areas")),
  list("area_saa_vs_copernicus", "copernicus_total_ha", "Total Copernicus mask area, metropolitan France", "ha", comp("02_calibration_france.R", "Copernicus mask areas")),
  list("area_saa_vs_copernicus", "correlation_nuts3", "Correlation between SAA and Copernicus areas across departements", "-", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "median_ratio_nuts3", "Median across departements of Copernicus area / SAA area", "ratio", comp("02_calibration_france.R")),
  list("area_saa_vs_copernicus", "ratio_total", "Total Copernicus area / total SAA area", "ratio", comp("02_calibration_france.R")),

  # ---- Step 3 ------------------------------------------------------------------
  list("forage_yields", "green_maize", "Estimated green maize yield (step 2 model on maize DMP, capped to [4.7, 23.9])", "t DM/ha/yr", comp("03_prediction_eu.R", "the step 2 model and the step 1 DMP")),
  list("forage_yields", "permanent_grass", "Estimated permanent grassland yield (step 2 model on grassland DMP, capped to [0.1, 11])", "t DM/ha/yr", comp("03_prediction_eu.R", "the step 2 model and the step 1 DMP")),
  list("forage_yields", "artificial_grass", "Estimated artificial grassland yield = permanent_grass x SAA ratio artificial/permanent", "t DM/ha/yr", comp("03_prediction_eu.R", "permanent_grass and the step 2 yield ratios")),
  list("forage_yields", "temporary_grass", "Estimated temporary grassland yield = permanent_grass x SAA ratio temporary/permanent", "t DM/ha/yr", comp("03_prediction_eu.R", "permanent_grass and the step 2 yield ratios")),
  list("forage_yields", "^n_pixels_(maize|grass)$", "Number of retained DMP pixels of the forage type in the suffix in the region", "pixels", comp("01d_regional_dmp.R", "the masks and NUTS maps")),
  list("forage_yields", "^dmp_source_(maize|grass)$", "Level whose DMP is used for the forage type in the suffix. Values: NUTS3, NUTS2, NUTS1, NUTS0 = region itself or its parent region; NEIGHBOUR_GRASS / NEIGHBOUR_MAIZE = national grassland / maize DMP of the neighbouring country (neighbour_country in 00_config.R)", "-", comp("01d_regional_dmp.R")),
  list("forage_yields", "^(maize|grass)_dmp_outside_FR_range$", "TRUE if the DMP is outside the range observed in the French calibration data (prediction is an extrapolation)", "TRUE/FALSE", comp("03_prediction_eu.R")),
  list("forage_yields", "note", "Explanatory notes", "-", comp("03_prediction_eu.R")),
  list("national_summary", "forage_type", "Forage type: green_maize, artificial_grass, temporary_grass or permanent_grass", "-", comp("03_prediction_eu.R")),
  list("national_summary", "n_nuts2", "Number of NUTS2 x year values summarised", "count", comp("03_prediction_eu.R")),
  list("national_summary", "^(mean|min|max)$", "Mean / minimum / maximum of the NUTS2 yields (unweighted)", "t DM/ha/yr", comp("03_prediction_eu.R", "the NUTS2 yields")),
  list("national_summary", "^p(5|25|50|75|95)$", "Percentile of the NUTS2 yields (p50 = median)", "t DM/ha/yr", comp("03_prediction_eu.R", "the NUTS2 yields")),

  # ---- Working paper (outputs/paper/) -------------------------------------------
  list("table1_inputs", "dmp_sensor", "Sensor of the DMP files used for the year (from the file names). Values: SPOT-VGT, PROBA-V, Sentinel-3 OLCI", "-", comp("03_prediction_eu.R", "dmp_files_used.csv")),
  list("table1_inputs", "n_dekads", "Number of 10-day DMP files used (growing season)", "count", comp("03_prediction_eu.R", "dmp_files_used.csv")),
  list("table1_inputs", "dmp_grid", "DMP grid. Values: 1km = DMP 1 km product; 300m = DMP 300 m product", "-", comp("00_functions.R (grid_of_year)")),
  list("table1_inputs", "^(grass|maize)_mask$", "Mask used for the forage type in the prefix: source (CLC = CORINE Land Cover; HRL = High Resolution Layer) and year", "-", comp("01c_masks.R", MASKS)),
  list("table1_inputs", "^n_pixels_(grass|maize)$", "Number of retained DMP pixels in EU27 + UK for the forage type in the suffix", "pixels", comp("01d_regional_dmp.R", "the masks and NUTS maps")),
  list("table1_inputs", "french_statistics", "TRUE if French departemental statistics (SAA) are available for calibration", "TRUE/FALSE", SAA_FOU),
  list("table2_calibration", "model", "Forage type modelled. Values: Green maize = SAA line 06; Permanent grassland = SAA line 17", "-", comp("02_calibration_france.R")),
  list("table2_calibration", "years", "Years of French data used to fit the model of the period", "years", comp("02_calibration_france.R")),
  list("table2_calibration", "intercept_se", "Intercept of the OLS model (standard error in brackets)", "t DM/ha", comp("02_calibration_france.R")),
  list("table2_calibration", "slope_se", "Slope on the seasonal DMP (standard error in brackets)", "t DM/ha per t DM/ha", comp("02_calibration_france.R")),
  list("table2_calibration", "n", "Number of departement x year used to fit the DMP model", "count", comp("02_calibration_france.R")),
  list("table2_calibration", "R2", "Coefficient of determination of the DMP model", "-", comp("02_calibration_france.R")),
  list("table2_calibration", "MAE", "Mean absolute error of the DMP model", "t DM/ha", comp("02_calibration_france.R")),
  list("table2_calibration", "n_benchmark", "Number of departement x year with both DMP and SAA winter wheat yield (benchmark sample)", "count", comp("02_calibration_france.R")),
  list("table2_calibration", "R2_benchmark", "Coefficient of determination of the benchmark model (predictor: SAA winter wheat yield)", "-", comp("02_calibration_france.R")),
  list("table2_calibration", "MAE_benchmark", "Mean absolute error of the benchmark model", "t DM/ha", comp("02_calibration_france.R")),
  list("table4_dmp_origin", "^(NUTS3|NUTS2|NUTS1|NUTS0|NEIGHBOUR_GRASS|NEIGHBOUR_MAIZE)$", "Share of NUTS3 x year whose DMP comes from this level (fallback when fewer than min_pixels retained pixels; see dmp_source in regional_dmp)", "share (0-1)", comp("03_prediction_eu.R", "regional_dmp_nuts3.csv")),
  list("table4_dmp_origin", "years", "Years of the period", "years", comp("00_config.R (period_label)")),
  list("fig5_time_trends", "cntr", "Country code, or EU27 + UK", "-", NUTS),
  list("fig5_time_trends", "forage", "Forage type. Values: green_maize; permanent_grass", "-", comp("03_prediction_eu.R")),
  list("fig5_time_trends", "yield", "Mean of NUTS2 yields weighted by the Copernicus mask area of the forage type", "t DM/ha/yr", comp("03_prediction_eu.R")),
  list("table5_national", "^area_kha_(green_maize|permanent_grass)$", "Mean yearly Copernicus mask area (maize mask for green_maize, grassland mask for permanent_grass)", "thousand ha", comp("03_prediction_eu.R", MASKS)),
  list("table5_national", "^mean_(green_maize|permanent_grass)$", "Mean of NUTS2 yearly yields weighted by the Copernicus mask area", "t DM/ha/yr", comp("03_prediction_eu.R")),
  list("table5_national", "^p(5|95)_(green_maize|permanent_grass)$", "5th / 95th percentile of NUTS2 yearly yields (unweighted)", "t DM/ha/yr", comp("03_prediction_eu.R")),
  list("table5_national", "^n_nuts2_years_(green_maize|permanent_grass)$", "Number of NUTS2 x year values", "count", comp("03_prediction_eu.R"))
))
setnames(var_dict, c("file", "variable", "description", "unit", "source"))

# Description, unit and source of the columns `vars` of the output file `file`
describe_vars <- function(vars, file) {
  f <- basename(file)
  out <- rbindlist(lapply(vars, function(v) {
    cand <- var_dict[vapply(file, function(p) p == "" || grepl(p, f), logical(1)) &
                       ifelse(startsWith(variable, "^"), vapply(variable, function(r) grepl(r, v), logical(1)), variable == v)]
    if (!nrow(cand)) return(data.table(variable = v, description = "UNDOCUMENTED", unit = "", source = ""))
    cand <- cand[order(file == "")][1]       # file-specific entry first
    data.table(variable = v, description = cand$description, unit = cand$unit, source = cand$source)
  }))
  undoc <- out[description == "UNDOCUMENTED", variable]
  if (length(undoc)) warning(f, ": columns not in 00_metadata.R: ", paste(undoc, collapse = ", "), call. = FALSE)
  out
}
