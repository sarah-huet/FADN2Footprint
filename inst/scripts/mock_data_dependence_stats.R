# Dependence statistics used to make the mock FADN data more realistic
#
# Computes, from confidential EU-FADN microdata kept locally, *aggregated*
# statistics describing (1) correlations between variables and (2) the panel
# structure of the national samples (farms staying in the sample several
# years in a row). Only aggregates are exported (correlation coefficients,
# shares, typology/NUTS lookups); no farm-level record leaves `data_raw/`.
# Every statistic is computed on at least `min_n` farms (or farm pairs).
#
# Output: inst/ext_data/mock_dependence_stats_EU_17_21.RData, containing the
# list `mock_dependence`, consumed by inst/scripts/mock_data.R through
# h_generate_mock_panel(). Elements:
#   core_vars       continuous variables modelled with a Gaussian copula
#   cor_core        per country, normal-score (van der Waerden) correlation
#                   matrix of core_vars, computed within TF8 x YEAR groups so
#                   that it describes within-group dependence
#   size_loading    per country x variable, correlation of each continuous
#                   variable (normal scores) with the latent farm-size score
#                   (first principal component of the core variables)
#   family_cor      per country, typical correlation between variables of the
#                   same product family (e.g. CWHTC_TA and CWHTC_PRQ)
#   autocorr        per country x variable, year-to-year correlation of the
#                   normal scores for farms present in two consecutive years
#   size_autocorr   per country, year-to-year correlation of the size score
#   cat_persistence per country x categorical variable, share of farms whose
#                   level is unchanged between two consecutive years
#   ordinal_loading per country, correlation of the ordinal size class (SIZC)
#                   with the size score
#   retention       per country, share of the farms of year t still in the
#                   sample in year t+1 (sampling-plan rotation)
#   cat_lookup      nested classifications (NUTS3 -> NUTS2 -> NUTS1,
#                   TF_SUBP4 -> TF_PART3 -> TF_PRIN2 -> TF_GEN1/TF14, ...)
#   marginals       TF8 x COUNTRY mean/sd/min/max of variables required by
#                   the input parser but missing from the descriptive
#                   statistics sheet (currently SE295)
#   marginals_country per country mean/sd/min/quartiles/max of the same
#                   variables (for national equivalents, e.g. RICA ACHEN)
#   nonzero_share   TF8 x COUNTRY x variable share of non-zero values
#                   (zero mass of the mock marginals)
#   summary         per country medians used as defaults for variables that
#                   cannot be mapped (e.g. French RICA variable names)

library(dplyr)
library(readxl)

raw_path   <- "data_raw/raw_FADN_EU_17_21.RData"
marg_path  <- "data_raw/FADN_EU_16_18.RData"
stats_path <- "inst/ext_data/EU_FADN_2016_2018_desc_stats.xlsx"
out_path   <- "inst/ext_data/mock_dependence_stats_EU_17_21.RData"
min_n      <- 50L
max_core   <- 200L
practice_crop_codes <- c("CWHTC", "CWHTD", "CBRL", "COAT", "CMZ", "CRYE",
                         "CRICE", "CGRSXRG", "CRG")

## Variables (aligned with the descriptive statistics used for the mock) ----
# Variables required by .parse_input_data() but absent from the descriptive
# statistics sheet: their marginals are exported too (see `marginals`).
extra_cont_vars <- c("SE295") # Purchased fertilisers and soil improvers (EUR)
cont_vars <- unique(c(
  suppressMessages(read_excel(stats_path, "continuous_vars"))$variable,
  extra_cont_vars
))
sheet_cont_vars <- cont_vars
cat_vars  <- unique(suppressMessages(read_excel(stats_path, "categorical_vars"))$variable)

e <- new.env()
load(raw_path, envir = e)
cont_vars <- intersect(cont_vars, names(e$my_FADN_data))
cat_vars  <- setdiff(intersect(cat_vars, names(e$my_FADN_data)), c("TF8", "COUNTRY"))
raw <- as.data.frame(e$my_FADN_data[, c("ID", "YEAR", "COUNTRY", "TF8", cont_vars, cat_vars)])
rm(e); invisible(gc())
for (v in cont_vars) raw[[v]] <- as.numeric(raw[[v]])
for (v in cat_vars)  raw[[v]] <- as.character(raw[[v]])

## Helpers ----
# Normal scores (mid-ranks), so that correlations estimate the Gaussian
# copula parameter directly whatever the (skewed, zero-inflated) marginal.
ns <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  if (sum(ok) > 1) out[ok] <- qnorm((rank(x[ok]) - 0.5) / sum(ok))
  out
}
ns_by_group <- function(x, grp) {
  out <- rep(NA_real_, length(x))
  for (idx in split(seq_along(x), grp)) out[idx] <- ns(x[idx])
  out
}
safe_cor <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  if (sum(ok) < min_n || stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NA_real_)
  stats::cor(a[ok], b[ok])
}
family_of <- function(v) sub("_[^_]+$", "", sub("_X$", "", v))

## Core variables (same set for every country) ----
nz_share <- vapply(cont_vars, function(v) mean(raw[[v]] != 0, na.rm = TRUE), numeric(1))
explicit <- intersect(c(
  "SE025", "SE080", "INUSE_Q", "IPUSE_Q", "IKUSE_Q", "IELE_V", "IFULS_V",
  "IHFULS_V", "IPROT_V", "IGRFEDCNCTRPUR_V", "IGRFEDCRSPUR_V",
  "IPIGFEDPUR_V", "IPLTRFEDPUR_V",
  grep(paste0("^(", paste(practice_crop_codes, collapse = "|"),
              ")_(TA|PRQ|SQ|SV|TO|FUV)$"), cont_vars, value = TRUE)
), cont_vars)
frequent <- names(sort(nz_share[nz_share >= 0.10], decreasing = TRUE))
core_vars <- head(unique(c(explicit, frequent)), max_core)

## Panel transitions (farm present in t and t+1) ----
key <- paste(raw$ID, raw$YEAR)
next_idx <- match(paste(raw$ID, raw$YEAR + 1L), key)

## Per-country statistics ----
countries <- sort(unique(raw$COUNTRY))
only <- Sys.getenv("MOCK_DEP_COUNTRIES")
if (nzchar(only)) countries <- intersect(countries, strsplit(only, ",")[[1]])
country_stats <- function(cty) {
  message("Processing ", cty)
  rows <- which(raw$COUNTRY == cty)
  d <- raw[rows, ]
  grp <- interaction(d$TF8, d$YEAR, drop = TRUE)
  Z <- vapply(cont_vars, function(v) ns_by_group(d[[v]], grp), numeric(nrow(d)))

  # Retention
  yrs <- sort(unique(d$YEAR))
  trans <- do.call(rbind, lapply(yrs[-length(yrs)], function(y) {
    a <- unique(d$ID[d$YEAR == y]); b <- unique(d$ID[d$YEAR == y + 1L])
    data.frame(n_t = length(a), retained = sum(a %in% b))
  }))
  retention <- data.frame(
    COUNTRY = cty,
    retention = if (sum(trans$n_t) >= min_n) sum(trans$retained) / sum(trans$n_t) else NA_real_,
    n_transitions = nrow(trans),
    mean_farms_per_year = mean(table(d$YEAR))
  )

  # Core correlation (pairwise, NA -> 0 off-diagonal for degenerate pairs)
  Zc <- Z[, core_vars, drop = FALSE]
  R <- suppressWarnings(stats::cor(Zc, use = "pairwise.complete.obs"))
  n_pair <- crossprod(!is.na(Zc))
  R[is.na(R) | n_pair < min_n] <- 0
  diag(R) <- 1

  # Latent size score: first principal component of the core block
  v1 <- eigen(R, symmetric = TRUE)$vectors[, 1]
  if (sum(v1) < 0) v1 <- -v1
  Zc0 <- Zc; Zc0[is.na(Zc0)] <- 0
  S <- as.numeric(scale(Zc0 %*% v1))

  nz_cty <- vapply(cont_vars, function(v) mean(d[[v]] != 0, na.rm = TRUE), numeric(1))
  size_loading <- data.frame(
    COUNTRY = cty, variable = cont_vars,
    loading = vapply(cont_vars, function(v) safe_cor(Z[, v], S), numeric(1)),
    nonzero_share = nz_cty
  )

  # Within-family correlation among core variables
  fam <- family_of(core_vars)
  fam_cor <- unlist(lapply(split(seq_along(core_vars), fam), function(i) {
    if (length(i) < 2) return(NULL)
    stats::median(R[i, i][upper.tri(R[i, i])])
  }))
  family_cor <- data.frame(COUNTRY = cty,
                           family_cor = if (length(fam_cor)) stats::median(fam_cor) else NA_real_)

  # Year-to-year persistence
  nx <- match(next_idx[rows], rows)
  has_next <- which(!is.na(nx))
  autocorr <- data.frame(
    COUNTRY = cty, variable = cont_vars,
    rho = vapply(cont_vars, function(v) safe_cor(Z[has_next, v], Z[nx[has_next], v]), numeric(1))
  )
  size_autocorr <- data.frame(COUNTRY = cty, rho = safe_cor(S[has_next], S[nx[has_next]]))
  cat_persistence <- data.frame(
    COUNTRY = cty, variable = cat_vars,
    p_same = vapply(cat_vars, function(v) {
      a <- d[[v]][has_next]; b <- d[[v]][nx[has_next]]; ok <- !is.na(a) & !is.na(b)
      if (sum(ok) < min_n) NA_real_ else mean(a[ok] == b[ok])
    }, numeric(1))
  )

  ordinal_loading <- data.frame(
    COUNTRY = cty, variable = "SIZC",
    loading = if ("SIZC" %in% cat_vars) safe_cor(ns_by_group(as.numeric(d$SIZC), grp), S) else NA_real_
  )

  frequent_cty <- nz_cty >= 0.2
  summary <- data.frame(
    COUNTRY = cty,
    median_size_loading = stats::median(size_loading$loading[frequent_cty], na.rm = TRUE),
    median_autocorr = stats::median(autocorr$rho[frequent_cty], na.rm = TRUE),
    median_cat_persistence = stats::median(cat_persistence$p_same, na.rm = TRUE)
  )

  list(vars = cont_vars,
       cor_core = R, size_loading = size_loading, family_cor = family_cor,
       autocorr = autocorr, size_autocorr = size_autocorr,
       cat_persistence = cat_persistence, ordinal_loading = ordinal_loading,
       retention = retention, summary = summary)
}

# Per-country aggregates are cached in the (git-ignored) data_raw/ folder so
# that the script can be resumed if it stops (it needs a lot of memory).
cache_dir <- "data_raw/mock_dependence_cache"
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
res <- lapply(countries, function(cty) {
  f <- file.path(cache_dir, paste0(cty, ".rds"))
  # Recompute when the variable set changed since the cache was written
  if (!file.exists(f) || !identical(readRDS(f)$vars, cont_vars)) {
    saveRDS(country_stats(cty), f)
    invisible(gc())
  }
  readRDS(f)
})
names(res) <- countries

## Nested classifications (modal parent of each child level) ----
lookup_pairs <- list(
  c("NUTS3", "NUTS2"), c("NUTS2", "NUTS1"),
  c("TF_SUBP4", "TF"), c("TF_SUBP4", "TF_PART3"), c("TF_PART3", "TF_PRIN2"),
  c("TF_PRIN2", "TF_GEN1"), c("TF_PRIN2", "TF14"), c("SIZC", "SIZC_CLU")
)
cat_lookup <- do.call(rbind, lapply(lookup_pairs, function(p) {
  if (!all(p %in% cat_vars)) return(NULL)
  raw[!is.na(raw[[p[1]]]) & !is.na(raw[[p[2]]]), p] |>
    dplyr::count(from_level = .data[[p[1]]], to_level = .data[[p[2]]]) |>
    dplyr::slice_max(n, n = 1, by = from_level, with_ties = FALSE) |>
    dplyr::transmute(from_var = p[1], from_level, to_var = p[2], to_level)
}))

## Marginal statistics (same period as the descriptive statistics sheet) ----
# The zero shares and the marginals of extra_cont_vars must match the
# 2016-2018 marginals of the sheet, and some sheet variables (e.g. PCNTR*
# sales of animals under contract) are absent from the 2017-2021 extract.
rm(raw); invisible(gc())
e <- new.env()
load(marg_path, envir = e)
marg_vars <- intersect(sheet_cont_vars, names(e$FADN_16_18))
mraw <- as.data.frame(e$FADN_16_18[, c("COUNTRY", "TF8", marg_vars)])
rm(e); invisible(gc())
for (v in marg_vars) mraw[[v]] <- as.numeric(mraw[[v]])
mraw$TF8 <- as.character(mraw$TF8)

## Marginals of extra_cont_vars ----
# Same layout as the `continuous_vars` sheet (TF8, COUNTRY, variable, mean,
# sd, min, max), pooled over all years. Every TF8 x COUNTRY group gets a row:
# groups with fewer than min_n observations fall back to the country level,
# then to the EU-wide TF8 level.
marg_stats <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < min_n) return(NULL)
  data.frame(mean = mean(x), sd = stats::sd(x), min = min(x), max = max(x))
}
groups <- unique(mraw[c("TF8", "COUNTRY")])
marginals <- do.call(rbind, lapply(intersect(extra_cont_vars, marg_vars), function(v) {
  do.call(rbind, lapply(seq_len(nrow(groups)), function(i) {
    tf <- groups$TF8[i]; cty <- groups$COUNTRY[i]
    s <- marg_stats(mraw[[v]][mraw$TF8 == tf & mraw$COUNTRY == cty])
    if (is.null(s)) s <- marg_stats(mraw[[v]][mraw$COUNTRY == cty])
    if (is.null(s)) s <- marg_stats(mraw[[v]][mraw$TF8 == tf])
    if (is.null(s)) return(NULL)
    cbind(data.frame(TF8 = tf, COUNTRY = cty, variable = v), s)
  }))
}))

## Share of non-zero values per TF8 x COUNTRY ----
# The descriptive statistics sheet has no quartiles, so without this share
# the mock (moment-matched gamma) never produces exact zeros. Only groups
# with at least min_n observations and shares below 0.995 are kept.
grp_key <- paste(mraw$TF8, mraw$COUNTRY, sep = "\r")
nonzero_share <- do.call(rbind, lapply(split(marg_vars, ceiling(seq_along(marg_vars) / 100)), function(vs) {
  X <- as.matrix(mraw[vs])
  n_obs <- rowsum(1 * !is.na(X), grp_key)
  n_nz <- rowsum(1 * (!is.na(X) & X != 0), grp_key)
  keep <- which(n_obs >= min_n & n_nz / n_obs < 0.995, arr.ind = TRUE)
  if (!nrow(keep)) return(NULL)
  g <- do.call(rbind, strsplit(rownames(n_obs)[keep[, 1]], "\r"))
  data.frame(TF8 = g[, 1], COUNTRY = g[, 2], variable = vs[keep[, 2]],
             p_nonzero = round(n_nz[keep] / n_obs[keep], 3))
}))
rownames(nonzero_share) <- NULL

# Country-level marginals with quartiles (same layout as the French
# descriptive statistics), used for national variables equivalent to
# extra_cont_vars (e.g. RICA ACHEN = SE295 for France).
marginals_country <- do.call(rbind, lapply(intersect(extra_cont_vars, marg_vars), function(v) {
  do.call(rbind, lapply(sort(unique(mraw$COUNTRY)), function(cty) {
    x <- mraw[[v]][mraw$COUNTRY == cty]; x <- x[!is.na(x)]
    if (length(x) < min_n) return(NULL)
    q <- stats::quantile(x, c(0, 0.25, 0.5, 0.75, 1), names = FALSE)
    data.frame(COUNTRY = cty, variable = v, mean = mean(x), sd = stats::sd(x),
               min = q[1], p25 = q[2], p50 = q[3], p75 = q[4], max = q[5])
  }))
}))

## Export ----
bind <- function(el) do.call(rbind, lapply(res, `[[`, el)) |> `rownames<-`(NULL)
mock_dependence <- list(
  source = paste("EU-FADN microdata: dependence and panel from 2017-2021",
                 "(data_raw/raw_FADN_EU_17_21.RData); marginals and zero shares",
                 "from 2016-2018 (data_raw/FADN_EU_16_18.RData)"),
  min_n = min_n,
  core_vars = core_vars,
  cor_core = lapply(res, `[[`, "cor_core"),
  size_loading = bind("size_loading"),
  family_cor = bind("family_cor"),
  autocorr = bind("autocorr"),
  size_autocorr = bind("size_autocorr"),
  cat_persistence = bind("cat_persistence"),
  ordinal_loading = bind("ordinal_loading"),
  retention = bind("retention"),
  cat_lookup = as.data.frame(cat_lookup),
  marginals = marginals,
  marginals_country = marginals_country,
  nonzero_share = nonzero_share,
  summary = bind("summary")
)
save(mock_dependence, file = out_path, compress = "xz")
