#' FADN-derived Regional Averages for Livestock Parameters
#'
#' A named list of internal reference datasets aggregated from farm-level FADN
#' data. Each element contains regional averages at the NUTS2 level, with
#' country-level and EU-level fallbacks applied to handle missing observations.
#' This dataset is used internally by the \code{FADN2Footprint} package to
#' supply default parameter values when farm-level data are insufficient.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{sales_shares}{A data frame with one row per NUTS2 region containing
#'     the average share of slaughter sales (\code{_share_SSN}) and rearing
#'     sales (\code{_share_SRN}) over total livestock sales (\code{SN}), for
#'     each livestock category. See Details for the imputation hierarchy.
#'     Variables include:
#'     \describe{
#'       \item{NUTS2}{NUTS2 regional code (character).}
#'       \item{COUNTRY}{Country code (character).}
#'       \item{[PREFIX]_share_SSN}{Weighted average share of slaughter sales,
#'         bounded in \eqn{[0, 1]}.}
#'       \item{[PREFIX]_share_SRN}{Weighted average share of rearing sales,
#'         bounded in \eqn{[0, 1]}.}
#'     }
#'   }
#'   \item{GE_MJ_anim_day}{A data frame with one row per NUTS2 region x livestock
#'     category containing the mean and standard deviation of daily gross energy
#'     intake per animal, derived from \code{f_herd_feed()}. Variables include:
#'     \describe{
#'       \item{FADN_code_letter}{Livestock category code as used in FADN
#'         (character).}
#'       \item{NUTS2}{NUTS2 regional code (character).}
#'       \item{COUNTRY}{Country code (character).}
#'       \item{mean_GE_MJ_anim}{Mean daily gross energy intake per animal
#'         (MJ animal\eqn{^{-1}} day\eqn{^{-1}}).}
#'       \item{sd_GE_MJ_anim}{Standard deviation of daily gross energy intake
#'         per animal (MJ animal\eqn{^{-1}} day\eqn{^{-1}}).}
#'     }
#'   }
#' }
#'
#' @details
#' Both datasets follow the same three-step geographic imputation hierarchy to
#' handle missing values:
#' \enumerate{
#'   \item Weighted mean at \strong{NUTS2} level (primary).
#'   \item Weighted mean at \strong{country} level if the NUTS2 value is
#'     \code{NA}.
#'   \item Unweighted mean at \strong{EU} level if both NUTS2 and country
#'     values are \code{NA}.
#' }
#'
#' \strong{sales_shares} — Shares are computed at farm level as:
#' \deqn{\text{share\_SSN} = \frac{\text{SSN}}{\text{SN}}, \quad
#'       \text{share\_SRN} = \frac{\text{SRN}}{\text{SN}}}
#' and set to \code{NA} when \code{SN = 0}. Aggregation uses FADN farm
#' weights (\code{WF}).
#'
#' \strong{GE_MJ_anim_day} — Gross energy intake is derived from
#' \code{\link{f_herd_feed}} and expressed as a daily value by dividing the
#' annual estimate by 365:
#' \deqn{GE_{\text{day}} = \frac{GE_{\text{year}}}{365}}
#'
#' @note This is an internal package dataset produced during the
#'   \code{FADN2Footprint} data preparation pipeline. It is not intended for
#'   direct use by end users but can be accessed via
#'   \code{FADN2Footprint:::FADN_averages}.
#'
#' @seealso
#'   \code{\link{h_average_practices}},
#'   \code{\link{f_herd_feed}}
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Access the full list
#' FADN2Footprint:::FADN_averages
#'
#' # Access livestock sales shares by NUTS2
#' FADN2Footprint:::FADN_averages$sales_shares
#'
#' # Access gross energy averages by NUTS2 and livestock category
#' FADN2Footprint:::FADN_averages$GE_MJ_anim_day
#' }



# ---- Load FADN data ----
load("../FADN2Footprint/data_raw/FADN_16_18.RData")
fadn_data = FADN_16_18

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Share of rearing vs slaughter sales by livestock category ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# --- Identify SSN and SRN variables ---
ssn_vars <- grep("SSN$", names(fadn_data), value = TRUE)
srn_vars <- grep("SRN$", names(fadn_data), value = TRUE)

# Corresponding total sales variables (SN suffix)
# e.g., SE206SSN -> SE206SN, SE206SRN -> SE206SN
all_livestock_vars <- unique(c(ssn_vars, srn_vars))

# Extract base codes to find corresponding SN variables
# Assumes naming convention: [PREFIX]SSN or [PREFIX]SRN -> [PREFIX]SN
base_codes_ssn <- sub("_SSN$", "", ssn_vars)
base_codes_srn <- sub("_SRN$", "", srn_vars)
base_codes_all <- unique(c(base_codes_ssn, base_codes_srn))

sn_vars <- paste0(base_codes_all, "_SN")
sn_vars_present <- intersect(sn_vars, names(fadn_data))

# --- Compute share variables at farm level ---
# Share = SSN / SN and SRN / SN for each livestock category

share_vars <- c()

for (base in base_codes_all) {
  sn_col  <- paste0(base, "_SN")
  ssn_col <- paste0(base, "_SSN")
  srn_col <- paste0(base, "_SRN")

  # SSN share
  if (ssn_col %in% names(fadn_data) & sn_col %in% names(fadn_data)) {
    share_col_ssn <- paste0(base, "_share_SSN")
    fadn_data[[share_col_ssn]] <- ifelse(
      fadn_data[[sn_col]] > 0,
      fadn_data[[ssn_col]] / fadn_data[[sn_col]],
      NA_real_
    )
    share_vars <- c(share_vars, share_col_ssn)
  }

  # SRN share
  if (srn_col %in% names(fadn_data) & sn_col %in% names(fadn_data)) {
    share_col_srn <- paste0(base, "_share_SRN")
    fadn_data[[share_col_srn]] <- ifelse(
      fadn_data[[sn_col]] > 0,
      fadn_data[[srn_col]] / fadn_data[[sn_col]],
      NA_real_
    )
    share_vars <- c(share_vars, share_col_srn)
  }
}

# --- Summarise shares at NUTS2 level using h_average_practices ---
# primary_grp  : NUTS2 (the level we want results at)
# secondary_grp: COUNTRY (fallback if NUTS2 has missing values)
# weight_var   : farm weight (e.g., "WF" in FADN)

sales_shares <- h_average_practices(
  data          = fadn_data,
  target_vars   = share_vars,
  primary_grp   = "NUTS2",
  secondary_grp = "COUNTRY",
  weight_var    = "SYS02"            # adjust to actual weight column name
)

# --- Final fallback: replace remaining NAs with the overall variable mean ---
# This handles cases where both NUTS2 and country-level data are unavailable

sales_shares <- sales_shares |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(share_vars),
      ~ dplyr::coalesce(., mean(., na.rm = TRUE))
    )
  )

# Report variables that still contain NAs after all imputation steps
remaining_nas <- colSums(is.na(sales_shares[share_vars]))
if (any(remaining_nas > 0)) {
  warning(
    "The following variables still contain NAs after all imputation steps ",
    "(all values were NA, no mean could be computed):\n",
    paste(names(remaining_nas[remaining_nas > 0]), collapse = ", ")
  )
} else {
  message("No remaining NAs in share variables after imputation.")
}

View(sales_shares)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Gross Energy (GE) intake per animal per day by livestock category ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- Create object ---
object = data_4FADN2Footprint(fadn_data)

# --- Estimate herd feed ---
herd_feed <- f_herd_feed(object, overwrite = overwrite)

# --- Compute GE mean and sd at farm level by livestock category ---
tmp_GE <- herd_feed$feed_intake$total |>
  dplyr::filter(Qobs >0) |>
  dplyr::mutate(GE_MJ_anim_day = GE_MJ_anim / 365) |>
  # add SYS02
  dplyr::left_join(
    object@farm |>
      dplyr::select(dplyr::all_of(object@traceability$id_cols), SYS02),
    by = object@traceability$id_cols
  )

# --- Summarise GE by livestock category x NUTS2 (with country and EU fallback) ---

# Helper to compute weighted or unweighted mean and sd at a given grouping level
summarise_GE <- function(data, grp_vars) {
  data |>
    dplyr::summarise(
      mean_GE_MJ_anim_day = mean(GE_MJ_anim_day, na.rm = TRUE),
      sd_GE_MJ_anim_day   = sd(GE_MJ_anim_day,   na.rm = TRUE),
      .by = dplyr::all_of(grp_vars)
    )
}

# Compute summaries at each geographic level
#GE_by_nuts2   <- summarise_GE(tmp_GE, c("FADN_code_letter", "NUTS2", "COUNTRY"))
GE_by_country <- summarise_GE(tmp_GE, c("FADN_code_letter", "COUNTRY"))
GE_by_europe  <- summarise_GE(tmp_GE,   "FADN_code_letter")

# --- Apply two-step fallback hierarchy ---
tmp_mean_GE <- GE_by_country |>
  # join EU-level estimates
  dplyr::left_join(
    GE_by_europe,
    by     = "FADN_code_letter",
    suffix = c("", "_europe")
  ) |>
  # Step 3: coalesce NUTS2 -> country -> EU
  dplyr::mutate(
    mean_GE_MJ_anim_day = dplyr::coalesce(
      mean_GE_MJ_anim_day,
      mean_GE_MJ_anim_day_europe
    ),
    sd_GE_MJ_anim_day = dplyr::coalesce(
      sd_GE_MJ_anim_day,
      sd_GE_MJ_anim_day_europe
    )
  ) |>
  # Drop intermediate fallback columns
  dplyr::select(
    FADN_code_letter, COUNTRY,
    mean_GE_MJ_anim_day, sd_GE_MJ_anim_day
  )

# --- Report remaining NAs after all fallback steps ---
remaining_nas <- colSums(is.na(tmp_mean_GE[c("mean_GE_MJ_anim_day", "sd_GE_MJ_anim_day")]))
if (any(remaining_nas > 0)) {
  warning(
    "The following GE variables still contain NAs after all fallback steps:\n",
    paste(names(remaining_nas[remaining_nas > 0]), collapse = ", ")
  )
} else {
  message("No remaining NAs in GE variables after fallback imputation.")
}

View(tmp_mean_GE)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Export data ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FADN_averages = list(
  sales_shares = sales_shares,
  GE_MJ_anim_day = tmp_mean_GE
)

usethis::use_data(FADN_averages, overwrite = T)

