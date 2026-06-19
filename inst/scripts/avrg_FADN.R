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

# TODO: recalculate averages using SYS02 variable

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Crop yields ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

crop_raw <- fadn_data |>
  dplyr::select(ID, YEAR, COUNTRY, NUTS2, SYS02,
                dplyr::matches("_A$|_TA$|_AA$|_PRQ$") & dplyr::matches("^C"))

crop_raw_long0 <- crop_raw |>
  tidyr::pivot_longer(
    cols = dplyr::matches("_A$|_TA$|_AA$|_PRQ$")
  ) |>
  dplyr::filter(value >0) |>
  dplyr::mutate(
    FADN_code_letter = gsub("_A$|_TA$|_AA$|_PRQ$","",name),
    var = stringr::str_extract(name,"A$|TA$|AA$|PRQ$")
  ) |>
  tidyr::pivot_wider(
    id_cols = c('ID', 'YEAR', 'COUNTRY', 'NUTS2', 'SYS02', 'FADN_code_letter'),
    names_from = 'var',
    values_from = 'value'
  ) |>
  # Add missing columns with NA
  dplyr::bind_rows(tibble::tibble(A = numeric(), TA = numeric(), AA = numeric())) |>
  dplyr::mutate(
    area_ha = pmax(A, TA, AA, na.rm = TRUE),
    prod_t = PRQ
  )
crop_raw_long <- crop_raw_long0 |>
  # keep complete data
  dplyr::filter(area_ha >0 & prod_t >0) |>
  # estimate yields
  dplyr::mutate(
    avrg_FADN_yield = prod_t / area_ha
  )

# Missing NUTS2 x livestock category
missing_cat <- dplyr::anti_join(
  crop_raw_long0 |>
    dplyr::select(NUTS2, FADN_code_letter) |>
    dplyr::distinct(),
  crop_raw_long |>
    dplyr::select(NUTS2, FADN_code_letter) |>
    dplyr::distinct(),
  by = c('NUTS2', 'FADN_code_letter')
)

# --- Sum up at NUTS2 level using h_average_practices ---
# primary_grp  : NUTS2 (the level we want results at)
# secondary_grp: COUNTRY (fallback if NUTS2 has missing values)
# weight_var   : farm extrapolation coefficient (e.g., "SYS02" in FADN)

crop_yields <- h_average_practices(
  data          = crop_raw_long |>
    dplyr::bind_rows(missing_cat),
  target_vars   = c("avrg_FADN_yield"),
  primary_grp   = c('FADN_code_letter', "NUTS2"),
  secondary_grp = c('FADN_code_letter'),
  weight_var    = "SYS02"            # adjust to actual weight column name
)

View(crop_yields)






# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Share of rearing vs slaughter sales by livestock category ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# --- Compute share variables at farm level ---
# Share = SSN / SN and SRN / SN for each livestock category

sales_raw <- fadn_data |>
  dplyr::select(ID, YEAR, COUNTRY, NUTS2, SYS02,
                dplyr::matches("_SN$|_SSN$|_SRN$"))

sales_raw_long <- sales_raw |>
  tidyr::pivot_longer(
    cols = dplyr::matches("_SN$|_SSN$|_SRN$")
  ) |>
  dplyr::filter(value >0) |>
  dplyr::mutate(
    FADN_code_letter = gsub("_SN$|_SSN$|_SRN$","",name),
    var = stringr::str_extract(name,"SN$|SSN$|SRN$")
  ) |>
  tidyr::pivot_wider(
    id_cols = c('ID', 'YEAR', 'COUNTRY', 'NUTS2', 'SYS02', 'FADN_code_letter'),
    names_from = 'var',
    values_from = 'value',
    values_fill = 0
  ) |>
  # check if SN = SRN + SSN
  dplyr::mutate(
    complete_sales = SN == SRN + SSN
  )

table(sales_raw_long$complete_sales)

sales_shares_raw <- sales_raw_long |>
  # keep only farms with complete sales values
  dplyr::filter(complete_sales) |>
  # estimate shares
  dplyr::mutate(
    share_SRN = SRN / SN,
    share_SSN = SSN / SN
  )

# Missing NUTS2 x livestock category
missing_cat <- dplyr::anti_join(
  sales_raw_long |>
    dplyr::select(NUTS2, FADN_code_letter) |>
    dplyr::distinct(),
  sales_shares_raw |>
    dplyr::select(NUTS2, FADN_code_letter) |>
    dplyr::distinct(),
  by = c('NUTS2', 'FADN_code_letter')
)

# --- Summarise shares at NUTS2 level using h_average_practices ---
# primary_grp  : NUTS2 (the level we want results at)
# secondary_grp: COUNTRY (fallback if NUTS2 has missing values)
# weight_var   : farm extrapolation coefficient (e.g., "SYS02" in FADN)

sales_shares <- h_average_practices(
  data          = sales_shares_raw |>
    dplyr::bind_rows(missing_cat),
  target_vars   = c("share_SRN", "share_SSN"),
  primary_grp   = c('FADN_code_letter', "NUTS2"),
  secondary_grp = c('FADN_code_letter'),
  weight_var    = "SYS02"            # adjust to actual weight column name
)

View(sales_shares)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --- Feed averages ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# ---- Create object ---
object = data_4FADN2Footprint(fadn_data)

## Yields per feed type ----

feed_produced <- f_feed_onfarm(object, overwrite = overwrite)

feed_yield <- feed_produced |>
  dplyr::summarise(
    yield = mean(yield, na.rm = TRUE),
    .by = c(Sailley_feed, COUNTRY)
  )

FADN_averages = list(
  feed_yield = feed_yield
)

# --- Estimate herd feed ---
herd_feed <- f_herd_feed(object, overwrite = overwrite)

## Gross Energy (GE) intake per animal per day by livestock category ----

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
  crop_yields = crop_yields,
  sales_shares = sales_shares,
  feed_yield = feed_yield,
  GE_MJ_anim_day = tmp_mean_GE
)

usethis::use_data(FADN_averages, overwrite = T)

