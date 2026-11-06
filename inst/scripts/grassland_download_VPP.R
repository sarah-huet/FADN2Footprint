# ==============================================================================
#
#https://land.copernicus.eu/en/products/vegetation/high-resolution-total-productivity#download
#
#
#
#
#
#
#
#
#
#
#
#
#
#
##FADN2Footprint - download of the Copernicus HR-VPP Seasonal Productivity
# (SPROD, 10 m, yearly, 2017-present) via the WEkEO HDA REST API.
# DOI: 10.2909/5ae0f2a2-7ad8-4f7c-878d-f1b09d78d7a1
#
# Prerequisites:
#   - Free WEkEO account: https://www.wekeo.eu/
#   - Credentials in environment variables WEKEO_USER and WEKEO_PASSWORD
#     (e.g. in ~/.Renviron), never hard-coded.
#   - install.packages(c("httr2", "jsonlite"))
# ==============================================================================

library(httr2)
library(jsonlite)

HDA_URL <- "https://wekeo-broker.prod.wekeo2.eu/databroker"
DATASET_ID <- "EO:EEA:DAT:CLMS_HRVPP_VPP" # HR-VPP Vegetation Phenology and Productivity parameters
# (SPROD is a product within this collection)

# ---- Authentication ----------------------------------------------------------
hda_token <- function() {
    user <- Sys.getenv("WEKEO_USER")
    pass <- Sys.getenv("WEKEO_PASSWORD")
    if (user == "" || pass == "") {
        stop("Set WEKEO_USER and WEKEO_PASSWORD.")
    }
    resp <- request(paste0(HDA_URL, "/gettoken")) |>
        req_body_json(list(username = user, password = pass)) |>
        req_perform()
    resp_body_json(resp)$access_token
}

# ---- Accept terms and conditions (needed once per account) ---------------------
hda_accept_tnc <- function(token) {
    request(paste0(HDA_URL, "/termsaccepted/Copernicus_General_License")) |>
        req_auth_bearer_token(token) |>
        req_method("PUT") |>
        req_error(is_error = function(r) FALSE) |>
        req_perform()
    invisible(TRUE)
}

# ---- Submit a query and return the job id ---------------------------------------
hda_query <- function(
    token,
    year,
    tile = NULL,
    bbox = NULL,
    product_type = "SPROD",
    season = "s1"
) {
    q <- list(
        dataset_id = DATASET_ID,
        productType = product_type, # SPROD = Seasonal Productivity
        productGroupId = season, # s1 / s2 = first / second season
        start = sprintf("%d-01-01T00:00:00.000Z", year),
        end = sprintf("%d-12-31T23:59:59.999Z", year)
    )
    if (!is.null(tile)) {
        q$tileId <- tile
    }
    if (!is.null(bbox)) {
        q$bbox <- bbox
    } # c(xmin, ymin, xmax, ymax) in WGS84

    resp <- request(paste0(HDA_URL, "/datarequest")) |>
        req_auth_bearer_token(token) |>
        req_body_json(q) |>
        req_perform()
    resp_body_json(resp)$jobId
}

# ---- Wait for the job to complete ------------------------------------------------
hda_wait <- function(token, job_id, poll = 10, timeout = 3600) {
    t0 <- Sys.time()
    repeat {
        st <- request(paste0(HDA_URL, "/datarequest/status/", job_id)) |>
            req_auth_bearer_token(token) |>
            req_perform() |>
            resp_body_json()
        message("Job ", job_id, ": ", st$status)
        if (st$status == "completed") {
            return(invisible(TRUE))
        }
        if (st$status %in% c("failed", "cancelled")) {
            stop("Job ", st$status)
        }
        if (difftime(Sys.time(), t0, units = "secs") > timeout) {
            stop("Timeout")
        }
        Sys.sleep(poll)
    }
}

# ---- Download all results ----------------------------------------------------------
hda_download <- function(token, job_id, outdir) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    page <- 0
    repeat {
        res <- request(paste0(
            HDA_URL,
            "/datarequest/jobs/",
            job_id,
            "/result?page=",
            page,
            "&size=100"
        )) |>
            req_auth_bearer_token(token) |>
            req_perform() |>
            resp_body_json()
        items <- res$content
        if (length(items) == 0) {
            break
        }
        for (it in items) {
            dest <- file.path(outdir, basename(it$filename))
            if (file.exists(dest)) {
                message("Skip ", dest)
                next
            }
            # request a download order, then fetch the file
            ord <- request(paste0(HDA_URL, "/dataorder")) |>
                req_auth_bearer_token(token) |>
                req_body_json(list(jobId = job_id, uri = it$url)) |>
                req_perform() |>
                resp_body_json()
            oid <- ord$orderId
            repeat {
                s <- request(paste0(HDA_URL, "/dataorder/status/", oid)) |>
                    req_auth_bearer_token(token) |>
                    req_perform() |>
                    resp_body_json()
                if (s$status == "completed") {
                    break
                }
                if (s$status == "failed") {
                    stop("Order failed for ", it$filename)
                }
                Sys.sleep(5)
            }
            request(paste0(HDA_URL, "/dataorder/download/", oid)) |>
                req_auth_bearer_token(token) |>
                req_perform(path = dest)
            message("Downloaded ", dest)
        }
        page <- page + 1
    }
}

# ---- Main -----------------------------------------------------------------------------
download_sprod <- function(
    years = 2017:2025,
    tiles = NULL,
    bbox = NULL,
    outdir = "data/raw/hrvpp_sprod",
    seasons = c("s1", "s2")
) {
    token <- hda_token()
    hda_accept_tnc(token)
    for (y in years) {
        for (s in seasons) {
            message("== Year ", y, " season ", s)
            # Tiles are Sentinel-2 tile IDs (e.g. "T32TNR"); use bbox for an area instead.
            for (tl in (if (is.null(tiles)) list(NULL) else tiles)) {
                job <- hda_query(token, y, tile = tl, bbox = bbox, season = s)
                hda_wait(token, job)
                hda_download(token, job, file.path(outdir, y, s))
                token <- hda_token() # refresh, tokens expire
            }
        }
    }
}

# Example: France, bounding box, 2018-2024
# download_sprod(years = 2018:2024, bbox = c(-5.5, 41.0, 9.8, 51.5))
