library(httr2)
library(jsonlite)
library(dplyr)
library(stringr)
library(cli)

# ------------------------------------------------------------------
# 1. Authentication
# ------------------------------------------------------------------
# Tokens expire after ~10 minutes, so we request a new one when needed.

cdse_get_token <- function(
    user = Sys.getenv("CDSE_USER"),
    password = Sys.getenv("CDSE_PASSWORD")
) {
    if (user == "" || password == "") {
        stop("Set CDSE_USER and CDSE_PASSWORD in ~/.Renviron", call. = FALSE)
    }
    resp <- request(
        "https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token"
    ) |>
        req_body_form(
            client_id = "cdse-public",
            grant_type = "password",
            username = user,
            password = password
        ) |>
        req_perform()

    tok <- resp_body_json(resp)
    list(
        access_token = tok$access_token,
        expires_at = Sys.time() + tok$expires_in - 30 # 30 s safety margin
    )
}

# Keep one token in an environment and refresh it on demand
.cdse <- new.env()
cdse_token <- function() {
    if (is.null(.cdse$tok) || Sys.time() > .cdse$tok$expires_at) {
        .cdse$tok <- cdse_get_token()
    }
    .cdse$tok$access_token
}

# ------------------------------------------------------------------
# 2. OData query
# ------------------------------------------------------------------
# dataset: "grassland" or "confidence"
cdse_query_products <- function(
    dataset = c("grassland", "confidence"),
    name_contains = NULL,
    top = 1000
) {
    dataset <- match.arg(dataset)
    id <- switch(
        dataset,
        grassland = "clms_vlcc_grassland_europe_10m_yearly_v1",
        confidence = "clms_vlcc_grassland-confidence-layer_europe_10m_yearly_v1"
    )

    filter <- sprintf(
        paste0(
            "Collection/Name eq 'CLMS' and ",
            "Attributes/OData.CSC.StringAttribute/any(",
            "att:att/Name eq 'datasetIdentifier' and ",
            "att/OData.CSC.StringAttribute/Value eq '%s')"
        ),
        id
    )
    # Optional filter on the product name (year, tile, ...)
    if (!is.null(name_contains)) {
        filter <- paste0(
            filter,
            sprintf(" and contains(Name,'%s')", name_contains)
        )
    }

    out <- list()
    url <- "https://catalogue.dataspace.copernicus.eu/odata/v1/Products"
    query <- list(`$filter` = filter, `$top` = top, `$orderby` = "Name asc")

    repeat {
        resp <- request(url) |>
            req_url_query(!!!query, .multi = "explode") |>
            req_retry(max_tries = 3) |>
            req_perform()
        body <- resp_body_json(resp)
        out <- c(out, body$value)

        # Follow pagination if the API returns a next link
        if (is.null(body$`@odata.nextLink`)) {
            break
        }
        url <- body$`@odata.nextLink`
        query <- NULL
    }

    if (length(out) == 0) {
        cli_alert_warning("No products found.")
        return(tibble())
    }

    tibble(
        id = vapply(out, `[[`, "", "Id"),
        name = vapply(out, `[[`, "", "Name"),
        content_length = vapply(
            out,
            function(x) as.numeric(x$ContentLength %||% NA),
            0
        ),
        s3_path = vapply(out, function(x) x$S3Path %||% NA_character_, "")
    )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------------------------------------------------
# 3. Download
# ------------------------------------------------------------------
cdse_download_product <- function(
    product_id,
    product_name,
    dest_dir,
    overwrite = FALSE
) {
    dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
    dest <- file.path(dest_dir, product_name)
    # Products are usually zip archives; keep the original extension if present
    if (!grepl("\\.[A-Za-z0-9]+$", dest)) {
        dest <- paste0(dest, ".zip")
    }

    if (file.exists(dest) && !overwrite) {
        cli_alert_info("Skipping (already exists): {basename(dest)}")
        return(invisible(dest))
    }

    url <- sprintf(
        "https://download.dataspace.copernicus.eu/odata/v1/Products(%s)/$value",
        product_id
    )

    tmp <- paste0(dest, ".part")
    request(url) |>
        req_auth_bearer_token(cdse_token()) |>
        req_retry(max_tries = 3, backoff = ~10) |>
        req_perform(path = tmp)

    file.rename(tmp, dest)
    cli_alert_success("Downloaded {basename(dest)}")
    invisible(dest)
}

cdse_download_all <- function(products, dest_dir) {
    for (i in seq_len(nrow(products))) {
        cli_h2("{i}/{nrow(products)}")
        tryCatch(
            cdse_download_product(products$id[i], products$name[i], dest_dir),
            error = function(e) {
                cli_alert_danger(
                    "Failed {products$name[i]}: {conditionMessage(e)}"
                )
            }
        )
    }
}

# ------------------------------------------------------------------
# 4. Example usage
# ------------------------------------------------------------------
if (sys.nframe() == 0) {
    # Step 1: list what exists, WITHOUT downloading, to check sizes and names
    prods <- cdse_query_products("grassland")
    print(prods, n = 20)
    cat(
        "Total size (GB):",
        round(sum(prods$content_length, na.rm = TRUE) / 1e9, 1),
        "\n"
    )

    # Step 2: filter by year 2020 (check the naming pattern in `prods$name` first)
    sel <- prods |> filter(str_detect(name, "2020"))

    # Step 3: download
    cdse_download_all(sel, dest_dir = "data/raw/hrl_grassland")
}
