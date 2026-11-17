# =============================================================================
# 00_functions.R - helper functions (sourced by 00_config.R)
# =============================================================================

# ---- Logging ----------------------------------------------------------------
# Progress messages: printed to the console and, under run_logged(), to the log
msg <- function(...) {
  cat(format(Sys.time(), "%H:%M:%S"), " | ", ..., "\n", sep = "")
  utils::flush.console()
}

# ---- Names and CSV files -----------------------------------------------------
# Region names transliterated to plain ASCII (accents removed, e.g.
# "Cordoba" for the Spanish province), to avoid garbled characters in Excel,
# QGIS and logs. CSV files are written in UTF-8 with a byte-order mark (BOM), which
# Excel and QGIS use to detect the encoding (fread reads them normally).
to_ascii <- function(x) stringi::stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
# Every CSV output gets a companion <name>_metadata.csv listing its columns
# with description, unit and source (dictionary in 00_metadata.R).
write_csv <- function(x, file) {
  fwrite(x, file, bom = TRUE)
  fwrite(describe_vars(names(x), file), sub("\\.csv$", "_metadata.csv", file), bom = TRUE)
}

# Every Excel output gets a "metadata" sheet listing, for each data sheet, its
# columns with description, unit and source. `sheets`: named list of the tables
# written in the workbook (sheet name -> table).
add_metadata_sheet <- function(wb, file, sheets) {
  md <- rbindlist(lapply(names(sheets), function(s) cbind(sheet = s, describe_vars(names(sheets[[s]]), file))))
  openxlsx::addWorksheet(wb, "metadata")
  openxlsx::writeData(wb, "metadata", md)
  openxlsx::setColWidths(wb, "metadata", cols = 1:5, widths = c(18, 34, 100, 18, 80))
  openxlsx::freezePane(wb, "metadata", firstRow = TRUE)
  invisible(wb)
}

# ---- Figures of the working paper -------------------------------------------
# No title on the images: titles and legends are written in the paper, so that
# each figure can be understood on its own. Size: journal page width (17 cm),
# 300 dpi. Saved as PNG in outputs/paper/.
theme_paper <- function(base_size = 8)
  ggplot2::theme_bw(base_size) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.3),
                 strip.background = ggplot2::element_rect(fill = "grey95", colour = "grey70"),
                 legend.position = "bottom")
save_fig <- function(p, name, height_cm, width_cm = 17) {
  ggplot2::ggsave(file.path(dir_paper, paste0(name, ".png")), p,
                  width = width_cm, height = height_cm, units = "cm", dpi = 300)
  msg("Figure saved: ", file.path(dir_paper, paste0(name, ".png")))
}
# Categorical colours (fixed order, colour-blind checked palette) and labels
pal_cat <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")
forage_label <- c(green_maize = "Green maize", permanent_grass = "Permanent grassland",
                  maize = "Maize", grass = "Grassland")
# Vertical lines marking changes in input data (input_breaks in 00_config.R)
break_lines <- function() ggplot2::geom_vline(xintercept = input_breaks$year - 0.5,
                                              linetype = 3, colour = "grey40", linewidth = 0.3)

# Tables of the paper as images (PNG, booktabs style: rules above and below the
# header and at the bottom), so that they can be inserted and linked in Word.
# `d`: data.frame of already formatted text columns; `align`: "l" or "r" per column.
save_table <- function(d, name, width_cm = 17, align = NULL, font_size = 7.5) {
  hdr <- names(d)
  d <- lapply(d, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  n_row <- length(d[[1]])
  if (is.null(align)) align <- c("l", rep("r", length(d) - 1))
  nch <- pmax(nchar(hdr), vapply(d, function(x) max(nchar(x), 0), numeric(1))) + 3
  x_left <- c(0, cumsum(nch)[-length(d)]) / sum(nch); x_right <- cumsum(nch) / sum(nch)
  row_h <- font_size * 1.7 / 72 * 2.54                         # cm per row
  height_cm <- (n_row + 1) * row_h + 0.3
  png(file.path(dir_paper, paste0(name, ".png")), width = width_cm, height = height_cm, units = "cm", res = 300)
  grid::grid.newpage()
  vp <- grid::viewport(x = 0.5, y = 0.5, width = grid::unit(width_cm - 0.2, "cm"), height = grid::unit(height_cm - 0.2, "cm"))
  grid::pushViewport(vp)
  y_of <- function(i) grid::unit(1, "npc") - grid::unit((i - 0.5) * row_h, "cm")   # i = 1: header
  put <- function(txt, j, i, bold = FALSE) {
    x <- if (align[j] == "l") x_left[j] + 0.005 else x_right[j] - 0.005
    grid::grid.text(txt, x = grid::unit(x, "npc"), y = y_of(i), just = if (align[j] == "l") "left" else "right",
                    gp = grid::gpar(fontsize = font_size, fontface = if (bold) "bold" else "plain"))
  }
  for (j in seq_along(d)) { put(hdr[j], j, 1, TRUE); for (i in seq_len(n_row)) put(d[[j]][i], j, i + 1) }
  rule <- function(y, lwd) grid::grid.lines(c(0, 1), grid::unit.c(y, y), gp = grid::gpar(lwd = lwd))
  rule(grid::unit(1, "npc"), 1.2); rule(grid::unit(1, "npc") - grid::unit(row_h, "cm"), 0.6)
  rule(grid::unit(1, "npc") - grid::unit((n_row + 1) * row_h, "cm"), 1.2)
  dev.off()
  msg("Table image saved: ", file.path(dir_paper, paste0(name, ".png")))
}
fmt <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")
pct <- function(x) ifelse(x == 0, "-", formatC(100 * x, format = "f", digits = 1))

# Several ggplots side by side, each with its own legend, in one PNG
save_side_by_side <- function(plots, name, height_cm, width_cm = 17) {
  png(file.path(dir_paper, paste0(name, ".png")), width = width_cm, height = height_cm, units = "cm", res = 300)
  grid::grid.newpage(); grid::pushViewport(grid::viewport(layout = grid::grid.layout(1, length(plots))))
  for (i in seq_along(plots)) print(plots[[i]], vp = grid::viewport(layout.pos.row = 1, layout.pos.col = i))
  dev.off()
  msg("Figure saved: ", file.path(dir_paper, paste0(name, ".png")))
}
# Two-colour ramp for maps with discrete classes: orange (low) -> light neutral
# (middle) -> blue (high), interpolated in Lab space (maximal contrast between
# low and high classes)
ramp_2col <- function(n) grDevices::colorRampPalette(c("#b8521a", "#f3eee6", "#1f5aa6"), space = "Lab")(n)

scale_years <- function() ggplot2::scale_x_continuous(breaks = seq(2005, max(years_all), 5),
                                                      expand = ggplot2::expansion(add = 0.6))

# ---- CDSE S3 access through GDAL --------------------------------------------
setup_cdse_s3 <- function() {
  key <- Sys.getenv("CDSE_S3_KEY"); secret <- Sys.getenv("CDSE_S3_SECRET")
  if (key == "" || secret == "")
    stop("CDSE_S3_KEY / CDSE_S3_SECRET not set. See 00_config.R (Copernicus Data Space credentials).")
  opts <- c(AWS_S3_ENDPOINT = "eodata.dataspace.copernicus.eu",
            AWS_VIRTUAL_HOSTING = "FALSE", AWS_HTTPS = "YES",
            AWS_ACCESS_KEY_ID = key, AWS_SECRET_ACCESS_KEY = secret,
            AWS_DEFAULT_REGION = "default",
            GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
            CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.tiff",
            GDAL_HTTP_MAX_RETRY = "5", GDAL_HTTP_RETRY_DELAY = "2")
  do.call(Sys.setenv, as.list(opts))
  terra::setGDALconfig(paste0(names(opts), "=", opts))
  invisible(TRUE)
}
s3_to_vsi <- function(href) sub("^s3://", "/vsis3/", href)

# ---- STAC search (public, no authentication) --------------------------------
# Returns a data.table with one row per item: id, start, end, href of `asset`
# (or of the first .tif asset), projection code and bbox.
stac_parse <- function(js, asset) {
  rbindlist(lapply(js$features, function(f) {
    a <- f$assets
    key <- if (!is.null(asset) && !is.null(a[[asset]])) asset else {
      tifs <- names(a)[vapply(a, function(x) grepl("\\.tiff?$", x$href), logical(1))]
      # prefer the data layer over quality / count layers
      main <- tifs[!grepl("QFLAG|QL|NOBS|LENGTH|ERR", toupper(vapply(a[tifs], `[[`, "", "href")))]
      if (length(main)) main[1] else if (length(tifs)) tifs[1] else NA_character_
    }
    p <- f$properties
    first <- function(...) { for (x in list(...)) if (!is.null(x)) return(x); NA_character_ }
    data.table(
      id    = f$id,
      start = as.Date(substr(first(p$start_datetime, p$datetime), 1, 10)),
      end   = as.Date(substr(first(p$end_datetime, p$datetime), 1, 10)),
      href  = if (is.na(key)) NA_character_ else a[[key]]$href,
      proj  = first(p[["proj:code"]], if (!is.null(p[["proj:epsg"]])) paste0("EPSG:", p[["proj:epsg"]])),
      xmin = f$bbox[[1]], ymin = f$bbox[[2]], xmax = f$bbox[[3]], ymax = f$bbox[[4]])
  }))
}

# Results are cached in raw_data/stac_cache/ (a restart does not query again).
# The bounding box is applied locally on the item footprints, not sent to the
# server: spatial queries on large collections time out (HTTP 504).
# A request that keeps failing stops after ~max_tries x timeout seconds.
stac_search <- function(collection, start, end, bbox = NULL, asset = NULL, limit = 500,
                        timeout = 120, max_tries = 4, use_cache = TRUE) {
  cache_dir <- file.path(dir_raw, "stac_cache")
  dir.create(cache_dir, showWarnings = FALSE)
  cache <- file.path(cache_dir, sprintf("%s_%s_%s_%s.rds", collection, start, end,
                                        if (is.null(asset)) "any" else asset))
  if (use_cache && file.exists(cache)) {
    res <- readRDS(cache)
  } else {
    body <- list(collections = list(collection),
                 datetime = sprintf("%sT00:00:00Z/%sT23:59:59Z", start, end),
                 limit = limit)
    req <- httr2::request(paste0(stac_url, "/search")) |> httr2::req_body_json(body)
    out <- list()
    for (page in 1:1000) {
      js <- req |> httr2::req_timeout(timeout) |>
        httr2::req_retry(max_tries = max_tries, backoff = function(i) 10 * i) |>
        httr2::req_perform() |> httr2::resp_body_json(simplifyVector = FALSE)
      out[[page]] <- stac_parse(js, asset)
      nxt <- Filter(function(l) identical(l$rel, "next"), js$links)
      if (!length(nxt)) break
      nxt <- nxt[[1]]
      req <- httr2::request(nxt$href)
      if (!is.null(nxt$method) && toupper(nxt$method) == "POST")
        req <- httr2::req_body_json(req, if (!is.null(nxt$body)) nxt$body else body)
    }
    res <- unique(rbindlist(out), by = "id")
    # cache only complete, non-empty answers for past periods
    if (nrow(res) && as.Date(end) < Sys.Date()) saveRDS(res, cache)
  }
  if (!is.null(bbox) && nrow(res))
    res <- res[xmax >= bbox[["xmin"]] & xmin <= bbox[["xmax"]] & ymax >= bbox[["ymin"]] & ymin <= bbox[["ymax"]]]
  res
}

# ---- DMP: keep one item per dekad (most consolidated version) --------------
# Item ids contain RTx (RT0 = near real time ... RT6 = final consolidation).
# Items without RT tag (archive products) rank above all RT versions.
select_dekads <- function(items) {
  items <- copy(items)
  rt <- suppressWarnings(as.integer(sub(".*-RT([0-9]).*", "\\1", items$id)))
  rt[!grepl("-RT[0-9]", items$id)] <- 99L
  items[, rt := rt]
  setorder(items, end, -rt)
  items <- items[, .SD[1], by = end]
  items[, days := as.integer(end - start) + 1L]
  items[]
}

# ---- DMP grids ---------------------------------------------------------------
# European window of a global DMP grid, snapped outwards to multiples of
# block_size pixels so that the blocks read later coincide with the internal
# tiles of the cloud-optimised GeoTIFFs (fewer bytes downloaded).
make_eu_grid <- function(full, bbox, block = block_size) {
  r0 <- rowFromY(full, bbox[["ymax"]]); r1 <- rowFromY(full, bbox[["ymin"]])
  c0 <- colFromX(full, bbox[["xmin"]]); c1 <- colFromX(full, bbox[["xmax"]])
  r0 <- (r0 - 1) %/% block * block + 1; c0 <- (c0 - 1) %/% block * block + 1
  r1 <- min(nrow(full), ceiling(r1 / block) * block); c1 <- min(ncol(full), ceiling(c1 / block) * block)
  rx <- res(full)
  e <- ext(xFromCol(full, c0) - rx[1] / 2, xFromCol(full, c1) + rx[1] / 2,
           yFromRow(full, r1) - rx[2] / 2, yFromRow(full, r0) + rx[2] / 2)
  list(ext = as.vector(e), nrow = r1 - r0 + 1, ncol = c1 - c0 + 1, crs = crs(full),
       r0 = r0, c0 = c0, block = block)
}
template_from_grid <- function(g)
  rast(nrows = g$nrow, ncols = g$ncol, ext = ext(g$ext), crs = g$crs)
grid_of_year <- function(y) if (y <= 2013) "1km" else "300m"
grid_file <- function(g) file.path(dir_grids, paste0("grid_", g, ".rds"))
zone_file <- function(v, g) file.path(dir_grids, sprintf("nuts3_%d_%s.tif", v, g))
zone_lookup_file <- function(v) file.path(dir_grids, sprintf("nuts3_%d_ids.csv", v))

# Blocks of a grid: one row per block (local row/col in the window)
grid_blocks <- function(g) {
  b <- CJ(row = seq(1, g$nrow, by = g$block), col = seq(1, g$ncol, by = g$block))
  b[, `:=`(nrows = pmin(g$block, g$nrow - row + 1), ncols = pmin(g$block, g$ncol - col + 1))]
  b[]
}

# ---- Pixel index of a year (local files only) --------------------------------
# For each block: positions of the retained pixels (mask > mask_min_cover for
# grass or maize, inside an EU/UK NUTS3), their NUTS3 id, and which forage type
# they count for (1 = grass, 2 = maize; with mask_min_cover >= 0.5 a pixel
# cannot be both). Also returns per NUTS3 the
# number of retained pixels and the Copernicus area (cover share x pixel area).
build_year_index <- function(zone_f, mask_grass_f, mask_maize_f, g) {
  z  <- rast(zone_f); mg <- rast(mask_grass_f); mm <- rast(mask_maize_f)
  # pixel area (ha) only depends on the row in a lon/lat grid
  tmpl <- template_from_grid(g)
  col1 <- rast(nrows = g$nrow, ncols = 1, ext = ext(xmin(tmpl), xmin(tmpl) + res(tmpl)[1], ymin(tmpl), ymax(tmpl)),
               crs = g$crs)
  row_area <- values(cellSize(col1, unit = "ha"))[, 1]
  blocks <- grid_blocks(g)
  nz <- max(global(z, "max", na.rm = TRUE)[1, 1], 1)
  stats <- matrix(0, nz, 4, dimnames = list(NULL, c("n_grass", "n_maize", "area_grass", "area_maize")))
  out <- vector("list", nrow(blocks))
  readStart(z); readStart(mg); readStart(mm)
  on.exit({ readStop(z); readStop(mg); readStop(mm) }, add = TRUE)
  for (i in seq_len(nrow(blocks))) {
    b <- blocks[i]
    zv <- readValues(z, b$row, b$nrows, b$col, b$ncols)
    zv[!is.na(zv) & zv == 0] <- NA            # 0 = outside all NUTS3
    if (all(is.na(zv))) next
    gv <- readValues(mg, b$row, b$nrows, b$col, b$ncols)
    mv <- readValues(mm, b$row, b$nrows, b$col, b$ncols)
    inz <- !is.na(zv)
    ar <- rep(row_area[b$row:(b$row + b$nrows - 1)], each = b$ncols)
    # Copernicus areas (all pixels, cover share x area)
    for (k in 1:2) {
      fr <- if (k == 1) gv else mv
      ok <- inz & !is.na(fr)
      if (any(ok)) {
        rs <- rowsum(fr[ok] * ar[ok], zv[ok])
        stats[as.integer(rownames(rs)), 2 + k] <- stats[as.integer(rownames(rs)), 2 + k] + rs[, 1]
      }
    }
    kg <- inz & !is.na(gv) & gv > mask_min_cover
    km <- inz & !is.na(mv) & mv > mask_min_cover & !kg   # !kg: safety if masks overlap
    sel <- which(kg | km)
    if (!length(sel)) next
    stats[, 1] <- stats[, 1] + tabulate(zv[kg], nz)
    stats[, 2] <- stats[, 2] + tabulate(zv[km], nz)
    out[[i]] <- list(row = b$row, col = b$col, nrows = b$nrows, ncols = b$ncols, idx = sel,
                     zone = as.integer(zv[sel]), code = ifelse(kg[sel], 1L, 2L))
  }
  list(blocks = Filter(Negate(is.null), out), stats = stats)
}

# ---- DMP sums per NUTS3 for one dekad (run in parallel workers) ---------------
# Reads, in the remote dekad file, only the blocks containing retained pixels,
# and returns per NUTS3 and forage type the sum of valid DMP values (kg/ha/day)
# and the number of valid pixels. `task` has: year, vsi, index_file, out_file.
# Network read errors (e.g. "TIFFFillTile: Read error ... got 0 bytes") are
# retried after a pause; a dekad that still fails is reported back (text
# starting with "FAILED") instead of stopping all workers.
dekad_sums <- function(task, tries = 4, wait = 30) {
  if (file.exists(task$out_file)) return(task$out_file)
  for (i in seq_len(tries)) {
    res <- tryCatch(dekad_sums_once(task), error = function(e) paste("FAILED:", conditionMessage(e)))
    if (!startsWith(res, "FAILED")) return(res)
    cat(format(Sys.time(), "%H:%M:%S"), basename(task$vsi), "attempt", i, res, "\n")
    if (i < tries) Sys.sleep(wait * i)
  }
  paste0(res, " [", basename(task$vsi), "]")
}
dekad_sums_once <- function(task) {
  idx <- readRDS(task$index_file)
  g <- idx$grid
  r <- rast(task$vsi)
  manual_scale <- isTRUE(all.equal(unname(scoff(r)[1, 1]), 1))  # apply 0.01 if GDAL has no scale
  nz <- nrow(idx$stats)
  s <- matrix(0, nz, 2); n <- matrix(0, nz, 2)   # columns: grass, maize
  readStart(r); on.exit(readStop(r), add = TRUE)
  for (b in idx$blocks) {
    v <- readValues(r, g$r0 + b$row - 1, b$nrows, g$c0 + b$col - 1, b$ncols)[b$idx]
    if (manual_scale) v <- v * 0.01
    ok <- !is.na(v) & v >= 0          # -1 missing, -2 sea
    for (k in 1:2) {
      sel <- ok & b$code == k           # k = 1: grass pixels, k = 2: maize pixels
      if (!any(sel)) next
      rs <- rowsum(v[sel], b$zone[sel])
      id <- as.integer(rownames(rs))
      s[id, k] <- s[id, k] + rs[, 1]
      n[id, k] <- n[id, k] + tabulate(b$zone[sel], nz)[id]
    }
  }
  res <- data.table(zone = rep(seq_len(nz), 2), forage = rep(c("grass", "maize"), each = nz),
                    dmp_sum = c(s), n_valid = c(n))[n_valid > 0]
  tmp <- paste0(task$out_file, ".tmp")
  saveRDS(res, tmp); file.rename(tmp, task$out_file)   # atomic: no half-written files
  task$out_file
}

# Maize share per 100 m cell for one 10 m crop-type tile (run in workers)
cty_tile_share <- function(tk, tries = 4, wait = 60) {
  # Network errors (connection lost, DNS failure) are retried after a pause;
  # a tile that still fails is reported back instead of stopping all workers.
  for (i in seq_len(tries)) {
    res <- tryCatch({
      r <- rast(tk$vsi)
      # 1 = maize, 0 = other, NA = outside; stored as 1 byte per pixel (100 MB
      # per 10 m tile instead of 400 MB) in a temporary file deleted at the end
      m_file <- tempfile(tmpdir = file.path(dir_raw, "terra_tmp"), fileext = ".tif")
      m <- classify(r, cbind(c(cty_maize_code, 65535), c(1, NA)), others = 0,
                    filename = m_file, wopt = list(datatype = "INT1U"))
      tmp <- sub("\\.tif$", "_tmp.tif", tk$of)
      aggregate(m, fact = 10, fun = "mean", na.rm = TRUE, filename = tmp, overwrite = TRUE,
                wopt = list(datatype = "FLT4S", gdal = "COMPRESS=DEFLATE"))
      rm(m); unlink(m_file)
      file.rename(tmp, tk$of)   # only complete tiles get the final name
      "ok"
    }, error = function(e) conditionMessage(e))
    if (identical(res, "ok")) return("ok")
    cat(format(Sys.time(), "%H:%M:%S"), "tile", basename(tk$of), "attempt", i, "failed:", res, "\n")
    if (i < tries) Sys.sleep(wait * i)
  }
  paste(basename(tk$of), "->", res)
}

# Wrapper sent to the workers: calls the worker's own copy of the function
# (so that no large object of the main session is copied to the workers)
worker_fun <- function(f) {
  name <- deparse(substitute(f))
  w <- eval(bquote(function(x) .(as.name(name))(x)))
  environment(w) <- globalenv()
  w
}

# Start the parallel workers (each loads the config and the S3 settings)
start_workers <- function(n = n_workers) {
  gc()   # free memory of the main session before starting the workers
  # what the workers print (including crash messages) goes to a log file
  wlog <- file.path(dir_out, "logs", sprintf("workers_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(dirname(wlog), recursive = TRUE, showWarnings = FALSE)
  cl <- parallel::makeCluster(n, outfile = wlog)
  # start-up step by step, each step written in the workers log, errors returned
  init <- tryCatch(parallel::clusterCall(cl, function(wd, memfrac) {
    step <- function(x) { cat(format(Sys.time(), "%H:%M:%S"), "pid", Sys.getpid(), ":", x, "\n"); utils::flush.console() }
    tryCatch({
      setwd(wd);                                 step("wd set")
      Sys.setenv(FORAGE_WORKER = "1")            # workers do not write outputs/00_config.R
      suppressMessages(source("00_config.R"));   step("config loaded")
      setup_cdse_s3();                           step("S3 configured")
      terra::terraOptions(memfrac = memfrac);    step(paste("memfrac", round(memfrac, 3)))
      "ok"
    }, error = function(e) paste("error:", conditionMessage(e)))
  }, getwd(), 0.5 / n), error = function(e) {
    try(parallel::stopCluster(cl), silent = TRUE)
    stop("A worker crashed while starting (see ", wlog, "). Restart R (Session > Restart R), ",
         "check in the Task Manager that no Rscript.exe process is left, and run again. Original error: ",
         conditionMessage(e), call. = FALSE)
  })
  bad <- unlist(init) != "ok"
  if (any(bad)) {
    parallel::stopCluster(cl)
    stop("Worker start-up failed: ", paste(unique(unlist(init)[bad]), collapse = " | "), call. = FALSE)
  }
  msg("  ", n, " workers started (their messages: ", wlog, ")")
  cl
}

# ---- Masks: fraction of each DMP pixel covered by the forage type -----------
# `src` is a SpatRaster (EPSG:3035) of cover fractions in [0,1] (NA outside).
# GDAL "average" resampling gives the mean of all contributing source cells.
fraction_on_grid <- function(src, template, out_file, divide_by = 1) {
  wo <- list(datatype = "FLT4S", gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "TILED=YES"))
  if (divide_by == 1) return(project(src, template, method = "average",
                                     filename = out_file, overwrite = TRUE, wopt = wo))
  p <- project(src, template, method = "average")
  writeRaster(p / divide_by, out_file, overwrite = TRUE, wopt = wo)
}

# CLC raster -> 0/1 for the given 3-digit classes. CLC GeoTIFFs from CLMS store
# either class indices 1-44 (with a legend) or 3-digit codes; both handled.
clc_index <- c(`111`=1,`112`=2,`121`=3,`122`=4,`123`=5,`124`=6,`131`=7,`132`=8,`133`=9,
               `141`=10,`142`=11,`211`=12,`212`=13,`213`=14,`221`=15,`222`=16,`223`=17,
               `231`=18,`241`=19,`242`=20,`243`=21,`244`=22,`311`=23,`312`=24,`313`=25,
               `321`=26,`322`=27,`323`=28,`324`=29,`331`=30,`332`=31,`333`=32,`334`=33,
               `335`=34,`411`=35,`412`=36,`421`=37,`422`=38,`423`=39,`511`=40,`512`=41,
               `521`=42,`522`=43,`523`=44)
clc_binary <- function(clc, classes, out_file) {
  # stored min/max statistics when available (instant), else a full scan
  mx <- tryCatch(minmax(clc, compute = FALSE)[2, 1], error = function(e) NA)
  if (is.na(mx) || !is.finite(mx)) mx <- global(clc, "max", na.rm = TRUE)[1, 1]
  vals <- if (mx > 100) classes else unname(clc_index[as.character(classes)])
  valid <- if (mx > 100) unname(as.integer(names(clc_index))) else 1:44
  # 1 = class of interest, 0 = other valid class, NA = no data / outside
  tmp <- sub("\\.tif$", "_tmp.tif", out_file)
  classify(clc, rcl = cbind(c(vals, setdiff(valid, vals)),
                            c(rep(1, length(vals)), rep(0, length(setdiff(valid, vals))))),
           others = NA, filename = tmp, overwrite = TRUE,
           wopt = list(datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "TILED=YES")))
  file.rename(tmp, out_file)
  rast(out_file)
}

# ---- NUTS raster on a DMP grid (pixel centres) ------------------------------
nuts3_on_grid <- function(nuts3, template) {
  v <- vect(nuts3)
  v$id <- seq_len(nrow(v))
  rasterize(v, template, field = "id")
}
