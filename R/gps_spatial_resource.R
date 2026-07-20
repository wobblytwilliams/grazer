grz_spatial_epoch_arg <- function(epoch, epoch_mins, epoch_missing = FALSE) {
  if (!is.null(epoch_mins)) {
    if (!epoch_missing && !identical(epoch, "interval")) {
      stop("Use `epoch_mins` with `epoch = \"interval\"` or leave `epoch` unset.", call. = FALSE)
    }
    grz_require_positive_mins(epoch_mins, "epoch_mins")
    return("interval")
  }

  epoch <- match.arg(epoch, c("day", "hour", "week", "month", "interval"))
  if (epoch == "interval") {
    stop("`epoch_mins` is required when `epoch = \"interval\"`.", call. = FALSE)
  }
  epoch
}

grz_metric_crs <- function(data, metric_crs = NULL) {
  if (!is.null(metric_crs)) {
    return(metric_crs)
  }

  lon <- as.numeric(data$lon)
  lat <- as.numeric(data$lat)
  ok <- is.finite(lon) & is.finite(lat)
  if (!any(ok)) {
    return(3857)
  }

  centre_lon <- mean(lon[ok], na.rm = TRUE)
  centre_lat <- mean(lat[ok], na.rm = TRUE)
  zone <- floor((centre_lon + 180) / 6) + 1
  zone <- max(1, min(60, zone))
  if (centre_lat >= 0) {
    32600 + zone
  } else {
    32700 + zone
  }
}

grz_add_spatial_epoch <- function(dt, epoch, epoch_mins) {
  epoch_dt <- grz_epoch_table(dt$datetime, epoch = epoch, epoch_mins = epoch_mins)
  dt[, `:=`(
    epoch = epoch_dt$epoch,
    epoch_start = epoch_dt$epoch_start,
    epoch_end = epoch_dt$epoch_end,
    epoch_mins = epoch_dt$epoch_mins
  )]
  dt
}

grz_spatial_points <- function(data, epoch, epoch_mins, groups, metric_crs, fun_name) {
  grz_require_sf(fun_name)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  valid <- grz_gps_valid_coord(dt, fun_name = fun_name) & !is.na(dt$datetime)
  if (!all(valid)) {
    stop("`", fun_name, "` requires valid datetime, longitude, and latitude values.", call. = FALSE)
  }

  grp <- grz_default_group_cols(dt, groups = groups)
  dt <- grz_add_spatial_epoch(dt, epoch = epoch, epoch_mins = epoch_mins)
  dt[, .grz_row_id := .I]

  out_crs <- grz_metric_crs(dt, metric_crs = metric_crs)
  pts <- sf::st_as_sf(dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts_m <- sf::st_transform(pts, out_crs)
  xy <- sf::st_coordinates(pts_m)
  dt[, `:=`(.grz_x = xy[, 1], .grz_y = xy[, 2])]

  list(data = dt, points_m = pts_m, groups = grp, metric_crs = out_crs)
}

grz_validate_percent <- function(percent) {
  if (!is.numeric(percent) || length(percent) < 1L || any(!is.finite(percent)) || any(percent <= 0) || any(percent > 100)) {
    stop("`percent` must contain values > 0 and <= 100.", call. = FALSE)
  }
  unique(as.numeric(percent))
}

grz_validate_positive_number <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a single positive number.", call. = FALSE)
  }
  invisible(x)
}

grz_mcp_one <- function(x, y, percent, metric_crs) {
  ok <- is.finite(x) & is.finite(y)
  x <- as.numeric(x[ok])
  y <- as.numeric(y[ok])
  n_total <- length(x)
  if (n_total < 3L) {
    return(list(area_ha = NA_real_, geometry = sf::st_polygon(), n_used = n_total))
  }

  if (percent < 100) {
    centre_x <- mean(x)
    centre_y <- mean(y)
    radius <- sqrt((x - centre_x)^2 + (y - centre_y)^2)
    keep <- radius <= stats::quantile(radius, probs = percent / 100, na.rm = TRUE, type = 7)
    x <- x[keep]
    y <- y[keep]
  }

  coords <- unique(cbind(x, y))
  if (nrow(coords) < 3L) {
    return(list(area_ha = NA_real_, geometry = sf::st_polygon(), n_used = nrow(coords)))
  }

  hull <- grDevices::chull(coords[, 1], coords[, 2])
  ring <- coords[hull, , drop = FALSE]
  ring <- rbind(ring, ring[1L, , drop = FALSE])
  geom <- sf::st_polygon(list(ring))
  area_m2 <- as.numeric(sf::st_area(sf::st_sfc(geom, crs = metric_crs)))
  if (!is.finite(area_m2) || area_m2 <= 0) {
    return(list(area_ha = NA_real_, geometry = sf::st_polygon(), n_used = nrow(coords)))
  }

  list(area_ha = area_m2 / 10000, geometry = geom, n_used = nrow(coords))
}

grz_empty_resource_distance <- function(dt, rc, resource_id_col, resource_type_col) {
  dt[, nearest_resource_id := NA_character_]
  dt[, resource_distance_m := NA_real_]
  if (!is.null(resource_type_col)) {
    dt[, nearest_resource_type := NA_character_]
  }
  drop_cols <- intersect(".grz_row_id", names(dt))
  dt[, (drop_cols) := NULL]
  grz_as_output(dt, rc)
}

grz_prepare_resources <- function(resources, resource_crs, resource_id_col, resource_type_col, fun_name) {
  grz_require_sf_object(resources, arg = "resources")

  res <- resources
  if (is.na(sf::st_crs(res))) {
    if (is.null(resource_crs)) {
      stop("`resources` has no CRS. Set `resource_crs` explicitly.", call. = FALSE)
    }
    sf::st_crs(res) <- resource_crs
  }

  if (!is.null(resource_id_col)) {
    if (!is.character(resource_id_col) || length(resource_id_col) != 1L || is.na(resource_id_col) || trimws(resource_id_col) == "") {
      stop("`resource_id_col` must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!(resource_id_col %in% names(res))) {
      stop("`resource_id_col` was not found in `resources`.", call. = FALSE)
    }
  }

  if (is.null(resource_id_col)) {
    res$.grz_resource_id <- as.character(seq_len(nrow(res)))
    resource_id_col <- ".grz_resource_id"
  }
  if (!is.null(resource_type_col) && !(resource_type_col %in% names(res))) {
    stop("`resource_type_col` was not found in `resources`.", call. = FALSE)
  }

  res[[resource_id_col]] <- as.character(res[[resource_id_col]])
  list(resources = res, resource_id_col = resource_id_col, resource_type_col = resource_type_col)
}

#' Calculate GPS minimum convex polygon summaries
#'
#' Builds minimum convex polygons for each animal or sensor stream within an
#' epoch. GPS fixes are read as WGS84 longitude and latitude, then transformed
#' to `metric_crs` for area calculation. When `metric_crs = NULL`, a UTM CRS is
#' selected from the centre of the GPS fixes.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param percent MCP percentage or percentages. Values must be > 0 and <= 100.
#' @param epoch Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`,
#'   or `"interval"`.
#' @param epoch_mins Positive epoch duration in minutes. Supplying this uses
#'   fixed-duration `"interval"` epochs anchored to Unix time in UTC.
#' @param groups Grouping columns for summaries. Defaults to `deployment_id`
#'   and `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.
#' @param min_fixes Minimum number of fixes required before an MCP is computed.
#' @param metric_crs Projected CRS used for area calculations. `NULL` selects a
#'   UTM CRS from the GPS coordinates.
#' @param return_geometry Logical; return `sf` polygon geometry instead of a
#'   plain data frame.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class for tabular output: `"data.frame"`
#'   (default) or `"data.table"`. Ignored when `return_geometry = TRUE`.
#'
#' @return A data frame with MCP area summaries, or an `sf` object when
#'   `return_geometry = TRUE`.
#' @export
gps_mcp <- function(
  data,
  percent = c(100, 95),
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  min_fixes = 5,
  metric_crs = NULL,
  return_geometry = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_spatial_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)
  percent <- grz_validate_percent(percent)
  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || !is.finite(min_fixes) || min_fixes < 3) {
    stop("`min_fixes` must be a single number >= 3.", call. = FALSE)
  }
  if (!is.logical(return_geometry) || length(return_geometry) != 1L || is.na(return_geometry)) {
    stop("`return_geometry` must be TRUE or FALSE.", call. = FALSE)
  }

  sp <- grz_spatial_points(data, epoch = epoch, epoch_mins = epoch_mins, groups = groups, metric_crs = metric_crs, fun_name = "gps_mcp()")
  dt <- sp$data
  by_cols <- c(sp$groups, "epoch", "epoch_start", "epoch_end", "epoch_mins")
  groups_dt <- dt[, list(rows = list(.I)), by = by_cols]

  records <- vector("list", nrow(groups_dt) * length(percent))
  geoms <- vector("list", length(records))
  k <- 0L
  for (i in seq_len(nrow(groups_dt))) {
    idx <- groups_dt$rows[[i]]
    for (pct in percent) {
      k <- k + 1L
      enough <- length(idx) >= min_fixes
      hull <- if (enough) {
        grz_mcp_one(dt$.grz_x[idx], dt$.grz_y[idx], pct, sp$metric_crs)
      } else {
        list(area_ha = NA_real_, geometry = sf::st_polygon(), n_used = length(idx))
      }
      records[[k]] <- data.table::data.table(
        groups_dt[i, ..by_cols],
        mcp_percent = pct,
        n_fixes = length(idx),
        n_fixes_used = hull$n_used,
        mcp_area_ha = hull$area_ha,
        metric_crs = as.character(sf::st_crs(sp$metric_crs)$input)
      )
      geoms[[k]] <- hull$geometry
    }
  }

  out <- data.table::rbindlist(records, fill = TRUE)
  if (isTRUE(return_geometry)) {
    geom <- sf::st_sfc(geoms, crs = sp$metric_crs)
    sf_out <- sf::st_sf(out, geometry = geom)
    sf_out <- sf::st_transform(sf_out, 4326)
    if (isTRUE(verbose)) {
      cat(sprintf("[gps_mcp] rows=%s geometry=TRUE\n", format(nrow(sf_out), big.mark = ",")))
    }
    return(sf_out)
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_mcp] rows=%s\n", format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

grz_kde_bandwidth <- function(x, y, bandwidth_m = NULL) {
  if (!is.null(bandwidth_m)) {
    if (!is.numeric(bandwidth_m) || !(length(bandwidth_m) %in% c(1L, 2L)) || any(!is.finite(bandwidth_m)) || any(bandwidth_m <= 0)) {
      stop("`bandwidth_m` must be NULL, one positive number, or two positive numbers.", call. = FALSE)
    }
    if (length(bandwidth_m) == 1L) {
      return(c(x = as.numeric(bandwidth_m), y = as.numeric(bandwidth_m)))
    }
    return(c(x = as.numeric(bandwidth_m[[1L]]), y = as.numeric(bandwidth_m[[2L]])))
  }

  fallback <- function(v) {
    out <- suppressWarnings(stats::bw.nrd0(v))
    span <- diff(range(v, finite = TRUE))
    if (!is.finite(out) || out <= 0) {
      out <- if (is.finite(span) && span > 0) span / 4 else 1
    }
    max(out, 1)
  }

  c(x = fallback(x), y = fallback(y))
}

grz_kde_cell_geometry <- function(x, y, cell_size_m, crs) {
  geoms <- lapply(seq_along(x), function(i) {
    half <- cell_size_m[i] / 2
    ring <- matrix(
      c(
        x[i] - half, y[i] - half,
        x[i] + half, y[i] - half,
        x[i] + half, y[i] + half,
        x[i] - half, y[i] + half,
        x[i] - half, y[i] - half
      ),
      ncol = 2,
      byrow = TRUE
    )
    sf::st_polygon(list(ring))
  })
  sf::st_sfc(geoms, crs = crs)
}

#' Calculate GPS kernel-density space use
#'
#' Estimates two-dimensional kernel-density use surfaces for each animal or
#' sensor stream within an epoch. The output is a grid of high-use cells for
#' requested utilisation percentages, ranked from highest to lowest density.
#'
#' @inheritParams gps_mcp
#' @param bandwidth_m Kernel bandwidth in metres. Use `NULL` to estimate
#'   separate x and y bandwidths from the data, one number for both axes, or two
#'   numbers for x and y.
#' @param cell_size_m Grid cell size in metres. If `NULL`, a cell size is chosen
#'   from the estimated bandwidth.
#' @param percent Utilisation percentages to return, such as `95` or `50`.
#' @param keep_all Logical; return all density cells for each requested
#'   percentage and flag membership, rather than only cells inside the
#'   utilisation percentage.
#' @param max_cells Maximum grid cells per group before the cell size is
#'   increased automatically.
#' @param min_fixes Minimum number of fixes required before a KDE surface is
#'   computed.
#' @param return_geometry Logical; return `sf` grid-cell polygons instead of a
#'   plain data frame.
#' @param return_class Output class for tabular output: `"data.frame"`
#'   (default) or `"data.table"`. Ignored when `return_geometry = TRUE`.
#'
#' @return A data frame of KDE grid cells, or an `sf` object of grid-cell
#'   polygons when `return_geometry = TRUE`.
#' @export
gps_kde <- function(
  data,
  bandwidth_m = NULL,
  cell_size_m = NULL,
  percent = c(95, 50),
  keep_all = FALSE,
  max_cells = 10000,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  min_fixes = 5,
  metric_crs = NULL,
  return_geometry = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_spatial_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)
  percent <- grz_validate_percent(percent)
  if (!is.null(cell_size_m)) {
    grz_validate_positive_number(cell_size_m, "cell_size_m")
  }
  if (!is.logical(keep_all) || length(keep_all) != 1L || is.na(keep_all)) {
    stop("`keep_all` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(max_cells) || length(max_cells) != 1L || !is.finite(max_cells) || max_cells < 100) {
    stop("`max_cells` must be a single number >= 100.", call. = FALSE)
  }
  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || !is.finite(min_fixes) || min_fixes < 3) {
    stop("`min_fixes` must be a single number >= 3.", call. = FALSE)
  }
  if (!is.logical(return_geometry) || length(return_geometry) != 1L || is.na(return_geometry)) {
    stop("`return_geometry` must be TRUE or FALSE.", call. = FALSE)
  }

  sp <- grz_spatial_points(data, epoch = epoch, epoch_mins = epoch_mins, groups = groups, metric_crs = metric_crs, fun_name = "gps_kde()")
  dt <- sp$data
  by_cols <- c(sp$groups, "epoch", "epoch_start", "epoch_end", "epoch_mins")
  groups_dt <- dt[, list(rows = list(.I)), by = by_cols]

  records <- list()
  k <- 0L
  for (i in seq_len(nrow(groups_dt))) {
    idx <- groups_dt$rows[[i]]
    x <- dt$.grz_x[idx]
    y <- dt$.grz_y[idx]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]
    y <- y[ok]
    n <- length(x)
    if (n < min_fixes) {
      next
    }

    bw <- grz_kde_bandwidth(x, y, bandwidth_m = bandwidth_m)
    cell_size <- if (is.null(cell_size_m)) max(1, min(bw) / 2) else as.numeric(cell_size_m)
    x_grid <- seq(min(x) - 3 * bw[["x"]], max(x) + 3 * bw[["x"]], by = cell_size)
    y_grid <- seq(min(y) - 3 * bw[["y"]], max(y) + 3 * bw[["y"]], by = cell_size)
    n_cells <- length(x_grid) * length(y_grid)
    if (n_cells > max_cells) {
      cell_size <- cell_size * sqrt(n_cells / max_cells)
      x_grid <- seq(min(x) - 3 * bw[["x"]], max(x) + 3 * bw[["x"]], by = cell_size)
      y_grid <- seq(min(y) - 3 * bw[["y"]], max(y) + 3 * bw[["y"]], by = cell_size)
    }

    z <- matrix(0, nrow = length(x_grid), ncol = length(y_grid))
    for (j in seq_len(n)) {
      dx <- stats::dnorm((x_grid - x[j]) / bw[["x"]]) / bw[["x"]]
      dy <- stats::dnorm((y_grid - y[j]) / bw[["y"]]) / bw[["y"]]
      z <- z + outer(dx, dy, "*")
    }
    z <- z / n

    cells <- data.table::data.table(
      cell_centre_x = rep(x_grid, times = length(y_grid)),
      cell_centre_y = rep(y_grid, each = length(x_grid)),
      kde_density = as.vector(z)
    )
    cells[, cell_probability := kde_density * cell_size^2]
    total_probability <- sum(cells$cell_probability, na.rm = TRUE)
    if (!is.finite(total_probability) || total_probability <= 0) {
      next
    }
    cells[, cell_probability := cell_probability / total_probability]
    data.table::setorder(cells, -kde_density, cell_centre_x, cell_centre_y)
    cells[, density_rank := seq_len(.N)]
    cells[, cumulative_probability := cumsum(cell_probability)]

    centres <- sf::st_as_sf(cells, coords = c("cell_centre_x", "cell_centre_y"), crs = sp$metric_crs, remove = FALSE)
    centres <- sf::st_transform(centres, 4326)
    xy <- sf::st_coordinates(centres)
    cells[, `:=`(cell_centre_lon = xy[, 1], cell_centre_lat = xy[, 2])]

    group_row <- groups_dt[i, ..by_cols]
    for (pct in percent) {
      cutoff_rank <- which(cells$cumulative_probability >= pct / 100)[1L]
      if (is.na(cutoff_rank)) {
        cutoff_rank <- nrow(cells)
      }
      selected <- data.table::copy(cells)
      selected[, inside_kde_percent := density_rank <= cutoff_rank]
      if (!isTRUE(keep_all)) {
        selected <- selected[inside_kde_percent == TRUE]
      }
      selected[, `:=`(
        kde_percent = pct,
        n_fixes = n,
        bandwidth_x_m = bw[["x"]],
        bandwidth_y_m = bw[["y"]],
        cell_size_m = cell_size,
        density_threshold = cells[density_rank == cutoff_rank, kde_density],
        kde_area_ha = sum(inside_kde_percent) * cell_size^2 / 10000,
        metric_crs = as.character(sf::st_crs(sp$metric_crs)$input)
      )]
      k <- k + 1L
      records[[k]] <- data.table::as.data.table(cbind(group_row[rep(1L, nrow(selected))], selected))
    }
  }

  if (length(records) == 0L) {
    out <- data.table::data.table()
  } else {
    out <- data.table::rbindlist(records, use.names = TRUE, fill = TRUE)
    first_cols <- c(by_cols, "kde_percent", "n_fixes", "kde_area_ha")
    data.table::setcolorder(out, c(first_cols, setdiff(names(out), first_cols)))
    data.table::setorderv(out, c(by_cols, "kde_percent", "density_rank"))
  }

  if (isTRUE(return_geometry)) {
    geom <- grz_kde_cell_geometry(out$cell_centre_x, out$cell_centre_y, out$cell_size_m, sp$metric_crs)
    sf_out <- sf::st_sf(out, geometry = geom)
    sf_out <- sf::st_transform(sf_out, 4326)
    if (isTRUE(verbose)) {
      cat(sprintf("[gps_kde] rows=%s geometry=TRUE\n", format(nrow(sf_out), big.mark = ",")))
    }
    return(sf_out)
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_kde] rows=%s\n", format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Summarise GPS spatial use by epoch
#'
#' Produces basic spatial summaries and MCP area fields for each stream and
#' epoch. Coordinates are WGS84 GPS fixes; area calculations are made after
#' transforming to `metric_crs`.
#'
#' @inheritParams gps_mcp
#'
#' @return Spatial summary table with fix counts, time span, coordinate bounds,
#'   centroid coordinates, and MCP area fields.
#' @export
gps_spatial <- function(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  min_fixes = 5,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_spatial_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)

  sp <- grz_spatial_points(data, epoch = epoch, epoch_mins = epoch_mins, groups = groups, metric_crs = metric_crs, fun_name = "gps_spatial()")
  dt <- sp$data
  by_cols <- c(sp$groups, "epoch", "epoch_start", "epoch_end", "epoch_mins")

  basic <- dt[, list(
    n_fixes = .N,
    start_datetime = min(datetime, na.rm = TRUE),
    end_datetime = max(datetime, na.rm = TRUE),
    span_hours = as.numeric(max(datetime, na.rm = TRUE) - min(datetime, na.rm = TRUE), units = "hours"),
    lon_min = min(lon, na.rm = TRUE),
    lon_max = max(lon, na.rm = TRUE),
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE),
    centroid_lon = mean(lon, na.rm = TRUE),
    centroid_lat = mean(lat, na.rm = TRUE)
  ), by = by_cols]

  mcp <- data.table::as.data.table(gps_mcp(
    data = dt,
    percent = c(100, 95),
    epoch = epoch,
    epoch_mins = epoch_mins,
    groups = sp$groups,
    min_fixes = min_fixes,
    metric_crs = sp$metric_crs,
    verbose = FALSE,
    return_class = "data.table"
  ))
  mcp_wide <- stats::reshape(
    as.data.frame(mcp[, c(by_cols, "mcp_percent", "mcp_area_ha"), with = FALSE]),
    idvar = by_cols,
    timevar = "mcp_percent",
    direction = "wide"
  )
  names(mcp_wide) <- sub("^mcp_area_ha\\.", "mcp", names(mcp_wide))
  names(mcp_wide) <- sub("$", "_area_ha", names(mcp_wide))
  names(mcp_wide)[names(mcp_wide) %in% paste0(by_cols, "_area_ha")] <- by_cols

  out <- merge(basic, data.table::as.data.table(mcp_wide), by = by_cols, all.x = TRUE, sort = FALSE)
  out[, metric_crs := as.character(sf::st_crs(sp$metric_crs)$input)]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_spatial] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Identify GPS high-use grid cells
#'
#' Counts fixes in square grid cells after transforming GPS fixes to a metric
#' CRS. Cells at or above `hotspot_quantile` of the cell-count distribution are
#' returned by default.
#'
#' @inheritParams gps_mcp
#' @param cell_size_m Grid cell size in metres.
#' @param hotspot_quantile Quantile of cell counts used as the hotspot cutoff.
#' @param keep_all Logical; return all cells rather than only hotspot cells.
#'
#' @return Data frame of grid-cell use summaries.
#' @export
gps_hotspots <- function(
  data,
  cell_size_m = 50,
  hotspot_quantile = 0.9,
  keep_all = FALSE,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_spatial_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)
  grz_validate_positive_number(cell_size_m, "cell_size_m")
  if (!is.numeric(hotspot_quantile) || length(hotspot_quantile) != 1L || !is.finite(hotspot_quantile) || hotspot_quantile < 0 || hotspot_quantile > 1) {
    stop("`hotspot_quantile` must be a single number between 0 and 1.", call. = FALSE)
  }
  if (!is.logical(keep_all) || length(keep_all) != 1L || is.na(keep_all)) {
    stop("`keep_all` must be TRUE or FALSE.", call. = FALSE)
  }

  sp <- grz_spatial_points(data, epoch = epoch, epoch_mins = epoch_mins, groups = groups, metric_crs = metric_crs, fun_name = "gps_hotspots()")
  dt <- sp$data
  by_cols <- c(sp$groups, "epoch", "epoch_start", "epoch_end", "epoch_mins")
  dt[, `:=`(
    cell_x = floor(.grz_x / cell_size_m),
    cell_y = floor(.grz_y / cell_size_m)
  )]

  out <- dt[, list(
    n_fixes = .N,
    cell_centre_x = (cell_x[1L] + 0.5) * cell_size_m,
    cell_centre_y = (cell_y[1L] + 0.5) * cell_size_m
  ), by = c(by_cols, "cell_x", "cell_y")]
  out[, total_fixes := sum(n_fixes), by = by_cols]
  out[, prop_fixes := n_fixes / total_fixes]
  out[, hotspot_count_threshold := stats::quantile(n_fixes, probs = hotspot_quantile, na.rm = TRUE, type = 7), by = by_cols]
  out[, is_hotspot := n_fixes >= hotspot_count_threshold]

  centres <- sf::st_as_sf(out, coords = c("cell_centre_x", "cell_centre_y"), crs = sp$metric_crs, remove = FALSE)
  centres <- sf::st_transform(centres, 4326)
  xy <- sf::st_coordinates(centres)
  out[, `:=`(cell_centre_lon = xy[, 1], cell_centre_lat = xy[, 2])]
  out[, metric_crs := as.character(sf::st_crs(sp$metric_crs)$input)]
  if (!isTRUE(keep_all)) {
    out <- out[is_hotspot == TRUE]
  }
  data.table::setorderv(out, c(by_cols, "cell_x", "cell_y"))

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_hotspots] rows=%s cell_size_m=%s\n", format(nrow(out), big.mark = ","), format(cell_size_m, trim = TRUE)))
  }
  grz_as_output(out, rc)
}

#' Calculate distance from GPS fixes to resources
#'
#' Appends the nearest resource and distance in metres for each GPS fix.
#' GPS fixes are treated as WGS84 longitude and latitude. Resource geometries
#' must have a CRS, or `resource_crs` must be supplied explicitly.
#'
#' @param resources `sf` object containing point, line, or polygon resources.
#' @param resource_id_col Column in `resources` used as the resource identifier.
#'   If `NULL`, row numbers are used.
#' @param resource_type_col Optional column in `resources` describing resource
#'   type, such as water, shade, or supplement.
#' @param resource_crs CRS to assign when `resources` has no CRS.
#' @inheritParams gps_mcp
#'
#' @return Input GPS rows with nearest-resource fields appended.
#' @export
gps_resource_distance <- function(
  data,
  resources,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  grz_require_sf("gps_resource_distance()")
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  valid <- grz_gps_valid_coord(dt, fun_name = "gps_resource_distance()") & !is.na(dt$datetime)
  if (!all(valid)) {
    stop("`gps_resource_distance()` requires valid datetime, longitude, and latitude values.", call. = FALSE)
  }
  dt[, .grz_row_id := .I]

  res_info <- grz_prepare_resources(resources, resource_crs, resource_id_col, resource_type_col, "gps_resource_distance()")
  res <- res_info$resources
  resource_id_col <- res_info$resource_id_col
  resource_type_col <- res_info$resource_type_col
  if (nrow(res) == 0L) {
    return(grz_empty_resource_distance(dt, rc, resource_id_col, resource_type_col))
  }

  out_crs <- grz_metric_crs(dt, metric_crs = metric_crs)
  pts <- sf::st_as_sf(dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts_m <- sf::st_transform(pts, out_crs)
  res_m <- sf::st_transform(res, out_crs)
  d <- sf::st_distance(pts_m, res_m)
  d <- matrix(as.numeric(d), nrow = nrow(dt), ncol = nrow(res_m))
  nearest <- max.col(-d, ties.method = "first")
  nearest_dist <- d[cbind(seq_len(nrow(d)), nearest)]

  dt[, nearest_resource_id := as.character(res_m[[resource_id_col]][nearest])]
  dt[, resource_distance_m := nearest_dist]
  if (!is.null(resource_type_col)) {
    dt[, nearest_resource_type := as.character(res_m[[resource_type_col]][nearest])]
  }
  dt[, metric_crs := as.character(sf::st_crs(out_crs)$input)]
  dt[, .grz_row_id := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_resource_distance] rows=%s resources=%s\n", format(nrow(dt), big.mark = ","), format(nrow(res), big.mark = ",")))
  }
  grz_as_output(dt, rc)
}

#' Summarise GPS use near resources
#'
#' Summarises fixes assigned to their nearest resource and counts fixes within
#' `radius_m`. Use `radius_m = 0` for polygon zones where only fixes inside or
#' touching the resource should count as near.
#'
#' @param radius_m Distance threshold in metres for near-resource use.
#' @inheritParams gps_resource_distance
#' @inheritParams gps_mcp
#'
#' @return Resource-use summary table by stream, epoch, and resource.
#' @export
gps_resource_use <- function(
  data,
  resources,
  radius_m = 25,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_spatial_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)
  if (!is.numeric(radius_m) || length(radius_m) != 1L || !is.finite(radius_m) || radius_m < 0) {
    stop("`radius_m` must be a single non-negative number.", call. = FALSE)
  }

  dist <- data.table::as.data.table(gps_resource_distance(
    data = data,
    resources = resources,
    resource_id_col = resource_id_col,
    resource_type_col = resource_type_col,
    resource_crs = resource_crs,
    metric_crs = metric_crs,
    verbose = FALSE,
    return_class = "data.table"
  ))
  grp <- grz_default_group_cols(dist, groups = groups)
  dist <- grz_add_spatial_epoch(dist, epoch = epoch, epoch_mins = epoch_mins)
  by_epoch <- c(grp, "epoch", "epoch_start", "epoch_end", "epoch_mins")
  dist[, near_resource := is.finite(resource_distance_m) & resource_distance_m <= radius_m]
  dist[, total_epoch_fixes := .N, by = by_epoch]

  by_cols <- c(by_epoch, "nearest_resource_id")
  if ("nearest_resource_type" %in% names(dist)) {
    by_cols <- c(by_cols, "nearest_resource_type")
  }
  out <- dist[, list(
    n_total_fixes = total_epoch_fixes[1L],
    n_fixes = .N,
    n_fixes_near = sum(near_resource, na.rm = TRUE),
    prop_fixes_near = sum(near_resource, na.rm = TRUE) / total_epoch_fixes[1L],
    mean_distance_m = grz_mean_or_na(resource_distance_m),
    median_distance_m = grz_quantile_or_na(resource_distance_m, 0.5),
    min_distance_m = if (any(is.finite(resource_distance_m))) min(resource_distance_m, na.rm = TRUE) else NA_real_,
    radius_m = radius_m
  ), by = by_cols]
  data.table::setnames(out, "nearest_resource_id", "resource_id")
  if ("nearest_resource_type" %in% names(out)) {
    data.table::setnames(out, "nearest_resource_type", "resource_type")
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_resource_use] epoch=%s rows=%s radius_m=%s\n", epoch, format(nrow(out), big.mark = ","), format(radius_m, trim = TRUE)))
  }
  grz_as_output(out, rc)
}

#' Detect GPS visits to resources
#'
#' Detects runs of consecutive fixes within `radius_m` of the nearest resource.
#' A new visit starts when the resource changes, the animal moves outside the
#' radius, or the gap between near-resource fixes exceeds `max_gap_mins`.
#'
#' @param max_gap_mins Maximum gap in minutes allowed within a visit.
#' @param min_fixes Minimum fixes required for a visit.
#' @param min_duration_mins Minimum visit duration in minutes.
#' @inheritParams gps_resource_use
#'
#' @return Visit summary table.
#' @export
gps_resource_visits <- function(
  data,
  resources,
  radius_m = 25,
  max_gap_mins = 30,
  min_fixes = 1,
  min_duration_mins = 0,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  groups = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  if (!is.numeric(radius_m) || length(radius_m) != 1L || !is.finite(radius_m) || radius_m < 0) {
    stop("`radius_m` must be a single non-negative number.", call. = FALSE)
  }
  grz_require_positive_mins(max_gap_mins, "max_gap_mins")
  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || !is.finite(min_fixes) || min_fixes < 1) {
    stop("`min_fixes` must be a single number >= 1.", call. = FALSE)
  }
  if (!is.numeric(min_duration_mins) || length(min_duration_mins) != 1L || !is.finite(min_duration_mins) || min_duration_mins < 0) {
    stop("`min_duration_mins` must be a single non-negative number.", call. = FALSE)
  }

  dist <- data.table::as.data.table(gps_resource_distance(
    data = data,
    resources = resources,
    resource_id_col = resource_id_col,
    resource_type_col = resource_type_col,
    resource_crs = resource_crs,
    metric_crs = metric_crs,
    verbose = FALSE,
    return_class = "data.table"
  ))
  grp <- grz_default_group_cols(dist, groups = groups)
  dist[, .grz_row_id := .I]
  data.table::setorderv(dist, c(grp, "datetime", ".grz_row_id"))
  dist[, near_resource := is.finite(resource_distance_m) & resource_distance_m <= radius_m]
  dist[, .grz_gap_mins := as.numeric(datetime - data.table::shift(datetime), units = "mins"), by = grp]
  dist[, .grz_new_visit := near_resource & (
    !data.table::shift(near_resource, fill = FALSE) |
      nearest_resource_id != data.table::shift(nearest_resource_id) |
      is.na(.grz_gap_mins) |
      .grz_gap_mins > max_gap_mins
  ), by = grp]
  dist[, .grz_visit_index := cumsum(.grz_new_visit), by = grp]

  by_cols <- c(grp, ".grz_visit_index", "nearest_resource_id")
  if ("nearest_resource_type" %in% names(dist)) {
    by_cols <- c(by_cols, "nearest_resource_type")
  }
  visits <- dist[near_resource == TRUE, list(
    visit_start = min(datetime, na.rm = TRUE),
    visit_end = max(datetime, na.rm = TRUE),
    duration_mins = as.numeric(max(datetime, na.rm = TRUE) - min(datetime, na.rm = TRUE), units = "mins"),
    n_fixes = .N,
    min_distance_m = if (any(is.finite(resource_distance_m))) min(resource_distance_m, na.rm = TRUE) else NA_real_,
    mean_distance_m = grz_mean_or_na(resource_distance_m),
    radius_m = radius_m
  ), by = by_cols]
  visits <- visits[n_fixes >= min_fixes & duration_mins >= min_duration_mins]
  data.table::setnames(visits, "nearest_resource_id", "resource_id")
  if ("nearest_resource_type" %in% names(visits)) {
    data.table::setnames(visits, "nearest_resource_type", "resource_type")
  }
  visits[, visit_id := seq_len(.N)]
  data.table::setcolorder(visits, c(grp, "visit_id", "resource_id", setdiff(names(visits), c(grp, "visit_id", "resource_id", ".grz_visit_index"))))
  visits[, .grz_visit_index := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_resource_visits] visits=%s radius_m=%s\n", format(nrow(visits), big.mark = ","), format(radius_m, trim = TRUE)))
  }
  grz_as_output(visits, rc)
}
