grz_prepare_clean_dt <- function(data, require_core = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  dt <- data.table::copy(data.table::as.data.table(data))
  if (!isTRUE(require_core)) {
    return(dt)
  }

  grz_require_cols(dt, c("sensor_id", "datetime", "lon", "lat"), fun_name = "clean function")
  dt[, sensor_id := as.character(sensor_id)]
  dt[, datetime := grz_parse_datetime_utc(datetime)]
  dt[, lon := suppressWarnings(as.numeric(lon))]
  dt[, lat := suppressWarnings(as.numeric(lat))]
  dt
}

#' Validate GPS schema compliance
#'
#' Checks required columns and row-level validity. Designed as a lightweight
#' pipeline compliance check before cleaning.
#'
#' @param data Data frame containing GPS data.
#' @param drop_invalid Logical; if `TRUE`, invalid rows are removed.
#' @param verbose Logical; print summary to console.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Validated data as a data.frame by default. Validation summary is
#'   attached in attributes `validation_qc` and `invalid_rows`.
#' @export
grz_validate <- function(
  data,
  drop_invalid = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  val <- grz_validate_gps(data, drop_invalid = drop_invalid)
  out <- val$data
  attr(out, "validation_qc") <- val$qc
  attr(out, "invalid_rows") <- val$invalid_rows

  if (isTRUE(verbose)) {
    n_invalid <- nrow(val$invalid_rows)
    cat(
      sprintf(
        "[validate] rows=%s invalid=%s drop_invalid=%s\n",
        format(nrow(data), big.mark = ","),
        format(n_invalid, big.mark = ","),
        ifelse(isTRUE(drop_invalid), "TRUE", "FALSE")
      )
    )
  }

  grz_as_output(out, rc)
}

#' Drop duplicate rows
#'
#' @param data Data frame of GPS rows.
#' @param keys Columns used to identify duplicates.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return De-duplicated data.
#' @export
grz_clean_duplicates <- function(
  data,
  keys = c("sensor_id", "datetime", "lon", "lat"),
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = FALSE)
  grz_require_cols(dt, keys, fun_name = "grz_clean_duplicates()")

  before_n <- nrow(dt)
  keep <- !duplicated(dt, by = keys)
  out <- dt[keep, ]

  grz_print_clean_step("clean_duplicates", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_duplicates", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

grz_speed_dt <- function(data, groups = NULL) {
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  data.table::setorderv(dt, c(grp, "datetime"))
  dt[, `:=`(
    .grz_prev_time = shift(datetime),
    .grz_prev_lon = shift(lon),
    .grz_prev_lat = shift(lat)
  ), by = grp]
  dt[, step_dt_s := as.numeric(datetime - .grz_prev_time, units = "secs")]
  dt[, step_m := grz_haversine_m(.grz_prev_lon, .grz_prev_lat, lon, lat)]
  dt[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]
  dt[, c(".grz_prev_time", ".grz_prev_lon", ".grz_prev_lat") := NULL]
  dt
}

#' Clean speed outliers using fixed threshold
#'
#' @param data Data frame of GPS rows.
#' @param max_speed_mps Maximum biologically plausible speed (m/s).
#' @param groups Grouping columns for step/speed calculation.
#' @param keep_speed_cols Keep `step_dt_s`, `step_m`, and `speed_mps` columns.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned data.
#' @export
grz_clean_speed_fixed <- function(
  data,
  max_speed_mps = 4,
  groups = NULL,
  keep_speed_cols = FALSE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  if (!is.numeric(max_speed_mps) || length(max_speed_mps) != 1L || max_speed_mps <= 0) {
    stop("`max_speed_mps` must be a positive number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_speed_dt(data, groups = groups)
  before_n <- nrow(dt)

  drop_idx <- !is.na(dt$speed_mps) & dt$speed_mps > max_speed_mps
  out <- dt[!drop_idx, ]

  if (!isTRUE(keep_speed_cols)) {
    out[, c("step_dt_s", "step_m", "speed_mps") := NULL]
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[clean_speed_fixed] threshold=%.3f m/s\n", max_speed_mps))
  }
  grz_print_clean_step("clean_speed_fixed", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_speed_fixed", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

#' Clean speed outliers using data-driven threshold
#'
#' @param data Data frame of GPS rows.
#' @param method Threshold method: `"mad"` or `"quantile"`.
#' @param k MAD multiplier (used when `method = "mad"`).
#' @param prob Quantile probability (used when `method = "quantile"`).
#' @param min_threshold_mps Lower bound for threshold.
#' @param groups Grouping columns for step/speed calculation.
#' @param keep_speed_cols Keep `step_dt_s`, `step_m`, and `speed_mps` columns.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned data.
#' @export
grz_clean_speed_stat <- function(
  data,
  method = c("mad", "quantile"),
  k = 4,
  prob = 0.995,
  min_threshold_mps = 4,
  groups = NULL,
  keep_speed_cols = FALSE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  if (!is.numeric(k) || length(k) != 1L || k <= 0) {
    stop("`k` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(prob) || length(prob) != 1L || prob <= 0 || prob >= 1) {
    stop("`prob` must be a number in (0, 1).", call. = FALSE)
  }
  if (!is.numeric(min_threshold_mps) || length(min_threshold_mps) != 1L || min_threshold_mps <= 0) {
    stop("`min_threshold_mps` must be a positive number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_speed_dt(data, groups = groups)
  before_n <- nrow(dt)

  spd <- dt$speed_mps[is.finite(dt$speed_mps) & dt$speed_mps > 0]
  if (length(spd) == 0L) {
    threshold <- min_threshold_mps
  } else if (method == "mad") {
    threshold <- stats::median(spd, na.rm = TRUE) + k * stats::mad(spd, na.rm = TRUE, constant = 1.4826)
  } else {
    threshold <- as.numeric(stats::quantile(spd, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
  }
  threshold <- max(min_threshold_mps, threshold)

  drop_idx <- !is.na(dt$speed_mps) & dt$speed_mps > threshold
  out <- dt[!drop_idx, ]

  if (!isTRUE(keep_speed_cols)) {
    out[, c("step_dt_s", "step_m", "speed_mps") := NULL]
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[clean_speed_stat] method=%s threshold=%.3f m/s\n", method, threshold))
  }
  grz_print_clean_step("clean_speed_stat", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_speed_stat", snapshot = snapshot, verbose = verbose)
  attr(out, "speed_threshold_mps") <- threshold
  grz_as_output(out, rc)
}

#' Clean row-level data errors
#'
#' Removes invalid datetime/sensor/coordinate rows and optional `(0,0)` rows.
#'
#' @param data Data frame of GPS rows.
#' @param remove_invalid_datetime Logical; drop invalid datetimes.
#' @param remove_invalid_coords Logical; drop invalid coordinate rows.
#' @param remove_zero_zero Logical; drop `(0,0)` rows.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned data.
#' @export
grz_clean_errors <- function(
  data,
  remove_invalid_datetime = TRUE,
  remove_invalid_coords = TRUE,
  remove_zero_zero = TRUE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  before_n <- nrow(dt)

  bad_sensor <- is.na(dt$sensor_id) | trimws(dt$sensor_id) == ""
  bad_datetime <- is.na(dt$datetime)
  bad_lonlat <- is.na(dt$lon) | is.na(dt$lat) |
    !is.finite(dt$lon) | !is.finite(dt$lat) |
    dt$lon < -180 | dt$lon > 180 |
    dt$lat < -90 | dt$lat > 90
  bad_zero <- dt$lon == 0 & dt$lat == 0

  drop_idx <- bad_sensor
  if (isTRUE(remove_invalid_datetime)) {
    drop_idx <- drop_idx | bad_datetime
  }
  if (isTRUE(remove_invalid_coords)) {
    drop_idx <- drop_idx | bad_lonlat
  }
  if (isTRUE(remove_zero_zero)) {
    drop_idx <- drop_idx | bad_zero
  }

  out <- dt[!drop_idx, ]
  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[clean_errors] dropped_sensor=%s dropped_datetime=%s dropped_coord=%s dropped_zero_zero=%s\n",
        format(sum(bad_sensor, na.rm = TRUE), big.mark = ","),
        format(ifelse(remove_invalid_datetime, sum(bad_datetime, na.rm = TRUE), 0L), big.mark = ","),
        format(ifelse(remove_invalid_coords, sum(bad_lonlat, na.rm = TRUE), 0L), big.mark = ","),
        format(ifelse(remove_zero_zero, sum(bad_zero, na.rm = TRUE), 0L), big.mark = ",")
      )
    )
  }
  grz_print_clean_step("clean_errors", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_errors", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

grz_select_paddock_name_col <- function(paddocks) {
  has_name <- "NAME" %in% names(paddocks)
  has_desc <- "Description" %in% names(paddocks)
  if (!has_name && !has_desc) {
    stop("Paddock polygons must contain `NAME` or `Description` column.", call. = FALSE)
  }

  is_complete <- function(x) {
    all(!is.na(x) & trimws(as.character(x)) != "")
  }

  if (has_name && is_complete(paddocks$NAME)) {
    return("NAME")
  }
  if (has_desc && is_complete(paddocks$Description)) {
    return("Description")
  }
  stop(
    "Paddock names are partial across `NAME`/`Description`. ",
    "All polygons must be fully named in one column. `NAME` is preferred.",
    call. = FALSE
  )
}

#' Append paddock names by point-in-polygon intersection
#'
#' Uses buffered paddock polygons and assigns a daily modal paddock per animal.
#' Rows not intersecting the modal paddock are dropped.
#'
#' @param data Data frame with `lon`, `lat`, `datetime`, and `sensor_id`.
#' @param paddocks_sf `sf` polygon object.
#' @param buffer_m Buffer distance in meters.
#' @param pdk_col Output paddock column name.
#' @param groups Grouping columns used for modal paddock assignment
#'   (default deployment + sensor when available).
#' @param verbose Logical; print diagnostics.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Data with appended paddock column and dropped out-of-paddock rows.
#' @export
grz_append_pdk_names <- function(
  data,
  paddocks_sf,
  buffer_m = 100,
  pdk_col = "paddock",
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("`grz_append_pdk_names()` requires the `sf` package.", call. = FALSE)
  }
  if (!inherits(paddocks_sf, "sf")) {
    stop("`paddocks_sf` must be an sf object.", call. = FALSE)
  }
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || buffer_m < 0) {
    stop("`buffer_m` must be a non-negative number.", call. = FALSE)
  }
  if (!is.character(pdk_col) || length(pdk_col) != 1L || trimws(pdk_col) == "") {
    stop("`pdk_col` must be a single non-empty column name.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)

  name_col <- grz_select_paddock_name_col(paddocks_sf)
  pdks <- paddocks_sf
  pdks[[pdk_col]] <- as.character(pdks[[name_col]])

  if (is.na(sf::st_crs(pdks))) {
    sf::st_crs(pdks) <- 4326
    if (isTRUE(verbose)) {
      cat("[append_pdk_names] paddock CRS missing; assuming EPSG:4326.\n")
    }
  }

  pdks_3857 <- sf::st_transform(pdks[, pdk_col], 3857)
  pdks_buf <- sf::st_buffer(pdks_3857, dist = buffer_m)
  pdks_buf <- sf::st_make_valid(pdks_buf)

  pts <- sf::st_as_sf(dt, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  pts_3857 <- sf::st_transform(pts, 3857)
  pts_3857$.grz_row_id <- seq_len(nrow(pts_3857))

  hits <- sf::st_intersects(pts_3857, pdks_buf)
  candidates <- data.table::rbindlist(
    lapply(seq_along(hits), function(i) {
      idx <- hits[[i]]
      if (length(idx) == 0L) {
        return(NULL)
      }
      data.table::data.table(
        .grz_row_id = i,
        .grz_paddock = as.character(pdks_buf[[pdk_col]][idx])
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(candidates) == 0L) {
    stop("No GPS points intersect buffered paddocks.", call. = FALSE)
  }

  base <- data.table::as.data.table(sf::st_drop_geometry(pts_3857))
  base[, .grz_day := as.Date(datetime, tz = "UTC")]

  cand <- merge(
    candidates,
    base[, c(".grz_row_id", grp, ".grz_day"), with = FALSE],
    by = ".grz_row_id",
    all.x = TRUE,
    sort = FALSE
  )

  modal_by_day <- cand[, .N, by = c(grp, ".grz_day", ".grz_paddock")]
  data.table::setorderv(
    modal_by_day,
    cols = c(grp, ".grz_day", "N", ".grz_paddock"),
    order = c(rep(1L, length(grp)), 1L, -1L, 1L)
  )
  modal_by_day <- modal_by_day[, .SD[1], by = c(grp, ".grz_day")]
  data.table::setnames(modal_by_day, ".grz_paddock", ".grz_modal_pdk")

  keep_rows <- merge(
    cand,
    modal_by_day,
    by = c(grp, ".grz_day"),
    all.x = TRUE,
    sort = FALSE
  )
  keep_rows <- keep_rows[.grz_paddock == .grz_modal_pdk, .(.grz_row_id, .grz_modal_pdk)]
  keep_rows <- unique(keep_rows, by = ".grz_row_id")

  out <- merge(
    base,
    keep_rows,
    by = ".grz_row_id",
    all = FALSE,
    sort = FALSE
  )
  data.table::setnames(out, ".grz_modal_pdk", pdk_col)
  out[, c(".grz_row_id", ".grz_day") := NULL]

  if (isTRUE(verbose)) {
    dropped <- nrow(base) - nrow(out)
    cat(
      sprintf(
        "[append_pdk_names] name_col=%s buffer_m=%s dropped_nonmodal_or_outside=%s\n",
        name_col,
        format(buffer_m, trim = TRUE),
        format(dropped, big.mark = ",")
      )
    )
  }

  grz_as_output(out, rc)
}

#' Spatial cleaning using paddock polygons
#'
#' @param data Data frame of GPS rows.
#' @param paddocks_sf `sf` paddock polygons.
#' @param buffer_m Paddock buffer in meters.
#' @param append_pdk Logical; append paddock column.
#' @param pdk_col Name of paddock output column.
#' @param groups Grouping columns for modal paddock assignment.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Spatially cleaned data.
#' @export
grz_clean_spatial <- function(
  data,
  paddocks_sf,
  buffer_m = 100,
  append_pdk = TRUE,
  pdk_col = "paddock",
  groups = NULL,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  before_n <- nrow(data)

  out <- grz_append_pdk_names(
    data = data,
    paddocks_sf = paddocks_sf,
    buffer_m = buffer_m,
    pdk_col = pdk_col,
    groups = groups,
    verbose = verbose,
    return_class = "data.table"
  )

  if (!isTRUE(append_pdk) && pdk_col %in% names(out)) {
    out[, (pdk_col) := NULL]
  }

  grz_print_clean_step("clean_spatial", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_spatial", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

grz_denoise_keep_mask <- function(dt, radius_m, max_gap_s, group_cols) {
  data.table::setorderv(dt, c(group_cols, "datetime"))
  dt[, .grz_row_id := .I]
  split_idx <- split(seq_len(nrow(dt)), interaction(dt[, ..group_cols], drop = TRUE, lex.order = TRUE))
  keep_ids <- integer(0)

  for (idx in split_idx) {
    sub <- dt[idx, ]
    n <- nrow(sub)
    if (n == 0L) {
      next
    }
    keep_local <- rep(FALSE, n)
    keep_local[1] <- TRUE
    last_keep <- 1L

    if (n >= 2L) {
      for (i in 2:n) {
        d <- grz_haversine_m(
          lon1 = sub$lon[last_keep],
          lat1 = sub$lat[last_keep],
          lon2 = sub$lon[i],
          lat2 = sub$lat[i]
        )
        dt_s <- as.numeric(sub$datetime[i] - sub$datetime[last_keep], units = "secs")
        if (!is.finite(d) || !is.finite(dt_s) || d > radius_m || dt_s > max_gap_s) {
          keep_local[i] <- TRUE
          last_keep <- i
        }
      }
    }

    keep_ids <- c(keep_ids, sub$.grz_row_id[keep_local])
  }
  dt$.grz_row_id %in% keep_ids
}

#' Denoise static GPS jitter
#'
#' Drops near-duplicate static fixes by retaining the first point and then
#' retaining additional points only when movement exceeds `radius_m` or elapsed
#' time exceeds `max_gap_mins`.
#'
#' @param data Data frame of GPS rows.
#' @param radius_m Spatial jitter tolerance in meters.
#' @param max_gap_mins Maximum retained gap while static.
#' @param groups Grouping columns for denoise run.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Denoised data.
#' @export
grz_denoise <- function(
  data,
  radius_m = 8,
  max_gap_mins = 20,
  groups = NULL,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  if (!is.numeric(radius_m) || length(radius_m) != 1L || radius_m <= 0) {
    stop("`radius_m` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(max_gap_mins) || length(max_gap_mins) != 1L || max_gap_mins <= 0) {
    stop("`max_gap_mins` must be a positive number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  before_n <- nrow(dt)

  keep <- grz_denoise_keep_mask(
    dt = dt,
    radius_m = radius_m,
    max_gap_s = max_gap_mins * 60,
    group_cols = grp
  )
  out <- dt[keep, ]
  out[, .grz_row_id := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf("[denoise] radius_m=%.1f max_gap_mins=%.1f\n", radius_m, max_gap_mins))
  }
  grz_print_clean_step("denoise", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "denoise", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

#' Cleaning pipeline wrapper
#'
#' Applies selected cleaning steps and returns cleaned data. By design, cleaning
#' steps drop rows and print row-count changes when `verbose = TRUE`.
#'
#' @param data Data frame of GPS rows.
#' @param steps Steps to apply. Any of: `"duplicates"`, `"errors"`,
#'   `"speed_fixed"`, `"speed_stat"`, `"spatial"`, `"denoise"`.
#' @param paddocks_sf Optional paddock polygons (`sf`) required for `"spatial"`.
#' @param max_speed_mps Fixed speed threshold (m/s).
#' @param speed_stat_method Statistical speed method.
#' @param buffer_m Paddock buffer in meters.
#' @param append_pdk Logical; append paddock name column.
#' @param pdk_col Output paddock column name.
#' @param groups Grouping columns for speed/denoise/modal paddock operations.
#' @param snapshot Logical; print snapshots after each step.
#' @param verbose Logical; print details.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned data.
#' @export
grz_clean <- function(
  data,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  paddocks_sf = NULL,
  max_speed_mps = 4,
  speed_stat_method = c("mad", "quantile"),
  buffer_m = 100,
  append_pdk = TRUE,
  pdk_col = "paddock",
  groups = NULL,
  snapshot = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  speed_stat_method <- match.arg(speed_stat_method)

  allowed_steps <- c("duplicates", "errors", "speed_fixed", "speed_stat", "spatial", "denoise")
  if (!is.character(steps) || length(steps) < 1L) {
    stop("`steps` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(steps, allowed_steps)
  if (length(bad) > 0L) {
    stop("Unknown clean step(s): ", paste(bad, collapse = ", "), call. = FALSE)
  }

  out <- data.table::copy(data.table::as.data.table(data))
  if (isTRUE(verbose)) {
    cat(sprintf("[clean] start_rows=%s\n", format(nrow(out), big.mark = ",")))
  }

  for (st in steps) {
    if (st == "duplicates") {
      out <- grz_clean_duplicates(
        out,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "errors") {
      out <- grz_clean_errors(
        out,
        remove_invalid_datetime = TRUE,
        remove_invalid_coords = TRUE,
        remove_zero_zero = TRUE,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "speed_fixed") {
      out <- grz_clean_speed_fixed(
        out,
        max_speed_mps = max_speed_mps,
        groups = groups,
        keep_speed_cols = FALSE,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "speed_stat") {
      out <- grz_clean_speed_stat(
        out,
        method = speed_stat_method,
        min_threshold_mps = max_speed_mps,
        groups = groups,
        keep_speed_cols = FALSE,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "spatial") {
      if (is.null(paddocks_sf)) {
        stop("`steps` includes `spatial` but `paddocks_sf` is NULL.", call. = FALSE)
      }
      out <- grz_clean_spatial(
        out,
        paddocks_sf = paddocks_sf,
        buffer_m = buffer_m,
        append_pdk = append_pdk,
        pdk_col = pdk_col,
        groups = groups,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "denoise") {
      out <- grz_denoise(
        out,
        groups = groups,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    }
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[clean] final_rows=%s\n", format(nrow(out), big.mark = ",")))
  }

  grz_as_output(out, rc)
}
