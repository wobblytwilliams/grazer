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
#' @param paddock_col Output paddock column name.
#' @param groups Grouping columns used for modal paddock assignment
#'   (default deployment + sensor when available).
#' @param verbose Logical; print diagnostics.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Data with appended paddock column and dropped out-of-paddock rows.
#' @export
grz_append_paddock_names <- function(
  data,
  paddocks_sf,
  buffer_m = 100,
  paddock_col = "paddock",
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("`grz_append_paddock_names()` requires the `sf` package.", call. = FALSE)
  }
  if (!inherits(paddocks_sf, "sf")) {
    stop("`paddocks_sf` must be an sf object.", call. = FALSE)
  }
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || buffer_m < 0) {
    stop("`buffer_m` must be a non-negative number.", call. = FALSE)
  }
  if (!is.character(paddock_col) || length(paddock_col) != 1L || trimws(paddock_col) == "") {
    stop("`paddock_col` must be a single non-empty column name.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)

  name_col <- grz_select_paddock_name_col(paddocks_sf)
  pdks <- paddocks_sf
  pdks[[paddock_col]] <- as.character(pdks[[name_col]])

  if (is.na(sf::st_crs(pdks))) {
    sf::st_crs(pdks) <- 4326
    if (isTRUE(verbose)) {
      cat("[append_paddock_names] paddock CRS missing; assuming EPSG:4326.\n")
    }
  }

  pdks_3857 <- sf::st_transform(pdks[, paddock_col], 3857)
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
        .grz_paddock = as.character(pdks_buf[[paddock_col]][idx])
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
  data.table::setnames(out, ".grz_modal_pdk", paddock_col)
  out[, c(".grz_row_id", ".grz_day") := NULL]

  if (isTRUE(verbose)) {
    dropped <- nrow(base) - nrow(out)
    cat(
      sprintf(
        "[append_paddock_names] name_col=%s buffer_m=%s dropped_nonmodal_or_outside=%s\n",
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
#' @param append_paddock Logical; append paddock column.
#' @param paddock_col Name of paddock output column.
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
  append_paddock = TRUE,
  paddock_col = "paddock",
  groups = NULL,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  before_n <- nrow(data)

  out <- grz_append_paddock_names(
    data = data,
    paddocks_sf = paddocks_sf,
    buffer_m = buffer_m,
    paddock_col = paddock_col,
    groups = groups,
    verbose = verbose,
    return_class = "data.table"
  )

  if (!isTRUE(append_paddock) && paddock_col %in% names(out)) {
    out[, (paddock_col) := NULL]
  }

  grz_print_clean_step("clean_spatial", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "clean_spatial", snapshot = snapshot, verbose = verbose)
  grz_as_output(out, rc)
}

grz_denoise_normalize_state <- function(x) {
  out <- trimws(tolower(as.character(x)))
  out[out %in% c("", "na", "n/a", "null")] <- NA_character_
  out
}

grz_denoise_smooth_series <- function(y) {
  y_num <- suppressWarnings(as.numeric(y))
  out <- y_num
  idx <- which(is.finite(y_num))
  if (length(idx) < 4L) {
    return(out)
  }

  x <- seq_along(y_num)
  fit <- tryCatch(
    stats::smooth.spline(x = x[idx], y = y_num[idx], cv = TRUE),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(out)
  }

  pred <- tryCatch(
    stats::predict(fit, x = x[idx])$y,
    error = function(e) NULL
  )
  if (is.null(pred) || length(pred) != length(idx)) {
    return(out)
  }

  out[idx] <- as.numeric(pred)
  out
}

grz_denoise_pick_state_col <- function(dt, state_col = NULL) {
  if (!is.null(state_col)) {
    if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
      stop("`state_col` must be NULL or a single column name.", call. = FALSE)
    }
    if (!state_col %in% names(dt)) {
      stop("`state_col` not found in `data`: ", state_col, call. = FALSE)
    }
    return(state_col)
  }

  candidates <- c(
    "activity_state_gmm",
    "activity_state",
    "behavior_state",
    "label"
  )
  hit <- candidates[candidates %in% names(dt)]
  if (length(hit) == 0L) {
    return(NULL)
  }
  hit[[1]]
}

#' Denoise GPS jitter using statistical or state-aware smoothing
#'
#' Uses statistical smoothing to reduce coordinate noise without dropping rows.
#' If active/inactive state labels are available, a state-aware method can be
#' used where inactive runs are collapsed to a robust centroid and active runs
#' are smoothed statistically.
#'
#' @param data Data frame of GPS rows.
#' @param method Denoise method: `"auto"`, `"state_aware"`, or `"statistical"`.
#'   `"auto"` uses state-aware denoising when a usable state column is present;
#'   otherwise statistical smoothing is used.
#' @param state_col Optional state column used for `"state_aware"` mode.
#' @param inactive_states Character values treated as inactive in state-aware
#'   mode.
#' @param groups Grouping columns for denoise run.
#' @param keep_raw_coords Logical; when `TRUE`, adds `lon_raw` and `lat_raw`
#'   columns before replacing `lon` and `lat`.
#' @param verbose Logical; print processing details.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Denoised data (row count unchanged).
#' @export
grz_denoise <- function(
  data,
  method = c("auto", "state_aware", "statistical"),
  state_col = NULL,
  inactive_states = c("inactive", "rest", "resting", "idle", "stationary", "lying", "ruminating"),
  groups = NULL,
  keep_raw_coords = TRUE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  if (!is.character(inactive_states) || length(inactive_states) < 1L) {
    stop("`inactive_states` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.logical(keep_raw_coords) || length(keep_raw_coords) != 1L || is.na(keep_raw_coords)) {
    stop("`keep_raw_coords` must be TRUE or FALSE.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  before_n <- nrow(dt)
  data.table::setorderv(dt, c(grp, "datetime"))

  if (isTRUE(keep_raw_coords)) {
    if (!"lon_raw" %in% names(dt)) {
      dt[, lon_raw := lon]
    }
    if (!"lat_raw" %in% names(dt)) {
      dt[, lat_raw := lat]
    }
  }

  state_col_used <- grz_denoise_pick_state_col(dt, state_col = state_col)
  inactive_lookup <- unique(grz_denoise_normalize_state(inactive_states))

  mode_used <- method
  if (mode_used == "auto") {
    if (is.null(state_col_used)) {
      mode_used <- "statistical"
    } else {
      st <- grz_denoise_normalize_state(dt[[state_col_used]])
      mode_used <- ifelse(any(st %in% inactive_lookup, na.rm = TRUE), "state_aware", "statistical")
    }
  }

  if (mode_used == "state_aware" && is.null(state_col_used)) {
    warning("`method = \"state_aware\"` requested but no state column found; using statistical denoise.", call. = FALSE)
    mode_used <- "statistical"
  }

  split_idx <- split(seq_len(nrow(dt)), interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE))
  lon_out <- dt$lon
  lat_out <- dt$lat
  n_inactive_runs <- 0L

  for (idx in split_idx) {
    sub <- dt[idx, ]
    if (nrow(sub) == 0L) {
      next
    }

    if (mode_used == "statistical") {
      lon_out[idx] <- grz_denoise_smooth_series(sub$lon)
      lat_out[idx] <- grz_denoise_smooth_series(sub$lat)
      next
    }

    state_norm <- grz_denoise_normalize_state(sub[[state_col_used]])
    is_inactive <- state_norm %in% inactive_lookup
    if (!any(is_inactive, na.rm = TRUE)) {
      lon_out[idx] <- grz_denoise_smooth_series(sub$lon)
      lat_out[idx] <- grz_denoise_smooth_series(sub$lat)
      next
    }

    run_id <- cumsum(c(TRUE, is_inactive[-1] != is_inactive[-length(is_inactive)]))
    sub_lon <- sub$lon
    sub_lat <- sub$lat
    run_vals <- unique(run_id)

    for (r in run_vals) {
      ridx <- which(run_id == r)
      if (length(ridx) == 0L) {
        next
      }

      if (isTRUE(is_inactive[ridx[[1]]])) {
        lon_center <- suppressWarnings(stats::median(sub_lon[ridx], na.rm = TRUE))
        lat_center <- suppressWarnings(stats::median(sub_lat[ridx], na.rm = TRUE))
        if (is.finite(lon_center) && is.finite(lat_center)) {
          sub_lon[ridx] <- lon_center
          sub_lat[ridx] <- lat_center
          n_inactive_runs <- n_inactive_runs + 1L
        }
      } else {
        sub_lon[ridx] <- grz_denoise_smooth_series(sub_lon[ridx])
        sub_lat[ridx] <- grz_denoise_smooth_series(sub_lat[ridx])
      }
    }

    lon_out[idx] <- sub_lon
    lat_out[idx] <- sub_lat
  }

  dt[, lon := lon_out]
  dt[, lat := lat_out]

  if (isTRUE(verbose)) {
    if (mode_used == "state_aware") {
      cat(
        sprintf(
          "[denoise] method=%s state_col=%s inactive_runs_collapsed=%s\n",
          mode_used,
          state_col_used,
          format(n_inactive_runs, big.mark = ",")
        )
      )
    } else {
      cat(sprintf("[denoise] method=%s\n", mode_used))
    }
  }

  grz_print_clean_step("denoise", before_n, nrow(dt), verbose = verbose)
  grz_print_snapshot(dt, step = "denoise", snapshot = snapshot, verbose = verbose)
  grz_as_output(dt, rc)
}

#' Cleaning pipeline wrapper
#'
#' Applies selected cleaning steps and returns cleaned data. Cleaning steps
#' report row-count changes when `verbose = TRUE`.
#'
#' @param data Data frame of GPS rows.
#' @param steps Steps to apply. Any of: `"duplicates"`, `"errors"`,
#'   `"speed_fixed"`, `"speed_stat"`, `"spatial"`, `"denoise"`.
#' @param paddocks_sf Optional paddock polygons (`sf`) required for `"spatial"`.
#' @param max_speed_mps Fixed speed threshold (m/s).
#' @param speed_stat_method Statistical speed method.
#' @param buffer_m Paddock buffer in meters.
#' @param append_paddock Logical; append paddock name column.
#' @param paddock_col Output paddock column name.
#' @param denoise_method Denoise method passed to `grz_denoise()`.
#' @param denoise_state_col Optional state column for state-aware denoise.
#' @param denoise_inactive_states Inactive state labels for state-aware denoise.
#' @param denoise_keep_raw_coords Logical; keep `lon_raw` and `lat_raw`.
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
  append_paddock = TRUE,
  paddock_col = "paddock",
  denoise_method = c("auto", "state_aware", "statistical"),
  denoise_state_col = NULL,
  denoise_inactive_states = c("inactive", "rest", "resting", "idle", "stationary", "lying", "ruminating"),
  denoise_keep_raw_coords = TRUE,
  groups = NULL,
  snapshot = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  speed_stat_method <- match.arg(speed_stat_method)
  denoise_method <- match.arg(denoise_method)

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
        append_paddock = append_paddock,
        paddock_col = paddock_col,
        groups = groups,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      )
    } else if (st == "denoise") {
      out <- grz_denoise(
        out,
        method = denoise_method,
        state_col = denoise_state_col,
        inactive_states = denoise_inactive_states,
        groups = groups,
        keep_raw_coords = denoise_keep_raw_coords,
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
