grz_time_group_cols <- function(data, groups = NULL, fun_name = "GPS temporal function") {
  if (!is.null(groups)) {
    if (!is.character(groups) || length(groups) < 1L || any(is.na(groups)) || any(trimws(groups) == "")) {
      stop("`groups` must be NULL or a non-empty character vector.", call. = FALSE)
    }
    grz_require_cols(data, groups, fun_name = fun_name)
    return(unique(groups))
  }

  out <- intersect(c("deployment_id", "animal_id", "sensor_id", "segment_id"), names(data))
  if (!"sensor_id" %in% out) {
    out <- c(out, "sensor_id")
  }
  unique(out)
}

grz_require_positive_mins <- function(x, arg) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop("`", arg, "` must be a positive number.", call. = FALSE)
  }
  invisible(TRUE)
}

grz_require_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

grz_resolve_interval_mins <- function(data, groups, interval_mins = "base") {
  if (is.character(interval_mins)) {
    if (length(interval_mins) != 1L || interval_mins != "base") {
      stop("`interval_mins` must be numeric or \"base\".", call. = FALSE)
    }
    tmp <- data.table::copy(data)
    data.table::setorderv(tmp, c(groups, "datetime"))
    tmp[, .grz_diff_min := as.numeric(datetime - data.table::shift(datetime), units = "mins"), by = groups]
    return(grz_round_to_base_min(tmp$.grz_diff_min))
  }

  grz_require_positive_mins(interval_mins, "interval_mins")
  as.numeric(interval_mins)
}

grz_resolve_tolerance_mins <- function(tolerance_mins, interval_mins) {
  if (is.null(tolerance_mins)) {
    return(as.numeric(interval_mins) / 2)
  }
  if (!is.numeric(tolerance_mins) || length(tolerance_mins) != 1L || is.na(tolerance_mins) || tolerance_mins < 0) {
    stop("`tolerance_mins` must be NULL or a single non-negative number.", call. = FALSE)
  }
  as.numeric(tolerance_mins)
}

grz_mins_to_sec <- function(x, arg) {
  grz_require_positive_mins(x, arg)
  out <- as.integer(round(as.numeric(x) * 60))
  if (!is.finite(out) || out <= 0L) {
    stop("`", arg, "` must resolve to at least one second.", call. = FALSE)
  }
  out
}

grz_tolerance_to_sec <- function(x) {
  out <- as.integer(round(as.numeric(x) * 60))
  if (!is.finite(out) || out < 0L) {
    stop("`tolerance_mins` must resolve to a non-negative number of seconds.", call. = FALSE)
  }
  out
}

grz_fill_constant_metadata <- function(out, source, skip_cols) {
  fill_cols <- setdiff(names(out), skip_cols)
  for (col in fill_cols) {
    if (col %in% skip_cols || !col %in% names(source)) {
      next
    }
    values <- source[[col]]
    values <- values[!is.na(values)]
    if (length(values) == 0L) {
      next
    }
    unique_values <- unique(values)
    if (length(unique_values) != 1L) {
      next
    }
    missing_idx <- is.na(out[[col]])
    if (any(missing_idx)) {
      data.table::set(out, i = which(missing_idx), j = col, value = unique_values[[1L]])
    }
  }
  out
}

grz_regularise_group <- function(sub, groups, interval_sec, tolerance_sec, keep_extra) {
  sub <- data.table::copy(sub)
  data.table::setorderv(sub, c("datetime", ".grz_row_id"))

  t_obs <- as.numeric(sub$datetime)
  grid_start <- floor(min(t_obs) / interval_sec) * interval_sec
  grid_end <- ceiling(max(t_obs) / interval_sec) * interval_sec
  t_grid <- seq(grid_start, grid_end, by = interval_sec)
  grid <- data.table::data.table(
    datetime = as.POSIXct(t_grid, origin = "1970-01-01", tz = "UTC"),
    .grz_grid_id = seq_along(t_grid)
  )

  sub[, .grz_obs_time := as.numeric(datetime)]
  sub[, .grz_grid_id := as.integer(round((.grz_obs_time - grid_start) / interval_sec) + 1L)]
  sub[, .grz_grid_time := grid_start + (.grz_grid_id - 1L) * interval_sec]
  sub[, time_offset_s := .grz_obs_time - .grz_grid_time]
  sub[, .grz_abs_offset_s := abs(time_offset_s)]
  candidates <- sub[
    .grz_grid_id >= 1L & .grz_grid_id <= length(t_grid) &
      is.finite(.grz_abs_offset_s) & .grz_abs_offset_s <= tolerance_sec
  ]
  if (nrow(candidates) > 0L) {
    data.table::setorderv(candidates, c(".grz_grid_id", ".grz_abs_offset_s", ".grz_row_id"))
    candidates <- candidates[, .SD[1L], by = ".grz_grid_id"]
  }

  merge_cols <- c(".grz_grid_id", "datetime", "lon", "lat", "time_offset_s")
  if (isTRUE(keep_extra)) {
    merge_cols <- c(merge_cols, setdiff(names(candidates), c(".grz_row_id", groups, ".grz_grid_id", ".grz_obs_time", ".grz_grid_time", ".grz_abs_offset_s", "lon", "lat", "time_offset_s")))
  }
  merge_cols <- unique(merge_cols)
  matched <- candidates[, merge_cols, with = FALSE]
  data.table::setnames(matched, "datetime", "observed_datetime")

  out <- merge(
    grid,
    matched,
    by = ".grz_grid_id",
    all.x = TRUE,
    sort = TRUE
  )
  out[, is_observed := !is.na(observed_datetime)]

  for (group in groups) {
    out[, (group) := sub[[group]][1L]]
  }

  out <- grz_fill_constant_metadata(
    out,
    source = sub,
    skip_cols = c(groups, "datetime", "observed_datetime", "lon", "lat", "time_offset_s", "is_observed")
  )
  out[, .grz_grid_id := NULL]
  data.table::setcolorder(out, c(groups, "datetime", "lon", "lat", setdiff(names(out), c(groups, "datetime", "lon", "lat"))))
  out[]
}

grz_observed_gap_summary <- function(dt, groups) {
  if (nrow(dt) == 0L) {
    return(dt[, c(groups), with = FALSE][0L][, `:=`(
      n_observed_gaps = integer(),
      gap_min_s = numeric(),
      gap_median_s = numeric(),
      gap_max_s = numeric()
    )])
  }

  tmp <- data.table::copy(dt)
  if (!".grz_row_id" %in% names(tmp)) {
    tmp[, .grz_row_id := .I]
  }
  data.table::setorderv(tmp, c(groups, "datetime", ".grz_row_id"))
  tmp <- unique(tmp, by = c(groups, "datetime"))
  tmp[, .grz_gap_s := as.numeric(datetime - data.table::shift(datetime), units = "secs"), by = groups]
  tmp[, list(
    n_observed_gaps = sum(is.finite(.grz_gap_s) & .grz_gap_s > 0),
    gap_min_s = if (any(is.finite(.grz_gap_s) & .grz_gap_s > 0)) min(.grz_gap_s[is.finite(.grz_gap_s) & .grz_gap_s > 0]) else NA_real_,
    gap_median_s = if (any(is.finite(.grz_gap_s) & .grz_gap_s > 0)) stats::median(.grz_gap_s[is.finite(.grz_gap_s) & .grz_gap_s > 0]) else NA_real_,
    gap_max_s = if (any(is.finite(.grz_gap_s) & .grz_gap_s > 0)) max(.grz_gap_s[is.finite(.grz_gap_s) & .grz_gap_s > 0]) else NA_real_
  ), by = groups]
}

grz_temporal_diagnostics <- function(out, observed, groups, interval_sec, tolerance_sec = NA_real_, n_missing_datetime = 0L) {
  obs_counts <- observed[, list(
    n_observed_fixes = .N,
    n_observed_timestamps = data.table::uniqueN(datetime),
    n_duplicate_timestamps = .N - data.table::uniqueN(datetime)
  ), by = groups]

  expected <- out[, list(
    n_expected_fixes = .N,
    n_observed_on_grid = if ("is_observed" %in% names(.SD)) sum(is_observed %in% TRUE, na.rm = TRUE) else .N,
    n_interpolated = if ("is_interpolated" %in% names(.SD)) sum(is_interpolated %in% TRUE, na.rm = TRUE) else 0L,
    median_abs_time_offset_s = if ("time_offset_s" %in% names(.SD) && any(is.finite(time_offset_s))) stats::median(abs(time_offset_s), na.rm = TRUE) else NA_real_,
    max_abs_time_offset_s = if ("time_offset_s" %in% names(.SD) && any(is.finite(time_offset_s))) max(abs(time_offset_s), na.rm = TRUE) else NA_real_
  ), by = groups]

  sampling <- data.table::copy(out)
  data.table::setorderv(sampling, c(groups, "datetime"))
  sampling[, .grz_dt_s := as.numeric(datetime - data.table::shift(datetime), units = "secs"), by = groups]
  sampling <- sampling[, list(
    sampling_interval_target_s = as.numeric(interval_sec),
    sampling_interval_achieved_s = if (any(is.finite(.grz_dt_s) & .grz_dt_s > 0)) stats::median(.grz_dt_s[is.finite(.grz_dt_s) & .grz_dt_s > 0]) else NA_real_
  ), by = groups]

  gaps <- grz_observed_gap_summary(observed, groups = groups)

  diag <- Reduce(
    function(x, y) merge(x, y, by = groups, all = TRUE, sort = FALSE),
    list(expected, obs_counts, gaps, sampling)
  )
  diag[, prop_interpolated := n_interpolated / n_expected_fixes]
  diag[, n_unmatched_observed_fixes := pmax(0L, as.integer(n_observed_fixes - n_observed_on_grid))]
  diag[, n_missing_datetime := as.integer(n_missing_datetime)]
  diag[, tolerance_mins := as.numeric(tolerance_sec) / 60]
  data.table::setcolorder(
    diag,
    c(
      groups,
      "n_expected_fixes",
      "n_observed_fixes",
      "n_observed_timestamps",
      "n_observed_on_grid",
      "n_unmatched_observed_fixes",
      "n_interpolated",
      "prop_interpolated",
      "n_duplicate_timestamps",
      "n_missing_datetime",
      "median_abs_time_offset_s",
      "max_abs_time_offset_s",
      "tolerance_mins",
      "n_observed_gaps",
      "gap_min_s",
      "gap_median_s",
      "gap_max_s",
      "sampling_interval_target_s",
      "sampling_interval_achieved_s"
    )
  )
  diag[]
}

grz_finish_temporal <- function(out, diagnostics, parameters, return_class) {
  rc <- grz_match_output_class(return_class)
  diag <- data.table::as.data.table(diagnostics)
  for (nm in names(parameters)) {
    value <- parameters[[nm]]
    if (length(value) == 1L && (is.atomic(value) || is.null(value))) {
      diag[, (nm) := value]
    }
  }
  result <- grz_as_output(out, rc)
  attr(result, "gps_reg") <- grz_as_output(diag, "data.frame")
  result
}

#' Regularise GPS fixes to expected times
#'
#' Creates a regular time grid for each animal or sensor stream without filling
#' missing coordinates. Observed fixes are assigned to the nearest grid time
#' when they fall within `tolerance_mins`; missing expected fixes have `NA`
#' coordinates.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.
#' @param interval_mins Target interval in minutes, or `"base"` to infer the
#'   median positive observed interval.
#' @param tolerance_mins Tolerance in minutes for assigning observed fixes to
#'   the nearest grid time. `NULL` uses half of `interval_mins`. Use `0` for
#'   strict exact timestamp matching.
#' @param groups Grouping columns for independent streams. Defaults to available
#'   `deployment_id`, `animal_id`, and `sensor_id`.
#' @param keep_extra Logical; keep non-core metadata where exact observations
#'   exist and fill columns that are constant within a stream.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Regularised GPS data with `is_observed`, `observed_datetime`, and
#'   `time_offset_s`. A `gps_reg` attribute summarises expected fixes, observed
#'   fixes, gaps, grid offsets, and achieved sampling interval.
#' @export
gps_regularise <- function(
  data,
  interval_mins = "base",
  tolerance_mins = NULL,
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  grz_require_flag(keep_extra, "keep_extra")
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_time_group_cols(dt, groups = groups, fun_name = "gps_regularise()")
  interval_resolved <- grz_resolve_interval_mins(dt, groups = grp, interval_mins = interval_mins)
  tolerance_resolved <- grz_resolve_tolerance_mins(tolerance_mins, interval_mins = interval_resolved)
  interval_sec <- grz_mins_to_sec(interval_resolved, "interval_mins")
  tolerance_sec <- grz_tolerance_to_sec(tolerance_resolved)

  dt[, .grz_row_id := .I]
  n_missing_datetime <- sum(is.na(dt$datetime))
  dt <- dt[!is.na(datetime)]
  if (nrow(dt) == 0L) {
    stop("`data` must contain at least one valid `datetime`.", call. = FALSE)
  }

  split_idx <- split(seq_len(nrow(dt)), interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE))
  out <- data.table::rbindlist(
    lapply(split_idx, function(i) {
      grz_regularise_group(dt[i], groups = grp, interval_sec = interval_sec, tolerance_sec = tolerance_sec, keep_extra = keep_extra)
    }),
    use.names = TRUE,
    fill = TRUE
  )

  data.table::setorderv(out, c(grp, "datetime"))
  diagnostics <- grz_temporal_diagnostics(out, observed = dt, groups = grp, interval_sec = interval_sec, tolerance_sec = tolerance_sec, n_missing_datetime = n_missing_datetime)

  if (isTRUE(verbose)) {
    cat(sprintf(
      "[gps_regularise] interval_mins=%s tolerance_mins=%s rows=%s -> %s expected=%s\n",
      format(interval_resolved, trim = TRUE),
      format(tolerance_resolved, trim = TRUE),
      format(nrow(data), big.mark = ","),
      format(nrow(out), big.mark = ","),
      format(sum(diagnostics$n_expected_fixes), big.mark = ",")
    ))
  }

  grz_finish_temporal(
    out,
    diagnostics = diagnostics,
    parameters = list(interval_mins = interval_resolved, tolerance_mins = tolerance_resolved, keep_extra = keep_extra),
    return_class = rc
  )
}

grz_interpolate_group <- function(sub, max_gap_sec) {
  sub <- data.table::copy(sub)
  data.table::setorderv(sub, "datetime")
  sub[, is_interpolated := FALSE]
  sub[, interpolation_gap_s := NA_real_]

  anchors <- sub[is_observed %in% TRUE & is.finite(lon) & is.finite(lat)]
  anchors <- unique(anchors, by = "datetime")
  if (nrow(anchors) < 2L) {
    return(sub[])
  }

  t_grid <- as.numeric(sub$datetime)
  t_obs <- as.numeric(anchors$datetime)
  prev_idx <- findInterval(t_grid, t_obs)
  next_idx <- prev_idx + 1L
  can_fill <- !(sub$is_observed %in% TRUE) & prev_idx >= 1L & next_idx <= length(t_obs)
  gap_s <- rep(NA_real_, length(t_grid))
  gap_s[can_fill] <- t_obs[next_idx[can_fill]] - t_obs[prev_idx[can_fill]]
  can_fill <- can_fill & is.finite(gap_s) & gap_s <= max_gap_sec

  if (!any(can_fill)) {
    return(sub[])
  }

  lon_interp <- stats::approx(x = t_obs, y = anchors$lon, xout = t_grid[can_fill], method = "linear", rule = 1, ties = "ordered")$y
  lat_interp <- stats::approx(x = t_obs, y = anchors$lat, xout = t_grid[can_fill], method = "linear", rule = 1, ties = "ordered")$y
  candidate_rows <- which(can_fill)
  ok_interp <- is.finite(lon_interp) & is.finite(lat_interp)
  fill_rows <- candidate_rows[ok_interp]
  if (length(fill_rows) == 0L) {
    return(sub[])
  }

  data.table::set(sub, i = fill_rows, j = "lon", value = lon_interp[ok_interp])
  data.table::set(sub, i = fill_rows, j = "lat", value = lat_interp[ok_interp])
  data.table::set(sub, i = fill_rows, j = "is_interpolated", value = TRUE)
  data.table::set(sub, i = fill_rows, j = "interpolation_gap_s", value = gap_s[fill_rows])
  sub[]
}

#' Interpolate GPS fixes on a regular time grid
#'
#' Regularises each animal or sensor stream onto a time grid, assigns observed
#' fixes to nearby grid times using `tolerance_mins`, and linearly interpolates
#' longitude and latitude for remaining missing expected fixes. Interpolation is
#' done within groups only. Use `gps_append_segments()` before interpolation
#' when large gaps should split a track.
#'
#' @inheritParams gps_regularise
#'
#' @return Interpolated GPS data with `is_observed`, `is_interpolated`, and
#'   `interpolation_gap_s`, `observed_datetime`, and `time_offset_s`. A
#'   `gps_reg` attribute summarises expected fixes, observed fixes, interpolated
#'   fixes, gaps, grid offsets, and achieved sampling interval.
#' @export
gps_interpolate <- function(
  data,
  interval_mins = "base",
  tolerance_mins = NULL,
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)

  dt_raw <- grz_prepare_clean_dt(data, require_core = TRUE)
  if ("is_observed" %in% names(dt_raw)) {
    dt_raw <- dt_raw[is_observed %in% TRUE]
  }
  grp <- grz_time_group_cols(dt_raw, groups = groups, fun_name = "gps_interpolate()")
  interval_resolved <- grz_resolve_interval_mins(dt_raw, groups = grp, interval_mins = interval_mins)
  tolerance_resolved <- grz_resolve_tolerance_mins(tolerance_mins, interval_mins = interval_resolved)
  interval_sec <- grz_mins_to_sec(interval_resolved, "interval_mins")
  tolerance_sec <- grz_tolerance_to_sec(tolerance_resolved)

  regular <- data.table::as.data.table(gps_regularise(
    dt_raw,
    interval_mins = interval_resolved,
    tolerance_mins = tolerance_resolved,
    groups = grp,
    keep_extra = keep_extra,
    verbose = FALSE,
    return_class = "data.table"
  ))
  split_idx <- split(seq_len(nrow(regular)), interaction(regular[, ..grp], drop = TRUE, lex.order = TRUE))
  out <- data.table::rbindlist(
    lapply(split_idx, function(i) grz_interpolate_group(regular[i], max_gap_sec = Inf)),
    use.names = TRUE,
    fill = TRUE
  )
  data.table::setorderv(out, c(grp, "datetime"))

  observed <- data.table::copy(dt_raw)
  observed[, .grz_row_id := .I]
  observed <- observed[!is.na(datetime)]
  diagnostics <- grz_temporal_diagnostics(out, observed = observed, groups = grp, interval_sec = interval_sec, tolerance_sec = tolerance_sec, n_missing_datetime = sum(is.na(dt_raw$datetime)))

  if (isTRUE(verbose)) {
    cat(sprintf(
      "[gps_interpolate] interval_mins=%s tolerance_mins=%s interpolated=%s\n",
      format(interval_resolved, trim = TRUE),
      format(tolerance_resolved, trim = TRUE),
      format(sum(out$is_interpolated), big.mark = ",")
    ))
  }

  grz_finish_temporal(
    out,
    diagnostics = diagnostics,
    parameters = list(interval_mins = interval_resolved, tolerance_mins = tolerance_resolved, keep_extra = keep_extra),
    return_class = rc
  )
}

grz_downsample_group <- function(sub, groups, target_sec, phase_sec, method) {
  sub <- data.table::copy(sub)
  data.table::setorderv(sub, c("datetime", ".grz_row_id"))
  start_time <- min(as.numeric(sub$datetime), na.rm = TRUE) + phase_sec
  end_time <- max(as.numeric(sub$datetime), na.rm = TRUE)
  if (!is.finite(start_time) || !is.finite(end_time) || start_time > end_time) {
    return(sub[0L])
  }

  target_times <- seq(start_time, end_time, by = target_sec)
  targets <- data.table::data.table(
    .grz_target_id = seq_along(target_times),
    .grz_target_time = target_times,
    .grz_window_start = target_times,
    .grz_window_end = target_times + target_sec
  )

  sub[, .grz_time := as.numeric(datetime)]
  if (method == "rigid") {
    joined <- sub[targets, on = list(.grz_time >= .grz_window_start, .grz_time < .grz_window_end), allow.cartesian = TRUE, nomatch = 0L]
    if (nrow(joined) == 0L) {
      return(sub[0L])
    }
    data.table::setorderv(joined, c(".grz_target_id", "datetime", ".grz_row_id"))
    out <- joined[, .SD[1L], by = ".grz_target_id"]
  } else {
    targets[, `:=`(
      .grz_window_start = .grz_target_time - target_sec / 2,
      .grz_window_end = .grz_target_time + target_sec / 2
    )]
    joined <- sub[targets, on = list(.grz_time >= .grz_window_start, .grz_time < .grz_window_end), allow.cartesian = TRUE, nomatch = 0L]
    if (nrow(joined) == 0L) {
      return(sub[0L])
    }
    joined[, .grz_abs_target := abs(as.numeric(datetime) - .grz_target_time)]
    data.table::setorderv(joined, c(".grz_target_id", ".grz_abs_target", "datetime", ".grz_row_id"))
    out <- joined[, .SD[1L], by = ".grz_target_id"]
  }

  drop_cols <- intersect(c(".grz_target_id", ".grz_target_time", ".grz_window_start", ".grz_window_end", ".grz_time", ".grz_abs_target"), names(out))
  if (length(drop_cols) > 0L) {
    out[, (drop_cols) := NULL]
  }
  data.table::setcolorder(out, c(groups, "datetime", "lon", "lat", setdiff(names(out), c(groups, "datetime", "lon", "lat"))))
  out[]
}

grz_downsample_diagnostics <- function(out, observed, groups, target_sec, n_missing_datetime, n_expected_by_group) {
  observed_counts <- observed[, list(n_observed_fixes = .N), by = groups]
  retained_counts <- out[, list(
    n_retained_fixes = .N,
    n_interpolated = if ("is_interpolated" %in% names(.SD)) sum(is_interpolated %in% TRUE, na.rm = TRUE) else 0L
  ), by = groups]
  gaps <- grz_observed_gap_summary(out, groups = groups)

  sampling <- data.table::copy(out)
  data.table::setorderv(sampling, c(groups, "datetime"))
  sampling[, .grz_dt_s := as.numeric(datetime - data.table::shift(datetime), units = "secs"), by = groups]
  sampling <- sampling[, list(
    sampling_interval_target_s = as.numeric(target_sec),
    sampling_interval_achieved_s = if (any(is.finite(.grz_dt_s) & .grz_dt_s > 0)) stats::median(.grz_dt_s[is.finite(.grz_dt_s) & .grz_dt_s > 0]) else NA_real_
  ), by = groups]

  diag <- Reduce(
    function(x, y) merge(x, y, by = groups, all = TRUE, sort = FALSE),
    list(n_expected_by_group, observed_counts, retained_counts, gaps, sampling)
  )
  data.table::setnames(diag, "n_target_times", "n_expected_fixes")
  diag[is.na(n_retained_fixes), n_retained_fixes := 0L]
  diag[is.na(n_interpolated), n_interpolated := 0L]
  diag[, prop_interpolated := n_interpolated / n_expected_fixes]
  diag[!is.finite(prop_interpolated), prop_interpolated := NA_real_]
  diag[, n_missing_datetime := as.integer(n_missing_datetime)]
  data.table::setcolorder(
    diag,
    c(
      groups,
      "n_expected_fixes",
      "n_observed_fixes",
      "n_retained_fixes",
      "n_interpolated",
      "prop_interpolated",
      "n_missing_datetime",
      "n_observed_gaps",
      "gap_min_s",
      "gap_median_s",
      "gap_max_s",
      "sampling_interval_target_s",
      "sampling_interval_achieved_s"
    )
  )
  diag[]
}

#' Downsample GPS fixes to a lower frequency
#'
#' Selects one observed fix per target time within each animal or sensor stream.
#' `method = "rigid"` keeps the first fix in each target interval, while
#' `method = "representative"` keeps the fix closest to each target time.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.
#' @param target_mins Target interval in minutes.
#' @param method Downsample mode: `"representative"` or `"rigid"`.
#' @param phase_mins Offset from the first timestamp in each group before the
#'   first target time is created.
#' @param groups Grouping columns for independent streams. Defaults to available
#'   `deployment_id`, `animal_id`, and `sensor_id`.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Downsampled GPS data. A `gps_reg` attribute summarises input
#'   fixes, retained fixes, gaps, and achieved sampling interval.
#' @export
gps_downsample <- function(
  data,
  target_mins,
  method = c("representative", "rigid"),
  phase_mins = 0,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  grz_require_positive_mins(target_mins, "target_mins")
  if (!is.numeric(phase_mins) || length(phase_mins) != 1L || !is.finite(phase_mins) || phase_mins < 0) {
    stop("`phase_mins` must be a non-negative number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_time_group_cols(dt, groups = groups, fun_name = "gps_downsample()")
  dt[, .grz_row_id := .I]
  n_missing_datetime <- sum(is.na(dt$datetime))
  dt <- dt[!is.na(datetime)]
  if (nrow(dt) == 0L) {
    stop("`data` must contain at least one valid `datetime`.", call. = FALSE)
  }

  target_sec <- grz_mins_to_sec(target_mins, "target_mins")
  phase_sec <- as.numeric(phase_mins) * 60
  phase_sec <- phase_sec %% target_sec

  n_expected <- dt[, {
    first_target <- min(as.numeric(datetime), na.rm = TRUE) + phase_sec
    last_time <- max(as.numeric(datetime), na.rm = TRUE)
    n <- if (!is.finite(first_target) || first_target > last_time) 0L else length(seq(first_target, last_time, by = target_sec))
    list(n_target_times = as.integer(n))
  }, by = grp]

  split_idx <- split(seq_len(nrow(dt)), interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE))
  out <- data.table::rbindlist(
    lapply(split_idx, function(i) grz_downsample_group(dt[i], groups = grp, target_sec = target_sec, phase_sec = phase_sec, method = method)),
    use.names = TRUE,
    fill = TRUE
  )
  out[, .grz_row_id := NULL]
  data.table::setorderv(out, c(grp, "datetime"))

  diagnostics <- grz_downsample_diagnostics(
    out,
    observed = dt,
    groups = grp,
    target_sec = target_sec,
    n_missing_datetime = n_missing_datetime,
    n_expected_by_group = n_expected
  )

  if (isTRUE(verbose)) {
    cat(sprintf(
      "[gps_downsample] method=%s target_mins=%s phase_mins=%s rows=%s -> %s\n",
      method,
      format(target_mins, trim = TRUE),
      format(phase_mins, trim = TRUE),
      format(nrow(dt), big.mark = ","),
      format(nrow(out), big.mark = ",")
    ))
  }

  grz_finish_temporal(
    out,
    diagnostics = diagnostics,
    parameters = list(target_mins = target_mins, phase_mins = phase_mins, method = method, groups = grp),
    return_class = rc
  )
}
