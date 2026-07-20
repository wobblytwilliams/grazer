grz_prepare_clean_dt <- function(data, require_core = TRUE) {
  if (!isTRUE(require_core)) {
    return(grz_prepare_gps_dt(data, require_cols = FALSE, fun_name = "clean function"))
  }
  grz_prepare_gps_dt(data, require_cols = TRUE, fun_name = "clean function")
}

# Make a cleaning audit row.
grz_clean_summary_row <- function(step, action, before_n, after_n, n_flagged = 0L, threshold_mps = NA_real_, notes = NA_character_) {
  data.table::data.table(
    step = step,
    action = action,
    before_n = as.integer(before_n),
    after_n = as.integer(after_n),
    n_removed = as.integer(max(0L, before_n - after_n)),
    n_flagged = as.integer(n_flagged),
    threshold_mps = as.numeric(threshold_mps),
    notes = as.character(notes)
  )
}

grz_clean_finish <- function(data, return_class, summary, removed_rows = NULL, flagged_rows = NULL, parameters = NULL, source = data) {
  out <- grz_as_output(data, return_class)
  previous_summary <- attr(source, "cleaning_summary", exact = TRUE)
  summary_dt <- data.table::rbindlist(
    list(
      if (is.null(previous_summary)) NULL else data.table::as.data.table(previous_summary),
      data.table::as.data.table(summary)
    ),
    use.names = TRUE,
    fill = TRUE
  )
  attr(out, "cleaning_summary") <- grz_as_output(summary_dt, "data.frame")

  step_name <- if ("step" %in% names(summary) && nrow(summary) > 0L) as.character(summary$step[[nrow(summary)]]) else NA_character_
  add_step <- function(x) {
    if (is.null(x) || nrow(x) == 0L) {
      return(NULL)
    }
    x <- data.table::as.data.table(x)
    if (!"clean_step" %in% names(x)) {
      x[, clean_step := step_name]
    }
    x
  }

  previous_removed <- attr(source, "removed_rows", exact = TRUE)
  removed_dt <- data.table::rbindlist(
    list(
      if (is.null(previous_removed)) NULL else data.table::as.data.table(previous_removed),
      add_step(removed_rows)
    ),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(removed_dt) > 0L) {
    attr(out, "removed_rows") <- grz_as_output(removed_dt, "data.frame")
  }

  previous_flagged <- attr(source, "flagged_rows", exact = TRUE)
  flagged_dt <- data.table::rbindlist(
    list(
      if (is.null(previous_flagged)) NULL else data.table::as.data.table(previous_flagged),
      add_step(flagged_rows)
    ),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(flagged_dt) > 0L) {
    attr(out, "flagged_rows") <- grz_as_output(flagged_dt, "data.frame")
  }

  previous_parameters <- attr(source, "cleaning_parameters", exact = TRUE)
  if (!is.null(parameters) || !is.null(previous_parameters)) {
    params <- if (is.null(previous_parameters)) list() else previous_parameters
    if (!is.null(parameters)) {
      params[[step_name]] <- parameters
    }
    attr(out, "cleaning_parameters") <- params
  }
  out
}

grz_strip_clean_attrs <- function(data) {
  attr(data, "cleaning_summary") <- NULL
  attr(data, "removed_rows") <- NULL
  attr(data, "flagged_rows") <- NULL
  attr(data, "cleaning_parameters") <- NULL
  data
}

grz_clean_reasons <- function(masks) {
  if (length(masks) == 0L) {
    return(character())
  }
  n <- length(masks[[1L]])
  reasons <- rep("", n)
  for (label in names(masks)) {
    mask <- masks[[label]]
    mask[is.na(mask)] <- FALSE
    if (!any(mask)) {
      next
    }
    reasons[mask] <- ifelse(reasons[mask] == "", label, paste(reasons[mask], label, sep = ";"))
  }
  reasons[reasons == ""] <- NA_character_
  reasons
}

grz_clean_parse_scalar_time <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  out <- grz_parse_datetime_utc(x)
  if (length(out) != 1L || is.na(out)) {
    stop("`", arg, "` must be a single datetime value.", call. = FALSE)
  }
  out
}

grz_clean_window_flags <- function(dt, window_start = NULL, window_end = NULL, deployment_windows = NULL, deployment_groups = NULL) {
  n <- nrow(dt)
  outside <- rep(FALSE, n)
  missing_window <- rep(FALSE, n)

  start <- grz_clean_parse_scalar_time(window_start, "window_start")
  end <- grz_clean_parse_scalar_time(window_end, "window_end")
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("`window_start` must be before or equal to `window_end`.", call. = FALSE)
  }
  if (!is.null(start)) {
    outside <- outside | (!is.na(dt$datetime) & dt$datetime < start)
  }
  if (!is.null(end)) {
    outside <- outside | (!is.na(dt$datetime) & dt$datetime > end)
  }

  if (is.null(deployment_windows)) {
    return(list(outside = outside, missing_window = missing_window))
  }

  grz_require_data_frame(deployment_windows, arg = "deployment_windows")
  win <- data.table::copy(data.table::as.data.table(deployment_windows))
  grz_require_cols(win, c("start_datetime", "end_datetime"), fun_name = "gps_clean_errors()")
  win[, start_datetime := grz_parse_datetime_utc(start_datetime)]
  win[, end_datetime := grz_parse_datetime_utc(end_datetime)]
  if (any(is.na(win$start_datetime) | is.na(win$end_datetime) | win$start_datetime > win$end_datetime)) {
    stop("`deployment_windows` must contain valid `start_datetime` and `end_datetime` values.", call. = FALSE)
  }

  if (is.null(deployment_groups)) {
    deployment_groups <- intersect(c("deployment_id", "sensor_id"), intersect(names(dt), names(win)))
  }
  if (!is.character(deployment_groups) || length(deployment_groups) < 1L) {
    stop("`deployment_groups` must identify at least one shared column.", call. = FALSE)
  }
  grz_require_cols(dt, deployment_groups, fun_name = "gps_clean_errors()")
  grz_require_cols(win, deployment_groups, fun_name = "gps_clean_errors()")
  if (nrow(unique(win[, deployment_groups, with = FALSE])) != nrow(win)) {
    stop("`deployment_windows` must have one row per deployment group.", call. = FALSE)
  }

  base <- data.table::copy(dt[, c(".grz_row_id", "datetime", deployment_groups), with = FALSE])
  joined <- merge(
    base,
    win[, c(deployment_groups, "start_datetime", "end_datetime"), with = FALSE],
    by = deployment_groups,
    all.x = TRUE,
    sort = FALSE
  )
  data.table::setorderv(joined, ".grz_row_id")
  missing_window <- is.na(joined$start_datetime) | is.na(joined$end_datetime)
  outside_window <- missing_window |
    (!is.na(joined$datetime) & joined$datetime < joined$start_datetime) |
    (!is.na(joined$datetime) & joined$datetime > joined$end_datetime)
  outside <- outside | outside_window

  list(outside = outside, missing_window = missing_window)
}

#' Drop or flag duplicate GPS fixes
#'
#' Identifies duplicate fixes from user-selected key columns. By default the
#' first matching row is retained and later duplicate rows are removed.
#'
#' @param data Data frame of GPS rows.
#' @param keys Columns used to identify duplicates.
#' @param action Either `"drop"` to remove duplicate rows or `"flag"` to keep
#'   all rows and add a logical flag column.
#' @param flag_col Name of the duplicate flag column when `action = "flag"`.
#' @param verbose Logical; print drop or flag counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned GPS data. Attributes `cleaning_summary`, `removed_rows`, and
#'   `flagged_rows` contain audit information where relevant.
#' @export
gps_clean_duplicates <- function(
  data,
  keys = c("sensor_id", "datetime", "lon", "lat"),
  action = c("drop", "flag"),
  flag_col = "is_duplicate_fix",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  action <- match.arg(action)
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = FALSE)
  grz_require_cols(dt, keys, fun_name = "gps_clean_duplicates()")

  before_n <- nrow(dt)
  duplicate_idx <- duplicated(dt, by = keys)
  duplicate_idx[is.na(duplicate_idx)] <- FALSE
  n_flagged <- sum(duplicate_idx)

  if (action == "flag") {
    out <- data.table::copy(dt)
    out[, (flag_col) := duplicate_idx]
    removed <- NULL
    flagged <- out[get(flag_col) == TRUE]
  } else {
    removed <- dt[duplicate_idx]
    flagged <- NULL
    out <- dt[!duplicate_idx]
  }

  summary <- grz_clean_summary_row(
    step = "duplicates",
    action = action,
    before_n = before_n,
    after_n = nrow(out),
    n_flagged = n_flagged,
    notes = paste(keys, collapse = ",")
  )

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean_duplicates] action=%s flagged=%s\n", action, format(n_flagged, big.mark = ",")))
  }
  grz_print_clean_step("gps_clean_duplicates", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "gps_clean_duplicates", snapshot = snapshot, verbose = verbose)
  grz_clean_finish(out, rc, summary, removed_rows = removed, flagged_rows = flagged, parameters = list(keys = keys), source = data)
}

grz_speed_dt <- function(data, groups = NULL) {
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  if (is.null(groups) && "segment_id" %in% names(dt) && !"segment_id" %in% grp) {
    grp <- c(grp, "segment_id")
  }
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

#' Clean GPS speed outliers using a fixed threshold
#'
#' @param data Data frame of GPS rows.
#' @param max_speed_mps Maximum biologically plausible speed (m/s).
#' @param groups Grouping columns for step/speed calculation.
#' @param action Either `"drop"` to remove rows above the speed threshold or
#'   `"flag"` to keep all rows and add a logical flag column.
#' @param flag_col Name of the speed flag column when `action = "flag"`.
#' @param keep_speed_cols Keep `step_dt_s`, `step_m`, and `speed_mps` columns.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned GPS data with cleaning audit attributes.
#' @export
gps_clean_speed_fixed <- function(
  data,
  max_speed_mps = 4,
  groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_speed_outlier",
  keep_speed_cols = FALSE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  action <- match.arg(action)
  if (!is.numeric(max_speed_mps) || length(max_speed_mps) != 1L || max_speed_mps <= 0) {
    stop("`max_speed_mps` must be a positive number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_speed_dt(data, groups = groups)
  before_n <- nrow(dt)

  flag_idx <- !is.na(dt$speed_mps) & dt$speed_mps > max_speed_mps
  flag_idx[is.na(flag_idx)] <- FALSE
  n_flagged <- sum(flag_idx)

  if (action == "flag") {
    out <- data.table::copy(dt)
    out[, (flag_col) := flag_idx]
    removed <- NULL
    flagged <- out[get(flag_col) == TRUE]
  } else {
    removed <- dt[flag_idx]
    flagged <- NULL
    out <- dt[!flag_idx]
  }

  if (!isTRUE(keep_speed_cols)) {
    out[, c("step_dt_s", "step_m", "speed_mps") := NULL]
  }

  summary <- grz_clean_summary_row(
    step = "speed_fixed",
    action = action,
    before_n = before_n,
    after_n = nrow(out),
    n_flagged = n_flagged,
    threshold_mps = max_speed_mps
  )

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean_speed_fixed] threshold=%.3f m/s action=%s flagged=%s\n", max_speed_mps, action, format(n_flagged, big.mark = ",")))
  }
  grz_print_clean_step("gps_clean_speed_fixed", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "gps_clean_speed_fixed", snapshot = snapshot, verbose = verbose)
  grz_clean_finish(out, rc, summary, removed_rows = removed, flagged_rows = flagged, parameters = list(max_speed_mps = max_speed_mps, groups = groups), source = data)
}

#' Clean GPS speed outliers using a data-driven threshold
#'
#' @param data Data frame of GPS rows.
#' @param method Threshold method: `"mad"` or `"quantile"`. The MAD method is
#'   fitted to the upper quartile of positive `log1p(speed_mps)` values so resting
#'   and near-resting fixes do not dominate the threshold.
#' @param k MAD multiplier (used when `method = "mad"`).
#' @param prob Quantile probability (used when `method = "quantile"`).
#' @param min_threshold_mps Lower bound for threshold.
#' @param groups Grouping columns for step/speed calculation.
#' @param action Either `"drop"` to remove rows above the speed threshold or
#'   `"flag"` to keep all rows and add a logical flag column.
#' @param flag_col Name of the speed flag column when `action = "flag"`.
#' @param keep_speed_cols Keep `step_dt_s`, `step_m`, and `speed_mps` columns.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned GPS data with cleaning audit attributes.
#' @export
gps_clean_speed_stat <- function(
  data,
  method = c("mad", "quantile"),
  k = 4,
  prob = 0.995,
  min_threshold_mps = 4,
  groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_speed_outlier",
  keep_speed_cols = FALSE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  action <- match.arg(action)
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
    upper_cut <- as.numeric(stats::quantile(spd, probs = 0.75, na.rm = TRUE, names = FALSE, type = 7))
    upper <- spd[spd >= upper_cut]
    if (length(upper) == 0L) {
      upper <- spd
    }
    upper_log <- log1p(upper)
    centre <- stats::median(upper_log, na.rm = TRUE)
    spread <- stats::mad(upper_log, na.rm = TRUE, constant = 1.4826)
    if (!is.finite(spread) || spread == 0) {
      spread <- stats::IQR(upper_log, na.rm = TRUE) / 1.349
    }
    if (!is.finite(spread) || spread == 0) {
      threshold <- as.numeric(stats::quantile(spd, probs = 0.995, na.rm = TRUE, names = FALSE, type = 7))
    } else {
      threshold <- expm1(centre + k * spread)
    }
  } else {
    threshold <- as.numeric(stats::quantile(spd, probs = prob, na.rm = TRUE, names = FALSE, type = 7))
  }
  threshold <- max(min_threshold_mps, threshold)

  flag_idx <- !is.na(dt$speed_mps) & dt$speed_mps > threshold
  flag_idx[is.na(flag_idx)] <- FALSE
  n_flagged <- sum(flag_idx)

  if (action == "flag") {
    out <- data.table::copy(dt)
    out[, (flag_col) := flag_idx]
    removed <- NULL
    flagged <- out[get(flag_col) == TRUE]
  } else {
    removed <- dt[flag_idx]
    flagged <- NULL
    out <- dt[!flag_idx]
  }

  if (!isTRUE(keep_speed_cols)) {
    out[, c("step_dt_s", "step_m", "speed_mps") := NULL]
  }

  summary <- grz_clean_summary_row(
    step = "speed_stat",
    action = action,
    before_n = before_n,
    after_n = nrow(out),
    n_flagged = n_flagged,
    threshold_mps = threshold,
    notes = method
  )

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean_speed_stat] method=%s threshold=%.3f m/s action=%s flagged=%s\n", method, threshold, action, format(n_flagged, big.mark = ",")))
  }
  grz_print_clean_step("gps_clean_speed_stat", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "gps_clean_speed_stat", snapshot = snapshot, verbose = verbose)
  finished <- grz_clean_finish(out, rc, summary, removed_rows = removed, flagged_rows = flagged, parameters = list(method = method, k = k, prob = prob, min_threshold_mps = min_threshold_mps, groups = groups), source = data)
  attr(finished, "speed_threshold_mps") <- threshold
  finished
}

#' Clean row-level GPS data errors
#'
#' Removes or flags invalid datetime, identifier, coordinate, and deployment
#' window rows.
#'
#' @param data Data frame of GPS rows.
#' @param remove_invalid_datetime Logical; drop invalid datetimes.
#' @param remove_invalid_coords Logical; drop invalid coordinate rows.
#' @param remove_zero_zero Logical; drop `(0,0)` rows.
#' @param window_start Optional global deployment start datetime.
#' @param window_end Optional global deployment end datetime.
#' @param deployment_windows Optional data frame with deployment group columns,
#'   `start_datetime`, and `end_datetime`.
#' @param deployment_groups Optional columns used to join `deployment_windows`.
#' @param action Either `"drop"` to remove rows or `"flag"` to keep all rows and
#'   add flag and reason columns.
#' @param flag_col Name of the error flag column when `action = "flag"`.
#' @param reason_col Name of the error reason column when `action = "flag"`.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned GPS data with cleaning audit attributes.
#' @export
gps_clean_errors <- function(
  data,
  remove_invalid_datetime = TRUE,
  remove_invalid_coords = TRUE,
  remove_zero_zero = TRUE,
  window_start = NULL,
  window_end = NULL,
  deployment_windows = NULL,
  deployment_groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_gps_error",
  reason_col = "gps_error_reason",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  action <- match.arg(action)
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  dt[, .grz_row_id := .I]
  before_n <- nrow(dt)

  bad_sensor <- is.na(dt$sensor_id) | trimws(dt$sensor_id) == ""
  bad_datetime <- is.na(dt$datetime)
  bad_lonlat <- is.na(dt$lon) | is.na(dt$lat) |
    !is.finite(dt$lon) | !is.finite(dt$lat) |
    dt$lon < -180 | dt$lon > 180 |
    dt$lat < -90 | dt$lat > 90
  bad_zero <- !is.na(dt$lon) & !is.na(dt$lat) & dt$lon == 0 & dt$lat == 0
  window_flags <- grz_clean_window_flags(
    dt,
    window_start = window_start,
    window_end = window_end,
    deployment_windows = deployment_windows,
    deployment_groups = deployment_groups
  )

  flag_idx <- bad_sensor
  if (isTRUE(remove_invalid_datetime)) {
    flag_idx <- flag_idx | bad_datetime
  }
  if (isTRUE(remove_invalid_coords)) {
    flag_idx <- flag_idx | bad_lonlat
  }
  if (isTRUE(remove_zero_zero)) {
    flag_idx <- flag_idx | bad_zero
  }
  flag_idx <- flag_idx | window_flags$outside
  flag_idx[is.na(flag_idx)] <- FALSE
  n_flagged <- sum(flag_idx)

  reason_masks <- list(missing_sensor_id = bad_sensor)
  if (isTRUE(remove_invalid_datetime)) {
    reason_masks$invalid_datetime <- bad_datetime
  }
  if (isTRUE(remove_invalid_coords)) {
    reason_masks$invalid_coordinate <- bad_lonlat
  }
  if (isTRUE(remove_zero_zero)) {
    reason_masks$zero_zero <- bad_zero
  }
  reason_masks$missing_deployment_window <- window_flags$missing_window
  reason_masks$outside_deployment_window <- window_flags$outside & !window_flags$missing_window
  reasons <- grz_clean_reasons(reason_masks)

  if (action == "flag") {
    out <- data.table::copy(dt)
    out[, (flag_col) := flag_idx]
    out[, (reason_col) := reasons]
    removed <- NULL
    flagged <- out[get(flag_col) == TRUE]
  } else {
    removed <- dt[flag_idx]
    if (nrow(removed) > 0L) {
      removed[, (reason_col) := reasons[flag_idx]]
    }
    flagged <- NULL
    out <- dt[!flag_idx]
  }
  out[, .grz_row_id := NULL]
  if (!is.null(removed) && ".grz_row_id" %in% names(removed)) {
    removed[, .grz_row_id := NULL]
  }
  if (!is.null(flagged) && ".grz_row_id" %in% names(flagged)) {
    flagged[, .grz_row_id := NULL]
  }

  summary <- grz_clean_summary_row(
    step = "errors",
    action = action,
    before_n = before_n,
    after_n = nrow(out),
    n_flagged = n_flagged,
    notes = paste(c(
      if (isTRUE(remove_invalid_datetime)) "datetime",
      if (isTRUE(remove_invalid_coords)) "coordinates",
      if (isTRUE(remove_zero_zero)) "zero_zero",
      if (!is.null(window_start) || !is.null(window_end) || !is.null(deployment_windows)) "deployment_window"
    ), collapse = ",")
  )

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[gps_clean_errors] action=%s flagged_sensor=%s flagged_datetime=%s flagged_coord=%s flagged_zero_zero=%s flagged_window=%s\n",
        action,
        format(sum(bad_sensor, na.rm = TRUE), big.mark = ","),
        format(ifelse(remove_invalid_datetime, sum(bad_datetime, na.rm = TRUE), 0L), big.mark = ","),
        format(ifelse(remove_invalid_coords, sum(bad_lonlat, na.rm = TRUE), 0L), big.mark = ","),
        format(ifelse(remove_zero_zero, sum(bad_zero, na.rm = TRUE), 0L), big.mark = ","),
        format(sum(window_flags$outside, na.rm = TRUE), big.mark = ",")
      )
    )
  }
  grz_print_clean_step("gps_clean_errors", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "gps_clean_errors", snapshot = snapshot, verbose = verbose)
  grz_clean_finish(
    out,
    rc,
    summary,
    removed_rows = removed,
    flagged_rows = flagged,
    parameters = list(
      remove_invalid_datetime = remove_invalid_datetime,
      remove_invalid_coords = remove_invalid_coords,
      remove_zero_zero = remove_zero_zero,
      window_start = window_start,
      window_end = window_end,
      deployment_groups = deployment_groups
    ),
    source = data
  )
}

grz_select_paddock_name_col <- function(paddocks) {
  name_cols <- intersect(c("NAME", "Name", "name"), names(paddocks))
  has_desc <- "Description" %in% names(paddocks)
  if (length(name_cols) == 0L && !has_desc) {
    stop("Paddock polygons must contain `NAME`, `Name`, `name`, or `Description` column.", call. = FALSE)
  }

  is_complete <- function(x) {
    all(!is.na(x) & trimws(as.character(x)) != "")
  }

  for (name_col in name_cols) {
    if (is_complete(paddocks[[name_col]])) {
      return(name_col)
    }
  }
  if (has_desc && is_complete(paddocks$Description)) {
    return("Description")
  }
  stop(
    "Paddock names are partial across `NAME`, `Name`, `name`, and `Description`. ",
    "All polygons must be fully named in one column. `NAME` is preferred.",
    call. = FALSE
  )
}

#' Append paddock names to GPS fixes
#'
#' Assigns paddock or area names to GPS fixes using a point-in-polygon overlay.
#' GPS fixes are treated as WGS84 longitude and latitude. Paddock polygons must
#' have a CRS, or are assumed to be WGS84 when the CRS is missing.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.
#' @param paddocks_sf `sf` polygon object containing paddock or area polygons.
#' @param name_col Column in `paddocks_sf` containing paddock names. If `NULL`,
#'   a complete `NAME`, `Name`, `name`, or `Description` column is selected.
#' @param paddock_col Output paddock column name.
#' @param buffer_m Optional buffer distance in metres. Use `0` for a direct
#'   point-in-polygon overlay.
#' @param metric_crs Projected CRS used when `buffer_m > 0`. `NULL` selects a
#'   UTM CRS from the GPS coordinates.
#' @param epoch Epoch level for assignment: `"day"` or `"hour"`. Supplying
#'   `epoch_mins` uses fixed-duration intervals.
#' @param epoch_mins Optional fixed epoch duration in minutes.
#' @param groups Grouping columns used for animal or sensor tracks.
#' @param min_prop Minimum proportion of valid GPS fixes in an animal-epoch
#'   that must fall in one paddock before it is assigned.
#' @param min_fixes Minimum valid GPS fixes required per animal-epoch.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return GPS data with an `assigned_paddock` column appended by default.
#' @export
gps_append_paddock_names <- function(
  data,
  paddocks_sf,
  name_col = NULL,
  paddock_col = "assigned_paddock",
  buffer_m = 0,
  metric_crs = NULL,
  epoch = c("day", "hour"),
  epoch_mins = NULL,
  groups = NULL,
  min_prop = 0.7,
  min_fixes = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  grz_require_sf("gps_append_paddock_names()")
  grz_require_sf_object(paddocks_sf, arg = "paddocks_sf")
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  if (!is.null(epoch_mins)) {
    if (!epoch_missing && !identical(epoch, "interval")) {
      stop("Use `epoch_mins` with `epoch = \"interval\"` or leave `epoch` unset.", call. = FALSE)
    }
    grz_require_positive_mins(epoch_mins, "epoch_mins")
    epoch <- "interval"
  } else {
    epoch <- match.arg(epoch)
  }

  if (!is.null(name_col)) {
    if (!is.character(name_col) || length(name_col) != 1L || is.na(name_col) || trimws(name_col) == "") {
      stop("`name_col` must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!(name_col %in% names(paddocks_sf))) {
      stop("`name_col` was not found in `paddocks_sf`.", call. = FALSE)
    }
  } else {
    name_col <- grz_select_paddock_name_col(paddocks_sf)
  }
  if (!is.character(paddock_col) || length(paddock_col) != 1L || is.na(paddock_col) || trimws(paddock_col) == "") {
    stop("`paddock_col` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || !is.finite(buffer_m) || buffer_m < 0) {
    stop("`buffer_m` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(min_prop) || length(min_prop) != 1L || !is.finite(min_prop) || min_prop <= 0 || min_prop > 1) {
    stop("`min_prop` must be a single number in (0, 1].", call. = FALSE)
  }
  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || !is.finite(min_fixes) || min_fixes < 1) {
    stop("`min_fixes` must be a positive number.", call. = FALSE)
  }
  min_fixes <- as.integer(min_fixes)

  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  valid <- grz_gps_valid_coord(dt, fun_name = "gps_append_paddock_names()") & !is.na(dt$datetime)
  if (!all(valid)) {
    stop("`gps_append_paddock_names()` requires valid datetime, longitude, and latitude values.", call. = FALSE)
  }
  dt[, .grz_row_id := .I]

  pdks <- paddocks_sf
  pdks[[paddock_col]] <- as.character(pdks[[name_col]])
  if (is.na(sf::st_crs(pdks))) {
    sf::st_crs(pdks) <- 4326
    if (isTRUE(verbose)) {
      cat("[gps_append_paddock_names] paddock CRS missing; assuming EPSG:4326.\n")
    }
  }

  pts <- grz_gps_as_sf(
    dt,
    crs = 4326,
    remove = FALSE,
    fun_name = "gps_append_paddock_names()"
  )

  if (buffer_m > 0) {
    out_crs <- grz_metric_crs(dt, metric_crs = metric_crs)
    pts_overlay <- sf::st_transform(pts, out_crs)
    pdks_overlay <- sf::st_transform(pdks[, paddock_col], out_crs)
    pdks_overlay <- sf::st_make_valid(sf::st_buffer(pdks_overlay, dist = buffer_m))
  } else {
    pts_overlay <- sf::st_transform(pts, sf::st_crs(pdks))
    pdks_overlay <- pdks[, paddock_col]
  }

  hits <- sf::st_intersects(pts_overlay, pdks_overlay)
  assigned <- rep(NA_character_, length(hits))
  has_hit <- which(lengths(hits) > 0L)
  if (length(has_hit) > 0L) {
    assigned[has_hit] <- vapply(
      has_hit,
      function(i) as.character(pdks_overlay[[paddock_col]][hits[[i]][1L]]),
      character(1L)
    )
  }

  out <- data.table::as.data.table(sf::st_drop_geometry(pts))
  out[, .grz_hit_paddock := assigned]
  epoch_dt <- grz_epoch_table(out$datetime, epoch = epoch, epoch_mins = epoch_mins)
  out[, .grz_epoch := epoch_dt$epoch]

  by_cols <- c(grp, ".grz_epoch")
  totals <- out[, list(n_valid_fixes = .N), by = by_cols]
  matched <- out[
    !is.na(.grz_hit_paddock),
    list(n_paddock_fixes = .N),
    by = c(by_cols, ".grz_hit_paddock")
  ]
  if (nrow(matched) > 0L) {
    data.table::setorderv(
      matched,
      cols = c(by_cols, "n_paddock_fixes", ".grz_hit_paddock"),
      order = c(rep(1L, length(by_cols)), -1L, 1L)
    )
    best <- matched[, .SD[1L], by = by_cols]
    best <- merge(best, totals, by = by_cols, all.y = TRUE, sort = FALSE)
  } else {
    best <- data.table::copy(totals)
    best[, `:=`(.grz_hit_paddock = NA_character_, n_paddock_fixes = 0L)]
  }
  best[is.na(n_paddock_fixes), n_paddock_fixes := 0L]
  best[, prop_fixes := n_paddock_fixes / n_valid_fixes]
  best[, (paddock_col) := data.table::fifelse(n_valid_fixes >= min_fixes & prop_fixes >= min_prop, .grz_hit_paddock, NA_character_)]
  assignment <- best[, c(by_cols, paddock_col), with = FALSE]

  out <- merge(out, assignment, by = by_cols, all.x = TRUE, sort = FALSE)
  data.table::setorderv(out, ".grz_row_id")
  unassigned_epochs <- best[is.na(get(paddock_col)), .N]
  out[, c(".grz_row_id", ".grz_hit_paddock", ".grz_epoch") := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf(
      "[gps_append_paddock_names] rows=%s assigned_rows=%s unassigned_animal_epochs=%s epoch=%s\n",
      format(nrow(out), big.mark = ","),
      format(sum(!is.na(out[[paddock_col]])), big.mark = ","),
      format(unassigned_epochs, big.mark = ","),
      epoch
    ))
  }
  if (unassigned_epochs > 0L) {
    warning(
      sprintf("%s animal-epoch(s) did not satisfy `min_prop` and `min_fixes`; `%s` was set to NA for those rows.", unassigned_epochs, paddock_col),
      call. = FALSE
    )
  }

  grz_as_output(out, rc)
}

#' Spatial cleaning using paddock or boundary polygons
#'
#' @param data Data frame of GPS rows.
#' @param paddocks_sf `sf` paddock polygons.
#' @param buffer_m Paddock buffer in meters.
#' @param append_paddock Logical; append paddock column.
#' @param paddock_col Name of paddock output column.
#' @param groups Grouping columns for modal paddock assignment.
#' @param action Either `"drop"` to remove rows outside polygons or `"flag"` to
#'   keep all rows and add a logical flag column.
#' @param flag_col Name of the outside-polygon flag column when
#'   `action = "flag"`.
#' @param verbose Logical; print drop counts.
#' @param snapshot Logical; print quick snapshot after step.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Spatially cleaned GPS data with cleaning audit attributes.
#' @export
gps_clean_spatial <- function(
  data,
  paddocks_sf,
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_outside_boundary",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
) {
  action <- match.arg(action)
  grz_require_sf("gps_clean_spatial()")
  grz_require_sf_object(paddocks_sf, arg = "paddocks_sf")
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || buffer_m < 0) {
    stop("`buffer_m` must be a non-negative number.", call. = FALSE)
  }
  if (!is.character(paddock_col) || length(paddock_col) != 1L || trimws(paddock_col) == "") {
    stop("`paddock_col` must be a single non-empty column name.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  before_n <- nrow(dt)
  name_col <- grz_select_paddock_name_col(paddocks_sf)

  pdks <- paddocks_sf
  pdks[[paddock_col]] <- as.character(pdks[[name_col]])
  if (is.na(sf::st_crs(pdks))) {
    sf::st_crs(pdks) <- 4326
    if (isTRUE(verbose)) {
      cat("[gps_clean_spatial] paddock CRS missing; assuming EPSG:4326.\n")
    }
  }

  pdks_3857 <- sf::st_transform(pdks[, paddock_col], 3857)
  pdks_buf <- sf::st_buffer(pdks_3857, dist = buffer_m)
  pdks_buf <- sf::st_make_valid(pdks_buf)

  pts <- grz_gps_as_sf(
    dt,
    crs = 4326,
    remove = FALSE,
    fun_name = "gps_clean_spatial()"
  )
  pts_3857 <- sf::st_transform(pts, 3857)
  hits <- sf::st_intersects(pts_3857, pdks_buf)
  inside <- lengths(hits) > 0L
  assigned <- rep(NA_character_, length(hits))
  has_hit <- which(inside)
  if (length(has_hit) > 0L) {
    assigned[has_hit] <- vapply(has_hit, function(i) as.character(pdks_buf[[paddock_col]][hits[[i]][1L]]), character(1L))
  }

  flag_idx <- !inside
  n_flagged <- sum(flag_idx)
  out <- data.table::copy(dt)
  if (isTRUE(append_paddock)) {
    out[, (paddock_col) := assigned]
  }

  if (action == "flag") {
    out[, (flag_col) := flag_idx]
    removed <- NULL
    flagged <- out[get(flag_col) == TRUE]
  } else {
    removed <- out[flag_idx]
    flagged <- NULL
    out <- out[!flag_idx]
  }

  summary <- grz_clean_summary_row(
    step = "spatial",
    action = action,
    before_n = before_n,
    after_n = nrow(out),
    n_flagged = n_flagged,
    notes = paste0("name_col=", name_col)
  )

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean_spatial] action=%s outside=%s buffer_m=%s\n", action, format(n_flagged, big.mark = ","), format(buffer_m, trim = TRUE)))
  }
  grz_print_clean_step("gps_clean_spatial", before_n, nrow(out), verbose = verbose)
  grz_print_snapshot(out, step = "gps_clean_spatial", snapshot = snapshot, verbose = verbose)
  grz_clean_finish(out, rc, summary, removed_rows = removed, flagged_rows = flagged, parameters = list(buffer_m = buffer_m, paddock_col = paddock_col, groups = groups), source = data)
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
  fit <- tryCatch({
    suppressWarnings(suppressMessages(stats::smooth.spline(x = x[idx], y = y_num[idx])))
  }, error = function(e) NULL)
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
  if (is.null(state_col)) {
    stop("`state_col` is required when `method = \"state_aware\"`.", call. = FALSE)
  }
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single column name.", call. = FALSE)
  }
  if (!state_col %in% names(dt)) {
    stop("`state_col` not found in `data`: ", state_col, call. = FALSE)
  }
  state_col
}

#' Denoise GPS jitter using statistical or state-aware smoothing
#'
#' Uses statistical smoothing to reduce coordinate noise without dropping rows.
#' If active/inactive state labels are available, a state-aware method can be
#' used where inactive runs are collapsed to a robust centroid and active runs
#' are smoothed statistically.
#'
#' @param data Data frame of GPS rows.
#' @param method Denoise method: `"statistical"` or `"state_aware"`.
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
gps_denoise <- function(
  data,
  method = c("statistical", "state_aware"),
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

  state_col_used <- if (method == "state_aware") grz_denoise_pick_state_col(dt, state_col = state_col) else NULL
  inactive_lookup <- unique(grz_denoise_normalize_state(inactive_states))

  mode_used <- method

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

  n_adjusted <- sum(is.finite(lon_out) & is.finite(lat_out) & (lon_out != dt$lon | lat_out != dt$lat), na.rm = TRUE)
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
  summary <- grz_clean_summary_row(
    step = "denoise",
    action = "modify",
    before_n = before_n,
    after_n = nrow(dt),
    n_flagged = 0L,
    notes = mode_used
  )

  finished <- grz_clean_finish(dt, rc, summary, parameters = list(method = method, mode_used = mode_used, state_col = state_col_used, groups = groups, n_adjusted = n_adjusted), source = data)
  attr(finished, "denoise_summary") <- list(
    method = method,
    state_col = state_col_used,
    n_adjusted = as.integer(n_adjusted),
    keep_raw_coords = keep_raw_coords
  )
  finished
}

#' Clean GPS data using a selected sequence of steps
#'
#' Applies selected cleaning steps and returns cleaned data. Each step attaches
#' an audit summary, and `gps_clean()` combines those summaries into the final
#' `cleaning_summary` attribute.
#'
#' @param data Data frame of GPS rows.
#' @param steps Steps to apply. Any of: `"duplicates"`, `"errors"`,
#'   `"speed_fixed"`, `"speed_stat"`, `"spatial"`, `"denoise"`.
#' @param action Either `"drop"` to remove rows in row-filtering steps or
#'   `"flag"` to keep rows and add step-specific flag columns.
#' @param paddocks_sf Optional paddock polygons (`sf`) required for `"spatial"`.
#' @param max_speed_mps Fixed speed threshold (m/s).
#' @param speed_stat_method Statistical speed method.
#' @param window_start Optional global deployment start datetime.
#' @param window_end Optional global deployment end datetime.
#' @param deployment_windows Optional deployment window table passed to
#'   `gps_clean_errors()`.
#' @param deployment_groups Optional join columns for `deployment_windows`.
#' @param buffer_m Paddock buffer in meters.
#' @param append_paddock Logical; append paddock name column.
#' @param paddock_col Output paddock column name.
#' @param denoise_method Denoise method passed to `gps_denoise()`.
#' @param denoise_state_col Optional state column for state-aware denoise.
#' @param denoise_inactive_states Inactive state labels for state-aware denoise.
#' @param denoise_keep_raw_coords Logical; keep `lon_raw` and `lat_raw`.
#' @param step_args Optional named list of step-specific arguments. Names must
#'   match requested steps.
#' @param groups Grouping columns for speed/denoise/modal paddock operations.
#' @param snapshot Logical; print snapshots after each step.
#' @param verbose Logical; print details.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Cleaned GPS data with combined cleaning audit attributes.
#' @export
gps_clean <- function(
  data,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  action = c("drop", "flag"),
  paddocks_sf = NULL,
  max_speed_mps = 4,
  speed_stat_method = c("mad", "quantile"),
  window_start = NULL,
  window_end = NULL,
  deployment_windows = NULL,
  deployment_groups = NULL,
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  denoise_method = c("statistical", "state_aware"),
  denoise_state_col = NULL,
  denoise_inactive_states = c("inactive", "rest", "resting", "idle", "stationary", "lying", "ruminating"),
  denoise_keep_raw_coords = TRUE,
  step_args = list(),
  groups = NULL,
  snapshot = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  action <- match.arg(action)
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
  if (!is.list(step_args) || (length(step_args) > 0L && is.null(names(step_args)))) {
    stop("`step_args` must be a named list.", call. = FALSE)
  }
  if (length(step_args) > 0L) {
    bad_step_args <- setdiff(names(step_args), allowed_steps)
    if (length(bad_step_args) > 0L) {
      stop("`step_args` has unknown step name(s): ", paste(bad_step_args, collapse = ", "), call. = FALSE)
    }
    not_lists <- names(step_args)[!vapply(step_args, is.list, logical(1))]
    if (length(not_lists) > 0L) {
      stop("Each `step_args` entry must be a list: ", paste(not_lists, collapse = ", "), call. = FALSE)
    }
  }

  out <- data.table::copy(data.table::as.data.table(data))
  summaries <- list()
  removed_rows <- list()
  flagged_rows <- list()

  build_args <- function(step, defaults) {
    extra <- step_args[[step]]
    if (is.null(extra)) {
      return(defaults)
    }
    utils::modifyList(defaults, extra, keep.null = TRUE)
  }

  record_audit <- function(step, result) {
    summary <- attr(result, "cleaning_summary", exact = TRUE)
    if (!is.null(summary)) {
      summaries[[length(summaries) + 1L]] <<- data.table::as.data.table(summary)
    }
    removed <- attr(result, "removed_rows", exact = TRUE)
    if (!is.null(removed) && nrow(removed) > 0L) {
      if (!"clean_step" %in% names(removed)) {
        removed$clean_step <- step
      }
      removed_rows[[length(removed_rows) + 1L]] <<- data.table::as.data.table(removed)
    }
    flagged <- attr(result, "flagged_rows", exact = TRUE)
    if (!is.null(flagged) && nrow(flagged) > 0L) {
      if (!"clean_step" %in% names(flagged)) {
        flagged$clean_step <- step
      }
      flagged_rows[[length(flagged_rows) + 1L]] <<- data.table::as.data.table(flagged)
    }
    invisible(NULL)
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean] start_rows=%s\n", format(nrow(out), big.mark = ",")))
  }

  for (st in steps) {
    if (st == "duplicates") {
      args <- build_args(st, list(
        data = out,
        action = action,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_clean_duplicates, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    } else if (st == "errors") {
      args <- build_args(st, list(
        data = out,
        remove_invalid_datetime = TRUE,
        remove_invalid_coords = TRUE,
        remove_zero_zero = TRUE,
        window_start = window_start,
        window_end = window_end,
        deployment_windows = deployment_windows,
        deployment_groups = deployment_groups,
        action = action,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_clean_errors, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    } else if (st == "speed_fixed") {
      args <- build_args(st, list(
        data = out,
        max_speed_mps = max_speed_mps,
        groups = groups,
        action = action,
        keep_speed_cols = FALSE,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_clean_speed_fixed, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    } else if (st == "speed_stat") {
      args <- build_args(st, list(
        data = out,
        method = speed_stat_method,
        min_threshold_mps = max_speed_mps,
        groups = groups,
        action = action,
        keep_speed_cols = FALSE,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_clean_speed_stat, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    } else if (st == "spatial") {
      if (is.null(paddocks_sf)) {
        stop("`steps` includes `spatial` but `paddocks_sf` is NULL.", call. = FALSE)
      }
      args <- build_args(st, list(
        data = out,
        paddocks_sf = paddocks_sf,
        buffer_m = buffer_m,
        append_paddock = append_paddock,
        paddock_col = paddock_col,
        groups = groups,
        action = action,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_clean_spatial, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    } else if (st == "denoise") {
      args <- build_args(st, list(
        data = out,
        method = denoise_method,
        state_col = denoise_state_col,
        inactive_states = denoise_inactive_states,
        groups = groups,
        keep_raw_coords = denoise_keep_raw_coords,
        verbose = verbose,
        snapshot = snapshot,
        return_class = "data.table"
      ))
      res <- do.call(gps_denoise, args)
      record_audit(st, res)
      out <- grz_strip_clean_attrs(data.table::as.data.table(res))
    }
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_clean] final_rows=%s\n", format(nrow(out), big.mark = ",")))
  }

  summary <- if (length(summaries) == 0L) {
    grz_clean_summary_row("none", action, nrow(out), nrow(out), 0L)
  } else {
    data.table::rbindlist(summaries, use.names = TRUE, fill = TRUE)
  }
  removed <- if (length(removed_rows) == 0L) NULL else data.table::rbindlist(removed_rows, use.names = TRUE, fill = TRUE)
  flagged <- if (length(flagged_rows) == 0L) NULL else data.table::rbindlist(flagged_rows, use.names = TRUE, fill = TRUE)

  grz_clean_finish(
    out,
    rc,
    summary,
    removed_rows = removed,
    flagged_rows = flagged,
    parameters = list(
      steps = steps,
      action = action,
      max_speed_mps = max_speed_mps,
      speed_stat_method = speed_stat_method,
      groups = groups,
      step_args = step_args
    ),
    source = grz_strip_clean_attrs(out)
  )
}
