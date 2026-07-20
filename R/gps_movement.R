grz_epoch_label <- function(datetime, epoch = c("day", "hour", "week", "month"), epoch_mins = NULL) {
  epoch <- match.arg(epoch)
  if (epoch == "day") {
    return(as.character(as.Date(datetime, tz = "UTC")))
  }
  if (epoch == "hour") {
    return(format(datetime, format = "%Y-%m-%d %H:00:00", tz = "UTC"))
  }
  if (epoch == "week") {
    return(strftime(datetime, format = "%G-W%V", tz = "UTC"))
  }
  strftime(datetime, format = "%Y-%m", tz = "UTC")
}

grz_month_start_utc <- function(datetime) {
  lt <- as.POSIXlt(datetime, tz = "UTC")
  as.POSIXct(
    sprintf("%04d-%02d-01 00:00:00", lt$year + 1900L, lt$mon + 1L),
    tz = "UTC"
  )
}

grz_month_end_utc <- function(datetime) {
  lt <- as.POSIXlt(datetime, tz = "UTC")
  year <- lt$year + 1900L
  month <- lt$mon + 2L
  year <- year + (month > 12L)
  month[month > 12L] <- 1L
  as.POSIXct(
    sprintf("%04d-%02d-01 00:00:00", year, month),
    tz = "UTC"
  )
}

grz_epoch_table <- function(datetime, epoch, epoch_mins = NULL) {
  seconds <- as.numeric(datetime)
  if (epoch == "interval") {
    grz_require_positive_mins(epoch_mins, "epoch_mins")
    epoch_s <- as.numeric(epoch_mins) * 60
    start <- as.POSIXct(floor(seconds / epoch_s) * epoch_s, origin = "1970-01-01", tz = "UTC")
    end <- start + epoch_s
    label <- format(start, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    mins <- rep(as.numeric(epoch_mins), length(datetime))
  } else if (epoch == "hour") {
    start <- as.POSIXct(floor(seconds / 3600) * 3600, origin = "1970-01-01", tz = "UTC")
    end <- start + 3600
    label <- grz_epoch_label(datetime, epoch = "hour")
    mins <- rep(60, length(datetime))
  } else if (epoch == "day") {
    start <- as.POSIXct(as.Date(datetime, tz = "UTC"), tz = "UTC")
    end <- start + 86400
    label <- grz_epoch_label(datetime, epoch = "day")
    mins <- rep(1440, length(datetime))
  } else if (epoch == "week") {
    day_start <- as.POSIXct(as.Date(datetime, tz = "UTC"), tz = "UTC")
    wday <- as.POSIXlt(day_start, tz = "UTC")$wday
    start <- day_start - ((wday + 6L) %% 7L) * 86400
    end <- start + 7 * 86400
    label <- grz_epoch_label(datetime, epoch = "week")
    mins <- rep(7 * 1440, length(datetime))
  } else {
    start <- grz_month_start_utc(datetime)
    end <- grz_month_end_utc(datetime)
    label <- grz_epoch_label(datetime, epoch = "month")
    mins <- as.numeric(end - start, units = "mins")
  }

  data.table::data.table(
    epoch = label,
    epoch_start = start,
    epoch_end = end,
    epoch_mins = mins
  )
}

grz_constant_or_na <- function(x) {
  if (!is.atomic(x)) {
    return(NA)
  }
  y <- x[!is.na(x)]
  if (length(y) == 0L) {
    return(x[NA_integer_][1L])
  }
  uy <- unique(y)
  if (length(uy) == 1L) {
    return(uy[1L])
  }
  x[NA_integer_][1L]
}

#' Calculate GPS step-level movement metrics
#'
#' Builds row-level step metrics independently within each animal or sensor
#' stream. Coordinates are assumed to be WGS84 longitude and latitude in decimal
#' degrees. Step distances are great-circle haversine distances using a
#' spherical earth radius of 6,371,000 m.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param groups Grouping columns for step calculations. Defaults to
#'   `deployment_id` and `sensor_id` when `deployment_id` is present, otherwise
#'   `sensor_id`.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Data with appended `step_dt_s`, `step_m`, `speed_mps`,
#'   `bearing_deg`, `turn_rad`, `cum_distance_m`, and `net_displacement_m`
#'   fields.
#' @export
gps_steps <- function(
  data,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  if (is.null(groups) && "segment_id" %in% names(dt) && !"segment_id" %in% grp) {
    grp <- c(grp, "segment_id")
  }
  dt[, .grz_row_id := .I]
  data.table::setorderv(dt, c(grp, "datetime", ".grz_row_id"))

  dt[, `:=`(
    .grz_prev_datetime = shift(datetime),
    .grz_prev_lon = shift(lon),
    .grz_prev_lat = shift(lat)
  ), by = grp]

  dt[, step_dt_s := as.numeric(datetime - .grz_prev_datetime, units = "secs")]
  dt[, step_m := grz_haversine_m(.grz_prev_lon, .grz_prev_lat, lon, lat)]
  dt[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]
  dt[, bearing_deg := grz_bearing_deg(.grz_prev_lon, .grz_prev_lat, lon, lat)]
  dt[, turn_rad := grz_abs_turn_rad(bearing_deg, shift(bearing_deg)), by = grp]
  dt[, cum_distance_m := cumsum(data.table::fifelse(is.na(step_m), 0, step_m)), by = grp]
  dt[, net_displacement_m := grz_haversine_m(lon[1L], lat[1L], lon, lat), by = grp]

  dt[, c(".grz_row_id", ".grz_prev_datetime", ".grz_prev_lon", ".grz_prev_lat") := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_steps] rows=%s groups=%s\n", format(nrow(dt), big.mark = ","), format(nrow(unique(dt[, ..grp])), big.mark = ",")))
  }
  grz_as_output(dt, rc)
}

#' Calculate GPS turning angles
#'
#' Thin wrapper around `gps_steps()` for workflows that need bearing and turning
#' fields. Turning angles are absolute changes between consecutive step bearings
#' within each group.
#'
#' @inheritParams gps_steps
#' @param unit Turning angle unit to return. `"radians"` returns `turn_rad`,
#'   `"degrees"` returns `turn_deg`, and `"both"` returns both columns.
#'
#' @return Data with step bearings and turning angle fields.
#' @export
gps_turning <- function(
  data,
  groups = NULL,
  unit = c("radians", "degrees", "both"),
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  unit <- match.arg(unit)
  rc <- grz_match_output_class(return_class)
  out <- data.table::as.data.table(gps_steps(
    data = data,
    groups = groups,
    verbose = FALSE,
    return_class = "data.table"
  ))

  if (unit %in% c("degrees", "both")) {
    out[, turn_deg := turn_rad * 180 / pi]
  }
  if (unit == "degrees") {
    out[, turn_rad := NULL]
  }

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_turning] rows=%s unit=%s\n", format(nrow(out), big.mark = ","), unit))
  }
  grz_as_output(out, rc)
}

#' Summarise GPS movement by epoch
#'
#' Summarises step-level movement within calendar or fixed-duration epochs.
#' Coordinates are assumed to be WGS84 longitude and latitude in decimal
#' degrees. Distances come from `gps_steps()`, which uses haversine distances on
#' a spherical earth. For epoch summaries, the first fix in each epoch has no
#' within-epoch step, so steps crossing an epoch boundary are not counted in
#' `total_distance_m` or speed summaries.
#'
#' @param data Data frame with GPS rows or output from `gps_steps()`.
#' @param epoch Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`,
#'   or `"interval"`.
#' @param epoch_mins Positive epoch duration in minutes. Supplying this uses
#'   fixed-duration `"interval"` epochs anchored to Unix time in UTC.
#' @inheritParams gps_steps
#'
#' @return Epoch summary table with movement metrics.
#' @export
gps_movement_summary <- function(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
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
    if (epoch == "interval") {
      stop("`epoch_mins` is required when `epoch = \"interval\"`.", call. = FALSE)
    }
  }

  movement_cols <- c("step_dt_s", "step_m", "speed_mps", "bearing_deg", "turn_rad")
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  step_grp <- grp
  if (!"segment_id" %in% step_grp && "segment_id" %in% names(dt)) {
    step_grp <- c(step_grp, "segment_id")
  }
  dt <- data.table::as.data.table(gps_steps(dt, groups = step_grp, verbose = FALSE, return_class = "data.table"))
  dt[, .grz_row_id := .I]
  data.table::setorderv(dt, c(step_grp, "datetime", ".grz_row_id"))

  epoch_dt <- grz_epoch_table(dt$datetime, epoch = epoch, epoch_mins = epoch_mins)
  dt[, `:=`(
    epoch = epoch_dt$epoch,
    epoch_start = epoch_dt$epoch_start,
    epoch_end = epoch_dt$epoch_end,
    epoch_mins = epoch_dt$epoch_mins
  )]

  by_cols <- c(grp, "epoch", "epoch_start", "epoch_end", "epoch_mins")
  dt[, .grz_epoch_row := seq_len(.N), by = by_cols]
  dt[, .grz_step_m_epoch := data.table::fifelse(.grz_epoch_row == 1L, NA_real_, step_m)]
  dt[, .grz_speed_mps_epoch := data.table::fifelse(.grz_epoch_row == 1L, NA_real_, speed_mps)]
  dt[, .grz_turn_rad_epoch := data.table::fifelse(.grz_epoch_row <= 2L, NA_real_, turn_rad)]

  out <- dt[, {
    total_distance_m <- sum(.grz_step_m_epoch, na.rm = TRUE)
    if (!any(is.finite(.grz_step_m_epoch))) {
      total_distance_m <- NA_real_
    }
    net_displacement_m <- if (.N >= 2L) {
      grz_haversine_m(lon[1L], lat[1L], lon[.N], lat[.N])
    } else {
      NA_real_
    }
    straightness_index <- if (is.finite(total_distance_m) && total_distance_m > 0) {
      net_displacement_m / total_distance_m
    } else {
      NA_real_
    }
    list(
      n_fixes = .N,
      n_steps = sum(is.finite(.grz_step_m_epoch)),
      start_datetime = min(datetime, na.rm = TRUE),
      end_datetime = max(datetime, na.rm = TRUE),
      span_s = as.numeric(max(datetime, na.rm = TRUE) - min(datetime, na.rm = TRUE), units = "secs"),
      total_distance_m = total_distance_m,
      net_displacement_m = net_displacement_m,
      mean_step_m = grz_mean_or_na(.grz_step_m_epoch),
      median_step_m = grz_quantile_or_na(.grz_step_m_epoch, 0.5),
      p95_step_m = grz_quantile_or_na(.grz_step_m_epoch, 0.95),
      mean_speed_mps = grz_mean_or_na(.grz_speed_mps_epoch),
      max_speed_mps = if (any(is.finite(.grz_speed_mps_epoch))) max(.grz_speed_mps_epoch, na.rm = TRUE) else NA_real_,
      p95_speed_mps = grz_quantile_or_na(.grz_speed_mps_epoch, 0.95),
      mean_abs_turn_rad = grz_mean_or_na(.grz_turn_rad_epoch),
      straightness_index = straightness_index
    )
  }, by = by_cols]

  metric_input_cols <- c(
    "datetime", "lon", "lat", movement_cols, "cum_distance_m",
    "net_displacement_m", "turn_deg", ".grz_row_id", ".grz_epoch_row",
    ".grz_step_m_epoch", ".grz_speed_mps_epoch", ".grz_turn_rad_epoch"
  )
  meta_cols <- setdiff(names(dt), unique(c(by_cols, metric_input_cols, setdiff(step_grp, grp))))
  meta_cols <- meta_cols[vapply(dt[, ..meta_cols], is.atomic, logical(1))]
  if (length(meta_cols) > 0L) {
    meta <- dt[, lapply(.SD, grz_constant_or_na), by = by_cols, .SDcols = meta_cols]
    out <- merge(meta, out, by = by_cols, all.y = TRUE, sort = FALSE)
  }

  data.table::setorderv(out, by_cols)
  data.table::setcolorder(
    out,
    c(
      grp,
      "epoch",
      "epoch_start",
      "epoch_end",
      "epoch_mins",
      meta_cols,
      setdiff(names(out), c(grp, "epoch", "epoch_start", "epoch_end", "epoch_mins", meta_cols))
    )
  )

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_movement_summary] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

