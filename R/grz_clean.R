#' Remove duplicate GPS fixes
#'
#' Removes duplicate rows using key columns. By default duplicates are detected
#' across all deployments. Set `across_deployments = FALSE` to treat
#' `deployment_id` as part of the duplicate key when available.
#'
#' @param data Data frame of GPS fixes.
#' @param keys Character vector of columns used to define duplicates.
#' @param across_deployments Logical; whether duplicate detection spans
#'   deployments.
#'
#' @return A `data.table` with duplicates removed. Attribute
#'   `duplicates_removed` contains the number of dropped rows.
#' @export
grz_remove_duplicates <- function(
  data,
  keys = c("sensor_id", "datetime", "lon", "lat"),
  across_deployments = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(keys) || length(keys) < 1L || any(is.na(keys)) || any(trimws(keys) == "")) {
    stop("`keys` must be a character vector of one or more column names.", call. = FALSE)
  }
  if (!is.logical(across_deployments) || length(across_deployments) != 1L) {
    stop("`across_deployments` must be TRUE or FALSE.", call. = FALSE)
  }

  dt <- data.table::copy(data.table::as.data.table(data))
  grz_require_cols(dt, keys, fun_name = "grz_remove_duplicates()")

  keys_use <- unique(keys)
  if (!isTRUE(across_deployments) && "deployment_id" %in% names(dt)) {
    keys_use <- unique(c("deployment_id", keys_use))
  }

  dup <- duplicated(dt, by = keys_use)
  out <- dt[!dup, ]
  attr(out, "duplicates_removed") <- sum(dup)
  attr(out, "duplicate_keys") <- keys_use
  out
}

#' Flag GPS track quality issues
#'
#' Adds per-fix step metrics and QC flags for large time gaps, non-positive time
#' deltas, and speed-cap exceedance.
#'
#' @param data Data frame of GPS fixes.
#' @param max_step_gap_min Gap threshold in minutes.
#' @param speed_cap_ms Speed threshold in m/s.
#'
#' @return A `data.table` containing original columns plus:
#'   `step_dt_s`, `step_m`, `speed_mps`, `qc_gap`, `qc_nonpositive_dt`,
#'   `qc_speed_cap`, and `qc_any`.
#' @export
grz_flag_track_qc <- function(
  data,
  max_step_gap_min = 45,
  speed_cap_ms = 2.0
) {
  if (!is.numeric(max_step_gap_min) || length(max_step_gap_min) != 1L || max_step_gap_min <= 0) {
    stop("`max_step_gap_min` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(speed_cap_ms) || length(speed_cap_ms) != 1L || speed_cap_ms <= 0) {
    stop("`speed_cap_ms` must be a positive number.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_flag_track_qc()")
  if (nrow(dt) == 0L) {
    dt[, `:=`(
      step_dt_s = numeric(),
      step_m = numeric(),
      speed_mps = numeric(),
      qc_gap = logical(),
      qc_nonpositive_dt = logical(),
      qc_speed_cap = logical(),
      qc_any = logical()
    )]
    return(dt)
  }

  id_cols <- grz_id_cols(dt)
  data.table::setorderv(dt, c(id_cols, "datetime"))

  dt[, `:=`(
    prev_datetime = shift(datetime),
    prev_lon = shift(lon),
    prev_lat = shift(lat)
  ), by = id_cols]

  dt[, step_dt_s := as.numeric(datetime - prev_datetime, units = "secs")]
  dt[, step_m := grz_haversine_m(prev_lon, prev_lat, lon, lat)]
  dt[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]

  dt[, qc_gap := !is.na(step_dt_s) & step_dt_s > (max_step_gap_min * 60)]
  dt[, qc_nonpositive_dt := !is.na(step_dt_s) & step_dt_s <= 0]
  dt[, qc_speed_cap := !is.na(speed_mps) & speed_mps > speed_cap_ms]
  dt[, qc_any := qc_gap | qc_nonpositive_dt | qc_speed_cap]

  dt[, c("prev_datetime", "prev_lon", "prev_lat") := NULL]
  dt
}
