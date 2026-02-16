#' Sampling interval QC metrics
#'
#' @param data Data frame of GPS fixes.
#' @param target_mins Target sampling interval in minutes.
#' @param tol_prop Proportional tolerance around target interval.
#'
#' @return A `data.table` with sampling QC metrics per entity.
#' @export
grz_qc_sampling <- function(data, target_mins, tol_prop = 0.20) {
  if (!is.numeric(target_mins) || length(target_mins) != 1L || target_mins <= 0) {
    stop("`target_mins` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(tol_prop) || length(tol_prop) != 1L || tol_prop < 0) {
    stop("`tol_prop` must be a non-negative number.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_qc_sampling()")
  if (nrow(dt) == 0L) {
    return(data.table::data.table())
  }

  id_cols <- grz_id_cols(dt)
  data.table::setorderv(dt, c(id_cols, "datetime"))
  dt[, dt_min := as.numeric(datetime - shift(datetime), units = "mins"), by = id_cols]

  out <- dt[, {
    intv <- dt_min
    span_h <- as.numeric(max(datetime) - min(datetime), units = "hours")
    expected_n <- floor(span_h * 60 / target_mins) + 1
    .(
      n_fixes = .N,
      span_hours = span_h,
      expected_n = expected_n,
      completeness = if (expected_n > 0) min(1, .N / expected_n) else NA_real_,
      mean_interval_mins = grz_mean_or_na(intv),
      median_interval_mins = grz_quantile_or_na(intv, 0.5),
      sd_interval_mins = if (sum(is.finite(intv)) >= 2L) stats::sd(intv, na.rm = TRUE) else NA_real_,
      p05_interval_mins = grz_quantile_or_na(intv, 0.05),
      p95_interval_mins = grz_quantile_or_na(intv, 0.95),
      prop_within_target = grz_mean_or_na(abs(intv - target_mins) <= (target_mins * tol_prop)),
      n_large_gaps = sum(intv > (2 * target_mins), na.rm = TRUE)
    )
  }, by = id_cols]

  data.table::setorderv(out, id_cols)
  out[]
}

#' Combined QC report
#'
#' @param data Data frame of GPS fixes.
#' @param target_mins Optional target sampling interval in minutes.
#' @param max_step_gap_min Gap threshold in minutes for track QC.
#' @param speed_cap_ms Speed threshold in m/s for track QC.
#'
#' @return A list with validation, track QC, and optional sampling QC tables.
#' @export
grz_qc_report <- function(
  data,
  target_mins = NULL,
  max_step_gap_min = 45,
  speed_cap_ms = 2.0
) {
  if (!is.null(target_mins) && (!is.numeric(target_mins) || length(target_mins) != 1L || target_mins <= 0)) {
    stop("`target_mins` must be NULL or a positive number.", call. = FALSE)
  }

  val <- grz_validate_gps(data, drop_invalid = FALSE)
  clean <- val$data[is.finite(lon) & is.finite(lat) & !is.na(datetime) & trimws(sensor_id) != "", ]
  track <- grz_flag_track_qc(
    data = clean,
    max_step_gap_min = max_step_gap_min,
    speed_cap_ms = speed_cap_ms
  )

  id_cols <- grz_id_cols(track)
  track_summary <- track[, .(
    n_fixes = .N,
    n_qc_any = sum(qc_any, na.rm = TRUE),
    n_qc_gap = sum(qc_gap, na.rm = TRUE),
    n_qc_nonpositive_dt = sum(qc_nonpositive_dt, na.rm = TRUE),
    n_qc_speed_cap = sum(qc_speed_cap, na.rm = TRUE),
    prop_qc_any = grz_mean_or_na(qc_any)
  ), by = id_cols]

  sampling <- if (!is.null(target_mins)) {
    grz_qc_sampling(track, target_mins = target_mins)
  } else {
    NULL
  }

  overall <- data.table::data.table(
    n_input_rows = nrow(data),
    n_valid_rows = nrow(clean),
    n_invalid_rows = nrow(val$invalid_rows)
  )

  structure(
    list(
      overall = overall,
      validation = val$qc,
      invalid_rows = val$invalid_rows,
      track_summary = track_summary,
      sampling = sampling
    ),
    class = "grz_qc_report"
  )
}
