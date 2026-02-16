grz_quantile_or_na <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = p, names = FALSE, type = 7))
}

grz_mean_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  mean(x)
}

#' Movement metrics by animal and period
#'
#' @param data Data frame of GPS fixes.
#' @param by_period Period grouping: `"total"`, `"week"`, `"date"`, or a vector
#'   of these values.
#'
#' @return A `data.table` of movement metrics by entity and period.
#' @export
grz_movement_metrics <- function(data, by_period = c("total", "week", "date")) {
  valid_periods <- c("total", "week", "date")
  if (!is.character(by_period) || length(by_period) < 1L) {
    stop("`by_period` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(by_period, valid_periods)
  if (length(bad) > 0L) {
    stop("Unsupported `by_period` values: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  by_period <- unique(by_period)

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_movement_metrics()")
  if (nrow(dt) == 0L) {
    return(data.table::data.table())
  }

  id_cols <- grz_id_cols(dt)
  out <- vector("list", length(by_period))

  for (i in seq_along(by_period)) {
    period_name <- by_period[[i]]
    tmp <- data.table::copy(dt)
    tmp[, period_type := period_name]
    tmp[, period := grz_period_label(datetime, period_name)]

    data.table::setorderv(tmp, c(id_cols, "period", "datetime"))
    tmp[, `:=`(
      prev_time = shift(datetime),
      prev_lon = shift(lon),
      prev_lat = shift(lat)
    ), by = c(id_cols, "period")]

    tmp[, step_dt_s := as.numeric(datetime - prev_time, units = "secs")]
    tmp[, step_m := grz_haversine_m(prev_lon, prev_lat, lon, lat)]
    tmp[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]
    tmp[, bearing_deg := grz_bearing_deg(prev_lon, prev_lat, lon, lat)]
    tmp[, turn_rad := grz_abs_turn_rad(bearing_deg, shift(bearing_deg)), by = c(id_cols, "period")]

    summary <- tmp[, {
      step <- step_m
      speed <- speed_mps
      turn <- turn_rad

      total_distance <- sum(step, na.rm = TRUE)
      net_disp <- grz_haversine_m(
        lon1 = lon[1L],
        lat1 = lat[1L],
        lon2 = lon[.N],
        lat2 = lat[.N]
      )

      .(
        n_fixes = .N,
        total_distance_m = total_distance,
        mean_step_m = grz_mean_or_na(step),
        median_step_m = grz_quantile_or_na(step, 0.5),
        sd_step_m = if (sum(is.finite(step)) >= 2L) stats::sd(step, na.rm = TRUE) else NA_real_,
        p95_step_m = grz_quantile_or_na(step, 0.95),
        mean_speed_mps = grz_mean_or_na(speed),
        p95_speed_mps = grz_quantile_or_na(speed, 0.95),
        p99_speed_mps = grz_quantile_or_na(speed, 0.99),
        max_speed_mps = if (any(is.finite(speed))) max(speed, na.rm = TRUE) else NA_real_,
        pct_steps_gt_0_8_mps = 100 * grz_mean_or_na(speed > 0.8),
        stationary_prop_step_lt10m = grz_mean_or_na(step < 10),
        mean_abs_turn_rad = grz_mean_or_na(turn),
        turn_sd_rad = if (sum(is.finite(turn)) >= 2L) stats::sd(turn, na.rm = TRUE) else NA_real_,
        net_displacement_m = net_disp,
        straightness_index = if (is.finite(total_distance) && total_distance > 0) net_disp / total_distance else NA_real_
      )
    }, by = c(id_cols, "period_type", "period")]

    out[[i]] <- summary
  }

  ans <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  data.table::setorderv(ans, c(id_cols, "period_type", "period"))
  ans[]
}
