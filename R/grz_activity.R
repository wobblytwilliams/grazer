#' Activity metrics
#'
#' Computes speed-derived activity summaries and a first-pass routine
#' consistency index from daily hourly activity profiles.
#'
#' @param data Data frame of GPS fixes.
#' @param rest_speed_thresh_ms Speed threshold (m/s) below which fixes are
#'   treated as rest.
#' @param tz_local Timezone used for diurnal summaries.
#'
#' @return A `data.table` of activity metrics by entity.
#' @export
grz_activity_metrics <- function(
  data,
  rest_speed_thresh_ms = 0.05,
  tz_local = "UTC"
) {
  if (!is.numeric(rest_speed_thresh_ms) || length(rest_speed_thresh_ms) != 1L || rest_speed_thresh_ms < 0) {
    stop("`rest_speed_thresh_ms` must be a non-negative number.", call. = FALSE)
  }
  if (!is.character(tz_local) || length(tz_local) != 1L || is.na(tz_local) || trimws(tz_local) == "") {
    stop("`tz_local` must be a single timezone string.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_activity_metrics()")
  if (nrow(dt) == 0L) {
    return(data.table::data.table())
  }

  id_cols <- grz_id_cols(dt)
  data.table::setorderv(dt, c(id_cols, "datetime"))
  dt[, `:=`(
    prev_time = shift(datetime),
    prev_lon = shift(lon),
    prev_lat = shift(lat)
  ), by = id_cols]

  dt[, step_dt_s := as.numeric(datetime - prev_time, units = "secs")]
  dt[, step_m := grz_haversine_m(prev_lon, prev_lat, lon, lat)]
  dt[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]
  dt[, is_active := speed_mps > rest_speed_thresh_ms]
  dt[, is_rest := !is.na(speed_mps) & speed_mps <= rest_speed_thresh_ms]
  dt[, hour_local := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, date_local := as.Date(datetime, tz = tz_local)]
  dt[, is_day := hour_local >= 6L & hour_local < 18L]

  activity <- dt[, {
    spd <- speed_mps
    active <- is_active
    rest <- is_rest
    .(
      n_fixes = .N,
      prop_active = grz_mean_or_na(active),
      prop_rest = grz_mean_or_na(rest),
      speed_mean_ms = grz_mean_or_na(spd),
      speed_sd_ms = if (sum(is.finite(spd)) >= 2L) stats::sd(spd, na.rm = TRUE) else NA_real_,
      day_speed_mean_ms = grz_mean_or_na(spd[is_day]),
      night_speed_mean_ms = grz_mean_or_na(spd[!is_day]),
      day_prop_active = grz_mean_or_na(active[is_day]),
      night_prop_active = grz_mean_or_na(active[!is_day])
    )
  }, by = id_cols]

  daily_hour <- dt[, .(
    active_mean = grz_mean_or_na(is_active),
    speed_mean = grz_mean_or_na(speed_mps)
  ), by = c(id_cols, "date_local", "hour_local")]

  routine <- daily_hour[, {
    if (data.table::uniqueN(date_local) < 2L) {
      .(routine_consistency_index = NA_real_)
    } else {
      wide <- data.table::dcast(
        .SD,
        date_local ~ hour_local,
        value.var = "active_mean",
        fill = NA_real_
      )
      mat <- as.matrix(wide[, setdiff(names(wide), "date_local"), with = FALSE])
      if (nrow(mat) < 2L) {
        .(routine_consistency_index = NA_real_)
      } else {
        cmat <- stats::cor(t(mat), use = "pairwise.complete.obs")
        if (nrow(cmat) < 2L) {
          .(routine_consistency_index = NA_real_)
        } else {
          .(routine_consistency_index = mean(cmat[lower.tri(cmat)], na.rm = TRUE))
        }
      }
    }
  }, by = id_cols]

  out <- merge(activity, routine, by = id_cols, all.x = TRUE, sort = FALSE)
  if ("in_core" %in% names(dt)) {
    core <- dt[, .(prop_in_core = grz_mean_or_na(as.numeric(in_core))), by = id_cols]
    out <- merge(out, core, by = id_cols, all.x = TRUE, sort = FALSE)
  }

  data.table::setorderv(out, id_cols)
  out[]
}
