grz_resolve_interval_mins <- function(data, groups, interval_mins = "base") {
  if (is.character(interval_mins)) {
    if (length(interval_mins) != 1L || interval_mins != "base") {
      stop("`interval_mins` must be numeric or \"base\".", call. = FALSE)
    }
    tmp <- data.table::copy(data)
    data.table::setorderv(tmp, c(groups, "datetime"))
    tmp[, .grz_diff_min := as.numeric(datetime - shift(datetime), units = "mins"), by = groups]
    return(grz_round_to_base_min(tmp$.grz_diff_min))
  }

  if (!is.numeric(interval_mins) || length(interval_mins) != 1L || !is.finite(interval_mins) || interval_mins <= 0) {
    stop("`interval_mins` must be a positive number or \"base\".", call. = FALSE)
  }
  as.integer(max(1L, round(interval_mins)))
}

grz_align_group <- function(sub, interval_sec, keep_extra) {
  sub <- data.table::copy(sub)
  data.table::setorderv(sub, "datetime")
  sub <- unique(sub, by = "datetime")

  t_obs <- as.numeric(sub$datetime)
  t_grid <- seq(min(t_obs), max(t_obs), by = interval_sec)
  dt_grid <- data.table::data.table(
    datetime = as.POSIXct(t_grid, origin = "1970-01-01", tz = "UTC")
  )

  dt_grid[, lon := stats::approx(x = t_obs, y = sub$lon, xout = t_grid, method = "linear", rule = 2)$y]
  dt_grid[, lat := stats::approx(x = t_obs, y = sub$lat, xout = t_grid, method = "linear", rule = 2)$y]

  obs_times <- sub$datetime
  dt_grid[, aligned_from_observation := datetime %in% obs_times]

  if (isTRUE(keep_extra)) {
    extra_cols <- setdiff(names(sub), c("datetime", "lon", "lat"))
    if (length(extra_cols) > 0L) {
      dt_grid <- merge(
        dt_grid,
        sub[, c("datetime", extra_cols), with = FALSE],
        by = "datetime",
        all.x = TRUE,
        sort = TRUE
      )
    }
  }
  dt_grid[]
}

#' Align/interpolate to a regular interval
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param interval_mins Target interval in minutes, or `"base"` to infer from
#'   median observed interval (rounded to nearest minute).
#' @param groups Grouping columns for independent alignment.
#' @param keep_extra Logical; carry extra columns where exact timestamps exist.
#' @param verbose Logical; print alignment summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Aligned data with `aligned_from_observation` flag.
#' @export
grz_align <- function(
  data,
  interval_mins = "base",
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)

  interval_resolved <- grz_resolve_interval_mins(dt, groups = grp, interval_mins = interval_mins)
  interval_sec <- as.integer(interval_resolved * 60)

  split_idx <- split(seq_len(nrow(dt)), interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE))
  out <- data.table::rbindlist(
    lapply(split_idx, function(i) {
      sub <- dt[i, c(grp, "datetime", "lon", "lat", setdiff(names(dt), c(grp, "datetime", "lon", "lat"))), with = FALSE]
      aligned <- grz_align_group(sub, interval_sec = interval_sec, keep_extra = keep_extra)
      for (g in grp) {
        aligned[[g]] <- sub[[g]][1L]
      }
      aligned
    }),
    use.names = TRUE,
    fill = TRUE
  )

  data.table::setcolorder(out, c(grp, setdiff(names(out), grp)))
  data.table::setorderv(out, c(grp, "datetime"))

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[align] interval_mins=%s rows=%s -> %s\n",
        format(interval_resolved, trim = TRUE),
        format(nrow(dt), big.mark = ","),
        format(nrow(out), big.mark = ",")
      )
    )
  }

  grz_as_output(out, rc)
}

#' Downsample to lower frequency
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param target_mins Target interval in minutes.
#' @param method Downsample mode: `"rigid"` (first in window) or
#'   `"representative"` (closest to window center).
#' @param groups Grouping columns for independent downsampling.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Downsampled data.
#' @export
grz_downsample <- function(
  data,
  target_mins,
  method = c("representative", "rigid"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  if (!is.numeric(target_mins) || length(target_mins) != 1L || !is.finite(target_mins) || target_mins <= 0) {
    stop("`target_mins` must be a positive number.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  target_sec <- as.integer(round(target_mins * 60))

  dt[, .grz_win := floor(as.numeric(datetime) / target_sec)]

  if (method == "rigid") {
    data.table::setorderv(dt, c(grp, ".grz_win", "datetime"))
    out <- dt[, .SD[1], by = c(grp, ".grz_win")]
  } else {
    dt[, .grz_center := (.grz_win * target_sec) + (target_sec / 2)]
    dt[, .grz_abs_center := abs(as.numeric(datetime) - .grz_center)]
    data.table::setorderv(dt, c(grp, ".grz_win", ".grz_abs_center", "datetime"))
    out <- dt[, .SD[1], by = c(grp, ".grz_win")]
    out[, c(".grz_center", ".grz_abs_center") := NULL]
  }

  out[, .grz_win := NULL]
  data.table::setorderv(out, c(grp, "datetime"))

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[downsample] method=%s target_mins=%s rows=%s -> %s\n",
        method,
        format(target_mins, trim = TRUE),
        format(nrow(dt), big.mark = ","),
        format(nrow(out), big.mark = ",")
      )
    )
  }

  grz_as_output(out, rc)
}
