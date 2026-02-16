#' Home-range metrics by period
#'
#' Computes MCP100, MCP95, and a first-draft KDE95 proxy (95% covariance
#' ellipse area) by selected period groupings.
#'
#' @param data Data frame of GPS fixes.
#' @param by_period Period grouping choices: `"date"`, `"week"`,
#'   `"paddock_week"`, or a vector of these values.
#' @param min_fixes Minimum fixes for QC pass.
#' @param min_span_hours Minimum time span (hours) for QC pass.
#'
#' @return A `data.table` of home-range metrics.
#' @export
grz_home_range_metrics <- function(
  data,
  by_period = c("date", "week", "paddock_week"),
  min_fixes = 10,
  min_span_hours = 6
) {
  valid_periods <- c("date", "week", "paddock_week")
  if (!is.character(by_period) || length(by_period) < 1L) {
    stop("`by_period` must be a non-empty character vector.", call. = FALSE)
  }
  bad <- setdiff(by_period, valid_periods)
  if (length(bad) > 0L) {
    stop("Unsupported `by_period` values: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  by_period <- unique(by_period)

  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || min_fixes < 1) {
    stop("`min_fixes` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(min_span_hours) || length(min_span_hours) != 1L || min_span_hours < 0) {
    stop("`min_span_hours` must be a non-negative number.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_home_range_metrics()")
  if (nrow(dt) == 0L) {
    return(data.table::data.table())
  }

  id_cols <- grz_id_cols(dt)
  out <- list()

  for (period_name in by_period) {
    tmp <- data.table::copy(dt)
    tmp[, period_type := period_name]

    if (identical(period_name, "paddock_week")) {
      if (!"paddock" %in% names(tmp)) {
        warning("Skipping `paddock_week` because `paddock` column is not present.", call. = FALSE)
        next
      }
      tmp[, week_label := grz_period_label(datetime, "week")]
      tmp[, paddock := as.character(paddock)]
      tmp[, period := paste0(paddock, "::", week_label)]
      group_cols <- c(id_cols, "paddock", "period_type", "period")
    } else {
      tmp[, period := grz_period_label(datetime, period_name)]
      group_cols <- c(id_cols, "period_type", "period")
    }

    hr <- tmp[, {
      span_h <- as.numeric(max(datetime) - min(datetime), units = "hours")
      mcp100 <- grz_convex_hull_area_ha(lon, lat)
      mcp95 <- grz_mcp95_area_ha(lon, lat)
      kde95 <- grz_ellipse95_area_ha(lon, lat)
      .(
        n_fixes = .N,
        span_hours = span_h,
        qc_pass = (.N >= min_fixes) & (span_h >= min_span_hours),
        mcp100_area_ha = mcp100,
        mcp95_area_ha = mcp95,
        kde95_ha = kde95
      )
    }, by = group_cols]

    out[[length(out) + 1L]] <- hr
  }

  if (length(out) == 0L) {
    return(data.table::data.table())
  }

  ans <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  data.table::setorderv(ans, c(id_cols, "period_type", "period"))
  ans[]
}
