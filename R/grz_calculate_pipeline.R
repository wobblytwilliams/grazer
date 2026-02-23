grz_epoch_label <- function(datetime, epoch = c("day", "week", "month")) {
  epoch <- match.arg(epoch)
  if (epoch == "day") {
    return(as.character(as.Date(datetime, tz = "UTC")))
  }
  if (epoch == "week") {
    return(strftime(datetime, format = "%G-W%V", tz = "UTC"))
  }
  strftime(datetime, format = "%Y-%m", tz = "UTC")
}

#' Calculate row-level movement metrics
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param groups Grouping columns for movement calculations.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Data with appended row-level movement fields.
#' @export
grz_calculate_movement <- function(
  data,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  data.table::setorderv(dt, c(grp, "datetime"))

  dt[, `:=`(
    prev_datetime = shift(datetime),
    prev_lon = shift(lon),
    prev_lat = shift(lat)
  ), by = grp]

  dt[, step_dt_s := as.numeric(datetime - prev_datetime, units = "secs")]
  dt[, step_m := grz_haversine_m(prev_lon, prev_lat, lon, lat)]
  dt[, speed_mps := data.table::fifelse(step_dt_s > 0, step_m / step_dt_s, NA_real_)]
  dt[, bearing_deg := grz_bearing_deg(prev_lon, prev_lat, lon, lat)]
  dt[, turn_rad := grz_abs_turn_rad(bearing_deg, shift(bearing_deg)), by = grp]
  dt[, cum_distance_m := cumsum(data.table::fifelse(is.na(step_m), 0, step_m)), by = grp]
  dt[, net_displacement_m := grz_haversine_m(lon[1L], lat[1L], lon, lat), by = grp]

  dt[, c("prev_datetime", "prev_lon", "prev_lat") := NULL]

  if (isTRUE(verbose)) {
    cat(sprintf("[calculate_movement] rows=%s groups=%s\n", format(nrow(dt), big.mark = ","), format(data.table::uniqueN(dt[, ..grp]), big.mark = ",")))
  }
  grz_as_output(dt, rc)
}

#' Calculate row-level social metrics
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, `lat`.
#' @param thresholds_m Distance thresholds for neighbour counts.
#' @param align_interval_mins Alignment interval in minutes or `"base"`.
#' @param interpolate Logical; align/interpolate before social calculations.
#' @param herd_groups Herd partition columns for pairwise calculations. Defaults
#'   to available `deployment_id` and `paddock`.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Data with appended row-level social fields.
#' @export
grz_calculate_social <- function(
  data,
  thresholds_m = c(25, 30, 50, 100),
  align_interval_mins = "base",
  interpolate = TRUE,
  herd_groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  if (!is.numeric(thresholds_m) || length(thresholds_m) < 1L || any(!is.finite(thresholds_m)) || any(thresholds_m <= 0)) {
    stop("`thresholds_m` must be positive numeric values.", call. = FALSE)
  }
  if (!is.logical(interpolate) || length(interpolate) != 1L) {
    stop("`interpolate` must be TRUE or FALSE.", call. = FALSE)
  }

  rc <- grz_match_output_class(return_class)
  thresholds_m <- sort(unique(as.numeric(thresholds_m)))

  dt <- if (isTRUE(interpolate)) {
    data.table::as.data.table(
      grz_align(
        data,
        interval_mins = align_interval_mins,
        keep_extra = TRUE,
        verbose = verbose,
        return_class = "data.table"
      )
    )
  } else {
    grz_prepare_clean_dt(data, require_core = TRUE)
  }

  dt[, .grz_row_id := .I]
  if (is.null(herd_groups)) {
    herd_groups <- intersect(c("deployment_id", "paddock"), names(dt))
  } else {
    grz_require_cols(dt, herd_groups, fun_name = "grz_calculate_social()")
  }

  split_key_cols <- c(herd_groups, "datetime")
  if (length(split_key_cols) == 1L) {
    split_idx <- split(seq_len(nrow(dt)), dt$datetime)
  } else {
    split_idx <- split(
      seq_len(nrow(dt)),
      interaction(dt[, ..split_key_cols], drop = TRUE, lex.order = TRUE)
    )
  }

  metric_list <- vector("list", length(split_idx))
  for (i in seq_along(split_idx)) {
    sub <- dt[split_idx[[i]], ]
    n <- nrow(sub)
    res <- data.table::data.table(.grz_row_id = sub$.grz_row_id)
    res[, social_group_size := n]

    if (n <= 1L) {
      res[, nearest_neighbor_m := NA_real_]
      res[, mean_dist_to_others_m := NA_real_]
      for (thr in thresholds_m) {
        lbl <- grz_threshold_label(thr)
        res[, (paste0("n_within_", lbl, "m")) := 0L]
        res[, (paste0("any_within_", lbl, "m")) := FALSE]
      }
      metric_list[[i]] <- res
      next
    }

    dmat <- grz_pairwise_haversine_matrix(sub$lon, sub$lat)
    diag(dmat) <- Inf
    nn <- apply(dmat, 1L, min, na.rm = TRUE)
    dmean <- rowMeans(replace(dmat, is.infinite(dmat), NA_real_), na.rm = TRUE)
    dmean[!is.finite(dmean)] <- NA_real_

    res[, nearest_neighbor_m := nn]
    res[, mean_dist_to_others_m := dmean]

    for (thr in thresholds_m) {
      lbl <- grz_threshold_label(thr)
      cnt <- rowSums(dmat <= thr, na.rm = TRUE)
      res[, (paste0("n_within_", lbl, "m")) := as.integer(cnt)]
      res[, (paste0("any_within_", lbl, "m")) := cnt > 0]
    }
    metric_list[[i]] <- res
  }

  metrics <- data.table::rbindlist(metric_list, use.names = TRUE, fill = TRUE)
  out <- merge(dt, metrics, by = ".grz_row_id", all.x = TRUE, sort = FALSE)
  out[, .grz_row_id := NULL]
  data.table::setorderv(out, c(intersect(c("deployment_id", "sensor_id"), names(out)), "datetime"))

  if (isTRUE(verbose)) {
    cat(sprintf("[calculate_social] rows=%s thresholds=%s\n", format(nrow(out), big.mark = ","), paste(thresholds_m, collapse = ",")))
  }
  grz_as_output(out, rc)
}

#' Summarise row-level movement metrics by epoch
#'
#' @param data Data frame (preferably with movement columns).
#' @param epoch Epoch level: `"day"` (default), `"week"`, `"month"`.
#' @param groups Grouping columns for summaries.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Epoch summary table.
#' @export
grz_summarise_movement <- function(
  data,
  epoch = c("day", "week", "month"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch <- match.arg(epoch)

  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  if (!all(c("step_m", "speed_mps", "turn_rad") %in% names(dt))) {
    dt <- data.table::as.data.table(grz_calculate_movement(dt, groups = groups, verbose = FALSE, return_class = "data.table"))
  }

  grp <- grz_default_group_cols(dt, groups = groups)
  dt[, epoch := grz_epoch_label(datetime, epoch = epoch)]

  out <- dt[, .(
    n_fixes = .N,
    total_distance_m = sum(step_m, na.rm = TRUE),
    mean_step_m = grz_mean_or_na(step_m),
    median_step_m = grz_quantile_or_na(step_m, 0.5),
    p95_step_m = grz_quantile_or_na(step_m, 0.95),
    mean_speed_mps = grz_mean_or_na(speed_mps),
    p95_speed_mps = grz_quantile_or_na(speed_mps, 0.95),
    max_speed_mps = if (any(is.finite(speed_mps))) max(speed_mps, na.rm = TRUE) else NA_real_,
    mean_abs_turn_rad = grz_mean_or_na(turn_rad)
  ), by = c(grp, "epoch")]

  if (isTRUE(verbose)) {
    cat(sprintf("[summarise_movement] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Summarise row-level social metrics by epoch
#'
#' @param data Data frame (preferably with social columns).
#' @param epoch Epoch level: `"day"` (default), `"week"`, `"month"`.
#' @param groups Grouping columns for summaries.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Epoch summary table.
#' @export
grz_summarise_social <- function(
  data,
  epoch = c("day", "week", "month"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch <- match.arg(epoch)
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)

  social_cols <- grep("^(nearest_neighbor_m|mean_dist_to_others_m|n_within_|any_within_)", names(dt), value = TRUE)
  if (length(social_cols) == 0L) {
    dt <- data.table::as.data.table(grz_calculate_social(dt, verbose = FALSE, return_class = "data.table"))
    social_cols <- grep("^(nearest_neighbor_m|mean_dist_to_others_m|n_within_|any_within_)", names(dt), value = TRUE)
  }

  grp <- grz_default_group_cols(dt, groups = groups)
  dt[, epoch := grz_epoch_label(datetime, epoch = epoch)]

  out <- dt[, {
    ans <- list(
      n_fixes = .N,
      mean_nn_m = grz_mean_or_na(nearest_neighbor_m),
      p50_nn_m = grz_quantile_or_na(nearest_neighbor_m, 0.5),
      mean_dist_to_others_m = grz_mean_or_na(mean_dist_to_others_m)
    )

    n_cols <- grep("^n_within_", names(.SD), value = TRUE)
    for (nc in n_cols) {
      ans[[paste0("mean_", nc)]] <- grz_mean_or_na(get(nc))
    }
    a_cols <- grep("^any_within_", names(.SD), value = TRUE)
    for (ac in a_cols) {
      ans[[paste0("prop_", ac)]] <- grz_mean_or_na(as.numeric(get(ac)))
    }
    ans
  }, by = c(grp, "epoch")]

  if (isTRUE(verbose)) {
    cat(sprintf("[summarise_social] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Calculate spatial/home-range metrics by epoch
#'
#' @param data Data frame with GPS rows.
#' @param epoch Epoch level: `"day"` (default), `"week"`, `"month"`.
#' @param groups Grouping columns for summaries.
#' @param min_fixes Minimum fixes to compute metrics.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Spatial summary table.
#' @export
grz_calculate_spatial <- function(
  data,
  epoch = c("day", "week", "month"),
  groups = NULL,
  min_fixes = 5,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch <- match.arg(epoch)
  if (!is.numeric(min_fixes) || length(min_fixes) != 1L || min_fixes < 3) {
    stop("`min_fixes` must be >= 3.", call. = FALSE)
  }

  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  dt[, epoch := grz_epoch_label(datetime, epoch = epoch)]

  out <- dt[, {
    span_h <- as.numeric(max(datetime) - min(datetime), units = "hours")
    n <- .N
    .(
      n_fixes = n,
      span_hours = span_h,
      mcp100_area_ha = if (n >= min_fixes) grz_convex_hull_area_ha(lon, lat) else NA_real_,
      mcp95_area_ha = if (n >= min_fixes) grz_mcp95_area_ha(lon, lat) else NA_real_,
      kde95_ha = if (n >= min_fixes) grz_ellipse95_area_ha(lon, lat) else NA_real_
    )
  }, by = c(grp, "epoch")]

  if (isTRUE(verbose)) {
    cat(sprintf("[calculate_spatial] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Calculate merged epoch metrics
#'
#' Wrapper that merges movement/social/spatial epoch summaries. Daily epoch is
#' the default.
#'
#' @param data Data frame with GPS rows.
#' @param epoch Epoch level: `"day"` (default), `"week"`, `"month"`.
#' @param include Which metric blocks to include.
#' @param groups Grouping columns for summaries.
#' @param verbose Logical; print summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Merged epoch metrics table.
#' @export
grz_calculate_epoch_metrics <- function(
  data,
  epoch = c("day", "week", "month"),
  include = c("movement", "social", "spatial"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch <- match.arg(epoch)

  allowed <- c("movement", "social", "spatial")
  include <- unique(include)
  bad <- setdiff(include, allowed)
  if (length(bad) > 0L) {
    stop("Unknown include values: ", paste(bad, collapse = ", "), call. = FALSE)
  }

  pieces <- list()
  if ("movement" %in% include) {
    pieces$movement <- data.table::as.data.table(
      grz_summarise_movement(data, epoch = epoch, groups = groups, verbose = FALSE, return_class = "data.table")
    )
  }
  if ("social" %in% include) {
    pieces$social <- data.table::as.data.table(
      grz_summarise_social(data, epoch = epoch, groups = groups, verbose = FALSE, return_class = "data.table")
    )
  }
  if ("spatial" %in% include) {
    pieces$spatial <- data.table::as.data.table(
      grz_calculate_spatial(data, epoch = epoch, groups = groups, verbose = FALSE, return_class = "data.table")
    )
  }

  out <- grz_merge_metric_tables(pieces)
  if (isTRUE(verbose)) {
    cat(sprintf("[calculate_epoch_metrics] epoch=%s include=%s rows=%s\n", epoch, paste(include, collapse = ","), format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}
