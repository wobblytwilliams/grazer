grz_threshold_label <- function(x) {
  gsub("\\.", "_", format(x, scientific = FALSE, trim = TRUE))
}

#' Social spacing and association metrics
#'
#' @param data Data frame of GPS fixes.
#' @param thresholds_m Distance thresholds (meters) for neighbour counts.
#' @param interpolate Logical; if `TRUE`, timestamps are rounded to nearest
#'   minute before synchronisation.
#'
#' @return A `data.table` of per-entity social metrics.
#' @export
grz_social_metrics <- function(
  data,
  thresholds_m = c(25, 30, 50, 100),
  interpolate = TRUE
) {
  if (!is.numeric(thresholds_m) || length(thresholds_m) < 1L || any(!is.finite(thresholds_m)) || any(thresholds_m <= 0)) {
    stop("`thresholds_m` must be a numeric vector of positive thresholds.", call. = FALSE)
  }
  if (!is.logical(interpolate) || length(interpolate) != 1L) {
    stop("`interpolate` must be TRUE or FALSE.", call. = FALSE)
  }

  thresholds_m <- sort(unique(as.numeric(thresholds_m)))
  dt <- grz_prepare_gps_dt(data, fun_name = "grz_social_metrics()")
  if (nrow(dt) == 0L) {
    return(data.table::data.table())
  }

  id_cols <- grz_id_cols(dt)
  herd_cols <- c(intersect("deployment_id", names(dt)), "time_sync")

  if (isTRUE(interpolate)) {
    dt[, time_sync := as.POSIXct(
      round(as.numeric(datetime) / 60) * 60,
      origin = "1970-01-01",
      tz = "UTC"
    )]
  } else {
    dt[, time_sync := datetime]
  }

  dt[, interp_shift_s := abs(as.numeric(datetime - time_sync, units = "secs"))]
  data.table::setorderv(dt, c(id_cols, "time_sync", "interp_shift_s", "datetime"))
  dt <- dt[, .SD[1], by = c(id_cols, "time_sync")]

  diag_tbl <- dt[, .(
    prop_interp_used = if (isTRUE(interpolate)) mean(interp_shift_s > 0, na.rm = TRUE) else 0,
    n_grid_points = data.table::uniqueN(time_sync)
  ), by = id_cols]

  group_sizes <- dt[, .(n_animals = data.table::uniqueN(sensor_id)), by = herd_cols]
  valid_groups <- group_sizes[n_animals >= 2, ]

  if (nrow(valid_groups) == 0L) {
    ans <- data.table::copy(diag_tbl)
    ans[, `:=`(
      mean_nn_m = NA_real_,
      p50_nn_m = NA_real_,
      mean_dist_to_others_m = NA_real_,
      CR30 = NA_real_,
      mean_sri30 = NA_real_,
      top5_mean_sri30 = NA_real_,
      max_sri30 = NA_real_
    )]
    for (thr in thresholds_m) {
      lbl <- grz_threshold_label(thr)
      ans[, (paste0("mean_n_within_", lbl, "m")) := NA_real_]
      ans[, (paste0("prop_time_any_within_", lbl, "m")) := NA_real_]
    }
    return(ans[])
  }

  dt_pairs <- merge(dt, valid_groups[, ..herd_cols], by = herd_cols, all = FALSE, sort = FALSE)
  dt_pairs[, .grz_group := .GRP, by = herd_cols]
  grp_idx <- split(seq_len(nrow(dt_pairs)), dt_pairs$.grz_group)

  per_fix_list <- vector("list", length(grp_idx))
  pair_list <- vector("list", length(grp_idx))

  for (i in seq_along(grp_idx)) {
    g <- dt_pairs[grp_idx[[i]], ]
    n <- nrow(g)
    if (n < 2L) {
      next
    }

    dmat <- grz_pairwise_haversine_matrix(g$lon, g$lat)
    diag(dmat) <- Inf
    mean_others <- rowSums(replace(dmat, !is.finite(dmat), NA_real_), na.rm = TRUE) / (n - 1L)
    nn <- apply(dmat, 1L, min, na.rm = TRUE)

    fix_tbl <- data.table::copy(g[, c(id_cols, "time_sync"), with = FALSE])
    fix_tbl[, mean_nn_m := nn]
    fix_tbl[, p50_nn_m := nn]
    fix_tbl[, mean_dist_to_others_m := mean_others]

    for (thr in thresholds_m) {
      lbl <- grz_threshold_label(thr)
      counts <- rowSums(dmat <= thr, na.rm = TRUE)
      fix_tbl[, (paste0("n_within_", lbl, "m")) := counts]
    }
    per_fix_list[[i]] <- fix_tbl

    if (n >= 2L) {
      cmb <- utils::combn(n, 2L)
      pair_tbl <- data.table::data.table(
        sensor_a = g$sensor_id[cmb[1L, ]],
        sensor_b = g$sensor_id[cmb[2L, ]],
        within_30 = dmat[cbind(cmb[1L, ], cmb[2L, ])] <= 30
      )
      if ("deployment_id" %in% names(g)) {
        pair_tbl[, deployment_id := g$deployment_id[1L]]
      }
      pair_tbl[, overlap := 1L]
      pair_list[[i]] <- pair_tbl
    }
  }

  fix_metrics <- data.table::rbindlist(per_fix_list, use.names = TRUE, fill = TRUE)
  if (nrow(fix_metrics) == 0L) {
    ans <- merge(diag_tbl, dt[, .(sensor_id = unique(sensor_id)), by = id_cols], by = id_cols, all = TRUE)
    return(ans[])
  }

  metric_cols <- setdiff(
    names(fix_metrics),
    c(id_cols, "time_sync", "p50_nn_m")
  )

  social <- fix_metrics[, {
    out <- list(
      mean_nn_m = grz_mean_or_na(mean_nn_m),
      p50_nn_m = grz_quantile_or_na(mean_nn_m, 0.5),
      mean_dist_to_others_m = grz_mean_or_na(mean_dist_to_others_m)
    )
    for (thr in thresholds_m) {
      lbl <- grz_threshold_label(thr)
      col <- paste0("n_within_", lbl, "m")
      out[[paste0("mean_n_within_", lbl, "m")]] <- grz_mean_or_na(get(col))
      out[[paste0("prop_time_any_within_", lbl, "m")]] <- grz_mean_or_na(get(col) > 0)
    }
    out
  }, by = id_cols]

  pair_metrics <- data.table::rbindlist(pair_list, use.names = TRUE, fill = TRUE)
  if (nrow(pair_metrics) > 0L) {
    pair_key <- c(intersect("deployment_id", names(pair_metrics)), "sensor_a", "sensor_b")
    pair_sri <- pair_metrics[, .(
      overlap = sum(overlap, na.rm = TRUE),
      within_30 = sum(within_30, na.rm = TRUE)
    ), by = pair_key]
    pair_sri[, sri30 := data.table::fifelse(overlap > 0, within_30 / overlap, NA_real_)]

    if ("deployment_id" %in% names(pair_sri)) {
      pair_long <- data.table::rbindlist(list(
        pair_sri[, .(deployment_id, sensor_id = sensor_a, partner = sensor_b, sri30)],
        pair_sri[, .(deployment_id, sensor_id = sensor_b, partner = sensor_a, sri30)]
      ))
      sri_by_sensor <- pair_long[, .(
        mean_sri30 = grz_mean_or_na(sri30),
        top5_mean_sri30 = grz_mean_or_na(head(sort(sri30, decreasing = TRUE), 5L)),
        max_sri30 = if (any(is.finite(sri30))) max(sri30, na.rm = TRUE) else NA_real_
      ), by = .(deployment_id, sensor_id)]
    } else {
      pair_long <- data.table::rbindlist(list(
        pair_sri[, .(sensor_id = sensor_a, partner = sensor_b, sri30)],
        pair_sri[, .(sensor_id = sensor_b, partner = sensor_a, sri30)]
      ))
      sri_by_sensor <- pair_long[, .(
        mean_sri30 = grz_mean_or_na(sri30),
        top5_mean_sri30 = grz_mean_or_na(head(sort(sri30, decreasing = TRUE), 5L)),
        max_sri30 = if (any(is.finite(sri30))) max(sri30, na.rm = TRUE) else NA_real_
      ), by = .(sensor_id)]
    }
  } else {
    sri_by_sensor <- data.table::copy(diag_tbl)[, .(mean_sri30 = NA_real_, top5_mean_sri30 = NA_real_, max_sri30 = NA_real_), by = id_cols]
  }

  ans <- merge(social, diag_tbl, by = id_cols, all = TRUE, sort = FALSE)
  ans <- merge(ans, sri_by_sensor, by = id_cols, all.x = TRUE, sort = FALSE)
  if ("prop_time_any_within_30m" %in% names(ans)) {
    ans[, CR30 := prop_time_any_within_30m]
  } else {
    ans[, CR30 := NA_real_]
  }

  metric_order <- c(
    "mean_nn_m",
    "p50_nn_m",
    "mean_dist_to_others_m",
    grep("^mean_n_within_", names(ans), value = TRUE),
    grep("^prop_time_any_within_", names(ans), value = TRUE),
    "CR30",
    "mean_sri30",
    "top5_mean_sri30",
    "max_sri30",
    "prop_interp_used",
    "n_grid_points"
  )
  data.table::setcolorder(ans, c(id_cols, unique(metric_order[metric_order %in% names(ans)])))
  ans[]
}
