grz_social_thresholds <- function(thresholds_m, arg = "thresholds_m") {
  if (!is.numeric(thresholds_m) || length(thresholds_m) < 1L ||
      any(!is.finite(thresholds_m)) || any(thresholds_m <= 0)) {
    stop("`", arg, "` must be positive numeric values.", call. = FALSE)
  }
  sort(unique(as.numeric(thresholds_m)))
}

grz_social_herd_cols <- function(data, herd_groups = NULL, fun_name = "GPS social function") {
  if (is.null(herd_groups)) {
    return(intersect(c("deployment_id", "paddock"), names(data)))
  }
  if (!is.character(herd_groups) || any(is.na(herd_groups)) || any(trimws(herd_groups) == "")) {
    stop("`herd_groups` must be NULL or a character vector of column names.", call. = FALSE)
  }
  grz_require_cols(data, herd_groups, fun_name = fun_name)
  unique(herd_groups)
}

grz_prepare_social_dt <- function(
  data,
  interpolate,
  align_interval_mins,
  herd_groups,
  fun_name
) {
  grz_require_flag(interpolate, "interpolate")

  dt <- if (isTRUE(interpolate)) {
    data.table::as.data.table(gps_interpolate(
      data = data,
      interval_mins = align_interval_mins,
      keep_extra = TRUE,
      verbose = FALSE,
      return_class = "data.table"
    ))
  } else {
    grz_prepare_clean_dt(data, require_core = TRUE)
  }

  herd <- grz_social_herd_cols(dt, herd_groups = herd_groups, fun_name = fun_name)
  dt[, .grz_row_id := .I]
  order_cols <- c(herd, "datetime", intersect(c("animal_id", "sensor_id"), names(dt)), ".grz_row_id")
  data.table::setorderv(dt, order_cols)
  list(data = dt, herd_groups = herd)
}

grz_social_split <- function(dt, herd_groups) {
  dt_valid_time <- dt[!is.na(datetime)]
  if (nrow(dt_valid_time) == 0L) {
    return(list())
  }

  split_cols <- c(herd_groups, "datetime")
  split(
    seq_len(nrow(dt_valid_time)),
    interaction(dt_valid_time[, ..split_cols], drop = TRUE, lex.order = TRUE)
  )
}

grz_social_pair_partition <- function(sub, herd_groups) {
  n <- nrow(sub)
  if (n < 2L) {
    return(data.table::data.table())
  }

  idx_a <- rep.int(seq_len(n - 1L), (n - 1L):1L)
  idx_b <- unlist(lapply(seq_len(n - 1L), function(i) (i + 1L):n), use.names = FALSE)

  out <- data.table::data.table(
    .grz_row_id_a = sub$.grz_row_id[idx_a],
    .grz_row_id_b = sub$.grz_row_id[idx_b],
    datetime = sub$datetime[idx_a],
    sensor_id_a = sub$sensor_id[idx_a],
    sensor_id_b = sub$sensor_id[idx_b]
  )

  if ("animal_id" %in% names(sub)) {
    out[, `:=`(
      animal_id_a = sub$animal_id[idx_a],
      animal_id_b = sub$animal_id[idx_b]
    )]
  }
  for (col in herd_groups) {
    out[, (col) := sub[[col]][idx_a]]
  }

  out[, distance_m := grz_haversine_m(
    sub$lon[idx_a],
    sub$lat[idx_a],
    sub$lon[idx_b],
    sub$lat[idx_b]
  )]
  out[, pair_id := paste(sensor_id_a, sensor_id_b, sep = ":")]

  data.table::setcolorder(
    out,
    c(
      herd_groups,
      "datetime",
      "pair_id",
      intersect(c("animal_id_a", "animal_id_b"), names(out)),
      "sensor_id_a",
      "sensor_id_b",
      "distance_m"
    )
  )
  out[]
}

grz_empty_proximity <- function(data, herd_groups) {
  out <- data.table::data.table(
    datetime = as.POSIXct(character(), tz = "UTC"),
    pair_id = character(),
    sensor_id_a = character(),
    sensor_id_b = character(),
    distance_m = numeric()
  )
  if ("animal_id" %in% names(data)) {
    out[, `:=`(animal_id_a = character(), animal_id_b = character())]
  }
  for (col in rev(herd_groups)) {
    out[, (col) := data[[col]][0L]]
  }
  data.table::setcolorder(
    out,
    c(
      herd_groups,
      "datetime",
      "pair_id",
      intersect(c("animal_id_a", "animal_id_b"), names(out)),
      "sensor_id_a",
      "sensor_id_b",
      "distance_m"
    )
  )
  out[]
}

grz_social_row_partition <- function(sub, thresholds_m = numeric()) {
  out <- data.table::data.table(
    .grz_row_id = sub$.grz_row_id,
    social_group_size = nrow(sub),
    n_valid_fixes = sum(is.finite(sub$lon) & is.finite(sub$lat)),
    nearest_neighbour_m = NA_real_,
    mean_distance_to_others_m = NA_real_,
    nearest_neighbour_sensor_id = NA_character_
  )
  if ("animal_id" %in% names(sub)) {
    out[, nearest_neighbour_animal_id := NA_character_]
  }
  for (thr in thresholds_m) {
    lbl <- grz_threshold_label(thr)
    out[, (paste0("n_neighbours_within_", lbl, "m")) := 0L]
    out[, (paste0("any_neighbour_within_", lbl, "m")) := FALSE]
  }

  pairs <- grz_social_pair_partition(sub, character())
  if (nrow(pairs) == 0L) {
    return(out[])
  }

  long <- data.table::rbindlist(
    list(
      data.table::data.table(
        .grz_row_id = pairs$.grz_row_id_a,
        .grz_neighbour_row_id = pairs$.grz_row_id_b,
        distance_m = pairs$distance_m
      ),
      data.table::data.table(
        .grz_row_id = pairs$.grz_row_id_b,
        .grz_neighbour_row_id = pairs$.grz_row_id_a,
        distance_m = pairs$distance_m
      )
    ),
    use.names = TRUE
  )

  finite_long <- long[is.finite(distance_m)]
  if (nrow(finite_long) > 0L) {
    nearest <- finite_long[order(distance_m), .SD[1L], by = ".grz_row_id"]
    neighbours <- sub[, c(".grz_row_id", "sensor_id", intersect("animal_id", names(sub))), with = FALSE]
    data.table::setnames(neighbours, ".grz_row_id", ".grz_neighbour_row_id")
    nearest <- merge(nearest, neighbours, by = ".grz_neighbour_row_id", all.x = TRUE, sort = FALSE)
    data.table::setnames(nearest, "distance_m", "nearest_neighbour_m")
    out[nearest, on = ".grz_row_id", `:=`(
      nearest_neighbour_m = i.nearest_neighbour_m,
      nearest_neighbour_sensor_id = i.sensor_id
    )]
    if ("animal_id" %in% names(sub)) {
      out[nearest, on = ".grz_row_id", nearest_neighbour_animal_id := i.animal_id]
    }

    means <- finite_long[, list(mean_distance_to_others_m = mean(distance_m)), by = ".grz_row_id"]
    out[means, on = ".grz_row_id", mean_distance_to_others_m := i.mean_distance_to_others_m]
  }

  for (thr in thresholds_m) {
    lbl <- grz_threshold_label(thr)
    count_col <- paste0("n_neighbours_within_", lbl, "m")
    any_col <- paste0("any_neighbour_within_", lbl, "m")
    counts <- long[, list(n = sum(is.finite(distance_m) & distance_m <= thr)), by = ".grz_row_id"]
    out[counts, on = ".grz_row_id", (count_col) := as.integer(i.n)]
    out[, (any_col) := get(count_col) > 0L]
  }

  out[]
}

grz_social_row_metrics <- function(dt, herd_groups, thresholds_m = numeric()) {
  metric_cols <- data.table::rbindlist(
    lapply(grz_social_split(dt, herd_groups), function(i) {
      grz_social_row_partition(dt[!is.na(datetime)][i], thresholds_m = thresholds_m)
    }),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(metric_cols) == 0L) {
    metric_cols <- data.table::data.table(
      .grz_row_id = integer(),
      social_group_size = integer(),
      n_valid_fixes = integer(),
      nearest_neighbour_m = numeric(),
      mean_distance_to_others_m = numeric(),
      nearest_neighbour_sensor_id = character()
    )
    if ("animal_id" %in% names(dt)) {
      metric_cols[, nearest_neighbour_animal_id := character()]
    }
    for (thr in thresholds_m) {
      lbl <- grz_threshold_label(thr)
      metric_cols[, (paste0("n_neighbours_within_", lbl, "m")) := integer()]
      metric_cols[, (paste0("any_neighbour_within_", lbl, "m")) := logical()]
    }
  }

  out <- merge(dt, metric_cols, by = ".grz_row_id", all.x = TRUE, sort = FALSE)
  out[is.na(social_group_size), social_group_size := 0L]
  out[is.na(n_valid_fixes), n_valid_fixes := 0L]
  for (col in grep("^n_neighbours_within_", names(out), value = TRUE)) {
    out[is.na(get(col)), (col) := 0L]
  }
  for (col in grep("^any_neighbour_within_", names(out), value = TRUE)) {
    out[is.na(get(col)), (col) := FALSE]
  }
  data.table::setorderv(out, c(herd_groups, "datetime", intersect(c("animal_id", "sensor_id"), names(out))))
  out[, .grz_row_id := NULL]
  out[]
}

#' Calculate pairwise GPS proximity by timestamp
#'
#' Returns one row for each unordered pair of fixes within the same timestamp
#' and herd partition. Distances are haversine distances in metres from WGS84
#' longitude and latitude. Pairwise output grows as `n * (n - 1) / 2` within
#' each timestamp, so large herds and fine sampling intervals can produce large
#' tables. For nearest-neighbour distances or range counts, use
#' `gps_nearest_neighbour()` or `gps_neighbours_within_range()` to avoid keeping
#' all pair rows in memory at once.
#'
#' @param data Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.
#' @param herd_groups Herd partition columns. Defaults to available
#'   `deployment_id` and `paddock`, so animals are only compared within those
#'   partitions.
#' @param interpolate Logical; if `TRUE`, regularise and interpolate fixes
#'   before comparing animals.
#' @param align_interval_mins Alignment interval in minutes, or `"base"` to use
#'   the median observed interval.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Pairwise proximity table with identifiers, `datetime`, `pair_id`, and
#'   `distance_m`.
#' @export
gps_proximity <- function(
  data,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  prepared <- grz_prepare_social_dt(
    data = data,
    interpolate = interpolate,
    align_interval_mins = align_interval_mins,
    herd_groups = herd_groups,
    fun_name = "gps_proximity()"
  )
  dt <- prepared$data
  herd <- prepared$herd_groups
  dt_valid <- dt[!is.na(datetime)]

  split_idx <- grz_social_split(dt, herd)
  out <- data.table::rbindlist(
    lapply(split_idx, function(i) grz_social_pair_partition(dt_valid[i], herd_groups = herd)),
    use.names = TRUE,
    fill = TRUE
  )
  if (nrow(out) == 0L) {
    out <- grz_empty_proximity(dt, herd)
  }
  out[, intersect(c(".grz_row_id_a", ".grz_row_id_b"), names(out)) := NULL]
  data.table::setorderv(out, c(herd, "datetime", "pair_id"))

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_proximity] pairs=%s\n", format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Calculate GPS nearest-neighbour distance
#'
#' Calculates the nearest other animal or sensor at each timestamp within each
#' herd partition. Rows with missing timestamps or coordinates are retained with
#' missing nearest-neighbour metrics.
#'
#' @inheritParams gps_proximity
#'
#' @return Input rows with `social_group_size`, `n_valid_fixes`,
#'   `nearest_neighbour_m`, `nearest_neighbour_sensor_id`, and, when present,
#'   `nearest_neighbour_animal_id`.
#' @export
gps_nearest_neighbour <- function(
  data,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  prepared <- grz_prepare_social_dt(
    data = data,
    interpolate = interpolate,
    align_interval_mins = align_interval_mins,
    herd_groups = herd_groups,
    fun_name = "gps_nearest_neighbour()"
  )
  out <- grz_social_row_metrics(prepared$data, prepared$herd_groups)

  keep <- c(
    names(prepared$data),
    "social_group_size",
    "n_valid_fixes",
    "nearest_neighbour_m",
    "nearest_neighbour_sensor_id",
    "nearest_neighbour_animal_id"
  )
  keep <- setdiff(intersect(keep, names(out)), ".grz_row_id")
  out <- out[, ..keep]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_nearest_neighbour] rows=%s\n", format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Count GPS neighbours within distance thresholds
#'
#' Counts the number of other fixes within each supplied threshold at each
#' timestamp and herd partition. Counts only use finite distances, so missing
#' fixes do not contribute as neighbours.
#'
#' @inheritParams gps_proximity
#' @param thresholds_m Distance thresholds in metres.
#'
#' @return Input rows with `social_group_size`, `n_valid_fixes`, and one
#'   `n_neighbours_within_*m` and `any_neighbour_within_*m` column per
#'   threshold.
#' @export
gps_neighbours_within_range <- function(
  data,
  thresholds_m,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  thresholds_m <- grz_social_thresholds(thresholds_m)
  prepared <- grz_prepare_social_dt(
    data = data,
    interpolate = interpolate,
    align_interval_mins = align_interval_mins,
    herd_groups = herd_groups,
    fun_name = "gps_neighbours_within_range()"
  )
  out <- grz_social_row_metrics(prepared$data, prepared$herd_groups, thresholds_m = thresholds_m)

  social_cols <- grep("^(social_group_size|n_valid_fixes|n_neighbours_within_|any_neighbour_within_)", names(out), value = TRUE)
  keep <- setdiff(c(names(prepared$data), social_cols), ".grz_row_id")
  out <- out[, ..keep]

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_neighbours_within_range] rows=%s thresholds=%s\n", format(nrow(out), big.mark = ","), paste(thresholds_m, collapse = ",")))
  }
  grz_as_output(out, rc)
}

#' Calculate GPS contact events
#'
#' Detects runs of pairwise proximity where distance is less than or equal to a
#' contact threshold. Events are built separately for each pair and herd
#' partition. A single contact fix has `duration_s = 0`. For irregular
#' timestamps, use `max_gap_mins` to decide how far apart contact fixes can be
#' while still belonging to the same event.
#'
#' @inheritParams gps_proximity
#' @param contact_distance_m Contact threshold in metres.
#' @param max_gap_mins Maximum gap between contact fixes in the same event. If
#'   `NULL`, the median positive timestamp interval in the pairwise data is used.
#' @param min_duration_mins Minimum event duration to retain.
#'
#' @return Contact event table with pair identifiers, event timing, duration,
#'   contact-fix counts, distance summaries, and `contact_distance_m`.
#' @export
gps_contacts <- function(
  data,
  contact_distance_m,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  max_gap_mins = NULL,
  min_duration_mins = 0,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  contact_distance_m <- grz_social_thresholds(contact_distance_m, arg = "contact_distance_m")
  if (length(contact_distance_m) != 1L) {
    stop("`contact_distance_m` must be a single positive value.", call. = FALSE)
  }
  if (!is.null(max_gap_mins)) {
    grz_require_positive_mins(max_gap_mins, "max_gap_mins")
  }
  if (!is.numeric(min_duration_mins) || length(min_duration_mins) != 1L ||
      !is.finite(min_duration_mins) || min_duration_mins < 0) {
    stop("`min_duration_mins` must be a non-negative number.", call. = FALSE)
  }

  prox <- data.table::as.data.table(gps_proximity(
    data = data,
    herd_groups = herd_groups,
    interpolate = interpolate,
    align_interval_mins = align_interval_mins,
    verbose = FALSE,
    return_class = "data.table"
  ))
  herd <- grz_social_herd_cols(prox, herd_groups = herd_groups, fun_name = "gps_contacts()")

  out_cols <- c(
    herd,
    "pair_id",
    intersect(c("animal_id_a", "animal_id_b"), names(prox)),
    "sensor_id_a",
    "sensor_id_b",
    "contact_event_id",
    "contact_start",
    "contact_end",
    "duration_s",
    "n_contact_fixes",
    "min_distance_m",
    "mean_distance_m",
    "max_distance_m",
    "contact_distance_m"
  )
  empty <- prox[0L, intersect(c(herd, "pair_id", "animal_id_a", "animal_id_b", "sensor_id_a", "sensor_id_b"), names(prox)), with = FALSE]
  for (col in setdiff(out_cols, names(empty))) {
    if (col %in% c("contact_event_id", "n_contact_fixes")) {
      empty[, (col) := integer()]
    } else if (col %in% c("contact_start", "contact_end")) {
      empty[, (col) := as.POSIXct(character(), tz = "UTC")]
    } else {
      empty[, (col) := numeric()]
    }
  }
  data.table::setcolorder(empty, out_cols)

  contacts <- prox[is.finite(distance_m) & distance_m <= contact_distance_m]
  if (nrow(contacts) == 0L) {
    if (isTRUE(verbose)) {
      cat("[gps_contacts] events=0\n")
    }
    return(grz_as_output(empty, rc))
  }

  by_pair <- c(herd, "pair_id", intersect(c("animal_id_a", "animal_id_b"), names(contacts)), "sensor_id_a", "sensor_id_b")
  data.table::setorderv(contacts, c(by_pair, "datetime"))

  if (is.null(max_gap_mins)) {
    gaps <- contacts[, as.numeric(datetime - data.table::shift(datetime), units = "mins"), by = by_pair]$V1
    gaps <- gaps[is.finite(gaps) & gaps > 0]
    max_gap_mins <- if (length(gaps) > 0L) stats::median(gaps) else Inf
  }
  max_gap_s <- if (is.infinite(max_gap_mins)) Inf else as.numeric(max_gap_mins) * 60

  contacts[, .grz_gap_s := as.numeric(datetime - data.table::shift(datetime), units = "secs"), by = by_pair]
  contacts[, .grz_new_event := is.na(.grz_gap_s) | .grz_gap_s > max_gap_s, by = by_pair]
  contacts[, .grz_event := cumsum(.grz_new_event), by = by_pair]

  out <- contacts[, list(
    contact_start = min(datetime),
    contact_end = max(datetime),
    duration_s = as.numeric(max(datetime) - min(datetime), units = "secs"),
    n_contact_fixes = .N,
    min_distance_m = min(distance_m),
    mean_distance_m = mean(distance_m),
    max_distance_m = max(distance_m),
    contact_distance_m = contact_distance_m
  ), by = c(by_pair, ".grz_event")]
  out[, contact_event_id := seq_len(.N), by = by_pair]
  out <- out[duration_s >= min_duration_mins * 60]
  out[, .grz_event := NULL]
  data.table::setcolorder(out, out_cols)

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_contacts] events=%s threshold_m=%s\n", format(nrow(out), big.mark = ","), format(contact_distance_m, trim = TRUE)))
  }
  grz_as_output(out, rc)
}

#' Calculate standard GPS social proximity metrics
#'
#' Convenience wrapper around nearest-neighbour distance and neighbour counts.
#' The output remains row-level so it can be summarised by animal, sensor,
#' datetime, or user-defined epochs. Use `gps_proximity()` when pair-level
#' distances are required and `gps_contacts()` when association events are
#' required.
#'
#' @inheritParams gps_neighbours_within_range
#'
#' @return Input rows with nearest-neighbour, mean-distance, group-size, and
#'   threshold count metrics.
#' @export
gps_social <- function(
  data,
  thresholds_m = c(25, 30, 50, 100),
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  thresholds_m <- grz_social_thresholds(thresholds_m)
  prepared <- grz_prepare_social_dt(
    data = data,
    interpolate = interpolate,
    align_interval_mins = align_interval_mins,
    herd_groups = herd_groups,
    fun_name = "gps_social()"
  )
  out <- grz_social_row_metrics(prepared$data, prepared$herd_groups, thresholds_m = thresholds_m)

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_social] rows=%s thresholds=%s\n", format(nrow(out), big.mark = ","), paste(thresholds_m, collapse = ",")))
  }
  grz_as_output(out, rc)
}
