grz_epoch_arg <- function(epoch, epoch_mins, epoch_missing = FALSE) {
  if (!is.null(epoch_mins)) {
    if (!epoch_missing && length(epoch) == 1L && !identical(epoch, "interval")) {
      stop("Use `epoch_mins` with `epoch = \"interval\"` or leave `epoch` unset.", call. = FALSE)
    }
    grz_require_positive_mins(epoch_mins, "epoch_mins")
    return("interval")
  }

  epoch <- match.arg(epoch, c("day", "hour", "week", "month", "interval"))
  if (epoch == "interval") {
    stop("`epoch_mins` is required when `epoch = \"interval\"`.", call. = FALSE)
  }
  epoch
}

grz_epoch_cols <- function() {
  c("epoch", "epoch_start", "epoch_end", "epoch_mins")
}

grz_has_epoch_cols <- function(data) {
  all(grz_epoch_cols() %in% names(data))
}

grz_summary_default_groups <- function(data, groups = NULL) {
  if (!is.null(groups)) {
    return(grz_default_group_cols(data, groups = groups))
  }

  out <- intersect(c("deployment_id", "animal_id", "sensor_id"), names(data))
  if (length(out) == 0L && "sensor_id" %in% names(data)) {
    out <- "sensor_id"
  }
  if (length(out) == 0L) {
    stop("`data` must contain `sensor_id` or explicit `groups` must be supplied.", call. = FALSE)
  }
  out
}

grz_add_epoch_columns <- function(dt, epoch, epoch_mins, time_col = "datetime") {
  grz_require_cols(dt, time_col, fun_name = "GPS epoch summary")
  epoch_dt <- grz_epoch_table(dt[[time_col]], epoch = epoch, epoch_mins = epoch_mins)
  dt[, `:=`(
    epoch = epoch_dt$epoch,
    epoch_start = epoch_dt$epoch_start,
    epoch_end = epoch_dt$epoch_end,
    epoch_mins = epoch_dt$epoch_mins
  )]
  dt
}

grz_social_summary_cols <- function(data) {
  grep(
    "^(social_group_size|n_valid_fixes|nearest_neighbour_m|mean_distance_to_others_m|n_neighbours_within_|any_neighbour_within_)",
    names(data),
    value = TRUE
  )
}

grz_movement_summary_present <- function(data) {
  any(c("total_distance_m", "step_m", "speed_mps") %in% names(data)) ||
    all(grz_gps_required_cols() %in% names(data))
}

grz_spatial_summary_present <- function(data) {
  any(c("mcp100_area_ha", "mcp95_area_ha", "centroid_lon", "centroid_lat") %in% names(data))
}

grz_resource_summary_present <- function(data) {
  all(c("resource_id", "n_fixes_near", "prop_fixes_near") %in% names(data))
}

grz_epoch_auto_include <- function(data, movement, social, spatial, resource_use) {
  include <- character()
  if (!is.null(movement) || (!is.null(data) && grz_movement_summary_present(data))) {
    include <- c(include, "movement")
  }
  if (!is.null(social) || (!is.null(data) && length(grz_social_summary_cols(data)) > 0L)) {
    include <- c(include, "social")
  }
  if (!is.null(spatial) || (!is.null(data) && grz_spatial_summary_present(data))) {
    include <- c(include, "spatial")
  }
  if (!is.null(resource_use) || (!is.null(data) && grz_resource_summary_present(data))) {
    include <- c(include, "resource_use")
  }
  unique(include)
}

grz_order_summary <- function(out, groups, extra_first = character()) {
  first <- unique(c(groups, extra_first, grz_epoch_cols()))
  first <- first[first %in% names(out)]
  data.table::setcolorder(out, c(first, setdiff(names(out), first)))
  out[]
}

grz_resource_use_summary <- function(data, groups = NULL, return_class = "data.table") {
  rc <- grz_match_output_class(return_class)
  dt <- data.table::copy(data.table::as.data.table(data))
  if (!grz_has_epoch_cols(dt)) {
    stop("Resource-use summaries must include `epoch`, `epoch_start`, `epoch_end`, and `epoch_mins`.", call. = FALSE)
  }
  grz_require_cols(dt, c("resource_id", "n_fixes", "n_fixes_near", "prop_fixes_near"), fun_name = "gps_epoch()")
  grp <- grz_summary_default_groups(dt, groups = groups)
  by_cols <- c(grp, grz_epoch_cols())

  out <- dt[, {
    total_fixes <- if ("n_total_fixes" %in% names(.SD) && any(is.finite(n_total_fixes))) {
      max(n_total_fixes, na.rm = TRUE)
    } else {
      sum(n_fixes, na.rm = TRUE)
    }
    near_fixes <- sum(n_fixes_near, na.rm = TRUE)
    w <- if ("mean_distance_m" %in% names(.SD)) n_fixes else numeric()
    list(
      n_resources = data.table::uniqueN(resource_id),
      n_resource_fix_rows = sum(n_fixes, na.rm = TRUE),
      n_fixes_near_resource = near_fixes,
      prop_fixes_near_resource = if (is.finite(total_fixes) && total_fixes > 0) near_fixes / total_fixes else NA_real_,
      mean_resource_distance_m = if ("mean_distance_m" %in% names(.SD) && any(is.finite(mean_distance_m))) {
        stats::weighted.mean(mean_distance_m, w = w, na.rm = TRUE)
      } else {
        NA_real_
      },
      min_resource_distance_m = if ("min_distance_m" %in% names(.SD) && any(is.finite(min_distance_m))) {
        min(min_distance_m, na.rm = TRUE)
      } else {
        NA_real_
      }
    )
  }, by = by_cols]

  data.table::setorderv(out, by_cols)
  grz_as_output(grz_order_summary(out, grp), rc)
}

#' Summarise GPS social metrics by epoch
#'
#' Converts row-level social proximity metrics into one row per animal or
#' sensor and epoch. If social columns are not present, `gps_social()` is run
#' first using the supplied thresholds and alignment settings.
#'
#' @param data Data frame with GPS rows or output from `gps_social()`,
#'   `gps_nearest_neighbour()`, or `gps_neighbours_within_range()`.
#' @param epoch Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`,
#'   or `"interval"`.
#' @param epoch_mins Positive epoch duration in minutes. Supplying this uses
#'   fixed-duration `"interval"` epochs anchored to Unix time in UTC.
#' @param groups Grouping columns for summaries. Defaults to available
#'   `deployment_id`, `animal_id`, and `sensor_id`.
#' @param thresholds_m Distance thresholds used when social metrics must be
#'   calculated from raw GPS rows.
#' @param herd_groups Herd partition columns passed to `gps_social()`.
#' @param interpolate Logical; passed to `gps_social()` when needed.
#' @param align_interval_mins Alignment interval passed to `gps_social()`.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or
#'   `"data.table"`.
#'
#' @return Epoch social summary table with stable identifier, epoch, nearest
#'   neighbour, group-size, and threshold columns.
#' @export
gps_social_summary <- function(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  thresholds_m = c(25, 30, 50, 100),
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)

  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  if (length(grz_social_summary_cols(dt)) == 0L) {
    dt <- data.table::as.data.table(gps_social(
      data = dt,
      thresholds_m = thresholds_m,
      herd_groups = herd_groups,
      interpolate = interpolate,
      align_interval_mins = align_interval_mins,
      verbose = FALSE,
      return_class = "data.table"
    ))
  }

  grp <- grz_summary_default_groups(dt, groups = groups)
  dt <- grz_add_epoch_columns(dt, epoch = epoch, epoch_mins = epoch_mins)
  by_cols <- c(grp, grz_epoch_cols())

  out <- dt[, {
    ans <- list(
      n_fixes = .N,
      n_social_fixes = sum(is.finite(nearest_neighbour_m)),
      mean_social_group_size = grz_mean_or_na(social_group_size),
      mean_valid_fixes = grz_mean_or_na(n_valid_fixes),
      mean_nearest_neighbour_m = grz_mean_or_na(nearest_neighbour_m),
      median_nearest_neighbour_m = grz_quantile_or_na(nearest_neighbour_m, 0.5),
      p95_nearest_neighbour_m = grz_quantile_or_na(nearest_neighbour_m, 0.95),
      mean_distance_to_others_m = grz_mean_or_na(mean_distance_to_others_m)
    )

    n_cols <- grep("^n_neighbours_within_", names(.SD), value = TRUE)
    for (nc in n_cols) {
      ans[[paste0("mean_", nc)]] <- grz_mean_or_na(get(nc))
    }
    any_cols <- grep("^any_neighbour_within_", names(.SD), value = TRUE)
    for (ac in any_cols) {
      ans[[sub("^any_", "prop_any_", ac)]] <- grz_mean_or_na(as.numeric(get(ac)))
    }
    ans
  }, by = by_cols]

  metric_input_cols <- c(
    "datetime", "lon", "lat",
    grz_social_summary_cols(dt),
    "nearest_neighbour_sensor_id",
    "nearest_neighbour_animal_id",
    ".grz_row_id"
  )
  meta_cols <- setdiff(names(dt), unique(c(by_cols, metric_input_cols)))
  meta_cols <- meta_cols[vapply(dt[, ..meta_cols], is.atomic, logical(1))]
  if (length(meta_cols) > 0L) {
    meta <- dt[, lapply(.SD, grz_constant_or_na), by = by_cols, .SDcols = meta_cols]
    out <- merge(meta, out, by = by_cols, all.y = TRUE, sort = FALSE)
  }

  data.table::setorderv(out, by_cols)
  out <- grz_order_summary(out, grp, extra_first = meta_cols)

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_social_summary] epoch=%s rows=%s\n", epoch, format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Join GPS epoch summaries
#'
#' Builds or joins modelling-ready GPS epoch summaries. Raw GPS rows can be
#' summarised directly for movement and social metrics. Pre-computed outputs
#' from `gps_movement_summary()`, `gps_social_summary()`, `gps_spatial()`, and
#' `gps_resource_use()` can also be supplied and will be joined by their common
#' identifier and epoch columns.
#'
#' @param data Optional GPS data or a GPS-derived metric table.
#' @param epoch Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`,
#'   or `"interval"`.
#' @param epoch_mins Positive epoch duration in minutes. Supplying this uses
#'   fixed-duration `"interval"` epochs anchored to Unix time in UTC.
#' @param include Metric blocks to include. Values are `"movement"`,
#'   `"social"`, `"spatial"`, and `"resource_use"`. If `NULL`, blocks are
#'   inferred from supplied tables and columns in `data`.
#' @param groups Grouping columns for summaries and joins.
#' @param movement Optional output from `gps_steps()` or
#'   `gps_movement_summary()`.
#' @param social Optional output from `gps_social()` or `gps_social_summary()`.
#' @param spatial Optional output from `gps_spatial()`.
#' @param resource_use Optional output from `gps_resource_use()`.
#' @inheritParams gps_social_summary
#' @inheritParams gps_spatial
#'
#' @return Joined epoch summary table.
#' @export
gps_epoch <- function(
  data = NULL,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  include = NULL,
  groups = NULL,
  movement = NULL,
  social = NULL,
  spatial = NULL,
  resource_use = NULL,
  thresholds_m = c(25, 30, 50, 100),
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  min_fixes = 5,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  epoch_missing <- missing(epoch)
  epoch <- grz_epoch_arg(epoch, epoch_mins, epoch_missing = epoch_missing)

  if (is.null(include)) {
    include <- grz_epoch_auto_include(data, movement, social, spatial, resource_use)
  }
  allowed <- c("movement", "social", "spatial", "resource_use")
  include <- unique(include)
  bad <- setdiff(include, allowed)
  if (length(bad) > 0L) {
    stop("Unknown include values: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  if (length(include) == 0L) {
    stop("No GPS epoch summary blocks were selected.", call. = FALSE)
  }

  pieces <- list()
  source_data <- data

  if ("movement" %in% include) {
    movement_data <- if (!is.null(movement)) movement else source_data
    if (is.null(movement_data)) {
      stop("Movement summaries require `data` or `movement`.", call. = FALSE)
    }
    movement_dt <- data.table::as.data.table(movement_data)
    pieces$movement <- if (grz_has_epoch_cols(movement_dt) && "total_distance_m" %in% names(movement_dt)) {
      movement_dt
    } else {
      data.table::as.data.table(gps_movement_summary(
        data = movement_data,
        epoch = epoch,
        epoch_mins = epoch_mins,
        groups = groups,
        verbose = FALSE,
        return_class = "data.table"
      ))
    }
  }

  if ("social" %in% include) {
    social_data <- if (!is.null(social)) social else source_data
    if (is.null(social_data)) {
      stop("Social summaries require `data` or `social`.", call. = FALSE)
    }
    social_dt <- data.table::as.data.table(social_data)
    pieces$social <- if (grz_has_epoch_cols(social_dt) && "mean_nearest_neighbour_m" %in% names(social_dt)) {
      social_dt
    } else {
      data.table::as.data.table(gps_social_summary(
        data = social_data,
        epoch = epoch,
        epoch_mins = epoch_mins,
        groups = groups,
        thresholds_m = thresholds_m,
        herd_groups = herd_groups,
        interpolate = interpolate,
        align_interval_mins = align_interval_mins,
        verbose = FALSE,
        return_class = "data.table"
      ))
    }
  }

  if ("spatial" %in% include) {
    spatial_data <- if (!is.null(spatial)) spatial else source_data
    if (is.null(spatial_data)) {
      stop("Spatial summaries require `data` or `spatial`.", call. = FALSE)
    }
    spatial_dt <- data.table::as.data.table(spatial_data)
    pieces$spatial <- if (grz_has_epoch_cols(spatial_dt) && grz_spatial_summary_present(spatial_dt)) {
      spatial_dt
    } else {
      data.table::as.data.table(gps_spatial(
        data = spatial_data,
        epoch = epoch,
        epoch_mins = epoch_mins,
        groups = groups,
        min_fixes = min_fixes,
        metric_crs = metric_crs,
        verbose = FALSE,
        return_class = "data.table"
      ))
    }
  }

  if ("resource_use" %in% include) {
    resource_data <- if (!is.null(resource_use)) resource_use else source_data
    if (is.null(resource_data)) {
      stop("Resource-use summaries require `data` or `resource_use`.", call. = FALSE)
    }
    pieces$resource_use <- data.table::as.data.table(grz_resource_use_summary(
      data = resource_data,
      groups = groups,
      return_class = "data.table"
    ))
  }

  out <- grz_merge_metric_tables(pieces)
  order_cols <- intersect(c(groups, grz_summary_default_groups(out, groups = NULL), grz_epoch_cols()), names(out))
  if (length(order_cols) > 0L) {
    data.table::setorderv(out, order_cols)
  }
  out <- grz_order_summary(out, order_cols)

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_epoch] epoch=%s include=%s rows=%s\n", epoch, paste(include, collapse = ","), format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}

#' Summarise GPS metrics by animal or sensor
#'
#' Produces one modelling-ready row per animal or sensor and epoch. By default
#' the function groups by available `deployment_id`, `animal_id`, and
#' `sensor_id`, then delegates to `gps_epoch()`.
#'
#' @inheritParams gps_epoch
#' @param ... Additional arguments passed to `gps_epoch()`.
#'
#' @return Animal or sensor epoch summary table.
#' @export
gps_animal_summary <- function(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  include = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
) {
  rc <- grz_match_output_class(return_class)
  dt <- data.table::as.data.table(data)
  grp <- grz_summary_default_groups(dt, groups = groups)
  out <- gps_epoch(
    data = data,
    epoch = epoch,
    epoch_mins = epoch_mins,
    include = include,
    groups = grp,
    verbose = verbose,
    return_class = "data.table",
    ...
  )
  grz_as_output(out, rc)
}

#' Summarise GPS metrics by group or herd metadata
#'
#' Aggregates animal or sensor epoch summaries to group-level rows, such as
#' deployment, paddock, treatment, herd, or user-supplied grouping columns.
#'
#' @param group_cols Group or herd metadata columns. If `NULL`, available
#'   `deployment_id`, `herd_id`, `group_id`, `paddock`, and `treatment` columns
#'   are used.
#' @inheritParams gps_animal_summary
#'
#' @return Group-level epoch summary table.
#' @export
gps_group_summary <- function(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  group_cols = NULL,
  include = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
) {
  rc <- grz_match_output_class(return_class)
  dt <- data.table::as.data.table(data)
  if (is.null(group_cols)) {
    group_cols <- intersect(c("deployment_id", "herd_id", "group_id", "paddock", "treatment"), names(dt))
  } else {
    grz_require_cols(dt, group_cols, fun_name = "gps_group_summary()")
  }
  if (length(group_cols) == 0L) {
    stop("No group metadata columns were found. Supply `group_cols`.", call. = FALSE)
  }

  animal_groups <- if (!is.null(groups)) {
    unique(c(group_cols, groups))
  } else {
    unique(c(group_cols, intersect(c("animal_id", "sensor_id"), names(dt))))
  }

  animal <- data.table::as.data.table(gps_animal_summary(
    data = data,
    epoch = epoch,
    epoch_mins = epoch_mins,
    include = include,
    groups = animal_groups,
    verbose = FALSE,
    return_class = "data.table",
    ...
  ))

  by_cols <- c(group_cols, grz_epoch_cols())
  out <- animal[, {
    ans <- list(
      n_animal_epochs = .N,
      n_animals = if ("animal_id" %in% names(.SD)) data.table::uniqueN(animal_id) else NA_integer_,
      n_sensors = if ("sensor_id" %in% names(.SD)) data.table::uniqueN(sensor_id) else NA_integer_
    )
    if ("n_fixes" %in% names(.SD)) {
      ans$n_fixes <- sum(n_fixes, na.rm = TRUE)
    }
    if ("total_distance_m" %in% names(.SD)) {
      ans$total_distance_m <- sum(total_distance_m, na.rm = TRUE)
      ans$mean_total_distance_m <- grz_mean_or_na(total_distance_m)
    }
    if ("mean_speed_mps" %in% names(.SD)) {
      ans$mean_speed_mps <- grz_mean_or_na(mean_speed_mps)
    }
    if ("mean_nearest_neighbour_m" %in% names(.SD)) {
      ans$mean_nearest_neighbour_m <- grz_mean_or_na(mean_nearest_neighbour_m)
    }
    for (col in grep("^prop_any_neighbour_within_", names(.SD), value = TRUE)) {
      ans[[col]] <- grz_mean_or_na(get(col))
    }
    if ("prop_fixes_near_resource" %in% names(.SD)) {
      ans$prop_fixes_near_resource <- grz_mean_or_na(prop_fixes_near_resource)
    }
    ans
  }, by = by_cols]

  data.table::setorderv(out, by_cols)
  out <- grz_order_summary(out, group_cols)

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_group_summary] rows=%s groups=%s\n", format(nrow(out), big.mark = ","), paste(group_cols, collapse = ",")))
  }
  grz_as_output(out, rc)
}

#' Summarise GPS metrics by date and hour
#'
#' Creates UTC hourly summaries with explicit `date` and `hour` columns for
#' diurnal analyses. The output is based on `gps_animal_summary()` with
#' `epoch = "hour"`.
#'
#' @inheritParams gps_animal_summary
#'
#' @return Hourly GPS summary table with `date` and `hour` columns.
#' @export
gps_diurnal <- function(
  data,
  include = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
) {
  rc <- grz_match_output_class(return_class)
  out <- data.table::as.data.table(gps_animal_summary(
    data = data,
    epoch = "hour",
    include = include,
    groups = groups,
    verbose = FALSE,
    return_class = "data.table",
    ...
  ))
  out[, date := as.Date(epoch_start, tz = "UTC")]
  out[, hour := as.integer(format(epoch_start, "%H", tz = "UTC"))]
  grp <- grz_summary_default_groups(out, groups = groups)
  out <- grz_order_summary(out, grp, extra_first = c("date", "hour"))

  if (isTRUE(verbose)) {
    cat(sprintf("[gps_diurnal] rows=%s\n", format(nrow(out), big.mark = ",")))
  }
  grz_as_output(out, rc)
}
