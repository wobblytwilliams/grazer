grz_require_ggplot2 <- function(fun_name) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`", fun_name, "` requires the `ggplot2` package.", call. = FALSE)
  }
}

grz_behavior_prepare_dt <- function(data, groups = NULL, ensure_features = TRUE) {
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)

  if (isTRUE(ensure_features)) {
    needed <- c("step_m", "speed_mps", "turn_rad")
    if (!all(needed %in% names(dt))) {
      dt <- data.table::as.data.table(
        grz_calculate_movement(
          data = dt,
          groups = grp,
          verbose = FALSE,
          return_class = "data.table"
        )
      )
    }
  }

  data.table::setorderv(dt, c(grp, "datetime"))
  list(data = dt, groups = grp)
}

grz_smooth_state_runs <- function(states, min_run_n = 1L) {
  if (is.null(states) || length(states) == 0L || min_run_n <= 1L) {
    return(states)
  }
  x <- as.character(states)
  r <- rle(x)
  nr <- length(r$lengths)
  if (nr <= 1L) {
    return(x)
  }

  starts <- cumsum(c(1L, head(r$lengths, -1L)))
  ends <- cumsum(r$lengths)
  out <- x

  for (i in seq_len(nr)) {
    if (r$lengths[[i]] >= min_run_n) {
      next
    }
    cur_state <- r$values[[i]]
    if (is.na(cur_state) || trimws(cur_state) == "") {
      next
    }

    prev_state <- if (i > 1L) r$values[[i - 1L]] else NA_character_
    next_state <- if (i < nr) r$values[[i + 1L]] else NA_character_
    prev_len <- if (i > 1L) r$lengths[[i - 1L]] else 0L
    next_len <- if (i < nr) r$lengths[[i + 1L]] else 0L

    replacement <- cur_state
    if (!is.na(prev_state) && !is.na(next_state)) {
      if (identical(prev_state, next_state)) {
        replacement <- prev_state
      } else {
        replacement <- if (prev_len >= next_len) prev_state else next_state
      }
    } else if (!is.na(prev_state)) {
      replacement <- prev_state
    } else if (!is.na(next_state)) {
      replacement <- next_state
    }

    out[starts[[i]]:ends[[i]]] <- replacement
  }

  out
}

grz_quantile_cols <- function(probs) {
  paste0("p", sprintf("%02d", round(probs * 100)))
}

#' Plot diurnal heatmaps for key movement metrics
#'
#' Creates cohort/group-level diurnal heatmaps to support threshold tuning
#' before behavior-state classification.
#'
#' @param data Input data containing at least `sensor_id`, `datetime`, `lon`,
#'   `lat`.
#' @param metrics Metrics to visualise (default speed/step/turn).
#' @param group_col Column used for y-axis grouping.
#' @param cohort_col Optional cohort column for faceting.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param agg_fun Aggregation function for hourly metric values.
#' @param scale Optional scaling for fill values (`"none"` or `"zscore"`).
#' @param return_data Logical; return plotting data along with plot.
#'
#' @return A ggplot object, or list with `plot` and `data` when
#'   `return_data = TRUE`.
#' @export
grz_plot_diurnal_metrics <- function(
  data,
  metrics = c("step_m", "turn_rad"),
  group_col = "sensor_id",
  cohort_col = NULL,
  tz_local = "UTC",
  agg_fun = c("median", "mean"),
  scale = c("none", "zscore"),
  return_data = FALSE
) {
  grz_require_ggplot2("grz_plot_diurnal_metrics()")
  agg_fun <- match.arg(agg_fun)
  scale <- match.arg(scale)

  prep <- grz_behavior_prepare_dt(data, ensure_features = TRUE)
  dt <- prep$data

  if (!is.character(metrics) || length(metrics) < 1L) {
    stop("`metrics` must be a non-empty character vector.", call. = FALSE)
  }
  missing_metrics <- setdiff(metrics, names(dt))
  if (length(missing_metrics) > 0L) {
    stop("Missing metric columns: ", paste(missing_metrics, collapse = ", "), call. = FALSE)
  }
  if (!group_col %in% names(dt)) {
    stop("`group_col` not found in data: ", group_col, call. = FALSE)
  }
  if (!is.null(cohort_col) && !cohort_col %in% names(dt)) {
    stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
  }

  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, .grz_group := as.character(get(group_col))]
  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }

  long <- data.table::melt(
    dt,
    id.vars = c(".grz_hour", ".grz_group", ".grz_cohort"),
    measure.vars = metrics,
    variable.name = ".grz_metric",
    value.name = ".grz_value"
  )
  long <- long[is.finite(.grz_value)]
  if (nrow(long) == 0L) {
    stop("No finite values available for selected metrics.", call. = FALSE)
  }

  agg <- if (agg_fun == "median") {
    long[, .(value = stats::median(.grz_value, na.rm = TRUE)), by = .(.grz_hour, .grz_group, .grz_cohort, .grz_metric)]
  } else {
    long[, .(value = mean(.grz_value, na.rm = TRUE)), by = .(.grz_hour, .grz_group, .grz_cohort, .grz_metric)]
  }

  if (scale == "zscore") {
    agg[, plot_value := {
      v <- value
      s <- stats::sd(v, na.rm = TRUE)
      if (is.finite(s) && s > 0) {
        (v - mean(v, na.rm = TRUE)) / s
      } else {
        rep(0, .N)
      }
    }, by = .(.grz_cohort, .grz_metric)]
  } else {
    agg[, plot_value := value]
  }

  fill_scale <- if (scale == "zscore") {
    ggplot2::scale_fill_gradient2(
      low = "green",
      mid = "yellow",
      high = "red",
      midpoint = 0
    )
  } else {
    ggplot2::scale_fill_gradient(
      low = "green",
      high = "red"
    )
  }

  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(x = .grz_hour, y = .grz_group, fill = plot_value)
  ) +
    ggplot2::geom_tile() +
    fill_scale +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = group_col,
      fill = if (scale == "zscore") "Z-score" else agg_fun,
      title = "Diurnal Metric Heatmap"
    ) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y", space = "free_y") +
    ggplot2::theme_minimal()

  if (isTRUE(return_data)) {
    return(list(plot = p, data = as.data.frame(agg)))
  }
  p
}

#' Build threshold guidance summaries for behavior classification
#'
#' Produces overall and hourly quantile summaries (plus diagnostic plots) for
#' selected metrics to help users choose rule-based behavior thresholds.
#'
#' @param data Input data containing at least `sensor_id`, `datetime`, `lon`,
#'   `lat`.
#' @param metrics Metrics used for threshold guidance.
#' @param cohort_col Optional cohort column used in summaries and facets.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param probs Quantile probabilities used in summary tables.
#' @param max_points_plot Maximum points used in density plot subsample.
#' @param seed Random seed used for plotting subsample.
#' @param return_class Output class for returned tables.
#'
#' @return A list with `overall`, `hourly`, and `plots` (ggplot objects).
#' @export
grz_behavior_threshold_guide <- function(
  data,
  metrics = c("step_m", "turn_rad"),
  cohort_col = NULL,
  tz_local = "UTC",
  probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
  max_points_plot = 150000L,
  seed = 1,
  return_class = c("data.frame", "data.table")
) {
  grz_require_ggplot2("grz_behavior_threshold_guide()")
  rc <- grz_match_output_class(return_class)

  if (!is.numeric(probs) || length(probs) < 1L || any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("`probs` must be numeric probabilities in (0, 1).", call. = FALSE)
  }
  probs <- sort(unique(as.numeric(probs)))

  prep <- grz_behavior_prepare_dt(data, ensure_features = TRUE)
  dt <- prep$data

  missing_metrics <- setdiff(metrics, names(dt))
  if (length(missing_metrics) > 0L) {
    stop("Missing metric columns: ", paste(missing_metrics, collapse = ", "), call. = FALSE)
  }

  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    if (!cohort_col %in% names(dt)) {
      stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
    }
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }
  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]

  long <- data.table::melt(
    dt,
    id.vars = c(".grz_cohort", ".grz_hour"),
    measure.vars = metrics,
    variable.name = ".grz_metric",
    value.name = ".grz_value"
  )
  long <- long[is.finite(.grz_value)]
  if (nrow(long) == 0L) {
    stop("No finite values available for selected metrics.", call. = FALSE)
  }

  qcols <- grz_quantile_cols(probs)
  overall <- long[, {
    qs <- stats::quantile(.grz_value, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
    out <- as.list(setNames(as.numeric(qs), qcols))
    out$n <- .N
    out$mean <- mean(.grz_value, na.rm = TRUE)
    out$sd <- if (.N >= 2L) stats::sd(.grz_value, na.rm = TRUE) else NA_real_
    out
  }, by = .(.grz_cohort, .grz_metric)]

  hourly <- long[, {
    qs <- stats::quantile(.grz_value, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
    out <- as.list(setNames(as.numeric(qs), qcols))
    out$n <- .N
    out$mean <- mean(.grz_value, na.rm = TRUE)
    out
  }, by = .(.grz_cohort, .grz_metric, .grz_hour)]

  hourly_band <- long[, .(
    p10 = grz_quantile_or_na(.grz_value, 0.10),
    p50 = grz_quantile_or_na(.grz_value, 0.50),
    p90 = grz_quantile_or_na(.grz_value, 0.90)
  ), by = .(.grz_cohort, .grz_metric, .grz_hour)]

  long_plot <- long
  if (nrow(long_plot) > as.integer(max_points_plot)) {
    set.seed(seed)
    long_plot <- long_plot[sample.int(nrow(long_plot), as.integer(max_points_plot))]
  }

  p_box <- ggplot2::ggplot(
    long_plot,
    ggplot2::aes(x = factor(.grz_hour), y = .grz_value)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.05, linewidth = 0.2) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y") +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = "Metric value",
      title = "Hourly Metric Distribution (Boxplots)"
    ) +
    ggplot2::theme_minimal()

  p_band <- ggplot2::ggplot(
    hourly_band,
    ggplot2::aes(x = .grz_hour, y = p50)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p10, ymax = p90),
      fill = "grey75",
      alpha = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.8, color = "black") +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = "Median (p10-p90 band)",
      title = "Hourly Metric Bands for Threshold Setting"
    ) +
    ggplot2::theme_minimal()

  p_density <- ggplot2::ggplot(
    long_plot,
    ggplot2::aes(x = .grz_value, fill = .grz_metric)
  ) +
    ggplot2::geom_density(alpha = 0.4) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free") +
    ggplot2::labs(
      x = "Metric value",
      y = "Density",
      fill = "Metric",
      title = "Metric Density Overview"
    ) +
    ggplot2::theme_minimal()

  data.table::setnames(overall, c(".grz_cohort", ".grz_metric"), c("cohort", "metric"))
  data.table::setnames(hourly, c(".grz_cohort", ".grz_metric", ".grz_hour"), c("cohort", "metric", "hour"))

  list(
    overall = grz_as_output(overall, rc),
    hourly = grz_as_output(hourly, rc),
    plots = list(
      hourly_boxplot = p_box,
      hourly_band = p_band,
      density = p_density
    )
  )
}

#' Classify behavior states from movement metrics
#'
#' Classifies each row into `rest`, `graze`, or `travel` using transparent rule
#' thresholds.
#'
#' @param data Input data containing GPS rows.
#' @param method Classification method. Currently only `"rules"` is supported.
#' @param state_col Name of output state column.
#' @param groups Grouping columns used for run-order and optional smoothing.
#' @param rest_speed_max Maximum speed for `rest` state (m/s).
#' @param rest_step_max Maximum step distance for `rest` state (m).
#' @param graze_speed_max Maximum speed for `graze` state (m/s).
#' @param travel_speed_min Minimum speed for `travel` state (m/s).
#' @param travel_turn_max Maximum absolute turning angle for `travel` (radians).
#' @param min_run_n Optional run-length smoothing threshold. Runs shorter than
#'   this value are replaced by neighbouring states.
#' @param verbose Logical; print summary counts.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended state column.
#' @export
grz_classify_behavior <- function(
  data,
  method = c("rules"),
  state_col = "behavior_state",
  groups = NULL,
  rest_speed_max = 0.05,
  rest_step_max = 5,
  graze_speed_max = 0.60,
  travel_speed_min = 0.60,
  travel_turn_max = 0.60,
  min_run_n = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  rc <- grz_match_output_class(return_class)
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  if (method != "rules") {
    stop("Only `method = \"rules\"` is currently supported.", call. = FALSE)
  }

  dt[, (state_col) := data.table::fifelse(
    is.na(speed_mps),
    NA_character_,
    data.table::fifelse(
      speed_mps <= rest_speed_max & (is.na(step_m) | step_m <= rest_step_max),
      "rest",
      data.table::fifelse(
        speed_mps >= travel_speed_min & (is.na(turn_rad) | abs(turn_rad) <= travel_turn_max),
        "travel",
        data.table::fifelse(speed_mps <= graze_speed_max, "graze", "travel")
      )
    )
  )]

  if (as.integer(min_run_n) > 1L) {
    dt[, (state_col) := grz_smooth_state_runs(get(state_col), min_run_n = as.integer(min_run_n)), by = grp]
  }

  attr(dt, "behavior_method") <- method
  attr(dt, "behavior_thresholds") <- list(
    rest_speed_max = rest_speed_max,
    rest_step_max = rest_step_max,
    graze_speed_max = graze_speed_max,
    travel_speed_min = travel_speed_min,
    travel_turn_max = travel_turn_max,
    min_run_n = as.integer(min_run_n)
  )

  if (isTRUE(verbose)) {
    counts <- dt[, .N, by = state_col][order(-N)]
    msg <- paste(paste0(counts[[state_col]], "=", counts$N), collapse = ", ")
    cat("[classify_behavior] ", msg, "\n", sep = "")
  }

  grz_as_output(dt, rc)
}

#' Plot diurnal state proportions
#'
#' Visualises hour-of-day state composition after behavior classification.
#'
#' @param data Input data with GPS rows and state column.
#' @param state_col Behavior-state column name.
#' @param group_col Optional grouping column for per-group facets.
#' @param cohort_col Optional cohort column for faceting.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param plot_type Plot type (`"line"` or `"heatmap"`).
#' @param return_data Logical; return plotting data with plot.
#'
#' @return A ggplot object, or list with `plot` and `data` when
#'   `return_data = TRUE`.
#' @export
grz_plot_diurnal_states <- function(
  data,
  state_col = "behavior_state",
  group_col = NULL,
  cohort_col = NULL,
  tz_local = "UTC",
  plot_type = c("line", "heatmap"),
  return_data = FALSE
) {
  grz_require_ggplot2("grz_plot_diurnal_states()")
  plot_type <- match.arg(plot_type)

  dt <- data.table::copy(data.table::as.data.table(data))
  if (!state_col %in% names(dt)) {
    dt <- data.table::as.data.table(
      grz_classify_behavior(dt, state_col = state_col, verbose = FALSE, return_class = "data.table")
    )
  }

  grz_require_cols(dt, c("datetime", state_col), fun_name = "grz_plot_diurnal_states()")
  dt[, datetime := grz_parse_datetime_utc(datetime)]
  dt <- dt[!is.na(datetime) & !is.na(get(state_col)) & trimws(as.character(get(state_col))) != ""]
  if (nrow(dt) == 0L) {
    stop("No valid rows available for state plotting.", call. = FALSE)
  }

  if (is.null(group_col)) {
    dt[, .grz_group := "all"]
  } else {
    if (!group_col %in% names(dt)) {
      stop("`group_col` not found in data: ", group_col, call. = FALSE)
    }
    dt[, .grz_group := as.character(get(group_col))]
  }

  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    if (!cohort_col %in% names(dt)) {
      stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
    }
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }

  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, .grz_state := as.character(get(state_col))]

  agg <- dt[, .N, by = .(.grz_cohort, .grz_group, .grz_hour, .grz_state)]
  agg[, prop := N / sum(N), by = .(.grz_cohort, .grz_group, .grz_hour)]

  if (plot_type == "line") {
    p <- ggplot2::ggplot(
      agg,
      ggplot2::aes(x = .grz_hour, y = prop, color = .grz_state, group = .grz_state)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      ggplot2::labs(
        x = paste0("Hour (", tz_local, ")"),
        y = "Proportion of time",
        color = "State",
        title = "Diurnal Behavior State Proportions"
      ) +
      ggplot2::facet_grid(.grz_group ~ .grz_cohort) +
      ggplot2::theme_minimal()
  } else {
    p <- ggplot2::ggplot(
      agg,
      ggplot2::aes(x = .grz_hour, y = .grz_state, fill = prop)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(
        low = "green",
        high = "red"
      ) +
      ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      ggplot2::labs(
        x = paste0("Hour (", tz_local, ")"),
        y = "State",
        fill = "Proportion",
        title = "Diurnal Behavior State Heatmap"
      ) +
      ggplot2::facet_grid(.grz_group ~ .grz_cohort) +
      ggplot2::theme_minimal()
  }

  if (isTRUE(return_data)) {
    return(list(plot = p, data = as.data.frame(agg)))
  }
  p
}

grz_behavior_bouts <- function(data, state_col, groups) {
  dt <- data.table::copy(data)
  data.table::setorderv(dt, c(groups, "datetime"))

  out <- data.table::rbindlist(
    lapply(split(seq_len(nrow(dt)), interaction(dt[, ..groups], drop = TRUE, lex.order = TRUE)), function(idx) {
      sub <- dt[idx, ]
      if (nrow(sub) == 0L) {
        return(NULL)
      }

      st <- as.character(sub[[state_col]])
      r <- rle(st)
      starts <- cumsum(c(1L, head(r$lengths, -1L)))
      ends <- cumsum(r$lengths)

      b <- data.table::data.table(
        state = r$values,
        n_points = r$lengths,
        start_idx = starts,
        end_idx = ends
      )
      b[, start_time := sub$datetime[start_idx]]
      b[, end_time := sub$datetime[end_idx]]
      b[, duration_mins := as.numeric(end_time - start_time, units = "mins")]

      for (g in groups) {
        b[[g]] <- sub[[g]][1L]
      }
      b
    }),
    use.names = TRUE,
    fill = TRUE
  )
  out[]
}

#' Validate behavior-state assignments
#'
#' Generates first-pass validation diagnostics for state assignments:
#' distribution summaries, transitions, bout statistics, and optional PCA view.
#'
#' @param data Input data with movement features and state labels.
#' @param state_col Behavior-state column name.
#' @param groups Grouping columns for transitions and bouts.
#' @param feature_cols Feature columns used in diagnostics and PCA.
#' @param pca Logical; compute PCA diagnostic.
#' @param pca_max_n Maximum rows used for PCA (random sample).
#' @param seed Random seed used for PCA sampling.
#' @param verbose Logical; print validation summary.
#' @param return_class Table output class (`"data.frame"` or `"data.table"`).
#'
#' @return A list of validation tables and optional PCA objects/plot.
#' @export
grz_validate_behavior <- function(
  data,
  state_col = "behavior_state",
  groups = NULL,
  feature_cols = c("speed_mps", "step_m", "turn_rad"),
  pca = TRUE,
  pca_max_n = 50000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  if (!state_col %in% names(dt)) {
    dt <- data.table::as.data.table(
      grz_classify_behavior(
        dt,
        state_col = state_col,
        groups = grp,
        verbose = FALSE,
        return_class = "data.table"
      )
    )
  }

  missing_feat <- setdiff(feature_cols, names(dt))
  if (length(missing_feat) > 0L) {
    stop("Missing feature columns: ", paste(missing_feat, collapse = ", "), call. = FALSE)
  }

  dt[, (state_col) := as.character(get(state_col))]
  use <- dt[!is.na(get(state_col)) & trimws(get(state_col)) != ""]
  if (nrow(use) == 0L) {
    stop("No non-missing behavior states available for validation.", call. = FALSE)
  }
  use[, .grz_state := as.character(get(state_col))]

  state_counts <- use[, .N, by = .grz_state][order(-N)]
  data.table::setnames(state_counts, ".grz_state", "state")

  long <- data.table::melt(
    use,
    id.vars = ".grz_state",
    measure.vars = feature_cols,
    variable.name = "feature",
    value.name = "value"
  )
  feature_summary <- long[is.finite(value), .(
    n = .N,
    mean = mean(value, na.rm = TRUE),
    median = stats::median(value, na.rm = TRUE),
    sd = if (.N >= 2L) stats::sd(value, na.rm = TRUE) else NA_real_,
    p10 = grz_quantile_or_na(value, 0.10),
    p90 = grz_quantile_or_na(value, 0.90)
  ), by = .(state = .grz_state, feature)]

  data.table::setorderv(use, c(grp, "datetime"))
  use[, .grz_prev_state := shift(.grz_state), by = grp]
  transitions <- use[!is.na(.grz_prev_state), .N, by = .(from = .grz_prev_state, to = .grz_state)]
  transitions[, prop_from := N / sum(N), by = from]

  bouts <- grz_behavior_bouts(use, state_col = ".grz_state", groups = grp)
  bout_summary <- bouts[!is.na(state), .(
    n_bouts = .N,
    median_bout_mins = stats::median(duration_mins, na.rm = TRUE),
    p90_bout_mins = grz_quantile_or_na(duration_mins, 0.90),
    short_bout_prop = grz_mean_or_na(duration_mins <= 10)
  ), by = state]

  pca_model <- NULL
  pca_scores <- NULL
  pca_plot <- NULL
  if (isTRUE(pca)) {
    pcs <- use[, c(state_col, feature_cols), with = FALSE]
    pcs <- pcs[stats::complete.cases(pcs), ]
    if (nrow(pcs) > as.integer(pca_max_n)) {
      set.seed(seed)
      pcs <- pcs[sample.int(nrow(pcs), as.integer(pca_max_n)), ]
    }
    if (nrow(pcs) >= 3L && length(feature_cols) >= 2L) {
      m <- as.matrix(pcs[, ..feature_cols])
      pca_model <- stats::prcomp(m, center = TRUE, scale. = TRUE)
      pca_scores <- data.table::data.table(
        state = pcs[[state_col]],
        PC1 = pca_model$x[, 1L],
        PC2 = pca_model$x[, 2L]
      )

      if (requireNamespace("ggplot2", quietly = TRUE)) {
        pca_plot <- ggplot2::ggplot(
          pca_scores,
          ggplot2::aes(x = PC1, y = PC2, color = state)
        ) +
          ggplot2::geom_point(alpha = 0.5, size = 1.2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Behavior-State PCA",
            x = "PC1",
            y = "PC2",
            color = "State"
          )
      }
    }
  }

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[validate_behavior] states=%s transitions=%s bouts=%s pca=%s\n",
        format(nrow(state_counts), big.mark = ","),
        format(nrow(transitions), big.mark = ","),
        format(nrow(bouts), big.mark = ","),
        ifelse(is.null(pca_model), "no", "yes")
      )
    )
  }

  list(
    state_counts = grz_as_output(state_counts, rc),
    feature_summary = grz_as_output(feature_summary, rc),
    transitions = grz_as_output(transitions, rc),
    bouts = grz_as_output(bouts, rc),
    bout_summary = grz_as_output(bout_summary, rc),
    pca_model = pca_model,
    pca_scores = if (is.null(pca_scores)) NULL else grz_as_output(pca_scores, rc),
    pca_plot = pca_plot
  )
}

#' Behavior interpretation pipeline
#'
#' End-to-end helper for iterative behavior interpretation:
#' diurnal metric plot -> classify states -> diurnal state plot -> validation.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used in movement/state calculations.
#' @param cohort_col Optional cohort column used in faceted plots.
#' @param metrics Metrics for diurnal metric visualisation.
#' @param classification_method Behavior classification method.
#' @param threshold_guide Logical; compute threshold guidance summaries and
#'   plots.
#' @param metrics_plot Logical; generate diurnal metric plot.
#' @param states_plot Logical; generate diurnal state plot.
#' @param validate Logical; run behavior validation.
#' @param verbose Logical; print step summaries.
#' @param return_class Output class for returned tables.
#'
#' @return A list containing classified data, plots, and validation outputs.
#' @export
grz_behavior_pipeline <- function(
  data,
  groups = NULL,
  cohort_col = NULL,
  metrics = c("step_m", "turn_rad"),
  classification_method = c("rules"),
  threshold_guide = TRUE,
  metrics_plot = TRUE,
  states_plot = TRUE,
  validate = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  classification_method <- match.arg(classification_method)

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  p_metrics <- NULL
  p_states <- NULL
  guide <- NULL
  if (isTRUE(threshold_guide)) {
    guide <- grz_behavior_threshold_guide(
      data = dt,
      metrics = metrics,
      cohort_col = cohort_col,
      return_class = rc
    )
  }
  if (isTRUE(metrics_plot)) {
    p_metrics <- grz_plot_diurnal_metrics(
      data = dt,
      metrics = metrics,
      group_col = "sensor_id",
      cohort_col = cohort_col,
      agg_fun = "median"
    )
  }

  classified <- grz_classify_behavior(
    data = dt,
    method = classification_method,
    groups = grp,
    verbose = verbose,
    return_class = "data.table"
  )

  if (isTRUE(states_plot)) {
    p_states <- grz_plot_diurnal_states(
      data = classified,
      state_col = "behavior_state",
      group_col = NULL,
      cohort_col = cohort_col,
      plot_type = "line"
    )
  }

  v <- NULL
  if (isTRUE(validate)) {
    v <- grz_validate_behavior(
      data = classified,
      state_col = "behavior_state",
      groups = grp,
      verbose = verbose,
      return_class = rc
    )
  }

  list(
    data = grz_as_output(classified, rc),
    threshold_guide = guide,
    plots = list(
      diurnal_metrics = p_metrics,
      diurnal_states = p_states,
      pca = if (is.null(v)) NULL else v$pca_plot
    ),
    validation = v
  )
}
