#!/usr/bin/env Rscript

# End-to-end walkthrough for behavior interpretation.
# Run from package root:
#   Rscript scripts/walkthrough_behavior.R

pkgload::load_all(".")

path <- file.path("test_data", "ManbullooToSimulate.csv")
n_max <- 50000

cat("=== 1) Read + Validate ===\n")
raw <- grz_read_gps(path, n_max = n_max, validate = FALSE)
val <- grz_validate(raw, drop_invalid = FALSE, verbose = TRUE)
cat("rows raw:", nrow(raw), " rows validated:", nrow(val), "\n\n")

cat("=== 2) Clean ===\n")
clean <- grz_clean(
  val,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  snapshot = TRUE,
  verbose = TRUE
)
cat("rows clean:", nrow(clean), "\n\n")

cat("=== 3) Distance Density (Check for Two Peaks) ===\n")
mv_for_density <- grz_calculate_movement(clean, verbose = FALSE)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  step_dt <- data.table::as.data.table(mv_for_density)
  step_dt <- step_dt[is.finite(step_m) & step_m >= 0 & !is.na(datetime)]
  step_dt[, hour_utc := as.integer(strftime(datetime, format = "%H", tz = "UTC"))]
  step_dt[, period := data.table::fifelse(hour_utc %in% c(20:23, 0:5), "night", "day")]
  distance_cap <- as.numeric(stats::quantile(step_dt$step_m, probs = 0.99, na.rm = TRUE, names = FALSE, type = 7))

  p_distance <- ggplot2::ggplot(
    step_dt,
    ggplot2::aes(x = step_m, color = period, fill = period)
  ) +
    ggplot2::geom_density(alpha = 0.25, adjust = 1.1) +
    ggplot2::coord_cartesian(xlim = c(0, distance_cap)) +
    ggplot2::labs(
      x = "Step distance (m)",
      y = "Density",
      color = "Period",
      fill = "Period",
      title = "Step Distance Density (99th percentile x-range)"
    ) +
    ggplot2::theme_minimal()
  print(p_distance)

  p_distance_log <- ggplot2::ggplot(
    step_dt[step_m > 0],
    ggplot2::aes(x = step_m, color = period, fill = period)
  ) +
    ggplot2::geom_density(alpha = 0.25, adjust = 1.1) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Step distance (m, log10 scale)",
      y = "Density",
      color = "Period",
      fill = "Period",
      title = "Step Distance Density (Log Scale)"
    ) +
    ggplot2::theme_minimal()
  print(p_distance_log)
}
cat("\n")

cat("=== 4) Threshold Guide (Distance + Turn) ===\n")
guide <- grz_behavior_threshold_guide(
  clean,
  metrics = c("step_m", "turn_rad")
)
cat("overall threshold guide:\n")
print(guide$overall)
cat("\n")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  print(guide$plots$hourly_band)
  print(guide$plots$hourly_boxplot)
  if (!is.null(guide$tuning)) {
    cat("suggested thresholds:\n")
    print(guide$tuning$suggested)
    print(guide$tuning$plots$step_density)
    print(guide$tuning$plots$step_cdf)
    print(guide$tuning$plots$sweep_score)
  }
}

cat("\n=== 5) Classify (HMM Active/Inactive) ===\n")
classed_hmm <- grz_classify_activity_hmm(
  clean,
  min_run_n = 2,
  verbose = TRUE
)
print(prop.table(table(classed_hmm$activity_state_hmm, useNA = "ifany")))
cat("\n")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_hmm_states <- grz_plot_diurnal_states(
    classed_hmm,
    state_col = "activity_state_hmm",
    plot_type = "line"
  )
  print(p_hmm_states)
}

cat("=== 6) Spatial + Consensus Activity ===\n")
classed_consensus <- grz_classify_activity_consensus(
  clean,
  decision_rule = "weighted",
  spatial_radius_m = 20,
  spatial_min_dwell_mins = 20,
  spatial_min_points = 3,
  inactive_threshold = 0.60,
  min_run_n = 2,
  verbose = TRUE
)
print(prop.table(table(classed_consensus$activity_state_consensus, useNA = "ifany")))
cat("\n")

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_consensus_states <- grz_plot_diurnal_states(
    classed_consensus,
    state_col = "activity_state_consensus",
    plot_type = "line"
  )
  print(p_consensus_states)
}

cat("=== 7) Consensus Timeline Map (1 animal, 1 week) ===\n")
if (
  requireNamespace("leaflet", quietly = TRUE) &&
  requireNamespace("leaflet.extras2", quietly = TRUE) &&
  requireNamespace("sf", quietly = TRUE)
) {
  hmm_map <- as.data.frame(classed_consensus, stringsAsFactors = FALSE)
  hmm_map$datetime <- grz_parse_datetime_utc(hmm_map$datetime)
  ids <- unique(hmm_map$sensor_id)
  ids <- ids[!is.na(ids) & trimws(ids) != ""]

  if (length(ids) > 0) {
    target_id <- ids[[1]]
    map_slice <- hmm_map[hmm_map$sensor_id == target_id, , drop = FALSE]
    map_slice <- map_slice[order(map_slice$datetime), , drop = FALSE]
    start_time <- min(map_slice$datetime, na.rm = TRUE)
    end_time <- start_time + as.difftime(7, units = "days")
    map_slice <- map_slice[map_slice$datetime >= start_time & map_slice$datetime < end_time, , drop = FALSE]

    cat(
      "map sensor:", target_id,
      "rows:", nrow(map_slice),
      "from:", format(start_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      "to:", format(end_time, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "\n"
    )

    if (nrow(map_slice) > 0) {
      m_hmm <- grz_map(
        data = map_slice,
        state_col = "activity_state_consensus",
        timeline = TRUE,
        popup_fields = c(
          "sensor_id",
          "datetime",
          "activity_state_consensus",
          "activity_state_hmm",
          "activity_state_spatial",
          "inactive_prob_hmm",
          "inactive_score_consensus"
        ),
        max_points = 5000,
        warnings = FALSE
      )
      print(m_hmm)
    }
  }
} else {
  cat("Skipping timeline map: install `leaflet`, `leaflet.extras2`, and `sf`.\n")
}
cat("\n")

cat("=== 8) Classify (Default Rules) ===\n")
classed_default <- grz_classify_behavior(
  clean,
  rest_speed_max = 0.05,
  rest_step_max = 5,
  graze_speed_max = 0.60,
  travel_speed_min = 0.60,
  travel_turn_max = 0.60,
  min_run_n = 2,
  verbose = TRUE
)
print(prop.table(table(classed_default$behavior_state, useNA = "ifany")))
cat("\n")

cat("=== 9) Validate Classification ===\n")
val_default <- grz_validate_behavior(classed_default, pca = TRUE, verbose = TRUE)
cat("state counts:\n")
print(val_default$state_counts)
cat("\nbout summary:\n")
print(val_default$bout_summary)
cat("\n")

if (!is.null(val_default$pca_plot)) {
  print(val_default$pca_plot)
}

cat("\n=== 10) Diurnal Comparison ===\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_metric <- grz_plot_diurnal_metrics(classed_default, metrics = c("step_m", "turn_rad"))
  p_state <- grz_plot_diurnal_states(classed_default, plot_type = "line")
  print(p_metric)
  print(p_state)
}

cat("\n=== 11) Iteration Example (Alternative Thresholds) ===\n")
classed_alt <- grz_classify_behavior(
  clean,
  rest_speed_max = 0.08,
  rest_step_max = 7,
  graze_speed_max = 0.70,
  travel_speed_min = 0.70,
  travel_turn_max = 0.75,
  min_run_n = 2,
  verbose = FALSE
)

state_prop <- function(x, run_id) {
  tb <- prop.table(table(x$behavior_state, useNA = "no"))
  data.frame(
    run = run_id,
    state = names(tb),
    prop = as.numeric(tb),
    stringsAsFactors = FALSE
  )
}

compare <- rbind(
  state_prop(classed_default, "default"),
  state_prop(classed_alt, "alternative")
)
print(compare)
cat("\n")

cat("=== 12) Wrapper Pipeline ===\n")
bp <- grz_behavior_pipeline(
  clean,
  threshold_guide = TRUE,
  metrics_plot = TRUE,
  states_plot = TRUE,
  validate = TRUE,
  verbose = TRUE
)

cat("pipeline data rows:", nrow(bp$data), "\n")
cat("pipeline states:\n")
print(bp$validation$state_counts)

if (!is.null(bp$plots$diurnal_metrics)) print(bp$plots$diurnal_metrics)
if (!is.null(bp$plots$diurnal_states)) print(bp$plots$diurnal_states)
if (!is.null(bp$plots$pca)) print(bp$plots$pca)

cat("\nWalkthrough complete.\n")
