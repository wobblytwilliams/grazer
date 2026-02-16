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

cat("=== 3) Threshold Guide (Distance + Turn) ===\n")
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
}

cat("\n=== 4) Classify (Default Rules) ===\n")
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

cat("=== 5) Validate Classification ===\n")
val_default <- grz_validate_behavior(classed_default, pca = TRUE, verbose = TRUE)
cat("state counts:\n")
print(val_default$state_counts)
cat("\nbout summary:\n")
print(val_default$bout_summary)
cat("\n")

if (!is.null(val_default$pca_plot)) {
  print(val_default$pca_plot)
}

cat("\n=== 6) Diurnal Comparison ===\n")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_metric <- grz_plot_diurnal_metrics(classed_default, metrics = c("step_m", "turn_rad"))
  p_state <- grz_plot_diurnal_states(classed_default, plot_type = "line")
  print(p_metric)
  print(p_state)
}

cat("\n=== 7) Iteration Example (Alternative Thresholds) ===\n")
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

cat("=== 8) Wrapper Pipeline ===\n")
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
