grz_behavior_test_data <- function(n = 1500) {
  path <- normalizePath(
    file.path(testthat::test_path("..", ".."), "test_data", "ManbullooToSimulate.csv"),
    winslash = "/",
    mustWork = TRUE
  )
  grz_read_gps(path, n_max = n, validate = TRUE, drop_invalid = TRUE)
}

test_that("grz_classify_behavior appends behavior state", {
  dat <- grz_behavior_test_data(1200)
  out <- grz_classify_behavior(dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_true("behavior_state" %in% names(out))
  expect_true(all(na.omit(unique(out$behavior_state)) %in% c("rest", "graze", "travel")))
})

test_that("grz_classify_activity_hmm appends active/inactive states", {
  dat <- grz_behavior_test_data(1500)
  out <- grz_classify_activity_hmm(dat, fit_max_rows = 1200, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("activity_state_hmm", "activity_state_id_hmm", "inactive_prob_hmm") %in% names(out)))
  expect_true(all(na.omit(unique(out$activity_state_hmm)) %in% c("inactive", "active")))
  probs <- out$inactive_prob_hmm[is.finite(out$inactive_prob_hmm)]
  expect_true(all(probs >= 0 & probs <= 1))
  expect_true(!is.null(attr(out, "hmm_activity_model")))
})

test_that("grz_classify_activity_spatial appends active/inactive states", {
  dat <- grz_behavior_test_data(1500)
  out <- grz_classify_activity_spatial(
    dat,
    radius_m = 25,
    min_dwell_mins = 15,
    min_points = 3,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("activity_state_spatial", "activity_cluster_spatial", "activity_dwell_mins_spatial") %in% names(out)))
  expect_true(all(na.omit(unique(out$activity_state_spatial)) %in% c("inactive", "active")))
})

test_that("grz_classify_activity_consensus combines hmm and spatial", {
  dat <- grz_behavior_test_data(1500)
  out <- grz_classify_activity_consensus(
    dat,
    spatial_radius_m = 25,
    spatial_min_dwell_mins = 15,
    spatial_min_points = 3,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c(
    "activity_state_hmm",
    "activity_state_spatial",
    "activity_state_consensus",
    "inactive_score_consensus"
  ) %in% names(out)))
  expect_true(all(na.omit(unique(out$activity_state_consensus)) %in% c("inactive", "active")))
  score <- out$inactive_score_consensus[is.finite(out$inactive_score_consensus)]
  expect_true(all(score >= 0 & score <= 1))
})

test_that("diurnal plotting functions return ggplot objects", {
  skip_if_not_installed("ggplot2")
  dat <- grz_behavior_test_data(1000)
  classed <- grz_classify_behavior(dat, verbose = FALSE)

  p1 <- grz_plot_diurnal_metrics(
    classed,
    metrics = c("speed_mps", "step_m", "turn_rad"),
    agg_fun = "median"
  )
  expect_s3_class(p1, "ggplot")

  p2 <- grz_plot_diurnal_states(classed, plot_type = "line")
  expect_s3_class(p2, "ggplot")
})

test_that("grz_validate_behavior returns core validation tables", {
  dat <- grz_behavior_test_data(1500)
  classed <- grz_classify_behavior(dat, verbose = FALSE)
  val <- grz_validate_behavior(classed, verbose = FALSE, pca = FALSE)

  expect_type(val, "list")
  expect_true(all(c("state_counts", "feature_summary", "transitions", "bouts", "bout_summary") %in% names(val)))
  expect_s3_class(val$state_counts, "data.frame")
  expect_gt(nrow(val$state_counts), 0)
})

test_that("grz_behavior_threshold_guide returns summaries and plots", {
  skip_if_not_installed("ggplot2")
  dat <- grz_behavior_test_data(1200)
  guide <- grz_behavior_threshold_guide(dat, metrics = c("step_m", "turn_rad"))

  expect_type(guide, "list")
  expect_true(all(c("overall", "hourly", "plots") %in% names(guide)))
  expect_s3_class(guide$overall, "data.frame")
  expect_s3_class(guide$hourly, "data.frame")
  expect_s3_class(guide$plots$hourly_boxplot, "ggplot")
  expect_s3_class(guide$plots$hourly_band, "ggplot")
  expect_s3_class(guide$plots$density, "ggplot")
  expect_type(guide$tuning, "list")
  expect_s3_class(guide$tuning$suggested, "data.frame")
  expect_s3_class(guide$tuning$sweep, "data.frame")
  expect_s3_class(guide$tuning$plots$step_density, "ggplot")
  expect_s3_class(guide$tuning$plots$step_cdf, "ggplot")
  expect_s3_class(guide$tuning$plots$sweep_score, "ggplot")
  step_vals <- guide$plots$hourly_boxplot$data[guide$plots$hourly_boxplot$data$.grz_metric == "step_m", ".grz_value"]
  expect_lte(max(step_vals, na.rm = TRUE), 3000)
})

test_that("grz_tune_thresholds returns suggested cutoffs and sweep", {
  skip_if_not_installed("ggplot2")
  dat <- grz_behavior_test_data(1200)
  out <- grz_tune_thresholds(
    dat,
    rest_step_grid = seq(3, 9, by = 2),
    rest_speed_grid = seq(0.03, 0.09, by = 0.03),
    max_rows = 1000,
    verbose = FALSE
  )

  expect_type(out, "list")
  expect_s3_class(out$suggested, "data.frame")
  expect_s3_class(out$sweep, "data.frame")
  expect_s3_class(out$best, "data.frame")
  expect_true("rest_step_max" %in% names(out$suggested))
  expect_true("rest_speed_max" %in% names(out$sweep))
  expect_s3_class(out$plots$step_density, "ggplot")
  expect_s3_class(out$plots$sweep_score, "ggplot")
})

test_that("grz_behavior_pipeline runs end-to-end", {
  dat <- grz_behavior_test_data(1500)
  out <- grz_behavior_pipeline(
    dat,
    threshold_guide = TRUE,
    metrics_plot = FALSE,
    states_plot = FALSE,
    validate = TRUE,
    verbose = FALSE
  )

  expect_type(out, "list")
  expect_true(all(c("data", "threshold_guide", "plots", "validation") %in% names(out)))
  expect_true("behavior_state" %in% names(out$data))
  expect_type(out$plots, "list")
  expect_type(out$threshold_guide, "list")
})
