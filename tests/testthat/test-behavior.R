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
