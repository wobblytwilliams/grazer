grz_test_data_path <- function() {
  normalizePath(
    file.path(testthat::test_path("..", ".."), "test_data", "ManbullooToSimulate.csv"),
    winslash = "/",
    mustWork = TRUE
  )
}

grz_test_sample <- function(n = 5000) {
  grz_read_gps(
    path = grz_test_data_path(),
    n_max = n,
    validate = TRUE,
    drop_invalid = TRUE
  )
}

test_that("grz_remove_duplicates removes duplicate rows", {
  dat <- grz_test_sample(100)
  dup_dat <- data.table::rbindlist(list(dat, dat[1:5, ]), use.names = TRUE, fill = TRUE)

  out <- grz_remove_duplicates(dup_dat)
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), nrow(dat))
  expect_equal(attr(out, "duplicates_removed"), 5)
})

test_that("grz_flag_track_qc adds expected QC fields", {
  dat <- grz_test_sample(200)
  out <- grz_flag_track_qc(dat, max_step_gap_min = 60, speed_cap_ms = 5)

  expect_s3_class(out, "data.table")
  expect_true(all(c(
    "step_dt_s", "step_m", "speed_mps",
    "qc_gap", "qc_nonpositive_dt", "qc_speed_cap", "qc_any"
  ) %in% names(out)))
})

test_that("grz_regularise_grid returns complete grid with match flag", {
  dat <- grz_test_sample(300)
  out <- grz_regularise_grid(dat, base_mins = 30, tol_prop = 0.6)

  expect_s3_class(out, "data.table")
  expect_true(all(c("datetime_original", "snap_diff_s", "grid_match") %in% names(out)))
  expect_true(any(out$grid_match %in% TRUE))
})

test_that("downsampling functions return labelled subsets", {
  dat <- grz_test_sample(600)

  phase <- grz_downsample_phases(dat, base_mins = 15, target_mins = 30, max_phases = 2)
  expect_s3_class(phase, "data.table")
  expect_true(all(c("phase_id", "phase_offset_mins") %in% names(phase)))

  async <- grz_downsample_async(dat, base_mins = 15, target_mins = 30, n_reps = 3, seed = 42)
  expect_s3_class(async, "data.table")
  expect_true("replicate" %in% names(async))
  expect_equal(sort(unique(async$replicate)), c(1, 2, 3))
})

test_that("grz_movement_metrics returns expected metric columns", {
  dat <- grz_test_sample(1000)
  out <- grz_movement_metrics(dat, by_period = c("total", "week"))

  expect_s3_class(out, "data.table")
  expect_true(all(c(
    "period_type", "period", "total_distance_m", "mean_speed_mps",
    "net_displacement_m", "straightness_index"
  ) %in% names(out)))
})

test_that("grz_social_metrics computes social summaries", {
  dat <- grz_test_sample(400)
  a <- data.table::copy(dat[1:200, ])
  b <- data.table::copy(dat[1:200, ])
  b$sensor_id <- "SIM_B"
  b$lon <- b$lon + 0.0003

  social_dat <- data.table::rbindlist(list(a, b), use.names = TRUE, fill = TRUE)
  out <- grz_social_metrics(social_dat, thresholds_m = c(25, 30, 50), interpolate = FALSE)

  expect_s3_class(out, "data.table")
  expect_true(all(c(
    "mean_nn_m", "mean_dist_to_others_m", "CR30",
    "mean_sri30", "top5_mean_sri30", "max_sri30"
  ) %in% names(out)))
  expect_gte(nrow(out), 2)
})

test_that("grz_home_range_metrics returns MCP/KDE draft fields", {
  dat <- grz_test_sample(3000)
  out <- grz_home_range_metrics(dat, by_period = c("date", "week"), min_fixes = 3, min_span_hours = 0)

  expect_s3_class(out, "data.table")
  expect_true(all(c("mcp100_area_ha", "mcp95_area_ha", "kde95_ha", "qc_pass") %in% names(out)))
})

test_that("grz_activity_metrics returns diurnal and routine fields", {
  dat <- grz_test_sample(1000)
  out <- grz_activity_metrics(dat, rest_speed_thresh_ms = 0.05, tz_local = "UTC")

  expect_s3_class(out, "data.table")
  expect_true(all(c(
    "prop_active", "prop_rest", "speed_mean_ms", "routine_consistency_index"
  ) %in% names(out)))
})

test_that("grz_qc_sampling and grz_qc_report return expected structures", {
  dat <- grz_test_sample(1200)

  samp <- grz_qc_sampling(dat, target_mins = 30)
  expect_s3_class(samp, "data.table")
  expect_true(all(c("completeness", "median_interval_mins", "prop_within_target") %in% names(samp)))

  rep <- grz_qc_report(dat, target_mins = 30)
  expect_type(rep, "list")
  expect_true(all(c("overall", "validation", "invalid_rows", "track_summary", "sampling") %in% names(rep)))
  expect_s3_class(rep$track_summary, "data.table")
})

test_that("grz_per_entity_summary merges metric tables", {
  dat <- grz_test_sample(1200)
  mv <- grz_movement_metrics(dat, by_period = "total")
  act <- grz_activity_metrics(dat)

  out <- grz_per_entity_summary(list(mv, act))
  expect_s3_class(out, "data.table")
  expect_true("sensor_id" %in% names(out))
  expect_true(any(grepl("speed_mean_ms", names(out))))
})
