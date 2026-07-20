test_that("gps_activity_state adds public activity state columns", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    datetime = t0 + seq(0, 59) * 60,
    lon = 150 + c(rep(0, 30), seq(0, 0.01, length.out = 30)),
    lat = -30 + c(rep(0, 30), seq(0, 0.01, length.out = 30)),
    stringsAsFactors = FALSE
  )

  out <- gps_activity_state(
    dat,
    method = "gmm",
    feature_set = "step_turn",
    fit_max_rows = 100,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(all(c("activity_state", "inactive_prob") %in% names(out)))
  expect_true(any(out$activity_state %in% c("active", "inactive"), na.rm = TRUE))
  expect_null(attr(out, "gmm_activity_model", exact = TRUE))
  expect_null(attr(out, "hmm_activity_model", exact = TRUE))

  model <- attr(out, "gps_activity_state", exact = TRUE)
  expect_equal(model$method, "gmm")
  expect_equal(model$feature_set, "step_turn")
  expect_true(all(c("step_m", "turn_rad") %in% model$features))
})

test_that("gps_activity_state supports GMM-HMM and direct HMM methods", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    datetime = t0 + seq(0, 79) * 60,
    lon = 150 + c(rep(0, 40), seq(0, 0.012, length.out = 40)),
    lat = -30 + c(rep(0, 40), seq(0, 0.012, length.out = 40)),
    stringsAsFactors = FALSE
  )

  gmm_hmm <- gps_activity_state(
    dat,
    method = "gmm_hmm",
    feature_set = "step_turn",
    fit_max_rows = 100,
    verbose = FALSE
  )
  hmm <- gps_activity_state(
    dat,
    method = "hmm",
    feature_set = "step_turn",
    fit_max_rows = 100,
    verbose = FALSE
  )

  expect_true(all(c("activity_state", "inactive_prob", "activity_state_component") %in% names(gmm_hmm)))
  expect_true(all(c("activity_state", "inactive_prob", "activity_state_id") %in% names(hmm)))
  expect_equal(attr(gmm_hmm, "gps_activity_state", exact = TRUE)$method, "gmm_hmm")
  expect_equal(attr(hmm, "gps_activity_state", exact = TRUE)$method, "hmm")
})
