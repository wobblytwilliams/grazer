test_that("gps_steps adds row-level movement fields", {
  out <- gps_steps(gps_fixture()[1:3, ], verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_true(all(c(
    "step_dt_s",
    "step_m",
    "speed_mps",
    "bearing_deg",
    "turn_rad",
    "cum_distance_m",
    "net_displacement_m"
  ) %in% names(out)))
  expect_equal(out$step_dt_s, c(NA, 60, 60))
  expect_true(is.na(out$speed_mps[1]))
  expect_true(out$cum_distance_m[3] > out$cum_distance_m[2])
})

test_that("gps_steps calculates known step distances and turning angles", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    animal_id = "cow_1",
    treatment = "shade",
    datetime = t0 + c(0, 60, 120),
    lon = c(0, 0, 0.001),
    lat = c(0, 0.001, 0.001),
    stringsAsFactors = FALSE
  )

  out <- gps_steps(dat, groups = c("animal_id", "sensor_id"), verbose = FALSE)

  expect_equal(out$animal_id, rep("cow_1", 3))
  expect_equal(out$treatment, rep("shade", 3))
  expect_equal(out$step_dt_s, c(NA, 60, 60))
  expect_equal(out$step_m[2], 111.195, tolerance = 0.2)
  expect_equal(out$step_m[3], 111.195, tolerance = 0.2)
  expect_equal(out$bearing_deg[2], 0, tolerance = 0.1)
  expect_equal(out$bearing_deg[3], 90, tolerance = 0.1)
  expect_equal(out$turn_rad[3], pi / 2, tolerance = 0.01)
})

test_that("gps_steps keeps streams independent", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")

  out <- gps_steps(dat, groups = c("animal_id", "sensor_id"), verbose = FALSE)
  first_rows <- out[!duplicated(out[c("animal_id", "sensor_id")]), ]

  expect_equal(nrow(first_rows), 2)
  expect_true(all(is.na(first_rows$step_m)))
  expect_true(all(is.na(first_rows$speed_mps)))
})

test_that("gps_steps suppresses gap steps and carries cumulative distance across segments", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    segment_id = c("A_seg001", "A_seg001", "A_seg002", "A_seg002"),
    datetime = t0 + c(0, 60, 3600, 3660),
    lon = c(0, 0.001, 1, 1.001),
    lat = 0,
    stringsAsFactors = FALSE
  )

  out <- gps_steps(dat, groups = "segment_id", verbose = FALSE)

  expect_true(is.na(out$step_m[3]))
  expect_true(is.na(out$speed_mps[3]))
  expect_true(is.na(out$bearing_deg[3]))
  expect_equal(out$cum_distance_m[3], out$cum_distance_m[2])
  expect_equal(out$cum_distance_m[4], out$cum_distance_m[2] + out$step_m[4])
  expect_equal(out$net_displacement_m[c(1, 3)], c(0, 0))
})

test_that("gps_turning can return degrees", {
  out <- gps_turning(gps_fixture()[1:4, ], unit = "degrees", verbose = FALSE)

  expect_true("turn_deg" %in% names(out))
  expect_false("turn_rad" %in% names(out))
  expect_true(any(is.finite(out$turn_deg)))
})

test_that("gps_movement_summary summarises fixed-duration epochs", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    animal_id = "cow_1",
    treatment = "shade",
    datetime = t0 + c(0, 60, 120),
    lon = c(0, 0, 0.001),
    lat = c(0, 0.001, 0.001),
    stringsAsFactors = FALSE
  )

  out <- gps_movement_summary(dat, epoch_mins = 10, verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$sensor_id, "A")
  expect_equal(out$animal_id, "cow_1")
  expect_equal(out$treatment, "shade")
  expect_equal(out$n_fixes, 3)
  expect_equal(out$n_steps, 2)
  expect_equal(out$total_distance_m, 222.39, tolerance = 0.4)
  expect_equal(out$mean_speed_mps, out$total_distance_m / 120, tolerance = 0.01)
  expect_true(out$net_displacement_m < out$total_distance_m)
  expect_true(out$straightness_index > 0.6)
  expect_true(out$straightness_index < 0.8)
})

test_that("gps_movement_summary summarises multiple sensors independently", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")

  out <- gps_movement_summary(dat, epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 2)
  expect_setequal(out$sensor_id, c("A", "B"))
  expect_setequal(out$animal_id, c("cow_1", "cow_2"))
  expect_true(all(out$n_fixes == 3))
  expect_true(all(out$n_steps == 2))
})

test_that("gps_movement_summary documents the current daily summary columns", {
  out <- gps_movement_summary(gps_fixture(), epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$sensor_id, "A")
  expect_equal(out$epoch, "2024-01-01")
  expect_equal(out$n_fixes, 6)
  expect_true(all(c(
    "total_distance_m",
    "mean_step_m",
    "mean_speed_mps",
    "mean_abs_turn_rad"
  ) %in% names(out)))
})

test_that("gps_movement_summary uses segment_id internally when present", {
  dat <- gps_fixture()
  dat$datetime[5] <- dat$datetime[4] + 3 * 3600
  segmented <- gps_append_segments(dat, large_gap_mins = 30, verbose = FALSE)

  out <- gps_movement_summary(segmented, epoch = "day", verbose = FALSE)
  by_segment <- gps_movement_summary(segmented, epoch = "day", groups = c("deployment_id", "sensor_id", "segment_id"), verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_false("segment_id" %in% names(out))
  expect_true(sum(by_segment$total_distance_m, na.rm = TRUE) >= out$total_distance_m)
  expect_equal(out$n_steps, sum(by_segment$n_steps, na.rm = TRUE))
})

test_that("gps_social counts neighbours at shared timestamps", {
  out <- gps_social(
    gps_two_sensor_fixture(),
    thresholds_m = 20,
    interpolate = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(out), 6)
  expect_true(all(out$social_group_size == 2))
  expect_true(all(out$n_neighbours_within_20m == 1))
  expect_true(all(out$any_neighbour_within_20m))
})

test_that("gps_social can align internally when requested", {
  out <- gps_social(
    gps_two_sensor_fixture(),
    thresholds_m = 20,
    align_interval_mins = 1,
    interpolate = TRUE,
    verbose = FALSE
  )

  expect_equal(nrow(out), 6)
})

test_that("gps_social_summary documents the current daily social summary", {
  out <- gps_social_summary(
    gps_two_sensor_fixture(),
    epoch = "day",
    verbose = FALSE
  )

  expect_equal(nrow(out), 2)
  expect_setequal(out$sensor_id, c("A", "B"))
  expect_true(all(c(
    "mean_nearest_neighbour_m",
    "median_nearest_neighbour_m",
    "mean_n_neighbours_within_25m"
  ) %in% names(out)))
  expect_true(all(out$n_fixes == 3))
})

test_that("gps_spatial returns epoch-level area fields", {
  skip_if_not_installed("sf")

  out <- gps_spatial(
    gps_fixture(),
    epoch = "day",
    min_fixes = 3,
    verbose = FALSE
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$n_fixes, 6)
  expect_equal(out$span_hours, 5 / 60, tolerance = 1e-12)
  expect_true(is.finite(out$mcp100_area_ha))
  expect_true(out$mcp100_area_ha > 0)
})

test_that("gps_epoch merges selected metric blocks", {
  skip_if_not_installed("sf")

  out <- gps_epoch(
    gps_fixture(),
    epoch = "day",
    include = c("movement", "spatial"),
    verbose = FALSE
  )

  expect_equal(nrow(out), 1)
  expect_true(all(c("total_distance_m", "mcp100_area_ha") %in% names(out)))
})
