test_that("gps_interpolate fills a regular time grid and flags observed fixes", {
  dat <- gps_fixture()[c(1, 3), ]

  out <- gps_interpolate(dat, interval_mins = 1, verbose = FALSE)

  expect_equal(nrow(out), 3)
  expect_equal(as.numeric(diff(out$datetime), units = "mins"), c(1, 1))
  expect_equal(out$is_observed, c(TRUE, FALSE, TRUE))
  expect_equal(out$is_interpolated, c(FALSE, TRUE, FALSE))
  expect_equal(out$lon[2], mean(dat$lon), tolerance = 1e-10)
  expect_equal(out$lat[2], mean(dat$lat), tolerance = 1e-10)
})

test_that("gps_downsample keeps the first fix per window in rigid mode", {
  dat <- gps_fixture()[1:5, ]

  out <- gps_downsample(dat, target_mins = 2, method = "rigid", verbose = FALSE)

  expect_equal(nrow(out), 3)
  expect_equal(
    as.numeric(out$datetime - min(dat$datetime), units = "mins"),
    c(0, 2, 4)
  )
})

test_that("gps_downsample validates the target interval", {
  expect_error(
    gps_downsample(gps_fixture(), target_mins = 0, verbose = FALSE),
    "`target_mins` must be a positive number",
    fixed = TRUE
  )
})

test_that("gps_regularise creates expected fixes for a single animal", {
  dat <- gps_fixture()[c(1, 3, 5), ]
  dat$animal_id <- "cow_1"
  dat$treatment <- "shade"

  out <- gps_regularise(dat, interval_mins = 1, verbose = FALSE)
  diag <- attr(out, "gps_reg")

  expect_equal(nrow(out), 5)
  expect_equal(out$is_observed, c(TRUE, FALSE, TRUE, FALSE, TRUE))
  expect_true(all(c("observed_datetime", "time_offset_s") %in% names(out)))
  expect_true(all(out$animal_id == "cow_1"))
  expect_true(all(out$treatment == "shade"))
  expect_equal(diag$n_expected_fixes, 5)
  expect_equal(diag$n_observed_fixes, 3)
  expect_equal(diag$n_interpolated, 0)
  expect_equal(diag$sampling_interval_achieved_s, 60)
})

test_that("gps_interpolate fills missing expected fixes within groups", {
  dat <- gps_fixture()[c(1, 3), ]

  out <- gps_interpolate(dat, interval_mins = 1, verbose = FALSE)
  diag <- attr(out, "gps_reg")

  expect_equal(nrow(out), 3)
  expect_equal(out$is_observed, c(TRUE, FALSE, TRUE))
  expect_equal(out$is_interpolated, c(FALSE, TRUE, FALSE))
  expect_equal(out$interpolation_gap_s[2], 120)
  expect_equal(out$lon[2], mean(dat$lon), tolerance = 1e-10)
  expect_equal(out$lat[2], mean(dat$lat), tolerance = 1e-10)
  expect_equal(diag$n_interpolated, 1)
  expect_equal(diag$prop_interpolated, 1 / 3)
  expect_true(all(c(
    "n_valid_anchor_timestamps",
    "n_invalid_coordinate_fixes",
    "n_unfilled_grid_fixes"
  ) %in% names(diag)))
  expect_false(any(c(
    "tolerance_mins",
    "n_unmatched_observed_fixes",
    "median_abs_time_offset_s",
    "max_abs_time_offset_s"
  ) %in% names(diag)))
})

test_that("gps_interpolate does not expose a maximum gap argument", {
  expect_false("max_gap_mins" %in% names(formals(gps_interpolate)))
})

test_that("time alignment functions respect segment_id by default", {
  dat <- gps_fixture()[c(1, 3), ]
  dat$segment_id <- c("A_seg001", "A_seg002")

  regular <- gps_regularise(dat, interval_mins = 1, verbose = FALSE)
  interp <- gps_interpolate(dat, interval_mins = 1, verbose = FALSE)
  down <- gps_downsample(dat, target_mins = 2, verbose = FALSE)

  expect_equal(nrow(regular), 2)
  expect_equal(sum(interp$is_interpolated), 0)
  expect_equal(nrow(attr(regular, "gps_reg")), 2)
  expect_equal(nrow(attr(interp, "gps_reg")), 2)
  expect_equal(nrow(attr(down, "gps_reg")), 2)
})

test_that("gps_regularise handles multiple animals independently", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")
  dat <- dat[dat$datetime != dat$datetime[2] | dat$sensor_id == "A", ]

  out <- gps_regularise(dat, interval_mins = 1, verbose = FALSE)
  diag <- attr(out, "gps_reg")

  expect_equal(nrow(out[out$sensor_id == "A", ]), 3)
  expect_equal(nrow(out[out$sensor_id == "B", ]), 3)
  expect_equal(sum(out$sensor_id == "B" & !out$is_observed), 1)
  expect_equal(nrow(diag), 2)
  expect_setequal(diag$animal_id, c("cow_1", "cow_2"))
})

test_that("gps_regularise reports missing timestamps without requiring private data", {
  dat <- gps_fixture()[1:3, ]
  dat$datetime[2] <- NA

  out <- gps_regularise(dat, interval_mins = 1, verbose = FALSE)
  diag <- attr(out, "gps_reg")

  expect_equal(nrow(out), 3)
  expect_equal(diag$n_observed_fixes, 2)
  expect_equal(diag$n_missing_datetime, 1)
})

test_that("gps_regularise snaps while gps_interpolate evaluates raw fixes at grid times", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    datetime = t0 + c(0, 90, 240),
    lon = c(150, 150.0003, 150.0008),
    lat = c(-30, -30.0003, -30.0008),
    stringsAsFactors = FALSE
  )

  regular <- gps_regularise(dat, interval_mins = 1, verbose = FALSE)
  interp <- gps_interpolate(dat, interval_mins = 1, verbose = FALSE)

  expect_equal(as.numeric(regular$datetime - t0, units = "secs"), c(0, 60, 120, 180, 240))
  expect_equal(sum(regular$is_observed), 3)
  expect_equal(regular$observed_datetime[3], dat$datetime[2])
  expect_equal(regular$time_offset_s[3], -30)
  expect_equal(sum(interp$is_observed), 2)
  expect_equal(sum(interp$is_interpolated), 3)
  expect_true(all(is.finite(interp$lon)))
  expect_equal(interp$lon[3], 150.0004, tolerance = 1e-10)
  expect_false("tolerance_mins" %in% names(formals(gps_interpolate)))
})

test_that("gps_interpolate aligns asynchronous collars to a common phase", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = rep(c("A", "B"), each = 3),
    datetime = c(t0 + c(2, 17, 32) * 60, t0 + c(7, 22, 37) * 60),
    lon = c(0, 15, 30, 100, 115, 130),
    lat = c(0, 30, 60, 20, 35, 50),
    stringsAsFactors = FALSE
  )

  out <- gps_interpolate(dat, interval_mins = 15, verbose = FALSE)
  usable <- out[is.finite(out$lon), ]

  expect_setequal(unique(usable$datetime), t0 + c(15, 30) * 60)
  expect_equal(as.integer(table(usable$sensor_id)), c(2L, 2L))
  expect_equal(usable$lon[usable$sensor_id == "A"], c(13, 28), tolerance = 1e-10)
  expect_equal(usable$lat[usable$sensor_id == "B"], c(28, 43), tolerance = 1e-10)
  expect_true(all(usable$is_interpolated))
  expect_true(all(!usable$is_observed))
})

test_that("gps_interpolate retains exact fixes and does not extrapolate", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    datetime = t0 + c(2, 15, 32) * 60,
    lon = c(0, 13, 30),
    lat = c(0, 26, 60),
    stringsAsFactors = FALSE
  )

  out <- gps_interpolate(dat, interval_mins = 15, verbose = FALSE)

  expect_equal(as.numeric(out$datetime - t0, units = "mins"), c(0, 15, 30, 45))
  expect_true(all(is.na(out$lon[c(1, 4)])))
  expect_identical(out$lon[2], 13)
  expect_true(out$is_observed[2])
  expect_false(out$is_interpolated[2])
  expect_identical(out$observed_datetime[2], dat$datetime[2])
  expect_identical(out$time_offset_s[2], 0)
  expect_true(out$is_interpolated[3])
  expect_equal(out$interpolation_gap_s[3], 17 * 60)
})

test_that("gps_interpolate always respects segment boundaries", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    segment_id = c("A_seg001", "A_seg001", "A_seg002", "A_seg002"),
    datetime = t0 + c(2, 17, 32, 47) * 60,
    lon = c(0, 15, 100, 115),
    lat = c(0, 15, 100, 115),
    stringsAsFactors = FALSE
  )

  out <- gps_interpolate(dat, interval_mins = 15, groups = "sensor_id", verbose = FALSE)

  expect_true("segment_id" %in% names(attr(out, "gps_reg")))
  expect_equal(nrow(attr(out, "gps_reg")), 2)
  expect_true(all(!is.finite(out$lon[out$datetime == t0 + 30 * 60])))
})

test_that("gps_interpolate handles single fixes, duplicate times, and invalid coordinates", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    segment_id = c(rep("A_seg001", 5), "A_seg002"),
    datetime = t0 + c(0, 15, 15, 30, 45, 60) * 60,
    lon = c(0, 15, 999, NA, 45, 60),
    lat = c(0, 15, 999, 30, 45, 60),
    row_note = letters[1:6],
    stringsAsFactors = FALSE
  )

  out <- gps_interpolate(dat, interval_mins = 15, verbose = FALSE)
  diag <- attr(out, "gps_reg")
  first_segment <- out[out$segment_id == "A_seg001", ]

  expect_identical(first_segment$lon[2], 15)
  expect_identical(first_segment$row_note[2], "b")
  expect_true(first_segment$is_interpolated[3])
  expect_false(first_segment$is_observed[3])
  expect_equal(first_segment$lon[3], 30)
  expect_equal(diag$n_duplicate_timestamps[diag$segment_id == "A_seg001"], 1)
  expect_equal(diag$n_invalid_coordinate_fixes[diag$segment_id == "A_seg001"], 2)
  expect_equal(diag$n_valid_anchor_timestamps[diag$segment_id == "A_seg002"], 1)
  expect_true(out$is_observed[out$segment_id == "A_seg002"])
})

test_that("gps_interpolate handles irregular intervals without snapped anchors", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    datetime = t0 + c(2, 29, 61) * 60,
    lon = c(0, 27, 59),
    lat = c(0, 27, 59),
    stringsAsFactors = FALSE
  )

  out <- gps_interpolate(dat, interval_mins = 15, verbose = FALSE)
  usable <- out[is.finite(out$lon), ]

  expect_equal(as.numeric(usable$datetime - t0, units = "mins"), c(15, 30, 45, 60))
  expect_equal(usable$lon, c(13, 28, 43, 58), tolerance = 1e-10)
  expect_equal(usable$lat, usable$lon, tolerance = 1e-10)
})

test_that("gps_interpolate rejects grid output", {
  regular <- gps_regularise(gps_fixture(), interval_mins = 1, verbose = FALSE)

  expect_error(
    gps_interpolate(regular, interval_mins = 1, verbose = FALSE),
    "requires raw observation rows",
    fixed = TRUE
  )
})

test_that("gps_regularise can require exact grid timestamps", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    sensor_id = "A",
    datetime = t0 + c(0, 90, 240),
    lon = c(150, 150.0003, 150.0008),
    lat = c(-30, -30.0003, -30.0008),
    stringsAsFactors = FALSE
  )

  regular <- gps_regularise(dat, interval_mins = 1, tolerance_mins = 0, verbose = FALSE)
  diag <- attr(regular, "gps_reg")

  expect_equal(sum(regular$is_observed), 2)
  expect_equal(diag$n_unmatched_observed_fixes, 1)
  expect_equal(diag$tolerance_mins, 0)
})

test_that("gps_downsample supports phase offsets by group", {
  dat <- gps_fixture()[1:6, ]

  out <- gps_downsample(dat, target_mins = 2, method = "rigid", phase_mins = 1, verbose = FALSE)
  diag <- attr(out, "gps_reg")

  expect_equal(
    as.numeric(out$datetime - min(dat$datetime), units = "mins"),
    c(1, 3, 5)
  )
  expect_equal(diag$n_expected_fixes, 3)
  expect_equal(diag$n_retained_fixes, 3)
  expect_equal(diag$sampling_interval_achieved_s, 120)
})
