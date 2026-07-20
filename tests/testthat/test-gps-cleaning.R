test_that("gps_clean_duplicates drops repeated key rows", {
  dat <- rbind(gps_fixture()[1:3, ], gps_fixture()[2, ])

  out <- gps_clean_duplicates(dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_equal(sum(duplicated(out[c("sensor_id", "datetime", "lon", "lat")])), 0)
})

test_that("gps_clean_duplicates can flag duplicates and report an audit", {
  dat <- rbind(gps_fixture()[1:3, ], gps_fixture()[2, ])

  out <- gps_clean_duplicates(dat, action = "flag", verbose = FALSE)
  summary <- attr(out, "cleaning_summary")

  expect_equal(nrow(out), 4)
  expect_true("is_duplicate_fix" %in% names(out))
  expect_equal(sum(out$is_duplicate_fix), 1)
  expect_equal(summary$step, "duplicates")
  expect_equal(summary$n_flagged, 1)
  expect_equal(nrow(attr(out, "flagged_rows")), 1)
})

test_that("gps_clean_errors removes invalid rows and zero-zero fixes by default", {
  base <- gps_fixture_character_time()
  dat <- rbind(
    base[1, ],
    transform(base[2, ], sensor_id = ""),
    transform(base[3, ], datetime = "bad time"),
    transform(base[4, ], lon = 181),
    transform(base[5, ], lon = 0, lat = 0)
  )

  out <- gps_clean_errors(dat, verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$sensor_id, "A")
  expect_true(is.finite(out$lon))
  expect_true(is.finite(out$lat))
})

test_that("gps_clean_errors can flag deployment-window rows", {
  dat <- gps_fixture()[1:4, ]
  windows <- data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    start_datetime = dat$datetime[2],
    end_datetime = dat$datetime[3],
    stringsAsFactors = FALSE
  )

  out <- gps_clean_errors(
    dat,
    deployment_windows = windows,
    action = "flag",
    verbose = FALSE
  )
  summary <- attr(out, "cleaning_summary")

  expect_equal(nrow(out), 4)
  expect_equal(sum(out$is_gps_error), 2)
  expect_true(all(grepl("outside_deployment_window", out$gps_error_reason[out$is_gps_error])))
  expect_equal(summary$n_flagged, 2)
})

test_that("gps_clean_speed_fixed removes an implausible jump", {
  dat <- gps_fixture()[1:3, ]
  dat$lon[3] <- dat$lon[3] + 1

  out <- gps_clean_speed_fixed(dat, max_speed_mps = 4, verbose = FALSE)

  expect_equal(nrow(out), 2)
  expect_false("speed_mps" %in% names(out))
})

test_that("gps_clean_speed_fixed can flag implausible speed", {
  dat <- gps_fixture()[1:3, ]
  dat$lon[3] <- dat$lon[3] + 1

  out <- gps_clean_speed_fixed(dat, max_speed_mps = 4, action = "flag", verbose = FALSE)
  summary <- attr(out, "cleaning_summary")

  expect_equal(nrow(out), 3)
  expect_equal(sum(out$is_speed_outlier), 1)
  expect_equal(summary$threshold_mps, 4)
  expect_equal(summary$n_removed, 0)
})

test_that("gps_clean_speed_stat records its current threshold behaviour", {
  out <- gps_clean_speed_stat(
    gps_fixture(),
    method = "quantile",
    prob = 0.8,
    min_threshold_mps = 0.01,
    keep_speed_cols = TRUE,
    verbose = FALSE,
    return_class = "data.table"
  )

  expect_s3_class(out, "data.table")
  expect_true(all(c("step_dt_s", "step_m", "speed_mps") %in% names(out)))
  expect_true(is.numeric(attr(out, "speed_threshold_mps")))
  expect_true(is.finite(attr(out, "speed_threshold_mps")))
})

test_that("gps_clean_speed_stat MAD threshold focuses on upper speeds", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  n <- 80L
  step_deg <- c(rep(0.000001, 65), rep(0.00002, 14), 0.01)
  dat <- data.frame(
    sensor_id = "A",
    datetime = t0 + seq_len(n) * 60,
    lon = 150 + cumsum(step_deg),
    lat = -30,
    stringsAsFactors = FALSE
  )

  out <- gps_clean_speed_stat(
    dat,
    method = "mad",
    min_threshold_mps = 0.01,
    action = "flag",
    keep_speed_cols = TRUE,
    verbose = FALSE
  )

  threshold <- attr(out, "speed_threshold_mps")
  expect_true(threshold > 0.01)
  expect_true(any(out$is_speed_outlier, na.rm = TRUE))
  expect_true(max(out$speed_mps[out$is_speed_outlier], na.rm = TRUE) > threshold)
})

test_that("gps_clean_speed_stat reports removed rows", {
  dat <- gps_fixture()[1:4, ]
  dat$lon[4] <- dat$lon[4] + 1

  out <- gps_clean_speed_stat(
    dat,
    method = "quantile",
    prob = 0.5,
    min_threshold_mps = 0.01,
    verbose = FALSE
  )

  expect_true(nrow(out) < nrow(dat))
  expect_true(nrow(attr(out, "removed_rows")) > 0)
  expect_true(is.finite(attr(out, "cleaning_summary")$threshold_mps))
})

test_that("gps_denoise preserves rows and can keep raw coordinates", {
  expect_silent({
    out <- gps_denoise(
      gps_fixture(),
      method = "statistical",
      keep_raw_coords = TRUE,
      verbose = FALSE
    )
  })

  expect_equal(nrow(out), nrow(gps_fixture()))
  expect_true(all(c("lon_raw", "lat_raw") %in% names(out)))
  expect_equal(out$lon_raw, gps_fixture()$lon)
  expect_equal(out$lat_raw, gps_fixture()$lat)
})

test_that("gps_denoise reports modification without removing rows", {
  out <- gps_denoise(
    gps_fixture(),
    method = "statistical",
    keep_raw_coords = TRUE,
    verbose = FALSE
  )

  summary <- attr(out, "cleaning_summary")
  expect_equal(nrow(out), nrow(gps_fixture()))
  expect_equal(summary$action, "modify")
  expect_equal(summary$n_removed, 0)
})

test_that("gps_clean runs the selected cleaning steps in order", {
  base <- gps_fixture_character_time()
  dat <- rbind(base[1:3, ], base[2, ])
  dat <- rbind(dat, transform(base[4, ], datetime = "bad time"))

  out <- gps_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)

  expect_equal(nrow(out), 3)
  expect_equal(sum(duplicated(out[c("sensor_id", "datetime", "lon", "lat")])), 0)
  expect_false(any(is.na(out$datetime)))
})

test_that("gps_clean combines audit summaries across selected steps", {
  base <- gps_fixture_character_time()
  dat <- rbind(base[1:3, ], base[2, ])
  dat <- rbind(dat, transform(base[4, ], datetime = "bad time"))

  out <- gps_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)
  summary <- attr(out, "cleaning_summary")
  removed <- attr(out, "removed_rows")

  expect_equal(nrow(out), 3)
  expect_setequal(summary$step, c("duplicates", "errors"))
  expect_equal(sum(summary$n_removed), 2)
  expect_equal(nrow(removed), 2)
  expect_true("clean_step" %in% names(removed))
})

test_that("standalone cleaning functions append incoming audit attributes", {
  dat <- rbind(gps_fixture()[1:3, ], gps_fixture()[2, ])

  out <- gps_clean_duplicates(dat, verbose = FALSE)
  out <- gps_clean_speed_fixed(out, max_speed_mps = 4, verbose = FALSE)

  summary <- attr(out, "cleaning_summary")
  expect_setequal(summary$step, c("duplicates", "speed_fixed"))
})

test_that("gps_clean accepts step_args for step-specific options", {
  dat <- gps_fixture()[1:3, ]
  dat$lon[3] <- dat$lon[3] + 1

  out <- gps_clean(
    dat,
    steps = "speed_fixed",
    step_args = list(speed_fixed = list(max_speed_mps = 5000)),
    verbose = FALSE
  )

  expect_equal(nrow(out), 3)
  expect_equal(attr(out, "cleaning_summary")$threshold_mps, 5000)
})

test_that("gps_denoise state-aware mode requires an explicit state column", {
  expect_error(
    gps_denoise(gps_fixture(), method = "state_aware", verbose = FALSE),
    "state_col"
  )
})

test_that("gps_clean_spatial adds the modal paddock for points inside polygons", {
  out <- gps_clean_spatial(
    gps_fixture(),
    paddocks_sf = gps_paddock_fixture(),
    buffer_m = 0,
    verbose = FALSE
  )

  expect_equal(nrow(out), nrow(gps_fixture()))
  expect_true("paddock" %in% names(out))
  expect_equal(unique(out$paddock), "home")
})

test_that("gps_clean_spatial accepts common KML name casing", {
  paddock <- gps_paddock_fixture()
  paddock$Name <- paddock$NAME
  paddock$NAME <- NULL

  out <- gps_clean_spatial(
    gps_fixture(),
    paddocks_sf = paddock,
    buffer_m = 0,
    verbose = FALSE
  )

  expect_equal(nrow(out), nrow(gps_fixture()))
  expect_equal(unique(out$paddock), "home")
})

test_that("gps_clean_spatial can drop the helper paddock column from output", {
  out <- gps_clean_spatial(
    gps_fixture(),
    paddocks_sf = gps_paddock_fixture(),
    buffer_m = 0,
    append_paddock = FALSE,
    verbose = FALSE
  )

  expect_equal(nrow(out), nrow(gps_fixture()))
  expect_false("paddock" %in% names(out))
})

test_that("gps_clean_spatial can flag fixes outside polygons", {
  dat <- gps_fixture()
  dat$lon[6] <- dat$lon[6] + 1

  out <- gps_clean_spatial(
    dat,
    paddocks_sf = gps_paddock_fixture(),
    buffer_m = 0,
    action = "flag",
    verbose = FALSE
  )

  expect_equal(nrow(out), nrow(dat))
  expect_true("is_outside_boundary" %in% names(out))
  expect_equal(sum(out$is_outside_boundary), 1)
  expect_equal(attr(out, "cleaning_summary")$n_flagged, 1)
})
