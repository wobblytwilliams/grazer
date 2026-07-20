test_that("gps_validate checks schema and returns typed data when valid", {
  val <- gps_validate(gps_fixture_character_time())

  expect_s3_class(val, "grz_validation")
  expect_true(val$is_valid)
  expect_match(val$message, "Valid GPS dataset")
  expect_s3_class(val$data, "data.frame")
  expect_true(inherits(val$data$datetime, "POSIXct"))
  expect_identical(attr(val$data$datetime, "tzone"), "UTC")
  expect_type(val$data$lon, "double")
  expect_type(val$data$lat, "double")
})

test_that("gps_validate describes missing datetime columns without QC counts", {
  dat <- data.frame(
    sensor_id = "A",
    time_stamp = "2024-01-01 00:00:00",
    lon = 150,
    lat = -30,
    stringsAsFactors = FALSE
  )

  val <- gps_validate(dat)

  expect_false(val$is_valid)
  expect_match(val$message, "Missing required column\\(s\\): datetime")
  expect_match(val$message, "datetime column must be named `datetime`")
  expect_false(grepl("duplicate", val$message))
  expect_null(val$qc)
})

test_that("gps_validate describes coordinate-like columns with incorrect names", {
  dat <- data.frame(
    sensor_id = "A",
    datetime = "2024-01-01 00:00:00",
    longitude = 150,
    latitude = -30,
    stringsAsFactors = FALSE
  )

  val <- gps_validate(dat)

  expect_false(val$is_valid)
  expect_match(val$message, "longitude column must be named `lon`")
  expect_match(val$message, "latitude column must be named `lat`")
})

test_that("gps_validate describes unparseable datetime formatting", {
  dat <- gps_fixture_character_time()
  dat$datetime <- "not a date"

  val <- gps_validate(dat)

  expect_false(val$is_valid)
  expect_match(val$message, "could not be parsed as date-time")
})

test_that("gps_qc_summary returns structured issue tables", {
  dat <- gps_fixture()
  dat$animal_id <- c("cow_1", "cow_1", "", "cow_1", "cow_1", "cow_1")
  dat <- rbind(dat, dat[3, ])
  dat$datetime[4] <- dat$datetime[3] - 60
  dat$lon[5] <- NA_real_
  dat$lat[6] <- -91
  dat$datetime[7] <- dat$datetime[7] + 3 * 3600

  qc <- gps_qc_summary(dat, large_gap_mins = 30)

  expect_s3_class(qc, "grz_qc")
  expect_type(qc$summary, "character")
  expect_true(all(c("invalid_rows", "duplicates", "gaps", "non_positive_intervals") %in% names(qc)))
  expect_false("columns" %in% names(qc))
  expect_false("parameters" %in% names(qc))
  expect_true(nrow(qc$invalid_rows) >= 2)
  expect_true(nrow(qc$duplicates) >= 1)
  expect_true(nrow(qc$gaps) >= 2)
  expect_true(all(c("gap_id", "gap_side", "row_id", "gap_mins") %in% names(qc$gaps)))
  expect_true(nrow(qc$non_positive_intervals) >= 1)
})

test_that("gps_check_gaps returns large and non-positive interval rows", {
  dat <- gps_fixture()[1:4, ]
  dat <- rbind(dat, dat[3, ])
  dat$datetime[4] <- dat$datetime[3] + 7200

  gaps <- gps_check_gaps(dat, large_gap_mins = 30)

  expect_true(any(gaps$is_large_gap))
  expect_true(any(gaps$is_non_positive_interval))
  expect_true(all(c("interval_mins", "previous_datetime", "interval_type") %in% names(gaps)))
  expect_true(all(gaps$is_large_gap | gaps$is_non_positive_interval))
  expect_false("gps_check_intervals" %in% getNamespaceExports("grazer"))
})

test_that("gps_check_gaps returns an empty issue table for regular data", {
  gaps <- gps_check_gaps(gps_fixture(), large_gap_mins = 30)

  expect_equal(nrow(gaps), 0)
  expect_true(all(c("datetime", "previous_datetime", "interval_mins", "interval_type") %in% names(gaps)))
})

test_that("gps_append_segments creates sensor-based segment ids", {
  dat <- gps_fixture()
  dat$datetime[5] <- dat$datetime[4] + 3 * 3600
  dat$datetime[6] <- dat$datetime[5] + 60

  out <- gps_append_segments(dat, large_gap_mins = 30, verbose = FALSE)

  expect_true("segment_id" %in% names(out))
  expect_equal(out$segment_id, c("A_seg001", "A_seg001", "A_seg001", "A_seg001", "A_seg002", "A_seg002"))
})

test_that("gps_validate can flag zero-zero coordinates when requested", {
  dat <- gps_fixture()[1:2, ]
  dat$lon[2] <- 0
  dat$lat[2] <- 0

  val <- gps_validate(dat, check_zero_zero = TRUE)

  expect_true(val$is_valid)
  expect_equal(nrow(val$invalid_rows), 1)
  expect_equal(val$invalid_rows$invalid_reason, "zero_zero")
})
