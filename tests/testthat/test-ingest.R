test_that("grz_standardise_gps maps required aliases", {
  raw <- data.frame(
    id = c("A001", "A001"),
    time = c("2022-08-28T00:54:28Z", "2022-08-28T01:24:54Z"),
    x = c(132.305676, 132.305923),
    y = c(-14.474048, -14.473868),
    stringsAsFactors = FALSE
  )

  out <- grz_standardise_gps(raw)
  expect_true(all(c("sensor_id", "datetime", "lon", "lat") %in% names(out)))
  expect_s3_class(out, "data.table")
  expect_equal(out$sensor_id[[1]], "A001")
  expect_s3_class(out$datetime, "POSIXct")
})

test_that("grz_standardise_gps allows optional deployment_id", {
  raw <- data.frame(
    sensor_id = "A001",
    datetime = "2022-08-28T00:54:28Z",
    lon = 132.305676,
    lat = -14.474048,
    stringsAsFactors = FALSE
  )

  out <- grz_standardise_gps(raw)
  expect_false("deployment_id" %in% names(out))

  out_dep <- grz_standardise_gps(raw, deployment_id = "DEP_1")
  expect_true("deployment_id" %in% names(out_dep))
  expect_equal(unique(out_dep$deployment_id), "DEP_1")
})

test_that("grz_standardise_gps keeps optional metadata columns", {
  raw <- data.frame(
    sensor_id = "A001",
    datetime = "2022-08-28T00:54:28Z",
    lon = 132.305676,
    lat = -14.474048,
    hdop = 2.4,
    treatment = "Control",
    stringsAsFactors = FALSE
  )

  out <- grz_standardise_gps(raw)
  expect_true(all(c("hdop", "treatment") %in% names(out)))
})

test_that("grz_validate_gps flags and drops invalid rows", {
  dat <- data.frame(
    sensor_id = c("A001", "", "A003"),
    datetime = c("2022-08-28T00:54:28Z", "bad_date", "2022-08-28T01:24:54Z"),
    lon = c(132.305676, 400, 132.305923),
    lat = c(-14.474048, -14.473868, NA_real_),
    stringsAsFactors = FALSE
  )

  val <- grz_validate_gps(dat, drop_invalid = TRUE)
  expect_type(val, "list")
  expect_s3_class(val$data, "data.table")
  expect_s3_class(val$qc, "data.table")
  expect_equal(nrow(val$data), 1)
  expect_gte(nrow(val$invalid_rows), 2)
})

test_that("grz_read_gps reads sample file with n_max", {
  path <- normalizePath(
    file.path(testthat::test_path("..", ".."), "test_data", "ManbullooToSimulate.csv"),
    winslash = "/",
    mustWork = TRUE
  )

  out <- grz_read_gps(path, n_max = 5)
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 5)
  expect_true(all(c("sensor_id", "datetime", "lon", "lat") %in% names(out)))
  expect_true(all(c("hdop", "sats_detected") %in% names(out)))

  qc <- attr(out, "qc_summary")
  expect_s3_class(qc, "data.table")
})
