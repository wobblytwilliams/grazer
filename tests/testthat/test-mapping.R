test_that("grz_map returns a leaflet map for minimal valid data", {
  skip_if_not_installed("leaflet")

  dat <- data.table::data.table(
    sensor_id = c("A001", "A001", "A002"),
    datetime = c("2022-08-28T00:54:28Z", "2022-08-28T01:24:54Z", "2022-08-28T01:40:39Z"),
    lon = c(132.305676, 132.305923, 132.305226),
    lat = c(-14.474048, -14.473868, -14.474055)
  )

  map <- grz_map(dat)
  expect_s3_class(map, "leaflet")

  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true("addCircleMarkers" %in% methods)
})

test_that("grz_map supports block colouring with legend", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    deployment_id = c("DEP1", "DEP1", "DEP2", "DEP2"),
    sensor_id = c("A001", "A001", "A002", "A002"),
    datetime = c(
      "2022-08-28T00:54:28Z",
      "2022-08-28T01:24:54Z",
      "2022-08-28T01:40:39Z",
      "2022-08-28T01:56:06Z"
    ),
    lon = c(132.305676, 132.305923, 132.305226, 132.306158),
    lat = c(-14.474048, -14.473868, -14.474055, -14.473746),
    treatment = c("Control", "Control", "Treatment", "Treatment"),
    stringsAsFactors = FALSE
  )

  map <- grz_map(dat, block = c("deployment_id", "treatment"))
  expect_s3_class(map, "leaflet")

  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true("addLegend" %in% methods)
  expect_true("addCircleMarkers" %in% methods)
  expect_false("addLayersControl" %in% methods)
})

test_that("grz_map timeline path adds timeslider calls", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.extras2")
  skip_if_not_installed("sf")

  dat <- data.frame(
    sensor_id = c("A001", "A001", "A002"),
    datetime = c("2022-08-28T00:54:28Z", "2022-08-28T01:24:54Z", "2022-08-28T01:40:39Z"),
    lon = c(132.305676, 132.305923, 132.305226),
    lat = c(-14.474048, -14.473868, -14.474055),
    stringsAsFactors = FALSE
  )

  map <- grz_map(dat, timeline = TRUE, block = "sensor_id")
  expect_s3_class(map, "leaflet")

  methods <- vapply(map$x$calls, function(x) x$method, character(1))
  expect_true(any(grepl("timeslider", methods, ignore.case = TRUE)))
})

test_that("grz_map errors when required columns are missing", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    sensor_id = "A001",
    lon = 132.305676,
    lat = -14.474048,
    stringsAsFactors = FALSE
  )

  expect_error(grz_map(dat), "Missing required columns for mapping")
})

test_that("grz_map warns when dropping invalid rows", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    sensor_id = c("A001", "A002"),
    datetime = c("2022-08-28T00:54:28Z", "bad_time"),
    lon = c(132.305676, 999),
    lat = c(-14.474048, -999),
    stringsAsFactors = FALSE
  )

  expect_warning(
    map <- grz_map(dat),
    "rows removed due to invalid datetime/lon/lat"
  )
  expect_s3_class(map, "leaflet")
})

test_that("grz_map halts for large point counts unless warnings are disabled", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    sensor_id = rep("A001", 12),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(12),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = rep(132.305676, 12),
    lat = rep(-14.474048, 12),
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_error(
      grz_map(dat, large_n = 10),
      "warnings = FALSE"
    ),
    "max_blocks =|max_points ="
  )

  expect_warning(
    map <- grz_map(dat, large_n = 10, warnings = FALSE),
    NA
  )
  expect_s3_class(map, "leaflet")
})

test_that("grz_map halts for high block count unless warnings are disabled", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    sensor_id = paste0("A", sprintf("%03d", seq_len(25))),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(25),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = rep(132.305676, 25),
    lat = rep(-14.474048, 25),
    stringsAsFactors = FALSE
  )

  expect_warning(
    expect_error(
      grz_map(dat, block = "sensor_id", block_n = 20),
      "warnings = FALSE"
    ),
    "max_blocks =|max_points ="
  )

  expect_warning(
    map <- grz_map(dat, block = "sensor_id", block_n = 20, warnings = FALSE),
    NA
  )
  expect_s3_class(map, "leaflet")
})

test_that("grz_map max_blocks limits number of block groups", {
  skip_if_not_installed("leaflet")

  set.seed(1)
  dat <- data.frame(
    sensor_id = rep(paste0("A", sprintf("%03d", seq_len(10))), each = 3),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(30),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = 132.305676 + runif(30, -0.001, 0.001),
    lat = -14.474048 + runif(30, -0.001, 0.001),
    stringsAsFactors = FALSE
  )

  map <- grz_map(
    dat,
    block = "sensor_id",
    max_blocks = 4
  )

  marker_calls <- Filter(function(x) identical(x$method, "addCircleMarkers"), map$x$calls)
  point_counts <- vapply(marker_calls, function(x) length(x$args[[1]]), integer(1))
  expect_equal(sum(point_counts), 12)
})

test_that("grz_map max_blocks requires block argument", {
  skip_if_not_installed("leaflet")

  dat <- data.frame(
    sensor_id = rep("A001", 5),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(5),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = rep(132.305676, 5),
    lat = rep(-14.474048, 5),
    stringsAsFactors = FALSE
  )

  expect_error(grz_map(dat, max_blocks = 2), "max_blocks.*requires.*block")
})

test_that("grz_map supports deprecated aliases sample_n and max_block", {
  skip_if_not_installed("leaflet")

  set.seed(1)
  dat <- data.frame(
    sensor_id = rep(c("A001", "A002"), each = 10),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(20),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = 132.305676 + runif(20, -0.001, 0.001),
    lat = -14.474048 + runif(20, -0.001, 0.001),
    stringsAsFactors = FALSE
  )

  map <- grz_map(dat, block = "sensor_id", sample_n = 6, max_block = 3)
  expect_s3_class(map, "leaflet")
})

test_that("grz_map max_points samples roughly evenly across blocks", {
  skip_if_not_installed("leaflet")

  set.seed(1)
  dat <- data.frame(
    sensor_id = rep(c("A001", "A002"), each = 10),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(20),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = 132.305676 + runif(20, -0.001, 0.001),
    lat = -14.474048 + runif(20, -0.001, 0.001),
    stringsAsFactors = FALSE
  )

  map <- grz_map(dat, block = "sensor_id", max_points = 6)
  marker_calls <- Filter(function(x) identical(x$method, "addCircleMarkers"), map$x$calls)
  point_counts <- vapply(marker_calls, function(x) length(x$args[[1]]), integer(1))

  expect_equal(sum(point_counts), 6)
})

test_that("grz_map max_points is a total cap across multiple block layers", {
  skip_if_not_installed("leaflet")

  set.seed(1)
  n <- 5000
  dat <- data.frame(
    deployment_id = sprintf("D%02d", ((seq_len(n) - 1) %% 25) + 1),
    sensor_id = sprintf("A%03d", ((seq_len(n) - 1) %% 40) + 1),
    datetime = format(
      as.POSIXct("2022-08-28 00:00:00", tz = "UTC") + seq_len(n),
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    lon = 132.305676 + runif(n, -0.001, 0.001),
    lat = -14.474048 + runif(n, -0.001, 0.001),
    stringsAsFactors = FALSE
  )

  map <- grz_map(
    dat,
    block = c("deployment_id", "sensor_id"),
    max_points = 500,
    warnings = FALSE
  )

  marker_calls <- Filter(
    function(x) x$method %in% c("addCircleMarkers", "addAwesomeMarkers"),
    map$x$calls
  )
  point_counts <- vapply(marker_calls, function(x) length(x$args[[1]]), integer(1))

  expect_lte(sum(point_counts), 500)
})
