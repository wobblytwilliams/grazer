test_that("gps_animal_summary returns daily movement summaries", {
  dat <- gps_fixture()
  dat$animal_id <- "cow_1"

  out <- gps_animal_summary(dat, epoch = "day", verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(out$deployment_id, "D1")
  expect_equal(out$animal_id, "cow_1")
  expect_equal(out$sensor_id, "A")
  expect_equal(out$epoch, "2024-01-01")
  expect_equal(out$epoch_mins, 1440)
  expect_equal(out$n_fixes, 6)
  expect_true(all(c("epoch_start", "epoch_end", "total_distance_m") %in% names(out)))
})

test_that("gps_diurnal returns hourly summaries with date and hour columns", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    animal_id = "cow_1",
    datetime = t0 + c(0, 60, 3600, 3660),
    lon = c(150, 150.0001, 150.0002, 150.0003),
    lat = c(-30, -30, -30, -30),
    stringsAsFactors = FALSE
  )

  out <- gps_diurnal(dat, verbose = FALSE)

  expect_equal(nrow(out), 2)
  expect_equal(out$date, as.Date(c("2024-01-01", "2024-01-01")))
  expect_equal(out$hour, c(0L, 1L))
  expect_equal(out$n_fixes, c(2L, 2L))
})

test_that("gps_animal_summary supports custom interval summaries", {
  dat <- gps_fixture()
  dat$animal_id <- "cow_1"

  out <- gps_animal_summary(dat, epoch_mins = 2, verbose = FALSE)

  expect_equal(nrow(out), 3)
  expect_true(all(out$epoch_mins == 2))
  expect_equal(out$n_fixes, c(2L, 2L, 2L))
})

test_that("gps_animal_summary keeps multiple animals separate", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")

  out <- gps_animal_summary(dat, epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 2)
  expect_setequal(out$animal_id, c("cow_1", "cow_2"))
  expect_setequal(out$sensor_id, c("A", "B"))
  expect_true(all(out$n_fixes == 3))
})

test_that("gps_social_summary summarises output from gps_social", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")
  social <- gps_social(dat, thresholds_m = 20, verbose = FALSE)

  out <- gps_social_summary(social, epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 2)
  expect_true(all(c(
    "mean_nearest_neighbour_m",
    "mean_n_neighbours_within_20m",
    "prop_any_neighbour_within_20m"
  ) %in% names(out)))
  expect_true(all(out$prop_any_neighbour_within_20m == 1))
})

test_that("gps_epoch joins movement and social summaries predictably", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")

  out <- gps_epoch(
    dat,
    epoch = "day",
    include = c("movement", "social"),
    thresholds_m = 20,
    verbose = FALSE
  )

  expect_equal(nrow(out), 2)
  expect_true(all(c(
    "total_distance_m",
    "mean_nearest_neighbour_m",
    "social_n_fixes"
  ) %in% names(out)))
  expect_false(any(grepl("^\\.grz_", names(out))))
})

test_that("gps_group_summary aggregates animals within metadata groups", {
  dat <- gps_two_sensor_fixture()
  dat$animal_id <- ifelse(dat$sensor_id == "A", "cow_1", "cow_2")
  dat$paddock <- "north"

  out <- gps_group_summary(dat, group_cols = "paddock", epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$paddock, "north")
  expect_equal(out$n_animals, 2)
  expect_equal(out$n_sensors, 2)
  expect_equal(out$n_fixes, 6)
})

test_that("gps_animal_summary handles missing coordinate values", {
  dat <- gps_fixture()
  dat$animal_id <- "cow_1"
  dat$lon[3] <- NA_real_

  out <- gps_animal_summary(dat, epoch = "day", verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$n_fixes, 6)
  expect_true(is.na(out$mean_step_m) || is.finite(out$mean_step_m))
})

test_that("gps_animal_summary accepts resource-use summaries", {
  skip_if_not_installed("sf")

  use <- gps_resource_use(
    gps_fixture(),
    resources = gps_resource_fixture(),
    radius_m = 20,
    resource_id_col = "resource_id",
    metric_crs = 3857,
    verbose = FALSE
  )

  out <- gps_animal_summary(use, include = "resource_use", verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$n_resources, 1)
  expect_equal(out$n_fixes_near_resource, 2)
  expect_equal(out$prop_fixes_near_resource, 2 / 6)
})
