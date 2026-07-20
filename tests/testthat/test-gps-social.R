gps_social_fixture <- function() {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data.frame(
    deployment_id = "D1",
    sensor_id = rep(c("A", "B", "C"), each = 3),
    animal_id = rep(c("cow_a", "cow_b", "cow_c"), each = 3),
    datetime = rep(t0 + c(0, 60, 120), times = 3),
    lon = 0,
    lat = c(
      0, 0, 0,
      0.0001, 0.0001, 0.0001,
      0.001, 0.001, NA
    ),
    stringsAsFactors = FALSE
  )
}

test_that("gps_proximity calculates pairwise distances by timestamp", {
  dat <- gps_social_fixture()[gps_social_fixture()$datetime == as.POSIXct("2024-01-01 00:00:00", tz = "UTC"), ]

  out <- gps_proximity(dat, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_setequal(out$pair_id, c("A:B", "A:C", "B:C"))
  expect_equal(out$distance_m[out$pair_id == "A:B"], 11.119, tolerance = 0.05)
  expect_equal(out$distance_m[out$pair_id == "A:C"], 111.195, tolerance = 0.2)
  expect_false(any(grepl("^\\.grz_", names(out))))
})

test_that("gps_nearest_neighbour returns nearest animal at each timestamp", {
  dat <- gps_social_fixture()

  out <- gps_nearest_neighbour(dat, verbose = FALSE)
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  first_time <- out[out$datetime == t0, ]
  missing_fix <- out[out$sensor_id == "C" & out$datetime == t0 + 120, ]

  expect_equal(first_time$nearest_neighbour_m[first_time$sensor_id == "A"], 11.119, tolerance = 0.05)
  expect_equal(first_time$nearest_neighbour_sensor_id[first_time$sensor_id == "C"], "B")
  expect_equal(first_time$nearest_neighbour_m[first_time$sensor_id == "C"], 100.075, tolerance = 0.2)
  expect_equal(missing_fix$social_group_size, 3)
  expect_equal(missing_fix$n_valid_fixes, 2)
  expect_true(is.na(missing_fix$nearest_neighbour_m))
})

test_that("gps_neighbours_within_range counts threshold neighbours", {
  dat <- gps_social_fixture()

  out <- gps_neighbours_within_range(dat, thresholds_m = c(20, 120), verbose = FALSE)
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  first_time <- out[out$datetime == t0, ]

  expect_equal(first_time$n_neighbours_within_20m[first_time$sensor_id == "A"], 1)
  expect_true(first_time$any_neighbour_within_20m[first_time$sensor_id == "A"])
  expect_equal(first_time$n_neighbours_within_20m[first_time$sensor_id == "C"], 0)
  expect_false(first_time$any_neighbour_within_20m[first_time$sensor_id == "C"])
  expect_equal(first_time$n_neighbours_within_120m[first_time$sensor_id == "C"], 2)
})

test_that("gps_social combines nearest-neighbour and threshold metrics", {
  dat <- gps_social_fixture()

  out <- gps_social(dat, thresholds_m = 20, verbose = FALSE)

  expect_true(all(c(
    "nearest_neighbour_m",
    "mean_distance_to_others_m",
    "n_neighbours_within_20m",
    "any_neighbour_within_20m"
  ) %in% names(out)))
  expect_equal(nrow(out), nrow(dat))
})

test_that("social functions keep internal interpolation without an alignment gap argument", {
  social_funs <- list(
    gps_proximity,
    gps_nearest_neighbour,
    gps_neighbours_within_range,
    gps_contacts,
    gps_social
  )

  expect_true(all(vapply(social_funs, function(fun) "interpolate" %in% names(formals(fun)), logical(1))))
  expect_true(all(vapply(social_funs, function(fun) !"align_max_gap_mins" %in% names(formals(fun)), logical(1))))
})

test_that("gps proximity functions handle irregular timestamps without interpolation", {
  dat <- gps_social_fixture()
  dat <- dat[!(dat$sensor_id == "B" & dat$datetime == as.POSIXct("2024-01-01 00:01:00", tz = "UTC")), ]

  prox <- gps_proximity(dat, verbose = FALSE)
  nn <- gps_nearest_neighbour(dat, verbose = FALSE)

  expect_equal(sum(prox$datetime == as.POSIXct("2024-01-01 00:01:00", tz = "UTC")), 1)
  expect_equal(nn$social_group_size[nn$sensor_id == "A" & nn$datetime == as.POSIXct("2024-01-01 00:01:00", tz = "UTC")], 2)
})

test_that("gps_contacts detects contact events by pair", {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  dat <- data.frame(
    deployment_id = "D1",
    sensor_id = rep(c("A", "B"), each = 3),
    animal_id = rep(c("cow_a", "cow_b"), each = 3),
    datetime = rep(t0 + c(0, 60, 120), times = 2),
    lon = 0,
    lat = c(0, 0, 0, 0.0001, 0.0001, 0.01),
    stringsAsFactors = FALSE
  )

  out <- gps_contacts(dat, contact_distance_m = 20, max_gap_mins = 2, verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_equal(out$pair_id, "A:B")
  expect_equal(out$n_contact_fixes, 2)
  expect_equal(out$duration_s, 60)
  expect_equal(out$min_distance_m, 11.119, tolerance = 0.05)
})
