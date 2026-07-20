test_that("GPS schema helpers define required and recognised optional columns", {
  expect_identical(
    grazer:::grz_gps_required_cols(),
    c("sensor_id", "datetime", "lon", "lat")
  )
  expect_true(all(
    c("animal_id", "deployment_id", "paddock", "treatment", "device_id") %in%
      grazer:::grz_gps_optional_cols()
  ))
})

test_that("GPS preparation preserves study metadata and types core columns", {
  dat <- gps_fixture_character_time()
  dat$animal_id <- "cow_1"
  dat$treatment <- "shade"
  dat$study_note <- paste0("note_", seq_len(nrow(dat)))

  out <- grazer:::grz_prepare_gps_dt(dat, fun_name = "test")

  expect_s3_class(out, "data.table")
  expect_true(all(c("animal_id", "treatment", "study_note") %in% names(out)))
  expect_identical(out$study_note, dat$study_note)
  expect_type(out$sensor_id, "character")
  expect_true(inherits(out$datetime, "POSIXct"))
  expect_type(out$lon, "double")
  expect_type(out$lat, "double")
})

test_that("GPS to sf conversion keeps lon and lat columns", {
  skip_if_not_installed("sf")

  dat <- gps_fixture()[1:2, ]
  dat$animal_id <- c("cow_1", "cow_2")

  out <- grazer:::grz_gps_as_sf(dat, fun_name = "test")

  expect_s3_class(out, "sf")
  expect_true(all(c("lon", "lat", "animal_id") %in% names(out)))
  expect_equal(out$lon, dat$lon)
  expect_equal(out$lat, dat$lat)
  expect_identical(sf::st_crs(out)$epsg, 4326L)
})

test_that("GPS to sf conversion rejects invalid coordinates before geometry work", {
  skip_if_not_installed("sf")

  dat <- gps_fixture()[1:2, ]
  dat$lon[2] <- 181

  expect_error(
    grazer:::grz_gps_as_sf(dat, fun_name = "test"),
    "valid longitude and latitude"
  )
})
