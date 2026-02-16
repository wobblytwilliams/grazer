grz_pipeline_sample <- function(n = 800) {
  path <- normalizePath(
    file.path(testthat::test_path("..", ".."), "test_data", "ManbullooToSimulate.csv"),
    winslash = "/",
    mustWork = TRUE
  )
  grz_read_gps(path, n_max = n, validate = FALSE)
}

test_that("grz_validate returns data.frame by default with qc attributes", {
  dat <- grz_pipeline_sample(200)
  out <- grz_validate(dat, drop_invalid = FALSE, verbose = FALSE)

  expect_s3_class(out, "data.frame")
  expect_false("data.table" %in% class(out))
  expect_true(!is.null(attr(out, "validation_qc")))
  expect_true(!is.null(attr(out, "invalid_rows")))
})

test_that("cleaning functions drop invalid and zero-zero rows", {
  dat <- as.data.frame(grz_pipeline_sample(200), stringsAsFactors = FALSE)
  bad <- dat[1:2, ]
  bad$lon <- c(0, 1000)
  bad$lat <- c(0, 0)
  bad$datetime <- as.POSIXct(c(NA, "2022-08-28 00:00:00"), tz = "UTC")
  x <- rbind(dat, bad)

  out <- grz_clean_errors(x, verbose = FALSE)
  expect_lt(nrow(out), nrow(x))
  expect_false(any(out$lon == 0 & out$lat == 0))
  expect_true(all(!is.na(grz_parse_datetime_utc(out$datetime))))
})

test_that("speed cleaners run and drop extreme speeds", {
  dat <- grz_pipeline_sample(100)
  dat <- grz_clean_errors(dat, verbose = FALSE)

  dat_fast <- dat
  dat_fast$datetime <- grz_parse_datetime_utc(dat_fast$datetime)
  dat_fast$lon[2] <- dat_fast$lon[1] + 2
  dat_fast$lat[2] <- dat_fast$lat[1]
  dat_fast$datetime[2] <- dat_fast$datetime[1] + 1

  fixed <- grz_clean_speed_fixed(dat_fast, max_speed_mps = 4, verbose = FALSE)
  stat <- grz_clean_speed_stat(dat_fast, verbose = FALSE)

  expect_lt(nrow(fixed), nrow(dat_fast))
  expect_lte(nrow(stat), nrow(dat_fast))
})

test_that("grz_align and grz_downsample produce expected outputs", {
  dat <- grz_pipeline_sample(400)
  dat <- grz_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)

  aligned <- grz_align(dat, interval_mins = "base", verbose = FALSE)
  down <- grz_downsample(aligned, target_mins = 60, method = "representative", verbose = FALSE)

  expect_s3_class(aligned, "data.frame")
  expect_true("aligned_from_observation" %in% names(aligned))
  expect_lt(nrow(down), nrow(aligned))
})

test_that("row-level calculators append expected columns", {
  dat <- grz_pipeline_sample(600)
  dat <- grz_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)

  mov <- grz_calculate_movement(dat, verbose = FALSE)
  expect_true(all(c("step_m", "speed_mps", "turn_rad", "cum_distance_m") %in% names(mov)))

  soc_base <- dat[1:200, ]
  soc_alt <- soc_base
  soc_alt$sensor_id <- "SIM_B"
  soc_alt$lon <- soc_alt$lon + 0.0003
  soc <- grz_calculate_social(rbind(soc_base, soc_alt), verbose = FALSE)
  expect_true(all(c("nearest_neighbor_m", "mean_dist_to_others_m") %in% names(soc)))
})

test_that("epoch summaries and wrapper return merged outputs", {
  dat <- grz_pipeline_sample(1200)
  dat <- grz_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)
  mov_rows <- grz_calculate_movement(dat, verbose = FALSE)

  sm <- grz_summarise_movement(mov_rows, epoch = "day", verbose = FALSE)
  ss <- grz_summarise_social(dat, epoch = "day", verbose = FALSE)
  sp <- grz_calculate_spatial(dat, epoch = "day", verbose = FALSE)
  ep <- grz_calculate_epoch_metrics(dat, epoch = "day", include = c("movement", "spatial"), verbose = FALSE)

  expect_true(all(c("epoch", "total_distance_m") %in% names(sm)))
  expect_true(all(c("epoch", "mean_nn_m") %in% names(ss)))
  expect_true(all(c("epoch", "mcp100_area_ha") %in% names(sp)))
  expect_true("epoch" %in% names(ep))
})

test_that("spatial cleaning appends paddock names when sf is available", {
  skip_if_not_installed("sf")

  dat <- grz_pipeline_sample(300)
  dat <- grz_clean(dat, steps = c("duplicates", "errors"), verbose = FALSE)

  xr <- range(dat$lon, na.rm = TRUE)
  yr <- range(dat$lat, na.rm = TRUE)
  poly <- sf::st_polygon(list(matrix(
    c(
      xr[1] - 0.01, yr[1] - 0.01,
      xr[2] + 0.01, yr[1] - 0.01,
      xr[2] + 0.01, yr[2] + 0.01,
      xr[1] - 0.01, yr[2] + 0.01,
      xr[1] - 0.01, yr[1] - 0.01
    ),
    ncol = 2,
    byrow = TRUE
  )))
  pdk <- sf::st_sf(NAME = "P1", geometry = sf::st_sfc(poly, crs = 4326))

  out <- grz_clean_spatial(dat, paddocks_sf = pdk, buffer_m = 100, verbose = FALSE)
  expect_true("paddock" %in% names(out))
  expect_gt(nrow(out), 0)
})
