test_that("gps_spatial returns CRS-explicit spatial summaries", {
  skip_if_not_installed("sf")

  out <- gps_spatial(
    gps_fixture(),
    epoch = "day",
    min_fixes = 3,
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1)
  expect_equal(out$n_fixes, 6)
  expect_equal(out$span_hours, 5 / 60, tolerance = 1e-12)
  expect_true(is.finite(out$mcp100_area_ha))
  expect_true(out$mcp100_area_ha > 0)
  expect_equal(out$metric_crs, "EPSG:3857")
})

test_that("gps_mcp can return polygon geometries in WGS84", {
  skip_if_not_installed("sf")

  out <- gps_mcp(
    gps_fixture(),
    percent = 100,
    min_fixes = 3,
    metric_crs = 3857,
    return_geometry = TRUE,
    verbose = FALSE
  )

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 1)
  expect_equal(sf::st_crs(out)$epsg, 4326L)
  expect_true(is.finite(out$mcp_area_ha))
  expect_true(out$mcp_area_ha > 0)
})

test_that("gps_kde returns utilisation grid cells", {
  skip_if_not_installed("sf")

  out <- gps_kde(
    gps_fixture(),
    bandwidth_m = 25,
    cell_size_m = 20,
    percent = c(50, 95),
    min_fixes = 3,
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1)
  expect_true(all(c("kde_percent", "kde_density", "kde_area_ha", "cell_centre_lon", "cell_centre_lat") %in% names(out)))
  expect_setequal(unique(out$kde_percent), c(50, 95))
  expect_true(all(out$inside_kde_percent))
  expect_true(all(out$kde_area_ha > 0))
})

test_that("gps_kde can return grid-cell geometries", {
  skip_if_not_installed("sf")

  out <- gps_kde(
    gps_fixture(),
    bandwidth_m = 25,
    cell_size_m = 20,
    percent = 95,
    min_fixes = 3,
    metric_crs = 3857,
    return_geometry = TRUE,
    verbose = FALSE
  )

  expect_s3_class(out, "sf")
  expect_equal(sf::st_crs(out)$epsg, 4326L)
  expect_true(nrow(out) >= 1)
})

test_that("gps_hotspots counts high-use grid cells", {
  skip_if_not_installed("sf")

  out <- gps_hotspots(
    gps_fixture(),
    cell_size_m = 25,
    hotspot_quantile = 0.5,
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_s3_class(out, "data.frame")
  expect_true(nrow(out) >= 1)
  expect_true(all(out$is_hotspot))
  expect_true(all(c("cell_centre_lon", "cell_centre_lat", "prop_fixes") %in% names(out)))
})

test_that("gps_append_paddock_names assigns paddocks by animal epoch", {
  skip_if_not_installed("sf")

  dat <- gps_fixture()
  dat$lon[6] <- 151
  dat$lat[6] <- -31

  out <- suppressWarnings(gps_append_paddock_names(
    dat,
    gps_paddock_fixture(),
    min_prop = 0.7,
    verbose = FALSE
  ))

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(dat))
  expect_true("assigned_paddock" %in% names(out))
  expect_equal(unique(out$assigned_paddock), "home")
  expect_false("paddock" %in% names(out))
})

test_that("gps_append_paddock_names returns NA when epoch threshold is not met", {
  skip_if_not_installed("sf")

  dat <- gps_fixture()
  dat$lon[4:6] <- 151
  dat$lat[4:6] <- -31

  expect_warning(
    out <- gps_append_paddock_names(
      dat,
      gps_paddock_fixture(),
      min_prop = 0.7,
      verbose = FALSE
    ),
    "did not satisfy"
  )
  expect_true(all(is.na(out$assigned_paddock)))
})

test_that("gps_append_paddock_names can use shorter epochs", {
  skip_if_not_installed("sf")

  dat <- gps_fixture()
  dat$datetime[4:6] <- dat$datetime[4:6] + 3600
  dat$lon[4:6] <- 151
  dat$lat[4:6] <- -31

  out <- suppressWarnings(gps_append_paddock_names(
    dat,
    gps_paddock_fixture(),
    epoch = "hour",
    min_prop = 0.7,
    verbose = FALSE
  ))

  expect_equal(out$assigned_paddock[1], "home")
  expect_true(is.na(out$assigned_paddock[4]))
})

test_that("gps_resource_distance appends nearest point-resource distances", {
  skip_if_not_installed("sf")

  out <- gps_resource_distance(
    gps_fixture()[1:3, ],
    resources = gps_resource_fixture(),
    resource_id_col = "resource_id",
    resource_type_col = "resource_type",
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_equal(nrow(out), 3)
  expect_equal(out$nearest_resource_id[1], "water")
  expect_equal(out$nearest_resource_type[1], "water")
  expect_equal(out$resource_distance_m[1], 0, tolerance = 0.001)
  expect_true(out$resource_distance_m[3] > out$resource_distance_m[2])
})

test_that("gps_resource_distance requires explicit CRS for untagged resources", {
  skip_if_not_installed("sf")

  resources <- sf::st_set_crs(gps_resource_fixture(), NA)
  expect_error(
    gps_resource_distance(
      gps_fixture()[1:2, ],
      resources = resources,
      resource_id_col = "resource_id",
      verbose = FALSE
    ),
    "Set `resource_crs` explicitly",
    fixed = TRUE
  )

  out <- gps_resource_distance(
    gps_fixture()[1:2, ],
    resources = resources,
    resource_id_col = "resource_id",
    resource_crs = 4326,
    verbose = FALSE
  )
  expect_equal(nrow(out), 2)
})

test_that("gps_resource_use summarises point and polygon resources", {
  skip_if_not_installed("sf")

  point_use <- gps_resource_use(
    gps_fixture(),
    resources = gps_resource_fixture(),
    radius_m = 20,
    resource_id_col = "resource_id",
    resource_type_col = "resource_type",
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_equal(point_use$resource_id, "water")
  expect_equal(point_use$n_total_fixes, 6)
  expect_equal(point_use$n_fixes_near, 2)
  expect_equal(point_use$prop_fixes_near, 2 / 6)

  zone_use <- gps_resource_use(
    gps_fixture()[1:3, ],
    resources = gps_zone_fixture(),
    radius_m = 0,
    resource_id_col = "resource_id",
    resource_type_col = "resource_type",
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_equal(zone_use$resource_id, "water_zone")
  expect_true(zone_use$n_fixes_near >= 2)
})

test_that("gps_resource_visits detects near-resource bouts", {
  skip_if_not_installed("sf")

  out <- gps_resource_visits(
    gps_fixture(),
    resources = gps_resource_fixture(),
    radius_m = 25,
    max_gap_mins = 2,
    min_fixes = 2,
    resource_id_col = "resource_id",
    resource_type_col = "resource_type",
    metric_crs = 3857,
    verbose = FALSE
  )

  expect_equal(nrow(out), 1)
  expect_equal(out$resource_id, "water")
  expect_equal(out$resource_type, "water")
  expect_equal(out$n_fixes, 2)
  expect_equal(out$duration_mins, 1)
})
