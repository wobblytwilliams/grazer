test_that("gps_map returns a leaflet widget for valid GPS fixes", {
  skip_if_not_installed("leaflet")

  out <- gps_map(
    gps_fixture(),
    groups = "sensor_id",
    max_points = 4,
    max_groups = 1,
    warnings = FALSE
  )

  expect_s3_class(out, "leaflet")
  expect_s3_class(out, "htmlwidget")
})

test_that("mapping functions use package grouping conventions", {
  map_args <- names(formals(gps_map))
  playback_args <- names(formals(gps_playback))

  expect_true(all(c("groups", "max_points", "max_groups") %in% map_args))
  expect_false(any(c("group", "block", "sample_n", "max_blocks", "max_block") %in% map_args))
  expect_true("groups" %in% playback_args)
  expect_false("group" %in% playback_args)
})

test_that("gps_playback returns a leaflet widget for a short track", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaftime")
  skip_if_not_installed("htmlwidgets")

  out <- gps_playback(
    gps_fixture()[1:3, ],
    groups = "sensor_id",
    align = FALSE,
    point_size_slider = FALSE,
    playback_steps = 10,
    playback_duration_ms = 100,
    warnings = FALSE,
    progress = FALSE,
    show_loading_overlay = FALSE
  )

  expect_s3_class(out, "leaflet")
  expect_s3_class(out, "htmlwidget")
})
