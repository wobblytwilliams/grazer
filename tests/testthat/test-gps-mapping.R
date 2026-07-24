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

  expect_true(all(c(
    "groups", "max_points", "max_groups", "layer_control",
    "polygons_sf", "polygon_label_col", "polygon_group"
  ) %in% map_args))
  expect_false(any(c("group", "block", "sample_n", "max_blocks", "max_block") %in% map_args))
  expect_true("groups" %in% playback_args)
  expect_false("group" %in% playback_args)
})

test_that("gps_map creates separate static overlay layers from multiple group columns", {
  skip_if_not_installed("leaflet")

  dat <- gps_two_sensor_fixture()
  dat$treatment <- ifelse(dat$sensor_id == "A", "control", "shade")
  out <- gps_map(dat, groups = c("sensor_id", "treatment"), warnings = FALSE)
  methods <- vapply(out$x$calls, function(call) call$method, character(1))
  marker_call <- out$x$calls[[which(methods == "addCircleMarkers")]]
  control_call <- out$x$calls[[which(methods == "addLayersControl")]]
  deselect_call <- out$x$calls[[which(methods == "addGrazerLayerDeselectAll")]]
  legend_call <- out$x$calls[[which(methods == "addLegend")]]

  expect_setequal(unique(marker_call$args[[5]]), c("A | control", "B | shade"))
  expect_setequal(control_call$args[[2]], c("A | control", "B | shade"))
  expect_setequal(deselect_call$args[[1]], c("A | control", "B | shade"))
  expect_identical(deselect_call$args[[2]], "Deselect all")
  expect_identical(legend_call$args[[1]]$position, "bottomleft")
})

test_that("gps_map creates switchable animal layers for grouped timelines", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.extras2")
  skip_if_not_installed("htmltools")

  dat <- gps_two_sensor_fixture()
  out <- gps_map(dat, groups = "sensor_id", timeline = TRUE, warnings = FALSE)
  methods <- vapply(out$x$calls, function(call) call$method, character(1))
  timeline_call <- out$x$calls[[which(methods == "addGrazerGroupedTimeslider")]]
  marker_call <- out$x$calls[[which(methods == "addCircleMarkers")]]
  control_call <- out$x$calls[[which(methods == "addLayersControl")]]
  dependency_names <- vapply(out$dependencies, function(dependency) dependency$name, character(1))

  expect_true("addTimeslider" %in% methods)
  expect_true("offsetGrazerTimelineLayerControl" %in% methods)
  expect_setequal(unique(marker_call$args[[5]]), c("A", "B"))
  expect_setequal(control_call$args[[2]], c("A", "B"))
  expect_equal(length(unique(timeline_call$args[[1]])), 3)
  expect_equal(length(timeline_call$args[[1]]), nrow(dat))
  expect_true("grazer-grouped-timeslider" %in% dependency_names)
})

test_that("gps_map accepts projected polygon overlays and includes them in controls", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  ring <- matrix(
    c(150.01, -30.01, 150.03, -30.01, 150.03, -30.03, 150.01, -30.03, 150.01, -30.01),
    ncol = 2,
    byrow = TRUE
  )
  polygons <- sf::st_sf(
    paddock = "north",
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
  polygons <- sf::st_transform(polygons, 32756)

  out <- gps_map(
    gps_fixture()[1:2, ],
    groups = "sensor_id",
    polygons_sf = polygons,
    polygon_label_col = "paddock",
    polygon_group = "Paddocks",
    polygon_color = "#008000",
    warnings = FALSE
  )
  methods <- vapply(out$x$calls, function(call) call$method, character(1))
  polygon_call <- out$x$calls[[which(methods == "addPolygons")]]
  control_call <- out$x$calls[[which(methods == "addLayersControl")]]

  expect_identical(polygon_call$args[[3]], "Paddocks")
  expect_identical(polygon_call$args[[7]], "north")
  expect_identical(polygon_call$args[[4]]$color, "#008000")
  expect_setequal(control_call$args[[2]], c("A", "Paddocks"))
  expect_true(max(out$x$limits$lng, na.rm = TRUE) < 181)
  expect_true(min(out$x$limits$lat, na.rm = TRUE) > -91)
})

test_that("gps_map validates polygon geometry and CRS", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("sf")

  point_sf <- sf::st_sf(
    name = "not a polygon",
    geometry = sf::st_sfc(sf::st_point(c(150, -30)), crs = 4326)
  )
  missing_crs <- sf::st_sf(
    name = "paddock",
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(150, -30, 150.01, -30, 150.01, -30.01, 150, -30.01, 150, -30),
      ncol = 2,
      byrow = TRUE
    ))))
  )

  expect_error(
    gps_map(gps_fixture(), polygons_sf = point_sf, warnings = FALSE),
    "POLYGON or MULTIPOLYGON",
    fixed = TRUE
  )
  expect_error(
    gps_map(gps_fixture(), polygons_sf = missing_crs, warnings = FALSE),
    "declared coordinate reference system",
    fixed = TRUE
  )
})

test_that("gps_map keeps polygon and animal controls on timeline maps", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leaflet.extras2")
  skip_if_not_installed("htmltools")
  skip_if_not_installed("sf")

  ring <- matrix(
    c(150, -30, 150.02, -30, 150.02, -30.02, 150, -30.02, 150, -30),
    ncol = 2,
    byrow = TRUE
  )
  polygons <- sf::st_sf(
    name = "study paddock",
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )

  out <- gps_map(
    gps_two_sensor_fixture(),
    groups = "sensor_id",
    timeline = TRUE,
    polygons_sf = polygons,
    polygon_group = "Paddocks",
    warnings = FALSE
  )
  methods <- vapply(out$x$calls, function(call) call$method, character(1))
  control_call <- out$x$calls[[which(methods == "addLayersControl")]]

  expect_true(all(c("addPolygons", "addTimeslider", "addGrazerGroupedTimeslider") %in% methods))
  expect_true("offsetGrazerTimelineLayerControl" %in% methods)
  expect_setequal(control_call$args[[2]], c("A", "B", "Paddocks"))
})

test_that("gps_map can omit layer controls", {
  skip_if_not_installed("leaflet")

  out <- gps_map(gps_two_sensor_fixture(), groups = "sensor_id", layer_control = FALSE, warnings = FALSE)
  methods <- vapply(out$x$calls, function(call) call$method, character(1))

  expect_false("addLayersControl" %in% methods)
  expect_false("addGrazerLayerDeselectAll" %in% methods)
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
