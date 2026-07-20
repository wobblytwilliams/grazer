gps_fixture <- function() {
  t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  data.frame(
    deployment_id = "D1",
    sensor_id = "A",
    datetime = t0 + seq(0, 5) * 60,
    lon = 150 + c(0, 0.0001, 0.0002, 0.0003, 0.0003, 0.0004),
    lat = -30 + c(0, 0, 0.0001, 0.0001, 0.0002, 0.0002),
    stringsAsFactors = FALSE
  )
}

gps_fixture_character_time <- function() {
  dat <- gps_fixture()
  dat$datetime <- format(dat$datetime, "%Y-%m-%d %H:%M:%S", tz = "UTC")
  dat
}

gps_two_sensor_fixture <- function() {
  base <- gps_fixture()[1:3, ]
  other <- base
  other$sensor_id <- "B"
  other$lon <- other$lon + 0.00005
  other$lat <- other$lat + 0.00005
  rbind(base, other)
}

gps_invalid_fixture <- function() {
  data.frame(
    sensor_id = c("A", "", "B", "C", NA),
    datetime = c(
      "2024-01-01 00:00:00",
      "2024-01-01 00:01:00",
      "not a time",
      "2024-01-01 00:03:00",
      "2024-01-01 00:04:00"
    ),
    lon = c(150, 150.1, 150.2, 181, 150.4),
    lat = c(-30, -30.1, -30.2, -30.3, -91),
    stringsAsFactors = FALSE
  )
}

gps_paddock_fixture <- function() {
  skip_if_not_installed("sf")

  coords <- matrix(
    c(
      149.9990, -30.0010,
      150.0010, -30.0010,
      150.0010, -29.9990,
      149.9990, -29.9990,
      149.9990, -30.0010
    ),
    ncol = 2,
    byrow = TRUE
  )

  sf::st_sf(
    NAME = "home",
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  )
}

gps_resource_fixture <- function() {
  skip_if_not_installed("sf")

  sf::st_sf(
    resource_id = c("water", "shade"),
    resource_type = c("water", "shade"),
    geometry = sf::st_sfc(
      sf::st_point(c(150.0000, -30.0000)),
      sf::st_point(c(150.0010, -30.0000)),
      crs = 4326
    )
  )
}

gps_zone_fixture <- function() {
  skip_if_not_installed("sf")

  coords <- matrix(
    c(
      149.9999, -30.0001,
      150.00025, -30.0001,
      150.00025, -29.9999,
      149.9999, -29.9999,
      149.9999, -30.0001
    ),
    ncol = 2,
    byrow = TRUE
  )

  sf::st_sf(
    resource_id = "water_zone",
    resource_type = "water",
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  )
}
