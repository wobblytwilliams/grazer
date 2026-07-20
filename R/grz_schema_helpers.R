grz_gps_required_cols <- function() {
  c("sensor_id", "datetime", "lon", "lat")
}

grz_gps_optional_cols <- function() {
  c(
    "animal_id",
    "deployment_id",
    "paddock",
    "treatment",
    "device_id",
    "fix_quality",
    "satellites",
    "hdop",
    "battery"
  )
}

grz_require_data_frame <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    stop("`", arg, "` must be a data.frame.", call. = FALSE)
  }
  invisible(data)
}

grz_require_gps_cols <- function(data, cols = grz_gps_required_cols(), fun_name = "GPS function") {
  grz_require_data_frame(data)
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    stop("Missing required columns in `data`: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(data)
}

grz_prepare_gps_dt <- function(data, require_cols = TRUE, fun_name = "GPS function") {
  grz_require_data_frame(data)
  dt <- data.table::copy(data.table::as.data.table(data))

  if (!isTRUE(require_cols)) {
    return(dt)
  }

  grz_require_gps_cols(dt, fun_name = fun_name)
  data.table::set(dt, j = "sensor_id", value = as.character(dt[["sensor_id"]]))
  data.table::set(dt, j = "datetime", value = grz_parse_datetime_utc(dt[["datetime"]]))
  data.table::set(dt, j = "lon", value = suppressWarnings(as.numeric(dt[["lon"]])))
  data.table::set(dt, j = "lat", value = suppressWarnings(as.numeric(dt[["lat"]])))
  dt
}

grz_gps_row_flags <- function(data, check_zero_zero = FALSE) {
  grz_require_gps_cols(data, fun_name = "grz_gps_row_flags()")

  bad_sensor <- is.na(data[["sensor_id"]]) | trimws(as.character(data[["sensor_id"]])) == ""
  bad_animal_id <- if ("animal_id" %in% names(data)) {
    is.na(data[["animal_id"]]) | trimws(as.character(data[["animal_id"]])) == ""
  } else {
    rep(FALSE, nrow(data))
  }
  bad_datetime <- is.na(data[["datetime"]])
  missing_lon <- is.na(data[["lon"]])
  missing_lat <- is.na(data[["lat"]])
  invalid_lon <- !missing_lon & (!is.finite(data[["lon"]]) | data[["lon"]] < -180 | data[["lon"]] > 180)
  invalid_lat <- !missing_lat & (!is.finite(data[["lat"]]) | data[["lat"]] < -90 | data[["lat"]] > 90)
  zero_zero <- isTRUE(check_zero_zero) & !missing_lon & !missing_lat & data[["lon"]] == 0 & data[["lat"]] == 0
  bad_lon <- missing_lon | invalid_lon | zero_zero
  bad_lat <- missing_lat | invalid_lat | zero_zero

  data.table::data.table(
    bad_sensor = bad_sensor,
    bad_animal_id = bad_animal_id,
    bad_datetime = bad_datetime,
    missing_lon = missing_lon,
    missing_lat = missing_lat,
    invalid_lon = invalid_lon,
    invalid_lat = invalid_lat,
    zero_zero = zero_zero,
    bad_lon = bad_lon,
    bad_lat = bad_lat,
    bad_row = bad_sensor | bad_datetime | bad_lon | bad_lat
  )
}

grz_gps_invalid_reasons <- function(flags) {
  n <- nrow(flags)
  reasons <- rep("", n)

  add_reason <- function(mask, label) {
    if (!any(mask)) {
      return(invisible(NULL))
    }
    reasons[mask] <<- ifelse(reasons[mask] == "", label, paste(reasons[mask], label, sep = ";"))
    invisible(NULL)
  }

  add_reason(flags$bad_sensor, "missing_sensor_id")
  add_reason(flags$bad_datetime, "invalid_datetime")
  add_reason(flags$missing_lon, "missing_lon")
  add_reason(flags$missing_lat, "missing_lat")
  add_reason(flags$invalid_lon, "invalid_lon")
  add_reason(flags$invalid_lat, "invalid_lat")
  add_reason(flags$zero_zero, "zero_zero")
  reasons
}

grz_gps_valid_coord <- function(data, lon = "lon", lat = "lat", fun_name = "GPS function") {
  grz_require_data_frame(data)
  grz_require_cols(data, c(lon, lat), fun_name = fun_name)

  lon_val <- suppressWarnings(as.numeric(data[[lon]]))
  lat_val <- suppressWarnings(as.numeric(data[[lat]]))
  is.finite(lon_val) & lon_val >= -180 & lon_val <= 180 &
    is.finite(lat_val) & lat_val >= -90 & lat_val <= 90
}

grz_require_sf <- function(fun_name = "spatial function") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("`", fun_name, "` requires the `sf` package.", call. = FALSE)
  }
  invisible(TRUE)
}

grz_require_sf_object <- function(x, arg = "x") {
  if (!inherits(x, "sf")) {
    stop("`", arg, "` must be an sf object.", call. = FALSE)
  }
  invisible(x)
}

grz_gps_as_sf <- function(
  data,
  lon = "lon",
  lat = "lat",
  crs = 4326,
  remove = FALSE,
  require_core = TRUE,
  fun_name = "spatial function"
) {
  grz_require_sf(fun_name)
  grz_require_data_frame(data)

  if (!is.character(lon) || length(lon) != 1L || is.na(lon) || trimws(lon) == "") {
    stop("`lon` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.character(lat) || length(lat) != 1L || is.na(lat) || trimws(lat) == "") {
    stop("`lat` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.logical(remove) || length(remove) != 1L || is.na(remove)) {
    stop("`remove` must be TRUE or FALSE.", call. = FALSE)
  }

  cols <- unique(c(if (isTRUE(require_core)) grz_gps_required_cols() else character(), lon, lat))
  grz_require_cols(data, cols, fun_name = fun_name)

  out <- data.table::copy(data.table::as.data.table(data))
  data.table::set(out, j = lon, value = suppressWarnings(as.numeric(out[[lon]])))
  data.table::set(out, j = lat, value = suppressWarnings(as.numeric(out[[lat]])))

  valid_coord <- grz_gps_valid_coord(out, lon = lon, lat = lat, fun_name = fun_name)
  if (!all(valid_coord)) {
    stop("`", fun_name, "` requires valid longitude and latitude values.", call. = FALSE)
  }

  sf::st_as_sf(out, coords = c(lon, lat), crs = crs, remove = remove)
}
