grz_require_cols <- function(data, cols, fun_name = "function") {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0) {
    stop(
      fun_name,
      " requires columns: ",
      paste(cols, collapse = ", "),
      ". Missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

grz_prepare_gps_dt <- function(data, fun_name = "function", drop_invalid = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  grz_require_cols(data, c("sensor_id", "datetime", "lon", "lat"), fun_name = fun_name)

  dt <- data.table::copy(data.table::as.data.table(data))
  data.table::set(dt, j = "sensor_id", value = as.character(dt[["sensor_id"]]))
  data.table::set(dt, j = "datetime", value = grz_parse_datetime_utc(dt[["datetime"]]))
  data.table::set(dt, j = "lon", value = suppressWarnings(as.numeric(dt[["lon"]])))
  data.table::set(dt, j = "lat", value = suppressWarnings(as.numeric(dt[["lat"]])))

  valid <- !is.na(dt$sensor_id) &
    trimws(dt$sensor_id) != "" &
    !is.na(dt$datetime) &
    is.finite(dt$lon) & dt$lon >= -180 & dt$lon <= 180 &
    is.finite(dt$lat) & dt$lat >= -90 & dt$lat <= 90

  if (isTRUE(drop_invalid)) {
    dt <- dt[valid, ]
  }

  data.table::setorderv(dt, c("sensor_id", "datetime"))
  dt
}

grz_id_cols <- function(data) {
  if ("deployment_id" %in% names(data)) {
    return(c("deployment_id", "sensor_id"))
  }
  "sensor_id"
}

grz_haversine_m <- function(lon1, lat1, lon2, lat2) {
  lon1 <- as.numeric(lon1)
  lat1 <- as.numeric(lat1)
  lon2 <- as.numeric(lon2)
  lat2 <- as.numeric(lat2)

  out <- rep(NA_real_, length(lon1))
  ok <- is.finite(lon1) & is.finite(lat1) & is.finite(lon2) & is.finite(lat2)
  if (!any(ok)) {
    return(out)
  }

  r <- 6371000
  to_rad <- pi / 180

  phi1 <- lat1[ok] * to_rad
  phi2 <- lat2[ok] * to_rad
  dphi <- (lat2[ok] - lat1[ok]) * to_rad
  dlambda <- (lon2[ok] - lon1[ok]) * to_rad

  a <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  out[ok] <- r * c
  out
}

grz_bearing_deg <- function(lon1, lat1, lon2, lat2) {
  lon1 <- as.numeric(lon1)
  lat1 <- as.numeric(lat1)
  lon2 <- as.numeric(lon2)
  lat2 <- as.numeric(lat2)

  out <- rep(NA_real_, length(lon1))
  ok <- is.finite(lon1) & is.finite(lat1) & is.finite(lon2) & is.finite(lat2)
  if (!any(ok)) {
    return(out)
  }

  to_rad <- pi / 180
  phi1 <- lat1[ok] * to_rad
  phi2 <- lat2[ok] * to_rad
  lambda1 <- lon1[ok] * to_rad
  lambda2 <- lon2[ok] * to_rad

  y <- sin(lambda2 - lambda1) * cos(phi2)
  x <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(lambda2 - lambda1)
  b <- atan2(y, x) * 180 / pi
  out[ok] <- (b + 360) %% 360
  out
}

grz_abs_turn_rad <- function(current_bearing_deg, previous_bearing_deg) {
  d <- (current_bearing_deg - previous_bearing_deg + 180) %% 360 - 180
  abs(d) * pi / 180
}

grz_period_label <- function(datetime, period) {
  period <- match.arg(period, c("total", "week", "date", "paddock_week"))

  if (period == "total") {
    return(rep("total", length(datetime)))
  }
  if (period == "week") {
    return(strftime(datetime, format = "%G-W%V", tz = "UTC"))
  }
  if (period == "date") {
    return(as.character(as.Date(datetime, tz = "UTC")))
  }
  strftime(datetime, format = "%G-W%V", tz = "UTC")
}

grz_lonlat_to_xy_m <- function(lon, lat) {
  lon <- as.numeric(lon)
  lat <- as.numeric(lat)
  lat0 <- mean(lat, na.rm = TRUE)
  x <- lon * 111320 * cos(lat0 * pi / 180)
  y <- lat * 110540
  list(x = x, y = y)
}

grz_polygon_area_m2 <- function(x, y) {
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  idx <- c(seq_len(n), 1L)
  abs(sum(x[idx[-1]] * y[idx[-length(idx)]] - x[idx[-length(idx)]] * y[idx[-1]]) / 2)
}

grz_convex_hull_area_ha <- function(lon, lat) {
  ok <- is.finite(lon) & is.finite(lat)
  lon <- as.numeric(lon[ok])
  lat <- as.numeric(lat[ok])
  if (length(lon) < 3) {
    return(NA_real_)
  }

  xy <- grz_lonlat_to_xy_m(lon, lat)
  hull <- chull(xy$x, xy$y)
  area_m2 <- grz_polygon_area_m2(xy$x[hull], xy$y[hull])
  area_m2 / 10000
}

grz_mcp95_area_ha <- function(lon, lat) {
  ok <- is.finite(lon) & is.finite(lat)
  lon <- as.numeric(lon[ok])
  lat <- as.numeric(lat[ok])
  n <- length(lon)
  if (n < 5) {
    return(NA_real_)
  }

  xy <- grz_lonlat_to_xy_m(lon, lat)
  cx <- mean(xy$x)
  cy <- mean(xy$y)
  r <- sqrt((xy$x - cx)^2 + (xy$y - cy)^2)
  keep <- r <= stats::quantile(r, probs = 0.95, na.rm = TRUE, type = 7)
  if (sum(keep) < 3) {
    return(NA_real_)
  }

  hull <- chull(xy$x[keep], xy$y[keep])
  area_m2 <- grz_polygon_area_m2(xy$x[keep][hull], xy$y[keep][hull])
  area_m2 / 10000
}

grz_ellipse95_area_ha <- function(lon, lat) {
  ok <- is.finite(lon) & is.finite(lat)
  lon <- as.numeric(lon[ok])
  lat <- as.numeric(lat[ok])
  if (length(lon) < 3) {
    return(NA_real_)
  }

  xy <- grz_lonlat_to_xy_m(lon, lat)
  cov_xy <- stats::cov(cbind(xy$x, xy$y), use = "complete.obs")
  if (any(!is.finite(cov_xy))) {
    return(NA_real_)
  }

  det_cov <- det(cov_xy)
  if (!is.finite(det_cov) || det_cov <= 0) {
    return(NA_real_)
  }

  area_m2 <- pi * stats::qchisq(0.95, df = 2) * sqrt(det_cov)
  area_m2 / 10000
}

grz_pairwise_haversine_matrix <- function(lon, lat) {
  n <- length(lon)
  out <- matrix(0, nrow = n, ncol = n)
  if (n <= 1L) {
    return(out)
  }

  for (i in seq_len(n - 1L)) {
    d <- grz_haversine_m(
      lon1 = rep(lon[i], n - i),
      lat1 = rep(lat[i], n - i),
      lon2 = lon[(i + 1L):n],
      lat2 = lat[(i + 1L):n]
    )
    out[i, (i + 1L):n] <- d
    out[(i + 1L):n, i] <- d
  }
  out
}
