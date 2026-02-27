grz_parse_datetime_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    out <- as.POSIXct(x, tz = "UTC")
    attr(out, "tzone") <- "UTC"
    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_

  out <- as.POSIXct(rep(NA_real_, length(x_chr)), origin = "1970-01-01", tz = "UTC")
  formats <- c(
    "%Y-%m-%dT%H:%M:%OSZ",
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%d %H:%M:%OS",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%OS%z",
    "%Y-%m-%dT%H:%M:%S%z",
    "%m/%d/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M:%S"
  )

  for (fmt in formats) {
    idx <- is.na(out) & !is.na(x_chr)
    if (!any(idx)) {
      break
    }
    out[idx] <- as.POSIXct(x_chr[idx], format = fmt, tz = "UTC")
  }

  idx <- is.na(out) & !is.na(x_chr)
  if (any(idx)) {
    parsed <- lapply(x_chr[idx], function(value) {
      tryCatch(
        suppressWarnings(as.POSIXct(value, tz = "UTC")),
        error = function(e) as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      )
    })
    out[idx] <- do.call(c, parsed)
  }

  attr(out, "tzone") <- "UTC"
  out
}

#' Validate canonical GPS data
#'
#' Validates required GPS columns and row values, returning cleaned/typed data
#' plus QC and invalid-row details.
#'
#' @param data Input data frame with canonical GPS columns.
#' @param drop_invalid Logical; if `TRUE`, invalid rows are removed from
#'   returned `data`.
#'
#' @return A list with class `grz_validation` containing:
#' \describe{
#'   \item{data}{Typed data (optionally with invalid rows removed).}
#'   \item{qc}{QC summary table (`data.table`).}
#'   \item{invalid_rows}{Rows flagged as invalid with reasons.}
#' }
#' @export
grz_validate_gps <- function(data, drop_invalid = FALSE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  required <- c("sensor_id", "datetime", "lon", "lat")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    stop("Missing required columns in `data`: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  dat <- data.table::copy(data.table::as.data.table(data))
  data.table::set(dat, j = "sensor_id", value = as.character(dat[["sensor_id"]]))
  data.table::set(dat, j = "datetime", value = grz_parse_datetime_utc(dat[["datetime"]]))
  data.table::set(dat, j = "lon", value = suppressWarnings(as.numeric(dat[["lon"]])))
  data.table::set(dat, j = "lat", value = suppressWarnings(as.numeric(dat[["lat"]])))

  bad_sensor <- is.na(dat$sensor_id) | trimws(dat$sensor_id) == ""
  bad_datetime <- is.na(dat$datetime)
  bad_lon <- is.na(dat$lon) | !is.finite(dat$lon) | dat$lon < -180 | dat$lon > 180
  bad_lat <- is.na(dat$lat) | !is.finite(dat$lat) | dat$lat < -90 | dat$lat > 90
  bad_row <- bad_sensor | bad_datetime | bad_lon | bad_lat

  reasons <- rep("", nrow(dat))
  add_reason <- function(mask, label) {
    if (!any(mask)) {
      return(invisible(NULL))
    }
    reasons[mask] <<- ifelse(reasons[mask] == "", label, paste(reasons[mask], label, sep = ";"))
    invisible(NULL)
  }

  add_reason(bad_sensor, "missing_sensor_id")
  add_reason(bad_datetime, "invalid_datetime")
  add_reason(bad_lon, "invalid_lon")
  add_reason(bad_lat, "invalid_lat")

  n_rows <- nrow(dat)
  safe_prop <- function(x) {
    if (n_rows == 0) {
      return(NA_real_)
    }
    x / n_rows
  }

  bad_idx <- which(bad_row)

  qc <- data.table::data.table(
    metric = c(
      "n_rows",
      "n_invalid_rows",
      "n_missing_sensor_id",
      "n_invalid_datetime",
      "n_invalid_lon",
      "n_invalid_lat"
    ),
    count = c(
      n_rows,
      length(bad_idx),
      sum(bad_sensor %in% TRUE),
      sum(bad_datetime %in% TRUE),
      sum(bad_lon %in% TRUE),
      sum(bad_lat %in% TRUE)
    ),
    proportion = c(
      1,
      safe_prop(length(bad_idx)),
      safe_prop(sum(bad_sensor %in% TRUE)),
      safe_prop(sum(bad_datetime %in% TRUE)),
      safe_prop(sum(bad_lon %in% TRUE)),
      safe_prop(sum(bad_lat %in% TRUE))
    )
  )

  invalid_rows <- dat[bad_idx, , drop = FALSE]
  if (nrow(invalid_rows) > 0) {
    invalid_reason <- reasons[bad_idx]
    if (length(invalid_reason) == 0) {
      invalid_reason <- rep(NA_character_, nrow(invalid_rows))
    } else if (length(invalid_reason) != nrow(invalid_rows)) {
      invalid_reason <- rep_len(invalid_reason, nrow(invalid_rows))
    }
    invalid_rows$invalid_reason <- invalid_reason
  }

  out_data <- if (isTRUE(drop_invalid)) {
    if (length(bad_idx) == 0L) dat else dat[-bad_idx, ]
  } else {
    dat
  }
  structure(
    list(
      data = out_data,
      qc = qc,
      invalid_rows = invalid_rows
    ),
    class = "grz_validation"
  )
}
