grz_default_aliases <- function() {
  list(
    sensor_id = c("sensor_id", "id", "IoTId", "animal_id", "cow_id", "GUID", "device_id", "DeviceID", "serial", "Serial"),
    datetime = c("datetime", "time", "Time", "timestamp", "Timestamp", "IngressDate", "date_time"),
    lon = c("lon", "longitude", "Longitude", "long", "x", "coords.x1"),
    lat = c("lat", "latitude", "Latitude", "y", "coords.x2"),
    deployment_id = c("deployment_id", "deployment", "deploymentID")
  )
}

grz_resolve_column <- function(input_names, canonical, colmap, aliases) {
  if (!is.null(colmap) && canonical %in% names(colmap)) {
    mapped <- unname(colmap[[canonical]])
    if (!mapped %in% input_names) {
      stop(
        "Column map for '", canonical, "' points to missing column: '", mapped, "'.",
        call. = FALSE
      )
    }
    return(mapped)
  }

  candidates <- aliases[[canonical]]
  hit <- candidates[candidates %in% input_names]
  if (length(hit) > 0) {
    return(hit[[1]])
  }

  NA_character_
}

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

#' Standardise GPS column names and core types
#'
#' Maps input headings to canonical names (`sensor_id`, `datetime`, `lon`,
#' `lat`), coerces core types, and carries optional metadata columns.
#'
#' @param data Input data frame.
#' @param colmap Optional named character vector mapping canonical names to
#'   source column names.
#' @param deployment_id Optional deployment identifier (single value or vector
#'   of length `nrow(data)`).
#' @param keep_extra Logical; keep non-core source columns.
#'
#' @return A `data.table` with canonical GPS columns.
#' @export
grz_standardise_gps <- function(data, colmap = NULL, deployment_id = NULL, keep_extra = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  if (!is.null(colmap)) {
    if (is.null(names(colmap)) || any(names(colmap) == "")) {
      stop("`colmap` must be a named character vector with canonical names.", call. = FALSE)
    }
    colmap <- as.character(colmap)
  }

  dat <- data.table::copy(data.table::as.data.table(data))
  aliases <- grz_default_aliases()

  src_sensor_id <- grz_resolve_column(names(dat), "sensor_id", colmap, aliases)
  src_datetime <- grz_resolve_column(names(dat), "datetime", colmap, aliases)
  src_lon <- grz_resolve_column(names(dat), "lon", colmap, aliases)
  src_lat <- grz_resolve_column(names(dat), "lat", colmap, aliases)
  src_deployment <- grz_resolve_column(names(dat), "deployment_id", colmap, aliases)

  required_sources <- c(
    sensor_id = src_sensor_id,
    datetime = src_datetime,
    lon = src_lon,
    lat = src_lat
  )
  missing_required <- names(required_sources)[is.na(required_sources)]
  if (length(missing_required) > 0) {
    stop(
      "Missing required columns after mapping: ",
      paste(missing_required, collapse = ", "),
      ". Required input headings are sensor_id, datetime, lon, lat.",
      call. = FALSE
    )
  }

  out <- data.table::data.table(
    sensor_id = as.character(dat[[src_sensor_id]]),
    datetime = grz_parse_datetime_utc(dat[[src_datetime]]),
    lon = suppressWarnings(as.numeric(dat[[src_lon]])),
    lat = suppressWarnings(as.numeric(dat[[src_lat]]))
  )

  dep_value <- NULL
  if (!is.null(deployment_id)) {
    if (length(deployment_id) == 1L) {
      dep_value <- rep(as.character(deployment_id), nrow(out))
    } else if (length(deployment_id) == nrow(out)) {
      dep_value <- as.character(deployment_id)
    } else {
      stop("`deployment_id` must be length 1 or nrow(data).", call. = FALSE)
    }
  } else if (!is.na(src_deployment)) {
    dep_value <- as.character(dat[[src_deployment]])
  }

  if (!is.null(dep_value)) {
    data.table::set(out, j = "deployment_id", value = dep_value)
  }

  if (isTRUE(keep_extra)) {
    used <- c(src_sensor_id, src_datetime, src_lon, src_lat)
    if (!is.na(src_deployment)) {
      used <- c(used, src_deployment)
    }
    extra_cols <- setdiff(names(dat), unique(used[!is.na(used)]))
    if (length(extra_cols) > 0) {
      out <- cbind(out, dat[, ..extra_cols])
    }
  }

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

#' Read and standardise GPS file input
#'
#' Reads CSV or parquet, maps columns to canonical names, and optionally runs
#' validation.
#'
#' @param path File path.
#' @param source Input source type (`"auto"`, `"csv"`, `"parquet"`).
#' @param colmap Optional named mapping for canonical columns.
#' @param deployment_id Optional deployment identifier override.
#' @param keep_extra Logical; keep non-core source columns.
#' @param n_max Optional row limit.
#' @param validate Logical; run `grz_validate_gps()` after standardisation.
#' @param drop_invalid Logical; if validating, drop invalid rows.
#'
#' @return A canonical GPS `data.table`. When `validate = TRUE`, QC attributes
#'   are attached: `qc_summary` and `invalid_rows`.
#' @export
grz_read_gps <- function(
  path,
  source = c("auto", "csv", "parquet"),
  colmap = NULL,
  deployment_id = NULL,
  keep_extra = TRUE,
  n_max = Inf,
  validate = TRUE,
  drop_invalid = FALSE
) {
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  source <- match.arg(source)
  if (identical(source, "auto")) {
    ext <- tolower(tools::file_ext(path))
    source <- switch(
      ext,
      csv = "csv",
      parquet = "parquet",
      stop("Could not infer source from file extension: ", ext, call. = FALSE)
    )
  }

  if (is.finite(n_max) && (length(n_max) != 1L || n_max < 1)) {
    stop("`n_max` must be `Inf` or a positive integer.", call. = FALSE)
  }

  raw <- switch(
    source,
    csv = data.table::fread(
      path,
      check.names = FALSE,
      nrows = if (is.finite(n_max)) as.integer(n_max) else Inf,
      showProgress = FALSE
    ),
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Reading parquet requires the `arrow` package.", call. = FALSE)
      }
      x <- data.table::as.data.table(arrow::read_parquet(path))
      if (is.finite(n_max) && nrow(x) > as.integer(n_max)) {
        x <- x[seq_len(as.integer(n_max)), , drop = FALSE]
      }
      x
    }
  )

  std <- grz_standardise_gps(
    data = raw,
    colmap = colmap,
    deployment_id = deployment_id,
    keep_extra = keep_extra
  )

  if (!isTRUE(validate)) {
    return(std)
  }

  val <- grz_validate_gps(std, drop_invalid = drop_invalid)
  out <- val$data
  attr(out, "qc_summary") <- val$qc
  attr(out, "invalid_rows") <- val$invalid_rows
  out
}
