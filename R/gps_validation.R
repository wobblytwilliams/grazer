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

grz_count_ids <- function(x) {
  if (is.null(x)) {
    return(NA_integer_)
  }
  value <- trimws(as.character(x))
  value <- value[!is.na(value) & value != ""]
  data.table::uniqueN(value)
}

grz_safe_prop <- function(count, denom) {
  if (!is.finite(denom) || denom <= 0) {
    return(NA_real_)
  }
  count / denom
}

grz_gps_groups <- function(data, groups = NULL, fun_name = "GPS function") {
  if (is.null(groups)) {
    return(grz_default_group_cols(data))
  }
  if (!is.character(groups) || length(groups) < 1L || any(is.na(groups)) || any(trimws(groups) == "")) {
    stop("`groups` must be NULL or a non-empty character vector.", call. = FALSE)
  }
  grz_require_cols(data, groups, fun_name = fun_name)
  unique(groups)
}

grz_gps_duplicate_flags <- function(data, groups) {
  n <- nrow(data)
  duplicate_fix <- rep(FALSE, n)
  duplicate_record <- rep(FALSE, n)

  fix_keys <- intersect(unique(c(groups, "datetime")), names(data))
  complete_fix_key <- rep(TRUE, n)
  for (key in fix_keys) {
    value <- data[[key]]
    if (is.character(value)) {
      complete_fix_key <- complete_fix_key & !is.na(value) & trimws(value) != ""
    } else {
      complete_fix_key <- complete_fix_key & !is.na(value)
    }
  }

  if (length(fix_keys) > 0L && any(complete_fix_key)) {
    duplicate_fix[complete_fix_key] <- duplicated(data[complete_fix_key, ], by = fix_keys)
  }
  if (n > 0L) {
    duplicate_record <- duplicated(data, by = names(data))
  }

  data.table::data.table(
    duplicate_fix = duplicate_fix,
    duplicate_record = duplicate_record
  )
}

grz_empty_gps_intervals <- function(groups) {
  out <- data.table::data.table(
    .grz_row_id = integer(),
    .grz_previous_row_id = integer(),
    previous_datetime = as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC"),
    datetime = as.POSIXct(numeric(), origin = "1970-01-01", tz = "UTC"),
    interval_s = numeric(),
    interval_mins = numeric(),
    interval_type = character(),
    is_non_positive_interval = logical(),
    is_large_gap = logical()
  )
  for (group in rev(groups)) {
    out[, c(group) := character()]
    data.table::setcolorder(out, c(group, setdiff(names(out), group)))
  }
  out
}

grz_gps_intervals_from_dt <- function(data, groups, large_gap_mins = 60) {
  if (!is.numeric(large_gap_mins) || length(large_gap_mins) != 1L || is.na(large_gap_mins) || large_gap_mins <= 0) {
    stop("`large_gap_mins` must be a single positive number.", call. = FALSE)
  }

  dt <- data.table::copy(data)
  data.table::set(dt, j = ".grz_row_id", value = seq_len(nrow(dt)))
  dt <- dt[!is.na(dt[["datetime"]])]
  if (nrow(dt) == 0L) {
    return(grz_empty_gps_intervals(groups))
  }

  data.table::setorderv(dt, c(groups, ".grz_row_id"))
  dt[, c(".grz_previous_row_id") := data.table::shift(.SD[[1L]]), by = groups, .SDcols = ".grz_row_id"]
  dt[, c("previous_datetime") := data.table::shift(.SD[[1L]]), by = groups, .SDcols = "datetime"]
  dt <- dt[!is.na(dt[["previous_datetime"]])]
  if (nrow(dt) == 0L) {
    return(grz_empty_gps_intervals(groups))
  }

  data.table::set(dt, j = "interval_s", value = as.numeric(dt[["datetime"]] - dt[["previous_datetime"]], units = "secs"))
  data.table::set(dt, j = "interval_mins", value = dt[["interval_s"]] / 60)
  data.table::set(dt, j = "is_non_positive_interval", value = !is.na(dt[["interval_s"]]) & dt[["interval_s"]] <= 0)
  data.table::set(dt, j = "is_large_gap", value = !is.na(dt[["interval_mins"]]) & dt[["interval_mins"]] > large_gap_mins)
  dt[, interval_type := data.table::fcase(
    is.na(interval_s), NA_character_,
    interval_s < 0, "negative",
    interval_s == 0, "zero",
    interval_mins > large_gap_mins, "large_gap",
    default = "positive"
  )]

  keep <- unique(c(groups, ".grz_row_id", ".grz_previous_row_id", "previous_datetime", "datetime", "interval_s", "interval_mins", "interval_type", "is_non_positive_interval", "is_large_gap"))
  dt[, keep, with = FALSE]
}

grz_count_out_of_order <- function(data, groups) {
  dt <- data.table::copy(data)
  if (nrow(dt) == 0L || !"datetime" %in% names(dt)) {
    return(0L)
  }
  dt[, c("previous_datetime") := data.table::shift(.SD[[1L]]), by = groups, .SDcols = "datetime"]
  sum(!is.na(dt[["datetime"]]) & !is.na(dt[["previous_datetime"]]) & dt[["datetime"]] < dt[["previous_datetime"]])
}

grz_qc_row <- function(metric, count = NA_real_, proportion = NA_real_, value = NA_real_, datetime_value = as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC"), threshold_mins = NA_real_) {
  data.table::data.table(
    metric = metric,
    count = as.numeric(count),
    proportion = as.numeric(proportion),
    value = as.numeric(value),
    datetime_value = datetime_value,
    threshold_mins = as.numeric(threshold_mins)
  )
}

grz_gps_schema_find_datetime_cols <- function(data, exclude = character()) {
  candidates <- setdiff(names(data), exclude)
  hits <- character()
  for (col in candidates) {
    value <- data[[col]]
    non_missing <- !is.na(value) & trimws(as.character(value)) != ""
    if (!any(non_missing)) {
      next
    }
    parsed <- grz_parse_datetime_utc(value)
    prop <- mean(!is.na(parsed[non_missing]))
    if (is.finite(prop) && prop >= 0.8) {
      hits <- c(hits, col)
    }
  }
  hits
}

grz_gps_schema_find_coord_cols <- function(data, exclude = character(), kind = c("lon", "lat")) {
  kind <- match.arg(kind)
  candidates <- setdiff(names(data), exclude)
  hits <- character()
  lower <- if (kind == "lon") -180 else -90
  upper <- if (kind == "lon") 180 else 90

  for (col in candidates) {
    value <- suppressWarnings(as.numeric(data[[col]]))
    value <- value[is.finite(value)]
    if (length(value) == 0L) {
      next
    }
    prop <- mean(value >= lower & value <= upper)
    if (is.finite(prop) && prop >= 0.8) {
      hits <- c(hits, col)
    }
  }
  hits
}

grz_gps_schema_message <- function(data, missing_cols, parse_issues) {
  lines <- c(
    "GPS data are not in the expected format.",
    "Required columns are: sensor_id, datetime, lon, lat.",
    "Expected formats: sensor_id is a non-empty identifier; datetime is parseable as a date-time; lon is decimal degrees from -180 to 180; lat is decimal degrees from -90 to 90."
  )

  if (length(missing_cols) > 0L) {
    lines <- c(lines, paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "), "."))
  }

  present <- intersect(names(parse_issues), grz_gps_required_cols())
  for (col in present) {
    lines <- c(lines, parse_issues[[col]])
  }

  if (length(missing_cols) > 0L) {
    exclude <- intersect(grz_gps_required_cols(), names(data))
    if ("datetime" %in% missing_cols) {
      hits <- grz_gps_schema_find_datetime_cols(data, exclude = exclude)
      if (length(hits) > 0L) {
        lines <- c(lines, paste0("Datetime-like values appear to be present in column(s): ", paste(hits, collapse = ", "), ". The datetime column must be named `datetime`."))
      }
    }
    if ("lon" %in% missing_cols) {
      hits <- grz_gps_schema_find_coord_cols(data, exclude = exclude, kind = "lon")
      if (length(hits) > 0L) {
        lines <- c(lines, paste0("Longitude-like numeric values appear to be present in column(s): ", paste(hits, collapse = ", "), ". The longitude column must be named `lon`."))
      }
    }
    if ("lat" %in% missing_cols) {
      hits <- grz_gps_schema_find_coord_cols(data, exclude = exclude, kind = "lat")
      if (length(hits) > 0L) {
        lines <- c(lines, paste0("Latitude-like numeric values appear to be present in column(s): ", paste(hits, collapse = ", "), ". The latitude column must be named `lat`."))
      }
    }
  }

  paste(lines, collapse = "\n")
}

grz_validate_schema <- function(data) {
  grz_require_data_frame(data)
  required <- grz_gps_required_cols()
  missing_cols <- setdiff(required, names(data))
  parse_issues <- list()

  if ("sensor_id" %in% names(data)) {
    sensor <- trimws(as.character(data$sensor_id))
    if (!any(!is.na(sensor) & sensor != "")) {
      parse_issues$sensor_id <- "The `sensor_id` column is present but does not contain any non-empty sensor identifiers."
    }
  }
  if ("datetime" %in% names(data)) {
    raw <- data$datetime
    non_missing <- !is.na(raw) & trimws(as.character(raw)) != ""
    parsed <- grz_parse_datetime_utc(raw)
    if (any(non_missing) && !any(!is.na(parsed))) {
      parse_issues$datetime <- "The `datetime` column is present but values could not be parsed as date-time values."
    } else if (!any(non_missing)) {
      parse_issues$datetime <- "The `datetime` column is present but does not contain any non-missing date-time values."
    }
  }
  if ("lon" %in% names(data)) {
    lon <- suppressWarnings(as.numeric(data$lon))
    if (!any(is.finite(lon) & lon >= -180 & lon <= 180)) {
      parse_issues$lon <- "The `lon` column is present but does not contain any valid longitude values in decimal degrees from -180 to 180."
    }
  }
  if ("lat" %in% names(data)) {
    lat <- suppressWarnings(as.numeric(data$lat))
    if (!any(is.finite(lat) & lat >= -90 & lat <= 90)) {
      parse_issues$lat <- "The `lat` column is present but does not contain any valid latitude values in decimal degrees from -90 to 90."
    }
  }

  is_valid <- length(missing_cols) == 0L && length(parse_issues) == 0L
  list(
    is_valid = is_valid,
    message = if (is_valid) {
      sprintf(
        "Valid GPS dataset: %s rows; required columns present; %s additional columns present.",
        format(nrow(data), big.mark = ","),
        format(max(0L, length(setdiff(names(data), required))), big.mark = ",")
      )
    } else {
      grz_gps_schema_message(data, missing_cols = missing_cols, parse_issues = parse_issues)
    },
    missing_columns = missing_cols,
    parse_issues = parse_issues
  )
}

grz_gps_qc_summary_from_dt <- function(data, flags, intervals, duplicates, n_out_of_order, large_gap_mins) {
  n_rows <- nrow(data)
  n_intervals <- nrow(intervals)
  valid_time <- data$datetime[!is.na(data$datetime)]
  datetime_na <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")

  datetime_min <- if (length(valid_time) == 0L) datetime_na else min(valid_time)
  datetime_max <- if (length(valid_time) == 0L) datetime_na else max(valid_time)
  attr(datetime_min, "tzone") <- "UTC"
  attr(datetime_max, "tzone") <- "UTC"

  interval_values <- intervals$interval_s
  interval_values <- interval_values[is.finite(interval_values)]
  interval_quantile <- function(prob) {
    if (length(interval_values) == 0L) {
      return(NA_real_)
    }
    as.numeric(stats::quantile(interval_values, probs = prob, na.rm = TRUE, type = 7))
  }

  missing_coordinate <- flags$missing_lon | flags$missing_lat
  invalid_coordinate <- flags$bad_lon | flags$bad_lat

  rows <- list(
    grz_qc_row("n_rows", n_rows, 1),
    grz_qc_row("n_fixes", n_rows, 1),
    grz_qc_row("n_sensors", grz_count_ids(data$sensor_id)),
    grz_qc_row("n_animals", if ("animal_id" %in% names(data)) grz_count_ids(data$animal_id) else NA_real_),
    grz_qc_row("datetime_min", datetime_value = datetime_min),
    grz_qc_row("datetime_max", datetime_value = datetime_max),
    grz_qc_row("n_invalid_rows", sum(flags$bad_row), grz_safe_prop(sum(flags$bad_row), n_rows)),
    grz_qc_row("n_missing_sensor_id", sum(flags$bad_sensor), grz_safe_prop(sum(flags$bad_sensor), n_rows)),
    grz_qc_row("n_missing_animal_id", if ("animal_id" %in% names(data)) sum(flags$bad_animal_id) else NA_real_, if ("animal_id" %in% names(data)) grz_safe_prop(sum(flags$bad_animal_id), n_rows) else NA_real_),
    grz_qc_row("n_invalid_datetime", sum(flags$bad_datetime), grz_safe_prop(sum(flags$bad_datetime), n_rows)),
    grz_qc_row("n_missing_lon", sum(flags$missing_lon), grz_safe_prop(sum(flags$missing_lon), n_rows)),
    grz_qc_row("n_missing_lat", sum(flags$missing_lat), grz_safe_prop(sum(flags$missing_lat), n_rows)),
    grz_qc_row("n_missing_coordinates", sum(missing_coordinate), grz_safe_prop(sum(missing_coordinate), n_rows)),
    grz_qc_row("n_invalid_lon", sum(flags$bad_lon), grz_safe_prop(sum(flags$bad_lon), n_rows)),
    grz_qc_row("n_invalid_lat", sum(flags$bad_lat), grz_safe_prop(sum(flags$bad_lat), n_rows)),
    grz_qc_row("n_invalid_coordinates", sum(invalid_coordinate), grz_safe_prop(sum(invalid_coordinate), n_rows)),
    grz_qc_row("n_zero_zero", sum(flags$zero_zero), grz_safe_prop(sum(flags$zero_zero), n_rows)),
    grz_qc_row("n_duplicate_fixes", sum(duplicates$duplicate_fix), grz_safe_prop(sum(duplicates$duplicate_fix), n_rows)),
    grz_qc_row("n_duplicate_records", sum(duplicates$duplicate_record), grz_safe_prop(sum(duplicates$duplicate_record), n_rows)),
    grz_qc_row("n_out_of_order", n_out_of_order, grz_safe_prop(n_out_of_order, n_rows)),
    grz_qc_row("n_intervals", n_intervals, if (n_rows > 1L) grz_safe_prop(n_intervals, n_rows - 1L) else NA_real_),
    grz_qc_row("interval_min_s", value = if (length(interval_values) == 0L) NA_real_ else min(interval_values)),
    grz_qc_row("interval_q25_s", value = interval_quantile(0.25)),
    grz_qc_row("interval_median_s", value = interval_quantile(0.5)),
    grz_qc_row("interval_mean_s", value = if (length(interval_values) == 0L) NA_real_ else mean(interval_values)),
    grz_qc_row("interval_q75_s", value = interval_quantile(0.75)),
    grz_qc_row("interval_max_s", value = if (length(interval_values) == 0L) NA_real_ else max(interval_values)),
    grz_qc_row("n_non_positive_intervals", sum(intervals$is_non_positive_interval), grz_safe_prop(sum(intervals$is_non_positive_interval), n_intervals)),
    grz_qc_row("n_large_gaps", sum(intervals$is_large_gap), grz_safe_prop(sum(intervals$is_large_gap), n_intervals), threshold_mins = large_gap_mins)
  )

  data.table::rbindlist(rows)
}

validate_grz_gps <- function(
  data,
  drop_invalid = FALSE,
  large_gap_mins = 60,
  groups = NULL,
  check_zero_zero = FALSE
) {
  schema <- grz_validate_schema(data)
  if (!isTRUE(schema$is_valid)) {
    out <- structure(
      list(
        is_valid = FALSE,
        message = schema$message,
        required_columns = grz_gps_required_cols(),
        missing_columns = schema$missing_columns,
        parse_issues = schema$parse_issues,
        data = data,
        qc = NULL,
        invalid_rows = NULL,
        intervals = NULL
      ),
      class = "grz_validation"
    )
    return(out)
  }

  dat <- grz_prepare_gps_dt(data, require_cols = TRUE, fun_name = "gps_validate()")
  group_cols <- grz_gps_groups(dat, groups = groups, fun_name = "gps_validate()")
  flags <- grz_gps_row_flags(dat, check_zero_zero = check_zero_zero)
  reasons <- grz_gps_invalid_reasons(flags)

  intervals <- grz_gps_intervals_from_dt(dat, groups = group_cols, large_gap_mins = large_gap_mins)
  duplicates <- grz_gps_duplicate_flags(dat, groups = group_cols)
  n_out_of_order <- grz_count_out_of_order(dat, groups = group_cols)
  qc <- grz_gps_qc_summary_from_dt(
    data = dat,
    flags = flags,
    intervals = intervals,
    duplicates = duplicates,
    n_out_of_order = n_out_of_order,
    large_gap_mins = large_gap_mins
  )
  bad_idx <- which(flags$bad_row)

  invalid_rows <- dat[bad_idx, , drop = FALSE]
  if (nrow(invalid_rows) > 0) {
    invalid_rows[, row_id := bad_idx]
    data.table::setcolorder(invalid_rows, c("row_id", setdiff(names(invalid_rows), "row_id")))
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
      invalid_rows = invalid_rows,
      intervals = intervals,
      is_valid = TRUE,
      message = grz_validate_schema(data)$message
    ),
    class = "grz_validation"
  )
}

grz_validation_as_class <- function(validation, return_class = c("data.frame", "data.table")) {
  rc <- grz_match_output_class(return_class)
  if (!is.null(validation$data) && isTRUE(validation$is_valid)) {
    validation$data <- grz_as_output(validation$data, rc)
  }
  if (!is.null(validation$qc)) {
    validation$qc <- grz_as_output(validation$qc, rc)
  }
  if (!is.null(validation$invalid_rows)) {
    validation$invalid_rows <- grz_as_output(validation$invalid_rows, rc)
  }
  if (!is.null(validation$intervals)) {
    validation$intervals <- grz_as_output(validation$intervals, rc)
  }
  validation
}

#' @export
print.grz_validation <- function(x, ...) {
  cat(x$message, "\n", sep = "")
  invisible(x)
}

#' Validate GPS data
#'
#' Checks that the required GPS columns are present and that the core fields are
#' parseable. Row-level data quality checks are handled by `gps_qc_summary()`
#' and cleaning functions.
#'
#' @param data Input data frame with standard GPS columns.
#' @param drop_invalid Logical; if `TRUE`, rows with invalid required fields
#'   are removed from returned `data`.
#' @param large_gap_mins Positive number. Intervals longer than this are counted
#'   as large gaps.
#' @param groups Optional grouping columns used for ordering, duplicates, and
#'   interval checks. Defaults to `deployment_id` and `sensor_id` when
#'   `deployment_id` is present, otherwise `sensor_id`.
#' @param check_zero_zero Logical; flag `(0, 0)` coordinates as invalid.
#' @param return_class Output class for returned tables: `"data.frame"`
#'   (default) or `"data.table"`.
#'
#' @return A list with class `grz_validation` containing the validation status,
#'   message, typed data when valid, and compatibility tables for older
#'   workflows.
#' @export
gps_validate <- function(
  data,
  drop_invalid = FALSE,
  large_gap_mins = 60,
  groups = NULL,
  check_zero_zero = FALSE,
  return_class = c("data.frame", "data.table")
) {
  val <- validate_grz_gps(
    data = data,
    drop_invalid = drop_invalid,
    large_gap_mins = large_gap_mins,
    groups = groups,
    check_zero_zero = check_zero_zero
  )
  grz_validation_as_class(val, return_class = return_class)
}

#' Append continuous GPS segment identifiers
#'
#' Adds a `segment_id` column for continuous pieces of each sensor track after a
#' large time gap or a negative time interval. Zero-length intervals stay in the
#' same segment because they are usually duplicate fixes rather than a true break
#' in the track. The default `segment_id` combines `sensor_id` with the local
#' segment number, for example `C001_seg001`.
#'
#' @param data Input data frame with standard GPS columns.
#' @param large_gap_mins Positive number. A new segment starts after intervals
#'   longer than this value.
#' @param groups Optional grouping columns. Defaults to `deployment_id` and
#'   `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.
#' @param segment_col Name of the output segment column.
#' @param verbose Logical; print a short summary.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return GPS data with a `segment_id` column appended by default.
#' @export
gps_append_segments <- function(
  data,
  large_gap_mins = 60,
  groups = NULL,
  segment_col = "segment_id",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  if (!is.numeric(large_gap_mins) || length(large_gap_mins) != 1L || is.na(large_gap_mins) || large_gap_mins <= 0) {
    stop("`large_gap_mins` must be a single positive number.", call. = FALSE)
  }
  if (!is.character(segment_col) || length(segment_col) != 1L || is.na(segment_col) || trimws(segment_col) == "") {
    stop("`segment_col` must be a single non-empty column name.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, require_cols = TRUE, fun_name = "gps_append_segments()")
  grp <- grz_gps_groups(dt, groups = groups, fun_name = "gps_append_segments()")
  dt[, .grz_row_id := .I]
  data.table::setorderv(dt, c(grp, ".grz_row_id"))
  dt[, .grz_prev_datetime := data.table::shift(datetime), by = grp]
  dt[, .grz_interval_mins := as.numeric(datetime - .grz_prev_datetime, units = "mins")]
  dt[, .grz_new_segment := is.na(.grz_interval_mins) | .grz_interval_mins < 0 | .grz_interval_mins > large_gap_mins]
  dt[, .grz_local_segment := cumsum(.grz_new_segment), by = grp]
  dt[, .grz_sensor_for_segment := trimws(as.character(sensor_id))]
  dt[is.na(.grz_sensor_for_segment) | .grz_sensor_for_segment == "", .grz_sensor_for_segment := NA_character_]
  dt[, (segment_col) := data.table::fifelse(
    is.na(.grz_sensor_for_segment),
    NA_character_,
    paste0(.grz_sensor_for_segment, "_seg", sprintf("%03d", as.integer(.grz_local_segment)))
  )]
  dt[, c(".grz_prev_datetime", ".grz_interval_mins", ".grz_new_segment", ".grz_local_segment", ".grz_sensor_for_segment") := NULL]
  data.table::setorderv(dt, ".grz_row_id")
  dt[, .grz_row_id := NULL]

  if (isTRUE(verbose)) {
    n_segments <- data.table::uniqueN(dt[[segment_col]])
    cat(sprintf("[gps_append_segments] rows=%s segments=%s segment_col=%s\n", format(nrow(dt), big.mark = ","), format(n_segments, big.mark = ","), segment_col))
  }

  grz_as_output(dt, rc)
}

#' Check GPS track gaps
#'
#' Finds large gaps and non-positive intervals between successive fixes within
#' each GPS stream. Use this before appending continuous `segment_id` values.
#'
#' @param data Input data frame with standard GPS columns.
#' @param large_gap_mins Positive number. Intervals longer than this are flagged
#'   as large gaps.
#' @param groups Optional grouping columns. Defaults to `deployment_id` and
#'   `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return A data frame with one row per large gap or non-positive interval.
#' @export
gps_check_gaps <- function(
  data,
  large_gap_mins = 60,
  groups = NULL,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  dat <- grz_prepare_gps_dt(data, require_cols = TRUE, fun_name = "gps_check_gaps()")
  group_cols <- grz_gps_groups(dat, groups = groups, fun_name = "gps_check_gaps()")
  intervals <- grz_gps_intervals_from_dt(dat, groups = group_cols, large_gap_mins = large_gap_mins)
  gaps <- intervals[is_large_gap %in% TRUE | is_non_positive_interval %in% TRUE]
  keep <- unique(c(
    group_cols,
    "datetime",
    "previous_datetime",
    "interval_mins",
    "interval_type",
    "is_large_gap",
    "is_non_positive_interval"
  ))
  grz_as_output(gaps[, keep, with = FALSE], rc)
}

#' Summarise GPS quality control
#'
#' Produces a structured quality-control object for row-level issues,
#' duplicated records, time intervals, and large gaps.
#'
#' @param data Input data frame with standard GPS columns.
#' @param large_gap_mins Positive number. Intervals longer than this are counted
#'   as large gaps.
#' @param groups Optional grouping columns. Defaults to `deployment_id` and
#'   `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.
#' @param check_zero_zero Logical; flag `(0, 0)` coordinates as invalid.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return A list with class `grz_qc` containing a printed summary and issue
#'   tables.
#' @export
gps_qc_summary <- function(
  data,
  large_gap_mins = 60,
  groups = NULL,
  check_zero_zero = FALSE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  val <- validate_grz_gps(
    data = data,
    drop_invalid = FALSE,
    large_gap_mins = large_gap_mins,
    groups = groups,
    check_zero_zero = check_zero_zero
  )
  if (!isTRUE(val$is_valid)) {
    stop(val$message, call. = FALSE)
  }

  dat <- data.table::as.data.table(val$data)
  dat[, row_id := .I]
  group_cols <- grz_gps_groups(dat, groups = groups, fun_name = "gps_qc_summary()")
  flags <- grz_gps_row_flags(dat, check_zero_zero = check_zero_zero)
  reasons <- grz_gps_invalid_reasons(flags)
  intervals <- data.table::as.data.table(val$intervals)
  duplicates <- grz_gps_duplicate_flags(dat, groups = group_cols)

  invalid_rows <- dat[flags$bad_row]
  if (nrow(invalid_rows) > 0L) {
    invalid_rows[, invalid_reason := reasons[flags$bad_row]]
  }

  duplicate_rows <- dat[duplicates$duplicate_record | duplicates$duplicate_fix]
  if (nrow(duplicate_rows) > 0L) {
    duplicate_rows[, `:=`(
      duplicate_record = duplicates$duplicate_record[duplicates$duplicate_record | duplicates$duplicate_fix],
      duplicate_fix = duplicates$duplicate_fix[duplicates$duplicate_record | duplicates$duplicate_fix]
    )]
  }

  gap_rows <- data.table::data.table()
  large_gaps <- intervals[is_large_gap == TRUE]
  if (nrow(large_gaps) > 0L) {
    gap_rows <- data.table::rbindlist(lapply(seq_len(nrow(large_gaps)), function(i) {
      row <- large_gaps[i]
      before <- dat[row_id == row$.grz_previous_row_id]
      after <- dat[row_id == row$.grz_row_id]
      if (nrow(before) == 0L || nrow(after) == 0L) {
        return(NULL)
      }
      out <- data.table::rbindlist(list(before, after), use.names = TRUE, fill = TRUE)
      out[, `:=`(
        gap_id = i,
        gap_side = c("before", "after"),
        gap_mins = row$interval_mins
      )]
      keep <- unique(c("gap_id", "gap_side", "row_id", group_cols, "datetime", "lon", "lat", "gap_mins"))
      out[, keep, with = FALSE]
    }), use.names = TRUE, fill = TRUE)
  }

  non_positive <- intervals[is_non_positive_interval == TRUE]
  if (nrow(non_positive) > 0L) {
    keep <- unique(c(group_cols, ".grz_previous_row_id", ".grz_row_id", "previous_datetime", "datetime", "interval_s", "interval_mins", "interval_type"))
    non_positive <- non_positive[, keep, with = FALSE]
    data.table::setnames(non_positive, c(".grz_previous_row_id", ".grz_row_id"), c("previous_row_id", "row_id"))
  }

  valid_time <- dat$datetime[!is.na(dat$datetime)]
  time_range <- if (length(valid_time) == 0L) {
    "time range unavailable"
  } else {
    paste0(
      format(min(valid_time), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      " to ",
      format(max(valid_time), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      " UTC"
    )
  }
  n_zero <- sum(intervals$interval_type == "zero", na.rm = TRUE)
  n_negative <- sum(intervals$interval_type == "negative", na.rm = TRUE)
  n_large <- sum(intervals$is_large_gap, na.rm = TRUE)
  n_invalid <- sum(flags$bad_row)
  n_dup_record <- sum(duplicates$duplicate_record)

  summary <- c(
    sprintf("GPS QC summary: %s rows across %s sensor(s).", format(nrow(dat), big.mark = ","), format(grz_count_ids(dat$sensor_id), big.mark = ",")),
    sprintf("Time range: %s.", time_range),
    sprintf("Row-level issues: %s invalid row(s), %s duplicate record(s), %s zero interval(s), %s negative interval(s), %s large gap(s) over %s minutes.", format(n_invalid, big.mark = ","), format(n_dup_record, big.mark = ","), format(n_zero, big.mark = ","), format(n_negative, big.mark = ","), format(n_large, big.mark = ","), format(large_gap_mins, trim = TRUE)),
    "Tables: invalid_rows, duplicates, gaps, non_positive_intervals."
  )

  out <- structure(
    list(
      summary = summary,
      invalid_rows = grz_as_output(invalid_rows, rc),
      duplicates = grz_as_output(duplicate_rows, rc),
      gaps = grz_as_output(gap_rows, rc),
      non_positive_intervals = grz_as_output(non_positive, rc)
    ),
    class = "grz_qc"
  )
  out
}

#' @export
print.grz_qc <- function(x, ...) {
  cat(paste(x$summary, collapse = "\n"), "\n", sep = "")
  invisible(x)
}
