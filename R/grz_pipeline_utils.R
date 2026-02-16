grz_match_output_class <- function(return_class = c("data.frame", "data.table")) {
  match.arg(return_class)
}

grz_as_output <- function(data, return_class = c("data.frame", "data.table")) {
  rc <- grz_match_output_class(return_class)
  if (rc == "data.table") {
    return(data.table::as.data.table(data))
  }
  as.data.frame(data, stringsAsFactors = FALSE)
}

grz_default_group_cols <- function(data, groups = NULL) {
  if (!is.null(groups)) {
    if (!is.character(groups) || length(groups) < 1L || any(is.na(groups)) || any(trimws(groups) == "")) {
      stop("`groups` must be NULL or a non-empty character vector.", call. = FALSE)
    }
    missing <- setdiff(groups, names(data))
    if (length(missing) > 0L) {
      stop("Missing group columns: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    return(unique(groups))
  }

  if ("deployment_id" %in% names(data)) {
    return(c("deployment_id", "sensor_id"))
  }
  "sensor_id"
}

grz_print_clean_step <- function(step, before_n, after_n, verbose = TRUE) {
  if (!isTRUE(verbose)) {
    return(invisible(NULL))
  }
  dropped <- before_n - after_n
  cat(
    sprintf(
      "[%s] dropped: %s | rows: %s -> %s\n",
      step,
      format(max(0, dropped), big.mark = ","),
      format(before_n, big.mark = ","),
      format(after_n, big.mark = ",")
    )
  )
  invisible(NULL)
}

grz_print_snapshot <- function(data, step, snapshot = FALSE, verbose = TRUE) {
  if (!isTRUE(snapshot) || !isTRUE(verbose)) {
    return(invisible(NULL))
  }

  n <- nrow(data)
  n_groups <- if ("sensor_id" %in% names(data)) data.table::uniqueN(data$sensor_id) else NA_integer_
  if ("datetime" %in% names(data)) {
    dt <- grz_parse_datetime_utc(data$datetime)
    min_dt <- if (all(is.na(dt))) NA_character_ else format(min(dt, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = "UTC")
    max_dt <- if (all(is.na(dt))) NA_character_ else format(max(dt, na.rm = TRUE), "%Y-%m-%d %H:%M:%S", tz = "UTC")
  } else {
    min_dt <- NA_character_
    max_dt <- NA_character_
  }

  cat(
    sprintf(
      "[snapshot:%s] rows=%s groups=%s datetime_min=%s datetime_max=%s\n",
      step,
      format(n, big.mark = ","),
      ifelse(is.na(n_groups), "NA", format(n_groups, big.mark = ",")),
      ifelse(is.na(min_dt), "NA", min_dt),
      ifelse(is.na(max_dt), "NA", max_dt)
    )
  )
  invisible(NULL)
}

grz_round_to_base_min <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & x > 0]
  if (length(x) == 0L) {
    return(1L)
  }
  as.integer(max(1L, round(stats::median(x, na.rm = TRUE))))
}
