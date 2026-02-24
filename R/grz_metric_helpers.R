grz_quantile_or_na <- function(x, p) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, type = 7, names = FALSE))
}

grz_mean_or_na <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

grz_threshold_label <- function(x) {
  gsub("\\.", "_", trimws(formatC(x, digits = 12, format = "fg", drop0trailing = TRUE)))
}

grz_merge_metric_tables <- function(...) {
  args <- list(...)
  if (length(args) == 1L && is.list(args[[1L]]) && !is.data.frame(args[[1L]])) {
    args <- args[[1L]]
  }

  is_table <- vapply(args, is.data.frame, logical(1))
  tbls <- args[is_table]
  if (length(tbls) < 1L) {
    stop("Provide at least one metric table.", call. = FALSE)
  }

  tbl_names <- names(tbls)
  if (is.null(tbl_names) || any(tbl_names == "")) {
    tbl_names <- paste0("table", seq_along(tbls))
  }

  tbls <- lapply(tbls, function(x) data.table::copy(data.table::as.data.table(x)))

  candidate_keys <- c(
    "deployment_id",
    "sensor_id",
    "epoch",
    "period_type",
    "period",
    "paddock",
    "date",
    "week"
  )
  common_cols <- Reduce(intersect, lapply(tbls, names))
  join_keys <- common_cols[common_cols %in% candidate_keys]
  if (length(join_keys) == 0L) {
    stop(
      "No common join keys across tables. Expected shared identifiers such as `sensor_id` and `epoch`.",
      call. = FALSE
    )
  }

  out <- tbls[[1L]]
  for (i in 2:length(tbls)) {
    rhs <- tbls[[i]]
    rhs_name <- tbl_names[[i]]

    dup_non_key <- intersect(
      setdiff(names(out), join_keys),
      setdiff(names(rhs), join_keys)
    )
    if (length(dup_non_key) > 0L) {
      data.table::setnames(
        rhs,
        old = dup_non_key,
        new = paste0(rhs_name, "_", dup_non_key)
      )
    }

    out <- merge(
      out,
      rhs,
      by = join_keys,
      all = TRUE,
      sort = FALSE
    )
  }

  attr(out, "join_keys") <- join_keys
  out[]
}
