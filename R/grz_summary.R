#' Merge metric tables into one per-entity summary
#'
#' @param ... Metric tables (data.frames/data.tables), or a single list of
#'   metric tables.
#'
#' @return A merged `data.table`.
#' @export
grz_per_entity_summary <- function(...) {
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
      "No common join keys across tables. Expected shared identifiers such as `sensor_id` or period keys.",
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
