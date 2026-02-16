#' Regularise GPS fixes to a time grid
#'
#' Snaps fixes to the nearest regular timestamp and returns a complete per-animal
#' grid between the first and last snapped timestamp.
#'
#' @param data Data frame of GPS fixes.
#' @param base_mins Target base interval in minutes.
#' @param tol_prop Maximum snapping tolerance as a proportion of `base_mins`.
#'
#' @return A `data.table` containing one row per grid time per entity with
#'   `grid_match` indicating whether an observed fix was matched.
#' @export
grz_regularise_grid <- function(data, base_mins, tol_prop = 0.60) {
  if (!is.numeric(base_mins) || length(base_mins) != 1L || base_mins <= 0) {
    stop("`base_mins` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(tol_prop) || length(tol_prop) != 1L || tol_prop < 0 || tol_prop > 1) {
    stop("`tol_prop` must be a number between 0 and 1.", call. = FALSE)
  }

  dt <- grz_prepare_gps_dt(data, fun_name = "grz_regularise_grid()")
  if (nrow(dt) == 0L) {
    dt[, `:=`(
      datetime_original = as.POSIXct(character()),
      snap_diff_s = numeric(),
      grid_match = logical()
    )]
    return(dt)
  }

  id_cols <- grz_id_cols(dt)
  step_sec <- as.integer(round(base_mins * 60))
  tol_sec <- tol_prop * step_sec

  dt[, datetime_original := datetime]
  dt[, datetime_grid := as.POSIXct(
    round(as.numeric(datetime) / step_sec) * step_sec,
    origin = "1970-01-01",
    tz = "UTC"
  )]
  dt[, snap_diff_s := abs(as.numeric(datetime - datetime_grid, units = "secs"))]
  dt <- dt[snap_diff_s <= tol_sec, ]
  if (nrow(dt) == 0L) {
    stop("No fixes were within the requested snapping tolerance.", call. = FALSE)
  }

  data.table::setorderv(dt, c(id_cols, "datetime_grid", "snap_diff_s", "datetime_original"))
  dt <- dt[, .SD[1], by = c(id_cols, "datetime_grid")]
  dt[, datetime := NULL]

  grid <- dt[, .(datetime_grid = seq(min(datetime_grid), max(datetime_grid), by = step_sec)), by = id_cols]
  out <- merge(
    grid,
    dt,
    by = c(id_cols, "datetime_grid"),
    all.x = TRUE,
    sort = TRUE
  )

  out[, grid_match := !is.na(datetime_original)]
  out[, datetime := datetime_grid]
  data.table::setcolorder(out, c(id_cols, "datetime", setdiff(names(out), c(id_cols, "datetime"))))
  out[, datetime_grid := NULL]
  out[]
}

grz_validate_downsample_ratio <- function(base_mins, target_mins) {
  if (!is.numeric(base_mins) || length(base_mins) != 1L || base_mins <= 0) {
    stop("`base_mins` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(target_mins) || length(target_mins) != 1L || target_mins <= 0) {
    stop("`target_mins` must be a positive number.", call. = FALSE)
  }
  if (target_mins < base_mins) {
    stop("`target_mins` must be greater than or equal to `base_mins`.", call. = FALSE)
  }

  ratio <- target_mins / base_mins
  ratio_int <- as.integer(round(ratio))
  if (abs(ratio - ratio_int) > sqrt(.Machine$double.eps)) {
    stop("`target_mins / base_mins` must be an integer ratio for first-draft downsampling.", call. = FALSE)
  }

  list(
    base_sec = as.integer(round(base_mins * 60)),
    ratio = ratio_int
  )
}

#' Phase-based deterministic downsampling
#'
#' Creates phase subsets by retaining every `target_mins / base_mins`th fix for
#' each phase offset.
#'
#' @param data Data frame of GPS fixes.
#' @param base_mins Base interval in minutes.
#' @param target_mins Target interval in minutes.
#' @param max_phases Maximum number of phase subsets to return.
#'
#' @return A `data.table` with retained rows and added `phase_id` and
#'   `phase_offset_mins`.
#' @export
grz_downsample_phases <- function(data, base_mins, target_mins, max_phases = 12) {
  if (!is.numeric(max_phases) || length(max_phases) != 1L || max_phases < 1) {
    stop("`max_phases` must be a positive integer.", call. = FALSE)
  }

  ratio_info <- grz_validate_downsample_ratio(base_mins = base_mins, target_mins = target_mins)
  dt <- grz_prepare_gps_dt(data, fun_name = "grz_downsample_phases()")
  if (nrow(dt) == 0L) {
    return(dt)
  }

  k <- ratio_info$ratio
  base_sec <- ratio_info$base_sec
  phase_offsets <- 0:(k - 1L)

  if (length(phase_offsets) > as.integer(max_phases)) {
    idx <- unique(round(seq(1, length(phase_offsets), length.out = as.integer(max_phases))))
    phase_offsets <- phase_offsets[idx]
  }

  dt[, base_index := floor(as.numeric(datetime) / base_sec)]
  out <- data.table::rbindlist(
    lapply(seq_along(phase_offsets), function(i) {
      off <- phase_offsets[[i]]
      ans <- dt[(base_index %% k) == off, ]
      ans[, phase_id := i]
      ans[, phase_offset_mins := off * base_mins]
      ans
    }),
    use.names = TRUE,
    fill = TRUE
  )
  out[, base_index := NULL]
  data.table::setorderv(out, c("phase_id", grz_id_cols(out), "datetime"))
  out[]
}

#' Asynchronous replicated downsampling
#'
#' Produces `n_reps` asynchronous samples by assigning each entity a random phase
#' offset in each replicate.
#'
#' @param data Data frame of GPS fixes.
#' @param base_mins Base interval in minutes.
#' @param target_mins Target interval in minutes.
#' @param n_reps Number of asynchronous replicates.
#' @param snap_tol_mins Optional tolerance filter around target schedule.
#' @param seed Seed for reproducible random offsets.
#'
#' @return A `data.table` with retained rows and added `replicate` column.
#' @export
grz_downsample_async <- function(
  data,
  base_mins,
  target_mins,
  n_reps = 12,
  snap_tol_mins = NULL,
  seed = 1
) {
  if (!is.numeric(n_reps) || length(n_reps) != 1L || n_reps < 1) {
    stop("`n_reps` must be a positive integer.", call. = FALSE)
  }
  if (!is.null(snap_tol_mins) &&
    (!is.numeric(snap_tol_mins) || length(snap_tol_mins) != 1L || snap_tol_mins < 0)) {
    stop("`snap_tol_mins` must be NULL or a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
    stop("`seed` must be a finite number.", call. = FALSE)
  }

  ratio_info <- grz_validate_downsample_ratio(base_mins = base_mins, target_mins = target_mins)
  dt <- grz_prepare_gps_dt(data, fun_name = "grz_downsample_async()")
  if (nrow(dt) == 0L) {
    return(dt)
  }

  id_cols <- grz_id_cols(dt)
  k <- ratio_info$ratio
  base_sec <- ratio_info$base_sec
  target_sec <- as.integer(round(target_mins * 60))
  dt[, base_index := floor(as.numeric(datetime) / base_sec)]

  ids <- unique(dt[, ..id_cols])
  out <- vector("list", length = as.integer(n_reps))
  for (rep_idx in seq_len(as.integer(n_reps))) {
    set.seed(as.integer(seed) + rep_idx - 1L)
    offsets <- data.table::copy(ids)
    offsets[, .grz_offset := sample.int(k, .N, replace = TRUE) - 1L]

    sub <- merge(dt, offsets, by = id_cols, all.x = FALSE, sort = FALSE)
    sub <- sub[(base_index %% k) == .grz_offset, ]

    if (!is.null(snap_tol_mins)) {
      rem_sec <- as.numeric(sub$datetime) %% target_sec
      dist_sec <- pmin(rem_sec, target_sec - rem_sec)
      sub <- sub[dist_sec <= (snap_tol_mins * 60), ]
    }

    sub[, replicate := rep_idx]
    sub[, .grz_offset := NULL]
    out[[rep_idx]] <- sub
  }

  ans <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  ans[, base_index := NULL]
  data.table::setorderv(ans, c("replicate", id_cols, "datetime"))
  ans[]
}
