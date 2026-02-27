grz_require_ggplot2 <- function(fun_name) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("`", fun_name, "` requires the `ggplot2` package.", call. = FALSE)
  }
}

grz_behavior_prepare_dt <- function(data, groups = NULL, ensure_features = TRUE) {
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)

  if (isTRUE(ensure_features)) {
    needed <- c("step_m", "speed_mps", "turn_rad")
    if (!all(needed %in% names(dt))) {
      dt <- data.table::as.data.table(
        grz_calculate_movement(
          data = dt,
          groups = grp,
          verbose = FALSE,
          return_class = "data.table"
        )
      )
    }
  }

  data.table::setorderv(dt, c(grp, "datetime"))
  list(data = dt, groups = grp)
}

grz_smooth_state_runs <- function(states, min_run_n = 1L) {
  if (is.null(states) || length(states) == 0L || min_run_n <= 1L) {
    return(states)
  }
  x <- as.character(states)
  r <- rle(x)
  nr <- length(r$lengths)
  if (nr <= 1L) {
    return(x)
  }

  starts <- cumsum(c(1L, head(r$lengths, -1L)))
  ends <- cumsum(r$lengths)
  out <- x

  for (i in seq_len(nr)) {
    if (r$lengths[[i]] >= min_run_n) {
      next
    }
    cur_state <- r$values[[i]]
    if (is.na(cur_state) || trimws(cur_state) == "") {
      next
    }

    prev_state <- if (i > 1L) r$values[[i - 1L]] else NA_character_
    next_state <- if (i < nr) r$values[[i + 1L]] else NA_character_
    prev_len <- if (i > 1L) r$lengths[[i - 1L]] else 0L
    next_len <- if (i < nr) r$lengths[[i + 1L]] else 0L

    replacement <- cur_state
    if (!is.na(prev_state) && !is.na(next_state)) {
      if (identical(prev_state, next_state)) {
        replacement <- prev_state
      } else {
        replacement <- if (prev_len >= next_len) prev_state else next_state
      }
    } else if (!is.na(prev_state)) {
      replacement <- prev_state
    } else if (!is.na(next_state)) {
      replacement <- next_state
    }

    out[starts[[i]]:ends[[i]]] <- replacement
  }

  out
}

grz_quantile_cols <- function(probs) {
  paste0("p", sprintf("%02d", round(probs * 100)))
}

grz_scale01 <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) {
    return(out)
  }
  rng <- range(x[ok], na.rm = TRUE)
  if (!all(is.finite(rng)) || isTRUE(all.equal(rng[[1]], rng[[2]]))) {
    out[ok] <- 0.5
    return(out)
  }
  out[ok] <- (x[ok] - rng[[1]]) / (rng[[2]] - rng[[1]])
  out
}

grz_mixture_intersection <- function(weights, means, sds) {
  if (length(weights) != 2L || length(means) != 2L || length(sds) != 2L) {
    return(NA_real_)
  }
  if (any(!is.finite(weights)) || any(!is.finite(means)) || any(!is.finite(sds))) {
    return(NA_real_)
  }
  if (any(weights <= 0) || any(sds <= 0)) {
    return(NA_real_)
  }

  a <- (1 / (2 * sds[[2]]^2)) - (1 / (2 * sds[[1]]^2))
  b <- (means[[1]] / (sds[[1]]^2)) - (means[[2]] / (sds[[2]]^2))
  c <- (means[[2]]^2 / (2 * sds[[2]]^2)) -
    (means[[1]]^2 / (2 * sds[[1]]^2)) +
    log((weights[[2]] * sds[[1]]) / (weights[[1]] * sds[[2]]))

  if (abs(a) < 1e-12) {
    if (abs(b) < 1e-12) {
      return(NA_real_)
    }
    return(-c / b)
  }

  roots <- polyroot(c(c, b, a))
  roots <- Re(roots[abs(Im(roots)) < 1e-8])
  if (length(roots) == 0L) {
    return(NA_real_)
  }

  low <- min(means)
  high <- max(means)
  inside <- roots[roots >= low & roots <= high]
  if (length(inside) >= 1L) {
    return(inside[[1]])
  }

  roots[[which.min(abs(roots - mean(means)))]]
}

grz_fit_step_mixture <- function(step_values, max_iter = 250L, tol = 1e-06, seed = 1) {
  x <- as.numeric(step_values)
  x <- x[is.finite(x) & x >= 0]
  n <- length(x)
  if (n < 20L) {
    return(NULL)
  }

  lx <- log1p(x)
  set.seed(seed)
  km <- tryCatch(
    stats::kmeans(lx, centers = 2L, nstart = 5L, iter.max = 100L),
    error = function(e) NULL
  )
  if (is.null(km)) {
    return(NULL)
  }

  ord <- order(as.numeric(km$centers))
  cl <- match(km$cluster, ord)
  means <- as.numeric(km$centers[ord])
  weights <- as.numeric(tabulate(cl, nbins = 2L)) / n
  sds <- vapply(seq_len(2L), function(k) {
    vals <- lx[cl == k]
    sd_k <- stats::sd(vals, na.rm = TRUE)
    if (!is.finite(sd_k) || sd_k < 1e-03) {
      1e-03
    } else {
      sd_k
    }
  }, numeric(1))

  ll_prev <- -Inf
  for (iter in seq_len(as.integer(max_iter))) {
    d1 <- weights[[1]] * stats::dnorm(lx, mean = means[[1]], sd = sds[[1]])
    d2 <- weights[[2]] * stats::dnorm(lx, mean = means[[2]], sd = sds[[2]])
    den <- d1 + d2 + 1e-12

    g1 <- d1 / den
    g2 <- 1 - g1
    n1 <- sum(g1)
    n2 <- sum(g2)
    if (n1 <= 1e-08 || n2 <= 1e-08) {
      break
    }

    weights <- c(n1, n2) / n
    means <- c(sum(g1 * lx) / n1, sum(g2 * lx) / n2)
    sds <- c(
      sqrt(sum(g1 * (lx - means[[1]])^2) / n1),
      sqrt(sum(g2 * (lx - means[[2]])^2) / n2)
    )
    sds[!is.finite(sds) | sds < 1e-03] <- 1e-03

    ll <- sum(log(den))
    if (is.finite(ll_prev) && abs(ll - ll_prev) < tol) {
      break
    }
    ll_prev <- ll
  }

  ord <- order(means)
  means <- means[ord]
  sds <- sds[ord]
  weights <- weights[ord]

  cut_log <- grz_mixture_intersection(weights, means, sds)
  if (!is.finite(cut_log)) {
    cut_log <- mean(means)
  }

  list(
    n = n,
    weights = as.numeric(weights),
    means_log = as.numeric(means),
    sds_log = as.numeric(sds),
    cutoff_log = as.numeric(cut_log),
    cutoff_step_m = as.numeric(max(expm1(cut_log), 0))
  )
}

grz_logsumexp <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(-Inf)
  }
  m <- max(x)
  m + log(sum(exp(x - m)))
}

grz_hmm_log_emission_diag <- function(x, means, vars, min_var = 1e-06) {
  x <- as.matrix(x)
  means <- as.matrix(means)
  vars <- as.matrix(vars)
  n <- nrow(x)
  k <- nrow(means)

  logb <- matrix(NA_real_, nrow = n, ncol = k)
  for (s in seq_len(k)) {
    v <- pmax(as.numeric(vars[s, ]), min_var)
    z <- sweep(x, 2, as.numeric(means[s, ]), "-")
    term_const <- -0.5 * sum(log(2 * pi * v))
    term_quad <- -0.5 * rowSums(sweep(z^2, 2, v, "/"))
    logb[, s] <- term_const + term_quad
  }
  logb
}

grz_hmm_forward_backward <- function(logb, a, pi) {
  logb <- as.matrix(logb)
  a <- as.matrix(a)
  pi <- as.numeric(pi)

  n <- nrow(logb)
  k <- ncol(logb)
  loga <- log(pmax(a, 1e-12))
  logpi <- log(pmax(pi, 1e-12))

  log_alpha <- matrix(-Inf, nrow = n, ncol = k)
  log_beta <- matrix(-Inf, nrow = n, ncol = k)

  log_alpha[1, ] <- logpi + logb[1, ]
  if (n >= 2L) {
    for (t in 2:n) {
      for (s in seq_len(k)) {
        log_alpha[t, s] <- logb[t, s] + grz_logsumexp(log_alpha[t - 1, ] + loga[, s])
      }
    }
  }

  loglik <- grz_logsumexp(log_alpha[n, ])

  log_beta[n, ] <- 0
  if (n >= 2L) {
    for (t in (n - 1L):1L) {
      for (s in seq_len(k)) {
        log_beta[t, s] <- grz_logsumexp(loga[s, ] + logb[t + 1L, ] + log_beta[t + 1L, ])
      }
    }
  }

  log_gamma <- log_alpha + log_beta - loglik
  gamma <- exp(log_gamma)
  gamma <- gamma / rowSums(gamma)

  xi_sum <- matrix(0, nrow = k, ncol = k)
  if (n >= 2L) {
    for (t in 1:(n - 1L)) {
      mat <- outer(log_alpha[t, ], rep(1, k)) +
        loga +
        matrix(rep(logb[t + 1L, ] + log_beta[t + 1L, ], each = k), nrow = k)
      denom <- grz_logsumexp(as.vector(mat))
      xi_sum <- xi_sum + exp(mat - denom)
    }
  }

  list(logLik = loglik, gamma = gamma, xi_sum = xi_sum)
}

grz_hmm_viterbi <- function(logb, a, pi) {
  logb <- as.matrix(logb)
  a <- as.matrix(a)
  pi <- as.numeric(pi)

  n <- nrow(logb)
  k <- ncol(logb)
  loga <- log(pmax(a, 1e-12))
  logpi <- log(pmax(pi, 1e-12))

  delta <- matrix(-Inf, nrow = n, ncol = k)
  psi <- matrix(1L, nrow = n, ncol = k)

  delta[1, ] <- logpi + logb[1, ]
  if (n >= 2L) {
    for (t in 2:n) {
      for (s in seq_len(k)) {
        vals <- delta[t - 1L, ] + loga[, s]
        idx <- which.max(vals)
        delta[t, s] <- vals[idx] + logb[t, s]
        psi[t, s] <- as.integer(idx)
      }
    }
  }

  path <- integer(n)
  path[n] <- as.integer(which.max(delta[n, ]))
  if (n >= 2L) {
    for (t in (n - 1L):1L) {
      path[t] <- psi[t + 1L, path[t + 1L]]
    }
  }

  list(path = path, logprob = max(delta[n, ]))
}

grz_hmm_fit_diag <- function(
  x,
  n_states = 2L,
  max_iter = 100L,
  tol = 1e-04,
  min_var = 1e-04,
  transition_prior = 1,
  self_transition_prior = 5,
  seed = 1
) {
  x <- as.matrix(x)
  n <- nrow(x)
  d <- ncol(x)
  if (n < max(20L, n_states * 5L)) {
    stop("Not enough rows for HMM fit.", call. = FALSE)
  }
  if (d < 1L) {
    stop("HMM feature matrix must have at least one column.", call. = FALSE)
  }

  set.seed(seed)
  km <- tryCatch(
    stats::kmeans(x, centers = n_states, nstart = 5L, iter.max = 100L),
    error = function(e) NULL
  )

  if (is.null(km)) {
    cl <- sample.int(n_states, n, replace = TRUE)
  } else {
    cl <- as.integer(km$cluster)
  }

  means <- matrix(NA_real_, nrow = n_states, ncol = d)
  vars <- matrix(NA_real_, nrow = n_states, ncol = d)
  for (s in seq_len(n_states)) {
    idx <- which(cl == s)
    if (length(idx) < 2L) {
      idx <- sample.int(n, min(20L, n), replace = TRUE)
    }
    means[s, ] <- colMeans(x[idx, , drop = FALSE], na.rm = TRUE)
    v <- apply(x[idx, , drop = FALSE], 2, stats::var, na.rm = TRUE)
    v[!is.finite(v) | v < min_var] <- min_var
    vars[s, ] <- v
  }

  if (n_states == 1L) {
    a <- matrix(1, nrow = 1L, ncol = 1L)
  } else {
    off <- (1 - 0.95) / (n_states - 1)
    a <- matrix(off, nrow = n_states, ncol = n_states)
    diag(a) <- 0.95
  }
  pi <- rep(1 / n_states, n_states)

  prior_mat <- matrix(transition_prior, nrow = n_states, ncol = n_states)
  diag(prior_mat) <- self_transition_prior

  ll_trace <- numeric(0)
  ll_prev <- -Inf
  iter_used <- 0L

  for (iter in seq_len(as.integer(max_iter))) {
    logb <- grz_hmm_log_emission_diag(x, means, vars, min_var = min_var)
    fb <- grz_hmm_forward_backward(logb, a, pi)
    gamma <- fb$gamma
    nk <- colSums(gamma) + 1e-08

    pi <- gamma[1, ]
    pi <- pi / sum(pi)

    a_num <- fb$xi_sum + prior_mat
    a <- a_num / rowSums(a_num)

    for (s in seq_len(n_states)) {
      w <- gamma[, s]
      means[s, ] <- colSums(x * w) / nk[s]
      centered <- sweep(x, 2, means[s, ], "-")
      v <- colSums(centered^2 * w) / nk[s]
      v[!is.finite(v) | v < min_var] <- min_var
      vars[s, ] <- v
    }

    ll <- fb$logLik
    ll_trace <- c(ll_trace, ll)
    iter_used <- iter
    if (is.finite(ll_prev) && abs(ll - ll_prev) < tol) {
      break
    }
    ll_prev <- ll
  }

  list(
    pi = as.numeric(pi),
    a = a,
    means = means,
    vars = vars,
    logLik = as.numeric(tail(ll_trace, 1L)),
    ll_trace = ll_trace,
    iterations = iter_used
  )
}

grz_gmm_fit_diag <- function(
  x,
  n_components = 2L,
  max_iter = 200L,
  tol = 1e-05,
  min_var = 1e-06,
  seed = 1
) {
  x <- as.matrix(x)
  n <- nrow(x)
  d <- ncol(x)
  if (n < max(20L, n_components * 5L)) {
    stop("Not enough rows for GMM fit.", call. = FALSE)
  }
  if (d < 1L) {
    stop("GMM feature matrix must have at least one column.", call. = FALSE)
  }

  set.seed(seed)
  km <- tryCatch(
    stats::kmeans(x, centers = n_components, nstart = 5L, iter.max = 100L),
    error = function(e) NULL
  )
  if (is.null(km)) {
    cl <- sample.int(n_components, n, replace = TRUE)
  } else {
    cl <- as.integer(km$cluster)
  }

  means <- matrix(NA_real_, nrow = n_components, ncol = d)
  vars <- matrix(NA_real_, nrow = n_components, ncol = d)
  weights <- rep(1 / n_components, n_components)

  for (k in seq_len(n_components)) {
    idx <- which(cl == k)
    if (length(idx) < 2L) {
      idx <- sample.int(n, min(20L, n), replace = TRUE)
    }
    means[k, ] <- colMeans(x[idx, , drop = FALSE], na.rm = TRUE)
    v <- apply(x[idx, , drop = FALSE], 2, stats::var, na.rm = TRUE)
    v[!is.finite(v) | v < min_var] <- min_var
    vars[k, ] <- v
    weights[k] <- max(length(idx), 1) / n
  }
  weights <- weights / sum(weights)

  ll_prev <- -Inf
  ll_trace <- numeric(0)
  iter_used <- 0L
  gamma <- matrix(NA_real_, nrow = n, ncol = n_components)

  for (iter in seq_len(as.integer(max_iter))) {
    logb <- grz_hmm_log_emission_diag(x, means, vars, min_var = min_var)
    logw <- log(pmax(weights, 1e-12))

    log_joint <- sweep(logb, 2, logw, FUN = "+")
    row_ll <- apply(log_joint, 1L, grz_logsumexp)
    log_gamma <- log_joint - row_ll
    gamma <- exp(log_gamma)
    gamma <- gamma / rowSums(gamma)

    nk <- colSums(gamma) + 1e-08
    weights <- nk / sum(nk)

    for (k in seq_len(n_components)) {
      w <- gamma[, k]
      means[k, ] <- colSums(x * w) / nk[k]
      centered <- sweep(x, 2, means[k, ], "-")
      v <- colSums(centered^2 * w) / nk[k]
      v[!is.finite(v) | v < min_var] <- min_var
      vars[k, ] <- v
    }

    ll <- sum(row_ll)
    ll_trace <- c(ll_trace, ll)
    iter_used <- iter
    if (is.finite(ll_prev) && abs(ll - ll_prev) < tol) {
      break
    }
    ll_prev <- ll
  }

  list(
    weights = as.numeric(weights),
    means = means,
    vars = vars,
    posterior = gamma,
    logLik = as.numeric(tail(ll_trace, 1L)),
    ll_trace = ll_trace,
    iterations = iter_used
  )
}

grz_gmm_predict_diag <- function(x, model, min_var = 1e-06) {
  x <- as.matrix(x)
  logb <- grz_hmm_log_emission_diag(x, model$means, model$vars, min_var = min_var)
  logw <- log(pmax(as.numeric(model$weights), 1e-12))
  log_joint <- sweep(logb, 2, logw, FUN = "+")
  row_ll <- apply(log_joint, 1L, grz_logsumexp)
  log_post <- log_joint - row_ll
  post <- exp(log_post)
  post <- post / rowSums(post)
  comp <- max.col(post, ties.method = "first")
  list(component = as.integer(comp), posterior = post, logLik = sum(row_ll))
}

grz_roll_median <- function(x, k = 5L) {
  y <- suppressWarnings(as.numeric(x))
  out <- y
  ok <- which(is.finite(y))
  if (length(ok) < 3L) {
    return(out)
  }
  kk <- as.integer(max(3L, round(k)))
  if (kk %% 2L == 0L) {
    kk <- kk + 1L
  }
  if (kk > length(ok)) {
    kk <- if (length(ok) %% 2L == 1L) length(ok) else length(ok) - 1L
  }
  if (kk < 3L) {
    return(out)
  }
  out_vals <- stats::runmed(y[ok], k = kk, endrule = "median")
  out[ok] <- out_vals
  out
}

grz_hmm_adaptive_window_track <- function(lon, lat, datetime, step_m, window_mins) {
  n <- length(lon)
  net_disp_m <- rep(NA_real_, n)
  path_len_m <- rep(NA_real_, n)
  straightness <- rep(NA_real_, n)

  if (n == 0L || !is.finite(window_mins) || window_mins <= 0) {
    return(list(net_disp_m = net_disp_m, path_len_m = path_len_m, straightness = straightness))
  }

  tnum <- suppressWarnings(as.numeric(datetime))
  step_num <- suppressWarnings(as.numeric(step_m))
  step_num[!is.finite(step_num) | step_num < 0] <- 0

  csum <- c(0, cumsum(step_num))
  wsec <- as.numeric(window_mins) * 60
  start_idx <- findInterval(tnum - wsec, tnum) + 1L
  start_idx[!is.finite(tnum)] <- seq_len(n)[!is.finite(tnum)]

  for (i in seq_len(n)) {
    if (!is.finite(tnum[[i]]) || !is.finite(lon[[i]]) || !is.finite(lat[[i]])) {
      next
    }

    s <- start_idx[[i]]
    if (!is.finite(s) || s < 1L || s > i) {
      s <- i
    }

    k <- s
    while (
      k < i &&
      (!is.finite(tnum[[k]]) || !is.finite(lon[[k]]) || !is.finite(lat[[k]]))
    ) {
      k <- k + 1L
    }

    path_i <- csum[[i + 1L]] - csum[[k + 1L]]
    if (!is.finite(path_i) || path_i < 0) {
      path_i <- 0
    }
    path_len_m[[i]] <- path_i

    net_i <- grz_haversine_m(lon[[k]], lat[[k]], lon[[i]], lat[[i]])
    if (!is.finite(net_i)) {
      next
    }
    net_disp_m[[i]] <- net_i

    if (is.finite(path_i) && path_i > 0) {
      straight_i <- net_i / path_i
      straightness[[i]] <- pmin(pmax(straight_i, 0), 1)
    } else {
      straightness[[i]] <- 0
    }
  }

  list(net_disp_m = net_disp_m, path_len_m = path_len_m, straightness = straightness)
}

grz_hmm_add_adaptive_features <- function(
  dt,
  groups,
  step_col,
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30
) {
  if (!is.character(adaptive_window_mins) || length(adaptive_window_mins) != 1L || adaptive_window_mins != "auto") {
    adaptive_window_mins <- as.numeric(adaptive_window_mins)
    if (!is.finite(adaptive_window_mins) || adaptive_window_mins <= 0) {
      stop("`adaptive_window_mins` must be \"auto\" or a positive number.", call. = FALSE)
    }
  }
  if (!is.numeric(adaptive_window_mult) || length(adaptive_window_mult) != 1L || adaptive_window_mult <= 0) {
    stop("`adaptive_window_mult` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(adaptive_window_min_mins) || length(adaptive_window_min_mins) != 1L || adaptive_window_min_mins <= 0) {
    stop("`adaptive_window_min_mins` must be a positive number.", call. = FALSE)
  }

  dt[, c(".grz_net_disp_w_m", ".grz_path_len_w_m", ".grz_straightness_w", ".grz_window_mins") := {
    if (is.character(adaptive_window_mins)) {
      base <- grz_round_to_base_min(as.numeric(diff(datetime), units = "mins"))
      win <- max(adaptive_window_min_mins, adaptive_window_mult * base)
    } else {
      win <- as.numeric(adaptive_window_mins)
    }

    res <- grz_hmm_adaptive_window_track(
      lon = lon,
      lat = lat,
      datetime = datetime,
      step_m = suppressWarnings(as.numeric(get(step_col))),
      window_mins = win
    )

    list(
      res$net_disp_m,
      res$path_len_m,
      res$straightness,
      rep(win, .N)
    )
  }, by = groups]

  dt
}

#' Plot diurnal heatmaps for key movement metrics
#'
#' Creates cohort/group-level diurnal heatmaps to support threshold tuning
#' before behavior-state classification.
#'
#' @param data Input data containing at least `sensor_id`, `datetime`, `lon`,
#'   `lat`.
#' @param metrics Metrics to visualise (default speed/step/turn).
#' @param group_col Column used for y-axis grouping.
#' @param cohort_col Optional cohort column for faceting.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param agg_fun Aggregation function for hourly metric values.
#' @param scale Optional scaling for fill values (`"none"` or `"zscore"`).
#' @param return_data Logical; return plotting data along with plot.
#'
#' @return A ggplot object, or list with `plot` and `data` when
#'   `return_data = TRUE`.
#' @export
grz_plot_diurnal_metrics <- function(
  data,
  metrics = c("step_m", "turn_rad"),
  group_col = "sensor_id",
  cohort_col = NULL,
  tz_local = "UTC",
  agg_fun = c("median", "mean"),
  scale = c("none", "zscore"),
  return_data = FALSE
) {
  grz_require_ggplot2("grz_plot_diurnal_metrics()")
  agg_fun <- match.arg(agg_fun)
  scale <- match.arg(scale)

  prep <- grz_behavior_prepare_dt(data, ensure_features = TRUE)
  dt <- prep$data

  if (!is.character(metrics) || length(metrics) < 1L) {
    stop("`metrics` must be a non-empty character vector.", call. = FALSE)
  }
  missing_metrics <- setdiff(metrics, names(dt))
  if (length(missing_metrics) > 0L) {
    stop("Missing metric columns: ", paste(missing_metrics, collapse = ", "), call. = FALSE)
  }
  if (!group_col %in% names(dt)) {
    stop("`group_col` not found in data: ", group_col, call. = FALSE)
  }
  if (!is.null(cohort_col) && !cohort_col %in% names(dt)) {
    stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
  }

  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, .grz_group := as.character(get(group_col))]
  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }

  long <- data.table::melt(
    dt,
    id.vars = c(".grz_hour", ".grz_group", ".grz_cohort"),
    measure.vars = metrics,
    variable.name = ".grz_metric",
    value.name = ".grz_value"
  )
  long <- long[is.finite(.grz_value)]
  if (nrow(long) == 0L) {
    stop("No finite values available for selected metrics.", call. = FALSE)
  }

  agg <- if (agg_fun == "median") {
    long[, .(value = stats::median(.grz_value, na.rm = TRUE)), by = .(.grz_hour, .grz_group, .grz_cohort, .grz_metric)]
  } else {
    long[, .(value = mean(.grz_value, na.rm = TRUE)), by = .(.grz_hour, .grz_group, .grz_cohort, .grz_metric)]
  }

  if (scale == "zscore") {
    agg[, plot_value := {
      v <- value
      s <- stats::sd(v, na.rm = TRUE)
      if (is.finite(s) && s > 0) {
        (v - mean(v, na.rm = TRUE)) / s
      } else {
        rep(0, .N)
      }
    }, by = .(.grz_cohort, .grz_metric)]
  } else {
    agg[, plot_value := value]
  }

  fill_scale <- if (scale == "zscore") {
    ggplot2::scale_fill_gradient2(
      low = "green",
      mid = "yellow",
      high = "red",
      midpoint = 0
    )
  } else {
    ggplot2::scale_fill_gradient(
      low = "green",
      high = "red"
    )
  }

  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(x = .grz_hour, y = .grz_group, fill = plot_value)
  ) +
    ggplot2::geom_tile() +
    fill_scale +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = group_col,
      fill = if (scale == "zscore") "Z-score" else agg_fun,
      title = "Diurnal Metric Heatmap"
    ) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y", space = "free_y") +
    ggplot2::theme_minimal()

  if (isTRUE(return_data)) {
    return(list(plot = p, data = as.data.frame(agg)))
  }
  p
}

#' Build threshold guidance summaries for behavior classification
#'
#' Produces overall and hourly quantile summaries (plus diagnostic plots) for
#' selected metrics to help users choose rule-based behavior thresholds.
#'
#' @param data Input data containing at least `sensor_id`, `datetime`, `lon`,
#'   `lat`.
#' @param metrics Metrics used for threshold guidance.
#' @param cohort_col Optional cohort column used in summaries and facets.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param probs Quantile probabilities used in summary tables.
#' @param max_points_plot Maximum points used in density plot subsample.
#' @param seed Random seed used for plotting subsample.
#' @param include_tuning Logical; run threshold tuning diagnostics.
#' @param tuning_rest_step_grid Candidate `rest_step_max` values for tuning.
#' @param tuning_rest_speed_grid Candidate `rest_speed_max` values for tuning.
#' @param tuning_max_rows Maximum rows used in tuning sweep.
#' @param return_class Output class for returned tables.
#'
#' @return A list with `overall`, `hourly`, `plots` (ggplot objects), and
#'   optional `tuning` diagnostics.
#' @export
grz_behavior_threshold_guide <- function(
  data,
  metrics = c("step_m", "turn_rad"),
  cohort_col = NULL,
  tz_local = "UTC",
  probs = c(0.10, 0.25, 0.50, 0.75, 0.90),
  max_points_plot = 150000L,
  seed = 1,
  include_tuning = TRUE,
  tuning_rest_step_grid = seq(3, 12, by = 1),
  tuning_rest_speed_grid = seq(0.03, 0.09, by = 0.02),
  tuning_max_rows = 25000L,
  return_class = c("data.frame", "data.table")
) {
  grz_require_ggplot2("grz_behavior_threshold_guide()")
  rc <- grz_match_output_class(return_class)

  if (!is.numeric(probs) || length(probs) < 1L || any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)) {
    stop("`probs` must be numeric probabilities in (0, 1).", call. = FALSE)
  }
  probs <- sort(unique(as.numeric(probs)))

  prep <- grz_behavior_prepare_dt(data, ensure_features = TRUE)
  dt <- prep$data

  missing_metrics <- setdiff(metrics, names(dt))
  if (length(missing_metrics) > 0L) {
    stop("Missing metric columns: ", paste(missing_metrics, collapse = ", "), call. = FALSE)
  }

  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    if (!cohort_col %in% names(dt)) {
      stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
    }
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }
  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]

  long <- data.table::melt(
    dt,
    id.vars = c(".grz_cohort", ".grz_hour"),
    measure.vars = metrics,
    variable.name = ".grz_metric",
    value.name = ".grz_value"
  )
  long <- long[is.finite(.grz_value)]
  if (nrow(long) == 0L) {
    stop("No finite values available for selected metrics.", call. = FALSE)
  }

  qcols <- grz_quantile_cols(probs)
  overall <- long[, {
    qs <- stats::quantile(.grz_value, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
    out <- as.list(setNames(as.numeric(qs), qcols))
    out$n <- .N
    out$mean <- mean(.grz_value, na.rm = TRUE)
    out$sd <- if (.N >= 2L) stats::sd(.grz_value, na.rm = TRUE) else NA_real_
    out
  }, by = .(.grz_cohort, .grz_metric)]

  hourly <- long[, {
    qs <- stats::quantile(.grz_value, probs = probs, na.rm = TRUE, names = FALSE, type = 7)
    out <- as.list(setNames(as.numeric(qs), qcols))
    out$n <- .N
    out$mean <- mean(.grz_value, na.rm = TRUE)
    out
  }, by = .(.grz_cohort, .grz_metric, .grz_hour)]

  hourly_band <- long[, .(
    p10 = grz_quantile_or_na(.grz_value, 0.10),
    p50 = grz_quantile_or_na(.grz_value, 0.50),
    p90 = grz_quantile_or_na(.grz_value, 0.90)
  ), by = .(.grz_cohort, .grz_metric, .grz_hour)]

  long_plot <- long
  if (nrow(long_plot) > as.integer(max_points_plot)) {
    set.seed(seed)
    long_plot <- long_plot[sample.int(nrow(long_plot), as.integer(max_points_plot))]
  }

  # Keep step-distance boxplots on a comparable visual range.
  long_plot_box <- data.table::copy(long_plot)
  long_plot_box[.grz_metric == "step_m" & .grz_value > 3000, .grz_value := 3000]

  box_axis_anchor <- unique(long_plot_box[.grz_metric == "step_m", .(.grz_metric, .grz_cohort)])
  if (nrow(box_axis_anchor) > 0L) {
    box_axis_anchor[, `:=`(.grz_hour = 0L, .grz_value = 3000)]
  }

  p_box <- ggplot2::ggplot(
    long_plot_box,
    ggplot2::aes(x = factor(.grz_hour), y = .grz_value)
  ) +
    ggplot2::geom_boxplot(outlier.alpha = 0.05, linewidth = 0.2) +
    ggplot2::geom_blank(
      data = box_axis_anchor,
      ggplot2::aes(x = factor(.grz_hour), y = .grz_value)
    ) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y") +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = "Metric value",
      title = "Hourly Metric Distribution (Boxplots)"
    ) +
    ggplot2::theme_minimal()

  p_band <- ggplot2::ggplot(
    hourly_band,
    ggplot2::aes(x = .grz_hour, y = p50)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = p10, ymax = p90),
      fill = "grey75",
      alpha = 0.6
    ) +
    ggplot2::geom_line(linewidth = 0.8, color = "black") +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free_y") +
    ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    ggplot2::labs(
      x = paste0("Hour (", tz_local, ")"),
      y = "Median (p10-p90 band)",
      title = "Hourly Metric Bands for Threshold Setting"
    ) +
    ggplot2::theme_minimal()

  p_density <- ggplot2::ggplot(
    long_plot,
    ggplot2::aes(x = .grz_value, fill = .grz_metric)
  ) +
    ggplot2::geom_density(alpha = 0.4) +
    ggplot2::facet_grid(.grz_metric ~ .grz_cohort, scales = "free") +
    ggplot2::labs(
      x = "Metric value",
      y = "Density",
      fill = "Metric",
      title = "Metric Density Overview"
    ) +
    ggplot2::theme_minimal()

  data.table::setnames(overall, c(".grz_cohort", ".grz_metric"), c("cohort", "metric"))
  data.table::setnames(hourly, c(".grz_cohort", ".grz_metric", ".grz_hour"), c("cohort", "metric", "hour"))

  tuning <- NULL
  if (isTRUE(include_tuning)) {
    tuning <- grz_tune_thresholds(
      data = dt,
      cohort_col = cohort_col,
      tz_local = tz_local,
      rest_step_grid = tuning_rest_step_grid,
      rest_speed_grid = tuning_rest_speed_grid,
      max_rows = tuning_max_rows,
      max_points_plot = max_points_plot,
      seed = seed,
      return_class = rc,
      verbose = FALSE
    )
  }

  list(
    overall = grz_as_output(overall, rc),
    hourly = grz_as_output(hourly, rc),
    plots = list(
      hourly_boxplot = p_box,
      hourly_band = p_band,
      density = p_density
    ),
    tuning = tuning
  )
}

#' Tune idle/graze thresholds with clearer diagnostics
#'
#' Builds practical threshold-tuning outputs for `rest` vs `graze` separation:
#' a data-driven starting cutoff from a two-component mixture on `log1p(step_m)`,
#' a threshold sweep score surface, and visual diagnostics.
#'
#' @param data Input data containing GPS rows.
#' @param groups Group columns used for transitions and bout calculations.
#' @param cohort_col Optional cohort column for faceting.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param step_col Step-distance column (m).
#' @param speed_col Speed column (m/s).
#' @param turn_col Turn-angle column (radians).
#' @param rest_step_grid Candidate `rest_step_max` values for sweep.
#' @param rest_speed_grid Candidate `rest_speed_max` values for sweep.
#' @param graze_speed_max Passed into classification rule during sweep.
#' @param travel_speed_min Passed into classification rule during sweep.
#' @param travel_turn_max Passed into classification rule during sweep.
#' @param min_run_n Run smoothing threshold used during sweep.
#' @param short_bout_mins Bout duration threshold for instability metric.
#' @param night_hours Night hours used for night-rest metric.
#' @param max_rows Maximum rows used in threshold sweep (sampled if needed).
#' @param max_points_plot Maximum points used for density/CDF plots.
#' @param seed Random seed for reproducible subsampling.
#' @param verbose Logical; print sweep summary.
#' @param return_class Output class for returned tables.
#'
#' @return A list with `suggested`, `sweep`, `best`, `mixture`, and `plots`.
#' @export
grz_tune_thresholds <- function(
  data,
  groups = NULL,
  cohort_col = NULL,
  tz_local = "UTC",
  step_col = "step_m",
  speed_col = "speed_mps",
  turn_col = "turn_rad",
  rest_step_grid = seq(3, 12, by = 1),
  rest_speed_grid = seq(0.03, 0.09, by = 0.02),
  graze_speed_max = 0.60,
  travel_speed_min = 0.60,
  travel_turn_max = 0.60,
  min_run_n = 2L,
  short_bout_mins = 10,
  night_hours = c(20:23, 0:5),
  max_rows = 25000L,
  max_points_plot = 150000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  grz_require_ggplot2("grz_tune_thresholds()")
  rc <- grz_match_output_class(return_class)

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  grz_require_cols(dt, c("datetime", step_col, speed_col, turn_col), fun_name = "grz_tune_thresholds()")

  if (!is.numeric(rest_step_grid) || length(rest_step_grid) < 1L || any(!is.finite(rest_step_grid)) || any(rest_step_grid <= 0)) {
    stop("`rest_step_grid` must be positive numeric values.", call. = FALSE)
  }
  if (!is.numeric(rest_speed_grid) || length(rest_speed_grid) < 1L || any(!is.finite(rest_speed_grid)) || any(rest_speed_grid <= 0)) {
    stop("`rest_speed_grid` must be positive numeric values.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  night_hours <- sort(unique(as.integer(night_hours)))
  night_hours <- night_hours[night_hours >= 0L & night_hours <= 23L]
  if (length(night_hours) == 0L) {
    stop("`night_hours` must include values between 0 and 23.", call. = FALSE)
  }

  dt <- dt[is.finite(get(step_col)) & is.finite(get(speed_col)) & !is.na(datetime)]
  if (nrow(dt) < 20L) {
    stop("Not enough valid rows for threshold tuning.", call. = FALSE)
  }

  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    if (!cohort_col %in% names(dt)) {
      stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
    }
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }
  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, .grz_period := data.table::fifelse(.grz_hour %in% night_hours, "night", "day")]

  step_vals <- dt[[step_col]]
  mix_fit <- grz_fit_step_mixture(step_vals, seed = seed)
  if (is.null(mix_fit)) {
    stop("Unable to fit threshold mixture model for step distance.", call. = FALSE)
  }

  plot_dt <- dt[, .(step_m = get(step_col), .grz_period, .grz_cohort)]
  if (nrow(plot_dt) > as.integer(max_points_plot)) {
    set.seed(seed)
    plot_dt <- plot_dt[sample.int(nrow(plot_dt), as.integer(max_points_plot))]
  }

  q99 <- stats::quantile(plot_dt$step_m, probs = 0.99, na.rm = TRUE, names = FALSE, type = 7)
  x_upper <- if (is.finite(q99) && q99 > 0) q99 else max(plot_dt$step_m, na.rm = TRUE)

  p_density <- ggplot2::ggplot(
    plot_dt,
    ggplot2::aes(x = step_m, color = .grz_period, fill = .grz_period)
  ) +
    ggplot2::geom_density(alpha = 0.25) +
    ggplot2::geom_vline(xintercept = mix_fit$cutoff_step_m, linetype = "dashed", color = "black") +
    ggplot2::coord_cartesian(xlim = c(0, x_upper)) +
    ggplot2::facet_wrap(~.grz_cohort, scales = "free_y") +
    ggplot2::labs(
      x = "Step distance (m)",
      y = "Density",
      color = "Period",
      fill = "Period",
      title = "Step Distance Density with Suggested Idle/Graze Cutoff",
      subtitle = paste0("Dashed line = mixture cutoff (", round(mix_fit$cutoff_step_m, 2), " m)")
    ) +
    ggplot2::theme_minimal()

  p_cdf <- ggplot2::ggplot(
    plot_dt,
    ggplot2::aes(x = step_m, color = .grz_period)
  ) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::geom_vline(xintercept = mix_fit$cutoff_step_m, linetype = "dashed", color = "black") +
    ggplot2::coord_cartesian(xlim = c(0, x_upper)) +
    ggplot2::facet_wrap(~.grz_cohort) +
    ggplot2::labs(
      x = "Step distance (m)",
      y = "Empirical CDF",
      color = "Period",
      title = "Step Distance CDF by Period"
    ) +
    ggplot2::theme_minimal()

  dt_sweep <- dt[, c(grp, "datetime", ".grz_hour", step_col, speed_col, turn_col), with = FALSE]
  if (nrow(dt_sweep) > as.integer(max_rows)) {
    set.seed(seed + 1L)
    dt_sweep <- dt_sweep[sample.int(nrow(dt_sweep), as.integer(max_rows))]
    data.table::setorderv(dt_sweep, c(grp, "datetime"))
  }

  grid <- data.table::CJ(
    rest_step_max = sort(unique(as.numeric(rest_step_grid))),
    rest_speed_max = sort(unique(as.numeric(rest_speed_grid)))
  )

  sweep_eval <- function(rest_step_max, rest_speed_max) {
    tmp <- data.table::copy(dt_sweep)
    tmp[, .grz_state := data.table::fifelse(
      is.na(get(speed_col)),
      NA_character_,
      data.table::fifelse(
        get(speed_col) <= rest_speed_max & (is.na(get(step_col)) | get(step_col) <= rest_step_max),
        "rest",
        data.table::fifelse(
          get(speed_col) >= travel_speed_min & (is.na(get(turn_col)) | abs(get(turn_col)) <= travel_turn_max),
          "travel",
          data.table::fifelse(get(speed_col) <= graze_speed_max, "graze", "travel")
        )
      )
    )]

    if (as.integer(min_run_n) > 1L) {
      tmp[, .grz_state := grz_smooth_state_runs(.grz_state, min_run_n = as.integer(min_run_n)), by = grp]
    }

    tmp[, .grz_prev_state := data.table::shift(.grz_state), by = grp]
    trans <- tmp[!is.na(.grz_state) & !is.na(.grz_prev_state), mean(.grz_state != .grz_prev_state)]
    trans <- if (is.finite(trans)) trans * 1000 else NA_real_

    night_mask <- tmp$.grz_hour %in% night_hours
    night_rest <- if (any(night_mask)) {
      tmp[night_mask, mean(.grz_state == "rest", na.rm = TRUE)]
    } else {
      NA_real_
    }

    bout_dt <- tmp[, c(grp, "datetime", ".grz_state"), with = FALSE]
    bouts <- grz_behavior_bouts(bout_dt, state_col = ".grz_state", groups = grp)
    short_bout_prop <- if (nrow(bouts) > 0L) {
      mean(bouts$duration_mins <= short_bout_mins, na.rm = TRUE)
    } else {
      NA_real_
    }

    data.table::data.table(
      rest_step_max = rest_step_max,
      rest_speed_max = rest_speed_max,
      transition_per_1000 = trans,
      short_bout_prop = short_bout_prop,
      night_rest_prop = night_rest
    )
  }

  sweep <- data.table::rbindlist(
    lapply(seq_len(nrow(grid)), function(i) {
      sweep_eval(
        rest_step_max = grid$rest_step_max[[i]],
        rest_speed_max = grid$rest_speed_max[[i]]
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )

  sweep[, score := grz_scale01(transition_per_1000) +
    grz_scale01(short_bout_prop) +
    (1 - grz_scale01(night_rest_prop))]
  sweep[!is.finite(score), score := NA_real_]

  best <- sweep[which.min(score)][1]
  if (nrow(best) == 0L || !is.finite(best$score[[1]])) {
    best <- sweep[order(rest_step_max, rest_speed_max)][1]
  }

  suggested <- data.table::data.table(
    method = c("mixture_step_cutoff", "sweep_best"),
    rest_step_max = c(mix_fit$cutoff_step_m, best$rest_step_max[[1]]),
    rest_speed_max = c(NA_real_, best$rest_speed_max[[1]]),
    score = c(NA_real_, best$score[[1]])
  )

  p_sweep <- ggplot2::ggplot(
    sweep,
    ggplot2::aes(x = rest_step_max, y = rest_speed_max, fill = score)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_point(
      data = best,
      ggplot2::aes(x = rest_step_max, y = rest_speed_max),
      inherit.aes = FALSE,
      shape = 21,
      fill = "white",
      color = "black",
      size = 3
    ) +
    ggplot2::scale_fill_gradient(
      low = "green",
      high = "red",
      na.value = "grey85"
    ) +
    ggplot2::labs(
      x = "rest_step_max (m)",
      y = "rest_speed_max (m/s)",
      fill = "Score",
      title = "Threshold Sweep Score Surface",
      subtitle = "Lower score is better (fewer transitions, fewer short bouts, more night rest)"
    ) +
    ggplot2::theme_minimal()

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[tune_thresholds] mixture_step=%.2f best_step=%.2f best_speed=%.3f score=%.3f grid=%s rows=%s\n",
        mix_fit$cutoff_step_m,
        best$rest_step_max[[1]],
        best$rest_speed_max[[1]],
        best$score[[1]],
        format(nrow(grid), big.mark = ","),
        format(nrow(dt_sweep), big.mark = ",")
      )
    )
  }

  list(
    suggested = grz_as_output(suggested, rc),
    sweep = grz_as_output(sweep, rc),
    best = grz_as_output(best, rc),
    mixture = mix_fit,
    plots = list(
      step_density = p_density,
      step_cdf = p_cdf,
      sweep_score = p_sweep
    )
  )
}

#' Classify Active/Inactive States Using HMM
#'
#' Fits a 2-state Gaussian HMM on transformed movement features and decodes
#' each track into `inactive`/`active` states. This is useful when simple
#' thresholding is unstable due to GPS jitter in low-movement periods.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used for track-wise decoding.
#' @param step_col Step-distance column (meters).
#' @param turn_col Turn-angle column (radians).
#' @param feature_set HMM feature set. `"adaptive"` (default) augments step and
#'   turn with adaptive-window displacement features. `"legacy"` uses only
#'   step and turn.
#' @param adaptive_window_mins Adaptive feature window size in minutes. Use
#'   `"auto"` (default) to scale window length from each track's sampling
#'   interval.
#' @param adaptive_window_mult Multiplier applied to base sampling interval when
#'   `adaptive_window_mins = "auto"`.
#' @param adaptive_window_min_mins Lower bound for auto window size (minutes).
#' @param state_col Output state column name.
#' @param state_id_col Output numeric state id column.
#' @param inactive_prob_col Output posterior probability column for inactive
#'   state.
#' @param fit_max_rows Maximum rows used to fit the HMM (sampled if needed).
#' @param max_iter Maximum EM iterations.
#' @param tol EM convergence tolerance on log-likelihood.
#' @param min_var Minimum per-state feature variance for numerical stability.
#' @param min_run_n Optional run-length smoothing threshold.
#' @param seed Random seed for reproducible fitting/subsampling.
#' @param verbose Logical; print summary output.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended HMM activity columns.
#' @keywords internal
#' @noRd
grz_classify_activity_hmm <- function(
  data,
  groups = NULL,
  step_col = "step_m",
  turn_col = "turn_rad",
  state_col = "activity_state_hmm",
  state_id_col = "activity_state_id_hmm",
  inactive_prob_col = "inactive_prob_hmm",
  fit_max_rows = 200000L,
  max_iter = 100L,
  tol = 1e-04,
  min_var = 1e-04,
  min_run_n = 2L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  feature_set = c("adaptive", "legacy"),
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30
) {
  rc <- grz_match_output_class(return_class)
  feature_set <- match.arg(feature_set)
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(state_id_col) || length(state_id_col) != 1L || trimws(state_id_col) == "") {
    stop("`state_id_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(inactive_prob_col) || length(inactive_prob_col) != 1L || trimws(inactive_prob_col) == "") {
    stop("`inactive_prob_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.numeric(fit_max_rows) || length(fit_max_rows) != 1L || fit_max_rows < 100) {
    stop("`fit_max_rows` must be a number >= 100.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups
  grz_require_cols(dt, c("datetime", step_col, turn_col), fun_name = "grz_classify_activity_hmm()")

  dt[, .grz_step := suppressWarnings(as.numeric(get(step_col)))]
  dt[, .grz_turn := suppressWarnings(as.numeric(get(turn_col)))]
  dt[, .grz_feat_step := log1p(.grz_step)]
  dt[, .grz_feat_turn := abs(.grz_turn)]

  feature_cols <- c(".grz_feat_step", ".grz_feat_turn")
  feature_transforms <- c("log1p", "abs")
  feature_names <- c(step_col, turn_col)

  if (feature_set == "adaptive") {
    dt <- grz_hmm_add_adaptive_features(
      dt = dt,
      groups = grp,
      step_col = step_col,
      adaptive_window_mins = adaptive_window_mins,
      adaptive_window_mult = adaptive_window_mult,
      adaptive_window_min_mins = adaptive_window_min_mins
    )
    dt[, .grz_feat_net := log1p(.grz_net_disp_w_m)]
    dt[, .grz_feat_straight := pmin(pmax(.grz_straightness_w, 0), 1)]

    feature_cols <- c(feature_cols, ".grz_feat_net", ".grz_feat_straight")
    feature_transforms <- c(feature_transforms, "log1p", "identity")
    feature_names <- c(feature_names, "net_displacement_window_m", "straightness_window")
  }

  valid <- !is.na(dt$datetime) & is.finite(dt$.grz_step) & dt$.grz_step >= 0 & is.finite(dt$.grz_turn)
  for (fc in feature_cols) {
    valid <- valid & is.finite(dt[[fc]])
  }

  n_valid <- sum(valid)
  if (n_valid < 50L) {
    stop("Not enough valid rows for HMM classification.", call. = FALSE)
  }

  fit_idx <- which(valid)
  if (length(fit_idx) > as.integer(fit_max_rows)) {
    set.seed(seed)
    fit_idx <- sample(fit_idx, as.integer(fit_max_rows))
  }

  x_fit <- as.matrix(dt[fit_idx, ..feature_cols])
  model <- grz_hmm_fit_diag(
    x = x_fit,
    n_states = 2L,
    max_iter = as.integer(max_iter),
    tol = tol,
    min_var = min_var,
    seed = seed
  )

  inactive_id <- as.integer(which.min(model$means[, 1L]))
  active_id <- as.integer(setdiff(seq_len(nrow(model$means)), inactive_id)[1L])
  state_by_id <- rep(NA_character_, 2L)
  state_by_id[inactive_id] <- "inactive"
  state_by_id[active_id] <- "active"

  dt[, (state_col) := NA_character_]
  dt[, (state_id_col) := NA_integer_]
  dt[, (inactive_prob_col) := NA_real_]

  g <- interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(dt)), g)

  for (idx_all in split_idx) {
    idx <- idx_all[valid[idx_all]]
    if (length(idx) == 0L) {
      next
    }

    xg <- as.matrix(dt[idx, ..feature_cols])
    logb <- grz_hmm_log_emission_diag(xg, model$means, model$vars, min_var = min_var)
    fb <- grz_hmm_forward_backward(logb, model$a, model$pi)
    vit <- grz_hmm_viterbi(logb, model$a, model$pi)

    sid <- as.integer(vit$path)
    dt[idx, (state_id_col) := sid]
    dt[idx, (state_col) := state_by_id[sid]]
    dt[idx, (inactive_prob_col) := fb$gamma[, inactive_id]]
  }

  if (as.integer(min_run_n) > 1L) {
    dt[!is.na(get(state_col)), (state_col) := grz_smooth_state_runs(get(state_col), min_run_n = as.integer(min_run_n)), by = grp]
    dt[get(state_col) == "inactive", (state_id_col) := inactive_id]
    dt[get(state_col) == "active", (state_id_col) := active_id]
  }

  state_counts <- dt[, .N, by = state_col][order(-N)]
  n_inactive <- state_counts[get(state_col) == "inactive", N]
  n_active <- state_counts[get(state_col) == "active", N]
  n_inactive <- if (length(n_inactive) == 0L) 0L else as.integer(n_inactive[[1L]])
  n_active <- if (length(n_active) == 0L) 0L else as.integer(n_active[[1L]])

  attr(dt, "hmm_activity_model") <- list(
    feature_set = feature_set,
    features = feature_names,
    transforms = feature_transforms,
    state_map = data.frame(
      state_id = c(inactive_id, active_id),
      state = c("inactive", "active"),
      stringsAsFactors = FALSE
    ),
    pi = model$pi,
    transition = model$a,
    means = model$means,
    vars = model$vars,
    logLik = model$logLik,
    iterations = model$iterations
  )

  if (isTRUE(verbose)) {
    step_centers <- pmax(expm1(model$means[, 1L]), 0)
    turn_centers <- model$means[, 2L]
    if (feature_set == "adaptive") {
      net_centers <- pmax(expm1(model$means[, 3L]), 0)
      straight_centers <- model$means[, 4L]
      cat(
        sprintf(
          "[classify_activity_hmm] feature_set=%s valid=%s inactive=%s active=%s center_step_m(inactive)=%.2f center_step_m(active)=%.2f center_abs_turn(inactive)=%.3f center_abs_turn(active)=%.3f center_net_disp_w_m(inactive)=%.2f center_net_disp_w_m(active)=%.2f center_straight_w(inactive)=%.3f center_straight_w(active)=%.3f\n",
          feature_set,
          format(n_valid, big.mark = ","),
          format(n_inactive, big.mark = ","),
          format(n_active, big.mark = ","),
          step_centers[inactive_id],
          step_centers[active_id],
          turn_centers[inactive_id],
          turn_centers[active_id],
          net_centers[inactive_id],
          net_centers[active_id],
          straight_centers[inactive_id],
          straight_centers[active_id]
        )
      )
    } else {
      cat(
        sprintf(
          "[classify_activity_hmm] feature_set=%s valid=%s inactive=%s active=%s center_step_m(inactive)=%.2f center_step_m(active)=%.2f center_abs_turn(inactive)=%.3f center_abs_turn(active)=%.3f\n",
          feature_set,
          format(n_valid, big.mark = ","),
          format(n_inactive, big.mark = ","),
          format(n_active, big.mark = ","),
          step_centers[inactive_id],
          step_centers[active_id],
          turn_centers[inactive_id],
          turn_centers[active_id]
        )
      )
    }
  }

  drop_tmp <- c(
    ".grz_step", ".grz_turn",
    ".grz_feat_step", ".grz_feat_turn", ".grz_feat_net", ".grz_feat_straight",
    ".grz_net_disp_w_m", ".grz_path_len_w_m", ".grz_straightness_w", ".grz_window_mins"
  )
  drop_tmp <- intersect(drop_tmp, names(dt))
  if (length(drop_tmp) > 0L) {
    dt[, (drop_tmp) := NULL]
  }
  grz_as_output(dt, rc)
}

#' Classify Active/Inactive States Using GMM
#'
#' Fits a 2-component Gaussian Mixture Model (GMM) on transformed movement
#' features and decodes each row into `inactive`/`active` states using posterior
#' probabilities. Optional median smoothing can be applied to posterior
#' inactivity probabilities to reduce label flicker.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used for track-wise decoding.
#' @param step_col Step-distance column (meters).
#' @param turn_col Turn-angle column (radians).
#' @param feature_set GMM feature set. `"adaptive"` (default) augments step and
#'   turn with adaptive-window displacement features. `"legacy"` uses only
#'   step and turn.
#' @param adaptive_window_mins Adaptive feature window size in minutes. Use
#'   `"auto"` (default) to scale window length from each track's sampling
#'   interval.
#' @param adaptive_window_mult Multiplier applied to base sampling interval when
#'   `adaptive_window_mins = "auto"`.
#' @param adaptive_window_min_mins Lower bound for auto window size (minutes).
#' @param state_col Output state column name.
#' @param component_col Output numeric component id column.
#' @param inactive_prob_col Output posterior probability column for inactive
#'   state.
#' @param fit_max_rows Maximum rows used to fit the GMM (sampled if needed).
#' @param max_iter Maximum EM iterations.
#' @param tol EM convergence tolerance on log-likelihood.
#' @param min_var Minimum per-component feature variance for numerical
#'   stability.
#' @param smoothing Posterior smoothing method: `"none"`, `"median"`, or
#'   `"hmm"`.
#' @param median_window_n Window size (number of rows) for median smoothing when
#'   `smoothing = "median"`.
#' @param hmm_self_transition Self-transition probability used when
#'   `smoothing = "hmm"`.
#' @param seed Random seed for reproducible fitting/subsampling.
#' @param verbose Logical; print summary output.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended GMM activity columns.
#' @export
grz_classify_activity_gmm <- function(
  data,
  groups = NULL,
  step_col = "step_m",
  turn_col = "turn_rad",
  feature_set = c("adaptive", "legacy"),
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30,
  state_col = "activity_state_gmm",
  component_col = "activity_component_gmm",
  inactive_prob_col = "inactive_prob_gmm",
  fit_max_rows = 200000L,
  max_iter = 200L,
  tol = 1e-05,
  min_var = 1e-06,
  smoothing = c("none", "median", "hmm"),
  median_window_n = 5L,
  hmm_self_transition = 0.98,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  feature_set <- match.arg(feature_set)
  smoothing <- match.arg(smoothing)

  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(component_col) || length(component_col) != 1L || trimws(component_col) == "") {
    stop("`component_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(inactive_prob_col) || length(inactive_prob_col) != 1L || trimws(inactive_prob_col) == "") {
    stop("`inactive_prob_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.numeric(fit_max_rows) || length(fit_max_rows) != 1L || fit_max_rows < 100) {
    stop("`fit_max_rows` must be a number >= 100.", call. = FALSE)
  }
  if (!is.numeric(median_window_n) || length(median_window_n) != 1L || median_window_n < 3) {
    stop("`median_window_n` must be >= 3.", call. = FALSE)
  }
  if (!is.numeric(hmm_self_transition) || length(hmm_self_transition) != 1L ||
      !is.finite(hmm_self_transition) || hmm_self_transition <= 0 || hmm_self_transition >= 1) {
    stop("`hmm_self_transition` must be a single number in (0, 1).", call. = FALSE)
  }

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups
  grz_require_cols(dt, c("datetime", step_col, turn_col), fun_name = "grz_classify_activity_gmm()")

  dt[, .grz_step := suppressWarnings(as.numeric(get(step_col)))]
  dt[, .grz_turn := suppressWarnings(as.numeric(get(turn_col)))]
  dt[, .grz_feat_step := log1p(.grz_step)]
  dt[, .grz_feat_turn := abs(.grz_turn)]

  feature_cols <- c(".grz_feat_step", ".grz_feat_turn")
  feature_names <- c(step_col, turn_col)
  feature_transforms <- c("log1p", "abs")

  if (feature_set == "adaptive") {
    dt <- grz_hmm_add_adaptive_features(
      dt = dt,
      groups = grp,
      step_col = step_col,
      adaptive_window_mins = adaptive_window_mins,
      adaptive_window_mult = adaptive_window_mult,
      adaptive_window_min_mins = adaptive_window_min_mins
    )
    dt[, .grz_feat_net := log1p(.grz_net_disp_w_m)]
    dt[, .grz_feat_straight := pmin(pmax(.grz_straightness_w, 0), 1)]

    feature_cols <- c(feature_cols, ".grz_feat_net", ".grz_feat_straight")
    feature_names <- c(feature_names, "net_displacement_window_m", "straightness_window")
    feature_transforms <- c(feature_transforms, "log1p", "identity")
  }

  valid <- !is.na(dt$datetime) & is.finite(dt$.grz_step) & dt$.grz_step >= 0 & is.finite(dt$.grz_turn)
  for (fc in feature_cols) {
    valid <- valid & is.finite(dt[[fc]])
  }
  n_valid <- sum(valid)
  if (n_valid < 50L) {
    stop("Not enough valid rows for GMM classification.", call. = FALSE)
  }

  fit_idx <- which(valid)
  if (length(fit_idx) > as.integer(fit_max_rows)) {
    set.seed(seed)
    fit_idx <- sample(fit_idx, as.integer(fit_max_rows))
  }
  x_fit <- as.matrix(dt[fit_idx, ..feature_cols])
  model <- grz_gmm_fit_diag(
    x = x_fit,
    n_components = 2L,
    max_iter = as.integer(max_iter),
    tol = tol,
    min_var = min_var,
    seed = seed
  )

  step_centers <- pmax(expm1(model$means[, 1L]), 0)
  if (feature_set == "adaptive") {
    net_centers <- pmax(expm1(model$means[, 3L]), 0)
    movement_score <- step_centers + net_centers
  } else {
    movement_score <- step_centers
  }

  inactive_comp <- as.integer(which.min(movement_score))
  active_comp <- as.integer(setdiff(seq_len(nrow(model$means)), inactive_comp)[1L])

  dt[, (state_col) := NA_character_]
  dt[, (component_col) := NA_integer_]
  dt[, (inactive_prob_col) := NA_real_]

  x_all <- as.matrix(dt[which(valid), ..feature_cols])
  pred <- grz_gmm_predict_diag(x_all, model, min_var = min_var)
  dt[which(valid), (component_col) := as.integer(pred$component)]
  dt[which(valid), (inactive_prob_col) := pred$posterior[, inactive_comp]]
  dt[which(valid), (state_col) := data.table::fifelse(get(inactive_prob_col) >= 0.5, "inactive", "active")]
  dt[get(state_col) == "inactive", (component_col) := inactive_comp]
  dt[get(state_col) == "active", (component_col) := active_comp]

  if (smoothing == "median") {
    dt[!is.na(get(inactive_prob_col)), (inactive_prob_col) := grz_roll_median(get(inactive_prob_col), k = as.integer(median_window_n)), by = grp]
    dt[!is.na(get(inactive_prob_col)), (state_col) := data.table::fifelse(get(inactive_prob_col) >= 0.5, "inactive", "active")]
    dt[get(state_col) == "inactive", (component_col) := inactive_comp]
    dt[get(state_col) == "active", (component_col) := active_comp]
  } else if (smoothing == "hmm") {
    eps <- 1e-09
    stay <- as.numeric(hmm_self_transition)
    trans <- 1 - stay
    a <- matrix(
      c(stay, trans, trans, stay),
      nrow = 2L,
      byrow = TRUE
    )

    dt[, c(".grz_hmm_state", ".grz_hmm_prob") := {
      p <- suppressWarnings(as.numeric(get(inactive_prob_col)))
      n_local <- length(p)
      out_state <- rep(NA_character_, n_local)
      out_prob <- rep(NA_real_, n_local)

      valid <- which(is.finite(p))
      if (length(valid) > 0L) {
        run_id <- cumsum(c(1L, diff(valid) != 1L))
        split_runs <- split(valid, run_id)

        for (idx in split_runs) {
          if (length(idx) == 0L) {
            next
          }

          p_run <- p[idx]
          p_run <- pmin(pmax(p_run, eps), 1 - eps)
          logb <- cbind(log(p_run), log(1 - p_run))

          pi_vec <- c(p_run[[1L]], 1 - p_run[[1L]])
          pi_vec <- pmax(pi_vec, eps)
          pi_vec <- pi_vec / sum(pi_vec)

          fb <- grz_hmm_forward_backward(logb, a, pi_vec)
          vit <- grz_hmm_viterbi(logb, a, pi_vec)

          out_prob[idx] <- fb$gamma[, 1L]
          out_state[idx] <- data.table::fifelse(vit$path == 1L, "inactive", "active")
        }
      }

      list(out_state, out_prob)
    }, by = grp]

    dt[!is.na(.grz_hmm_prob), (inactive_prob_col) := .grz_hmm_prob]
    dt[!is.na(.grz_hmm_state), (state_col) := .grz_hmm_state]
    dt[get(state_col) == "inactive", (component_col) := inactive_comp]
    dt[get(state_col) == "active", (component_col) := active_comp]
    dt[, c(".grz_hmm_state", ".grz_hmm_prob") := NULL]
  }

  attr(dt, "gmm_activity_model") <- list(
    feature_set = feature_set,
    features = feature_names,
    transforms = feature_transforms,
    component_map = data.frame(
      component = c(inactive_comp, active_comp),
      state = c("inactive", "active"),
      stringsAsFactors = FALSE
    ),
    weights = model$weights,
    means = model$means,
    vars = model$vars,
    logLik = model$logLik,
    iterations = model$iterations,
    smoothing = smoothing,
    median_window_n = as.integer(median_window_n),
    hmm_self_transition = as.numeric(hmm_self_transition)
  )

  if (isTRUE(verbose)) {
    counts <- dt[, .N, by = state_col][order(-N)]
    n_inactive <- counts[get(state_col) == "inactive", N]
    n_active <- counts[get(state_col) == "active", N]
    n_inactive <- if (length(n_inactive) == 0L) 0L else as.integer(n_inactive[[1L]])
    n_active <- if (length(n_active) == 0L) 0L else as.integer(n_active[[1L]])
    turn_centers <- model$means[, 2L]

    if (feature_set == "adaptive") {
      net_centers <- pmax(expm1(model$means[, 3L]), 0)
      straight_centers <- model$means[, 4L]
      cat(
        sprintf(
          "[classify_activity_gmm] feature_set=%s smoothing=%s valid=%s inactive=%s active=%s center_step_m(inactive)=%.2f center_step_m(active)=%.2f center_abs_turn(inactive)=%.3f center_abs_turn(active)=%.3f center_net_disp_w_m(inactive)=%.2f center_net_disp_w_m(active)=%.2f center_straight_w(inactive)=%.3f center_straight_w(active)=%.3f\n",
          feature_set,
          smoothing,
          format(n_valid, big.mark = ","),
          format(n_inactive, big.mark = ","),
          format(n_active, big.mark = ","),
          step_centers[inactive_comp],
          step_centers[active_comp],
          turn_centers[inactive_comp],
          turn_centers[active_comp],
          net_centers[inactive_comp],
          net_centers[active_comp],
          straight_centers[inactive_comp],
          straight_centers[active_comp]
        )
      )
    } else {
      cat(
        sprintf(
          "[classify_activity_gmm] feature_set=%s smoothing=%s valid=%s inactive=%s active=%s center_step_m(inactive)=%.2f center_step_m(active)=%.2f center_abs_turn(inactive)=%.3f center_abs_turn(active)=%.3f\n",
          feature_set,
          smoothing,
          format(n_valid, big.mark = ","),
          format(n_inactive, big.mark = ","),
          format(n_active, big.mark = ","),
          step_centers[inactive_comp],
          step_centers[active_comp],
          turn_centers[inactive_comp],
          turn_centers[active_comp]
        )
      )
    }
  }

  drop_tmp <- c(
    ".grz_step", ".grz_turn",
    ".grz_feat_step", ".grz_feat_turn", ".grz_feat_net", ".grz_feat_straight",
    ".grz_net_disp_w_m", ".grz_path_len_w_m", ".grz_straightness_w", ".grz_window_mins"
  )
  drop_tmp <- intersect(drop_tmp, names(dt))
  if (length(drop_tmp) > 0L) {
    dt[, (drop_tmp) := NULL]
  }

  grz_as_output(dt, rc)
}

grz_spatial_staypoint_track <- function(
  lon,
  lat,
  datetime,
  radius_m = 20,
  min_dwell_mins = 20,
  min_points = 3L,
  max_gap_mins = 60
) {
  n <- length(lon)
  state <- rep("active", n)
  cluster <- rep(NA_integer_, n)
  dwell <- rep(NA_real_, n)

  if (n == 0L) {
    return(list(state = state, cluster = cluster, dwell = dwell))
  }

  cid <- 0L
  i <- 1L

  while (i <= n) {
    if (!is.finite(lon[[i]]) || !is.finite(lat[[i]]) || is.na(datetime[[i]])) {
      i <- i + 1L
      next
    }

    j <- i
    center_lon <- lon[[i]]
    center_lat <- lat[[i]]
    n_center <- 1L

    while (j < n) {
      k <- j + 1L
      if (!is.finite(lon[[k]]) || !is.finite(lat[[k]]) || is.na(datetime[[k]])) {
        break
      }
      gap_mins <- as.numeric(datetime[[k]] - datetime[[j]], units = "mins")
      if (!is.finite(gap_mins) || gap_mins > max_gap_mins) {
        break
      }

      d <- grz_haversine_m(center_lon, center_lat, lon[[k]], lat[[k]])
      if (!is.finite(d) || d > radius_m) {
        break
      }

      n_center <- n_center + 1L
      center_lon <- center_lon + (lon[[k]] - center_lon) / n_center
      center_lat <- center_lat + (lat[[k]] - center_lat) / n_center
      j <- k
    }

    npts <- j - i + 1L
    dwell_mins <- as.numeric(datetime[[j]] - datetime[[i]], units = "mins")
    if (npts >= as.integer(min_points) && is.finite(dwell_mins) && dwell_mins >= min_dwell_mins) {
      cid <- cid + 1L
      state[i:j] <- "inactive"
      cluster[i:j] <- cid
      dwell[i:j] <- dwell_mins
      i <- j + 1L
    } else {
      i <- i + 1L
    }
  }

  list(state = state, cluster = cluster, dwell = dwell)
}

#' Classify Active/Inactive States Using Spatial Clustering
#'
#' Detects inactive bouts using a staypoint-style spatial clustering approach:
#' consecutive points that remain within a radius for at least a minimum dwell
#' time are labelled `inactive`; other points are `active`.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used for per-track clustering.
#' @param state_col Output state column name.
#' @param cluster_col Output cluster id column name.
#' @param dwell_col Output dwell duration column name (minutes).
#' @param radius_m Spatial radius for cluster membership (meters).
#' @param min_dwell_mins Minimum dwell time for an inactive cluster.
#' @param min_points Minimum consecutive points required in a cluster.
#' @param max_gap_mins Maximum allowed gap between consecutive fixes within a
#'   cluster.
#' @param min_run_n Optional run-length smoothing threshold.
#' @param verbose Logical; print summary output.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended spatial activity columns.
#' @keywords internal
#' @noRd
grz_classify_activity_spatial <- function(
  data,
  groups = NULL,
  state_col = "activity_state_spatial",
  cluster_col = "activity_cluster_spatial",
  dwell_col = "activity_dwell_mins_spatial",
  radius_m = 20,
  min_dwell_mins = 20,
  min_points = 3L,
  max_gap_mins = 60,
  min_run_n = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(cluster_col) || length(cluster_col) != 1L || trimws(cluster_col) == "") {
    stop("`cluster_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(dwell_col) || length(dwell_col) != 1L || trimws(dwell_col) == "") {
    stop("`dwell_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.numeric(radius_m) || length(radius_m) != 1L || radius_m <= 0) {
    stop("`radius_m` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(min_dwell_mins) || length(min_dwell_mins) != 1L || min_dwell_mins <= 0) {
    stop("`min_dwell_mins` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(min_points) || length(min_points) != 1L || min_points < 2) {
    stop("`min_points` must be an integer >= 2.", call. = FALSE)
  }
  if (!is.numeric(max_gap_mins) || length(max_gap_mins) != 1L || max_gap_mins <= 0) {
    stop("`max_gap_mins` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = FALSE)
  dt <- prep$data
  grp <- prep$groups

  dt[, (state_col) := NA_character_]
  dt[, (cluster_col) := NA_integer_]
  dt[, (dwell_col) := NA_real_]

  dt[, c(state_col, cluster_col, dwell_col) := {
    res <- grz_spatial_staypoint_track(
      lon = lon,
      lat = lat,
      datetime = datetime,
      radius_m = radius_m,
      min_dwell_mins = min_dwell_mins,
      min_points = as.integer(min_points),
      max_gap_mins = max_gap_mins
    )
    list(res$state, res$cluster, res$dwell)
  }, by = grp]

  if (as.integer(min_run_n) > 1L) {
    dt[!is.na(get(state_col)), (state_col) := grz_smooth_state_runs(get(state_col), min_run_n = as.integer(min_run_n)), by = grp]
  }

  counts <- dt[, .N, by = state_col][order(-N)]
  n_inactive <- counts[get(state_col) == "inactive", N]
  n_active <- counts[get(state_col) == "active", N]
  n_inactive <- if (length(n_inactive) == 0L) 0L else as.integer(n_inactive[[1L]])
  n_active <- if (length(n_active) == 0L) 0L else as.integer(n_active[[1L]])
  n_clusters <- dt[!is.na(get(cluster_col)), uniqueN(get(cluster_col)), by = grp][, sum(V1)]

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[classify_activity_spatial] inactive=%s active=%s clusters=%s radius_m=%.1f min_dwell_mins=%.1f\n",
        format(n_inactive, big.mark = ","),
        format(n_active, big.mark = ","),
        format(n_clusters, big.mark = ","),
        radius_m,
        min_dwell_mins
      )
    )
  }

  grz_as_output(dt, rc)
}

#' Combine HMM and Spatial Activity Classifications
#'
#' Runs both HMM and spatial clustering methods, then combines them into a final
#' `inactive`/`active` decision using either strict agreement, lenient union, or
#' a weighted score.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used for per-track classification.
#' @param decision_col Output final state column name.
#' @param score_col Output combined inactivity score column name.
#' @param decision_rule Combination rule: `"weighted"` (default), `"both"`,
#'   or `"either"`.
#' @param hmm_weight Weight for HMM inactivity probability in weighted rule.
#' @param spatial_weight Weight for spatial inactivity indicator in weighted rule.
#' @param inactive_threshold Threshold on weighted score for inactive label.
#' @param hmm_state_col Output column name for HMM state labels.
#' @param hmm_prob_col Output column name for HMM inactive probabilities.
#' @param spatial_state_col Output column name for spatial state labels.
#' @param spatial_cluster_col Output spatial cluster id column name.
#' @param spatial_dwell_col Output spatial dwell duration column name.
#' @param hmm_fit_max_rows Maximum rows for HMM fitting.
#' @param hmm_feature_set HMM feature set passed to
#'   `grz_classify_activity_hmm()`.
#' @param hmm_adaptive_window_mins Adaptive feature window passed to
#'   `grz_classify_activity_hmm()`.
#' @param hmm_adaptive_window_mult Adaptive window multiplier passed to
#'   `grz_classify_activity_hmm()`.
#' @param hmm_adaptive_window_min_mins Adaptive window lower bound (minutes)
#'   passed to `grz_classify_activity_hmm()`.
#' @param spatial_radius_m Radius for spatial clustering (meters).
#' @param spatial_min_dwell_mins Minimum dwell time for spatial inactivity.
#' @param spatial_min_points Minimum points for spatial inactivity cluster.
#' @param spatial_max_gap_mins Maximum allowable gap within spatial clusters.
#' @param min_run_n Optional run-length smoothing threshold.
#' @param seed Random seed.
#' @param verbose Logical; print summary output.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended HMM, spatial, and final consensus columns.
#' @keywords internal
#' @noRd
grz_classify_activity_consensus <- function(
  data,
  groups = NULL,
  decision_col = "activity_state_consensus",
  score_col = "inactive_score_consensus",
  decision_rule = c("weighted", "both", "either"),
  hmm_weight = 0.6,
  spatial_weight = 0.4,
  inactive_threshold = 0.6,
  hmm_state_col = "activity_state_hmm",
  hmm_prob_col = "inactive_prob_hmm",
  spatial_state_col = "activity_state_spatial",
  spatial_cluster_col = "activity_cluster_spatial",
  spatial_dwell_col = "activity_dwell_mins_spatial",
  hmm_fit_max_rows = 200000L,
  hmm_feature_set = c("adaptive", "legacy"),
  hmm_adaptive_window_mins = "auto",
  hmm_adaptive_window_mult = 4,
  hmm_adaptive_window_min_mins = 30,
  spatial_radius_m = 20,
  spatial_min_dwell_mins = 20,
  spatial_min_points = 3L,
  spatial_max_gap_mins = 60,
  min_run_n = 2L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  decision_rule <- match.arg(decision_rule)
  hmm_feature_set <- match.arg(hmm_feature_set)

  if (!is.numeric(hmm_weight) || length(hmm_weight) != 1L || hmm_weight < 0) {
    stop("`hmm_weight` must be a non-negative number.", call. = FALSE)
  }
  if (!is.numeric(spatial_weight) || length(spatial_weight) != 1L || spatial_weight < 0) {
    stop("`spatial_weight` must be a non-negative number.", call. = FALSE)
  }
  if ((hmm_weight + spatial_weight) <= 0) {
    stop("`hmm_weight + spatial_weight` must be > 0.", call. = FALSE)
  }
  if (!is.numeric(inactive_threshold) || length(inactive_threshold) != 1L || inactive_threshold < 0 || inactive_threshold > 1) {
    stop("`inactive_threshold` must be in [0, 1].", call. = FALSE)
  }

  dt <- data.table::as.data.table(
    grz_classify_activity_hmm(
      data = data,
      groups = groups,
      state_col = hmm_state_col,
      inactive_prob_col = hmm_prob_col,
      fit_max_rows = hmm_fit_max_rows,
      feature_set = hmm_feature_set,
      adaptive_window_mins = hmm_adaptive_window_mins,
      adaptive_window_mult = hmm_adaptive_window_mult,
      adaptive_window_min_mins = hmm_adaptive_window_min_mins,
      min_run_n = min_run_n,
      seed = seed,
      verbose = FALSE,
      return_class = "data.table"
    )
  )

  dt <- data.table::as.data.table(
    grz_classify_activity_spatial(
      data = dt,
      groups = groups,
      state_col = spatial_state_col,
      cluster_col = spatial_cluster_col,
      dwell_col = spatial_dwell_col,
      radius_m = spatial_radius_m,
      min_dwell_mins = spatial_min_dwell_mins,
      min_points = spatial_min_points,
      max_gap_mins = spatial_max_gap_mins,
      min_run_n = min_run_n,
      verbose = FALSE,
      return_class = "data.table"
    )
  )

  dt[, .grz_hmm_score := suppressWarnings(as.numeric(get(hmm_prob_col)))]
  dt[!is.finite(.grz_hmm_score), .grz_hmm_score := data.table::fifelse(get(hmm_state_col) == "inactive", 1, 0)]
  dt[!is.finite(.grz_hmm_score), .grz_hmm_score := NA_real_]

  dt[, .grz_spatial_score := data.table::fifelse(get(spatial_state_col) == "inactive", 1, 0)]
  dt[is.na(get(spatial_state_col)), .grz_spatial_score := NA_real_]

  if (decision_rule == "both") {
    dt[, (score_col) := data.table::fifelse(
      is.na(.grz_hmm_score) | is.na(.grz_spatial_score),
      NA_real_,
      data.table::fifelse(.grz_hmm_score >= 0.5 & .grz_spatial_score >= 0.5, 1, 0)
    )]
  } else if (decision_rule == "either") {
    dt[, (score_col) := data.table::fifelse(
      is.na(.grz_hmm_score) & is.na(.grz_spatial_score),
      NA_real_,
      data.table::fifelse(
        data.table::fcoalesce(.grz_hmm_score, 0) >= 0.5 | data.table::fcoalesce(.grz_spatial_score, 0) >= 0.5,
        1,
        0
      )
    )]
  } else {
    total_w <- hmm_weight + spatial_weight
    dt[, (score_col) := (
      hmm_weight * data.table::fcoalesce(.grz_hmm_score, 0) +
        spatial_weight * data.table::fcoalesce(.grz_spatial_score, 0)
    ) / total_w]
    dt[is.na(.grz_hmm_score) & is.na(.grz_spatial_score), (score_col) := NA_real_]
  }

  dt[, (decision_col) := data.table::fifelse(
    is.na(get(score_col)),
    NA_character_,
    data.table::fifelse(get(score_col) >= inactive_threshold, "inactive", "active")
  )]

  if (as.integer(min_run_n) > 1L) {
    grp <- grz_default_group_cols(dt, groups = groups)
    dt[!is.na(get(decision_col)), (decision_col) := grz_smooth_state_runs(get(decision_col), min_run_n = as.integer(min_run_n)), by = grp]
  }

  if (isTRUE(verbose)) {
    counts <- dt[, .N, by = decision_col][order(-N)]
    msg <- paste(paste0(counts[[decision_col]], "=", counts$N), collapse = ", ")
    cat(
      "[classify_activity_consensus] rule=",
      decision_rule,
      " threshold=",
      format(inactive_threshold, nsmall = 2),
      " ",
      msg,
      "\n",
      sep = ""
    )
  }

  dt[, c(".grz_hmm_score", ".grz_spatial_score") := NULL]
  grz_as_output(dt, rc)
}

#' Classify behavior states from movement metrics
#'
#' Classifies each row into `rest`, `graze`, or `travel` using transparent rule
#' thresholds.
#'
#' @param data Input data containing GPS rows.
#' @param method Classification method. Currently only `"rules"` is supported.
#' @param state_col Name of output state column.
#' @param groups Grouping columns used for run-order and optional smoothing.
#' @param rest_speed_max Maximum speed for `rest` state (m/s).
#' @param rest_step_max Maximum step distance for `rest` state (m).
#' @param graze_speed_max Maximum speed for `graze` state (m/s).
#' @param travel_speed_min Minimum speed for `travel` state (m/s).
#' @param travel_turn_max Maximum absolute turning angle for `travel` (radians).
#' @param min_run_n Optional run-length smoothing threshold. Runs shorter than
#'   this value are replaced by neighbouring states.
#' @param verbose Logical; print summary counts.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#'
#' @return Input data with appended state column.
#' @export
grz_classify_behavior <- function(
  data,
  method = c("rules"),
  state_col = "behavior_state",
  groups = NULL,
  rest_speed_max = 0.05,
  rest_step_max = 5,
  graze_speed_max = 0.60,
  travel_speed_min = 0.60,
  travel_turn_max = 0.60,
  min_run_n = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  method <- match.arg(method)
  rc <- grz_match_output_class(return_class)
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  if (method != "rules") {
    stop("Only `method = \"rules\"` is currently supported.", call. = FALSE)
  }

  dt[, (state_col) := data.table::fifelse(
    is.na(speed_mps),
    NA_character_,
    data.table::fifelse(
      speed_mps <= rest_speed_max & (is.na(step_m) | step_m <= rest_step_max),
      "rest",
      data.table::fifelse(
        speed_mps >= travel_speed_min & (is.na(turn_rad) | abs(turn_rad) <= travel_turn_max),
        "travel",
        data.table::fifelse(speed_mps <= graze_speed_max, "graze", "travel")
      )
    )
  )]

  if (as.integer(min_run_n) > 1L) {
    dt[, (state_col) := grz_smooth_state_runs(get(state_col), min_run_n = as.integer(min_run_n)), by = grp]
  }

  attr(dt, "behavior_method") <- method
  attr(dt, "behavior_thresholds") <- list(
    rest_speed_max = rest_speed_max,
    rest_step_max = rest_step_max,
    graze_speed_max = graze_speed_max,
    travel_speed_min = travel_speed_min,
    travel_turn_max = travel_turn_max,
    min_run_n = as.integer(min_run_n)
  )

  if (isTRUE(verbose)) {
    counts <- dt[, .N, by = state_col][order(-N)]
    msg <- paste(paste0(counts[[state_col]], "=", counts$N), collapse = ", ")
    cat("[classify_behavior] ", msg, "\n", sep = "")
  }

  grz_as_output(dt, rc)
}

#' Plot diurnal state proportions
#'
#' Visualises hour-of-day state composition after behavior classification.
#'
#' @param data Input data with GPS rows and state column.
#' @param state_col Behavior-state column name.
#' @param group_col Optional grouping column for per-group facets.
#' @param cohort_col Optional cohort column for faceting.
#' @param tz_local Time zone used to derive hour-of-day.
#' @param plot_type Plot type (`"line"` or `"heatmap"`).
#' @param return_data Logical; return plotting data with plot.
#'
#' @return A ggplot object, or list with `plot` and `data` when
#'   `return_data = TRUE`.
#' @export
grz_plot_diurnal_states <- function(
  data,
  state_col = "behavior_state",
  group_col = NULL,
  cohort_col = NULL,
  tz_local = "UTC",
  plot_type = c("line", "heatmap"),
  return_data = FALSE
) {
  grz_require_ggplot2("grz_plot_diurnal_states()")
  plot_type <- match.arg(plot_type)

  dt <- data.table::copy(data.table::as.data.table(data))
  if (!state_col %in% names(dt)) {
    dt <- data.table::as.data.table(
      grz_classify_behavior(dt, state_col = state_col, verbose = FALSE, return_class = "data.table")
    )
  }

  grz_require_cols(dt, c("datetime", state_col), fun_name = "grz_plot_diurnal_states()")
  dt[, datetime := grz_parse_datetime_utc(datetime)]
  dt <- dt[!is.na(datetime) & !is.na(get(state_col)) & trimws(as.character(get(state_col))) != ""]
  if (nrow(dt) == 0L) {
    stop("No valid rows available for state plotting.", call. = FALSE)
  }

  if (is.null(group_col)) {
    dt[, .grz_group := "all"]
  } else {
    if (!group_col %in% names(dt)) {
      stop("`group_col` not found in data: ", group_col, call. = FALSE)
    }
    dt[, .grz_group := as.character(get(group_col))]
  }

  if (is.null(cohort_col)) {
    dt[, .grz_cohort := "all"]
  } else {
    if (!cohort_col %in% names(dt)) {
      stop("`cohort_col` not found in data: ", cohort_col, call. = FALSE)
    }
    dt[, .grz_cohort := as.character(get(cohort_col))]
  }

  dt[, .grz_hour := as.integer(strftime(datetime, format = "%H", tz = tz_local))]
  dt[, .grz_state := as.character(get(state_col))]

  agg <- dt[, .N, by = .(.grz_cohort, .grz_group, .grz_hour, .grz_state)]
  agg[, prop := N / sum(N), by = .(.grz_cohort, .grz_group, .grz_hour)]

  if (plot_type == "line") {
    p <- ggplot2::ggplot(
      agg,
      ggplot2::aes(x = .grz_hour, y = prop, color = .grz_state, group = .grz_state)
    ) +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      ggplot2::labs(
        x = paste0("Hour (", tz_local, ")"),
        y = "Proportion of time",
        color = "State",
        title = "Diurnal Behavior State Proportions"
      ) +
      ggplot2::facet_grid(.grz_group ~ .grz_cohort) +
      ggplot2::theme_minimal()
  } else {
    p <- ggplot2::ggplot(
      agg,
      ggplot2::aes(x = .grz_hour, y = .grz_state, fill = prop)
    ) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_gradient(
        low = "green",
        high = "red"
      ) +
      ggplot2::scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      ggplot2::labs(
        x = paste0("Hour (", tz_local, ")"),
        y = "State",
        fill = "Proportion",
        title = "Diurnal Behavior State Heatmap"
      ) +
      ggplot2::facet_grid(.grz_group ~ .grz_cohort) +
      ggplot2::theme_minimal()
  }

  if (isTRUE(return_data)) {
    return(list(plot = p, data = as.data.frame(agg)))
  }
  p
}

grz_behavior_bouts <- function(data, state_col, groups) {
  dt <- data.table::copy(data)
  data.table::setorderv(dt, c(groups, "datetime"))

  out <- data.table::rbindlist(
    lapply(split(seq_len(nrow(dt)), interaction(dt[, ..groups], drop = TRUE, lex.order = TRUE)), function(idx) {
      sub <- dt[idx, ]
      if (nrow(sub) == 0L) {
        return(NULL)
      }

      st <- as.character(sub[[state_col]])
      r <- rle(st)
      starts <- cumsum(c(1L, head(r$lengths, -1L)))
      ends <- cumsum(r$lengths)

      b <- data.table::data.table(
        state = r$values,
        n_points = r$lengths,
        start_idx = starts,
        end_idx = ends
      )
      b[, start_time := sub$datetime[start_idx]]
      b[, end_time := sub$datetime[end_idx]]
      b[, duration_mins := as.numeric(end_time - start_time, units = "mins")]

      for (g in groups) {
        b[[g]] <- sub[[g]][1L]
      }
      b
    }),
    use.names = TRUE,
    fill = TRUE
  )
  out[]
}

#' Validate behavior-state assignments
#'
#' Generates first-pass validation diagnostics for state assignments:
#' distribution summaries, transitions, bout statistics, and optional PCA view.
#'
#' @param data Input data with movement features and state labels.
#' @param state_col Behavior-state column name.
#' @param groups Grouping columns for transitions and bouts.
#' @param feature_cols Feature columns used in diagnostics and PCA.
#' @param pca Logical; compute PCA diagnostic.
#' @param pca_max_n Maximum rows used for PCA (random sample).
#' @param seed Random seed used for PCA sampling.
#' @param verbose Logical; print validation summary.
#' @param return_class Table output class (`"data.frame"` or `"data.table"`).
#'
#' @return A list of validation tables and optional PCA objects/plot.
#' @export
grz_validate_behavior <- function(
  data,
  state_col = "behavior_state",
  groups = NULL,
  feature_cols = c("speed_mps", "step_m", "turn_rad"),
  pca = TRUE,
  pca_max_n = 50000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  if (!state_col %in% names(dt)) {
    dt <- data.table::as.data.table(
      grz_classify_behavior(
        dt,
        state_col = state_col,
        groups = grp,
        verbose = FALSE,
        return_class = "data.table"
      )
    )
  }

  missing_feat <- setdiff(feature_cols, names(dt))
  if (length(missing_feat) > 0L) {
    stop("Missing feature columns: ", paste(missing_feat, collapse = ", "), call. = FALSE)
  }

  dt[, (state_col) := as.character(get(state_col))]
  use <- dt[!is.na(get(state_col)) & trimws(get(state_col)) != ""]
  if (nrow(use) == 0L) {
    stop("No non-missing behavior states available for validation.", call. = FALSE)
  }
  use[, .grz_state := as.character(get(state_col))]

  state_counts <- use[, .N, by = .grz_state][order(-N)]
  data.table::setnames(state_counts, ".grz_state", "state")

  long <- data.table::melt(
    use,
    id.vars = ".grz_state",
    measure.vars = feature_cols,
    variable.name = "feature",
    value.name = "value"
  )
  feature_summary <- long[is.finite(value), .(
    n = .N,
    mean = mean(value, na.rm = TRUE),
    median = stats::median(value, na.rm = TRUE),
    sd = if (.N >= 2L) stats::sd(value, na.rm = TRUE) else NA_real_,
    p10 = grz_quantile_or_na(value, 0.10),
    p90 = grz_quantile_or_na(value, 0.90)
  ), by = .(state = .grz_state, feature)]

  data.table::setorderv(use, c(grp, "datetime"))
  use[, .grz_prev_state := shift(.grz_state), by = grp]
  transitions <- use[!is.na(.grz_prev_state), .N, by = .(from = .grz_prev_state, to = .grz_state)]
  transitions[, prop_from := N / sum(N), by = from]

  bouts <- grz_behavior_bouts(use, state_col = ".grz_state", groups = grp)
  bout_summary <- bouts[!is.na(state), .(
    n_bouts = .N,
    median_bout_mins = stats::median(duration_mins, na.rm = TRUE),
    p90_bout_mins = grz_quantile_or_na(duration_mins, 0.90),
    short_bout_prop = grz_mean_or_na(duration_mins <= 10)
  ), by = state]

  pca_model <- NULL
  pca_scores <- NULL
  pca_plot <- NULL
  if (isTRUE(pca)) {
    pcs <- use[, c(state_col, feature_cols), with = FALSE]
    pcs <- pcs[stats::complete.cases(pcs), ]
    if (nrow(pcs) > as.integer(pca_max_n)) {
      set.seed(seed)
      pcs <- pcs[sample.int(nrow(pcs), as.integer(pca_max_n)), ]
    }
    if (nrow(pcs) >= 3L && length(feature_cols) >= 2L) {
      m <- as.matrix(pcs[, ..feature_cols])
      pca_model <- stats::prcomp(m, center = TRUE, scale. = TRUE)
      pca_scores <- data.table::data.table(
        state = pcs[[state_col]],
        PC1 = pca_model$x[, 1L],
        PC2 = pca_model$x[, 2L]
      )

      if (requireNamespace("ggplot2", quietly = TRUE)) {
        pca_plot <- ggplot2::ggplot(
          pca_scores,
          ggplot2::aes(x = PC1, y = PC2, color = state)
        ) +
          ggplot2::geom_point(alpha = 0.5, size = 1.2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(
            title = "Behavior-State PCA",
            x = "PC1",
            y = "PC2",
            color = "State"
          )
      }
    }
  }

  if (isTRUE(verbose)) {
    cat(
      sprintf(
        "[validate_behavior] states=%s transitions=%s bouts=%s pca=%s\n",
        format(nrow(state_counts), big.mark = ","),
        format(nrow(transitions), big.mark = ","),
        format(nrow(bouts), big.mark = ","),
        ifelse(is.null(pca_model), "no", "yes")
      )
    )
  }

  list(
    state_counts = grz_as_output(state_counts, rc),
    feature_summary = grz_as_output(feature_summary, rc),
    transitions = grz_as_output(transitions, rc),
    bouts = grz_as_output(bouts, rc),
    bout_summary = grz_as_output(bout_summary, rc),
    pca_model = pca_model,
    pca_scores = if (is.null(pca_scores)) NULL else grz_as_output(pca_scores, rc),
    pca_plot = pca_plot
  )
}

#' Behavior interpretation pipeline
#'
#' End-to-end helper for iterative behavior interpretation:
#' diurnal metric plot -> classify states -> diurnal state plot -> validation.
#'
#' @param data Input GPS data.
#' @param groups Grouping columns used in movement/state calculations.
#' @param cohort_col Optional cohort column used in faceted plots.
#' @param metrics Metrics for diurnal metric visualisation.
#' @param classification_method Behavior classification method.
#' @param threshold_guide Logical; compute threshold guidance summaries and
#'   plots.
#' @param metrics_plot Logical; generate diurnal metric plot.
#' @param states_plot Logical; generate diurnal state plot.
#' @param validate Logical; run behavior validation.
#' @param verbose Logical; print step summaries.
#' @param return_class Output class for returned tables.
#'
#' @return A list containing classified data, plots, and validation outputs.
#' @export
grz_behavior_pipeline <- function(
  data,
  groups = NULL,
  cohort_col = NULL,
  metrics = c("step_m", "turn_rad"),
  classification_method = c("rules"),
  threshold_guide = TRUE,
  metrics_plot = TRUE,
  states_plot = TRUE,
  validate = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
) {
  rc <- grz_match_output_class(return_class)
  classification_method <- match.arg(classification_method)

  prep <- grz_behavior_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups

  p_metrics <- NULL
  p_states <- NULL
  guide <- NULL
  if (isTRUE(threshold_guide)) {
    guide <- grz_behavior_threshold_guide(
      data = dt,
      metrics = metrics,
      cohort_col = cohort_col,
      return_class = rc
    )
  }
  if (isTRUE(metrics_plot)) {
    p_metrics <- grz_plot_diurnal_metrics(
      data = dt,
      metrics = metrics,
      group_col = "sensor_id",
      cohort_col = cohort_col,
      agg_fun = "median"
    )
  }

  classified <- grz_classify_behavior(
    data = dt,
    method = classification_method,
    groups = grp,
    verbose = verbose,
    return_class = "data.table"
  )

  if (isTRUE(states_plot)) {
    p_states <- grz_plot_diurnal_states(
      data = classified,
      state_col = "behavior_state",
      group_col = NULL,
      cohort_col = cohort_col,
      plot_type = "line"
    )
  }

  v <- NULL
  if (isTRUE(validate)) {
    v <- grz_validate_behavior(
      data = classified,
      state_col = "behavior_state",
      groups = grp,
      verbose = verbose,
      return_class = rc
    )
  }

  list(
    data = grz_as_output(classified, rc),
    threshold_guide = guide,
    plots = list(
      diurnal_metrics = p_metrics,
      diurnal_states = p_states,
      pca = if (is.null(v)) NULL else v$pca_plot
    ),
    validation = v
  )
}
