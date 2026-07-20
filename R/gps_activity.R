grz_activity_prepare_dt <- function(data, groups = NULL, ensure_features = TRUE) {
  dt <- grz_prepare_clean_dt(data, require_core = TRUE)
  grp <- grz_default_group_cols(dt, groups = groups)
  if (is.null(groups) && "segment_id" %in% names(dt) && !"segment_id" %in% grp) {
    grp <- c(grp, "segment_id")
  }

  if (isTRUE(ensure_features)) {
    needed <- c("step_m", "speed_mps", "turn_rad")
    if (!all(needed %in% names(dt))) {
      dt <- data.table::as.data.table(gps_steps(
        data = dt,
        groups = grp,
        verbose = FALSE,
        return_class = "data.table"
      ))
    }
  }

  dt[, .grz_activity_row_id := .I]
  data.table::setorderv(dt, c(grp, "datetime", ".grz_activity_row_id"))
  dt[, .grz_activity_row_id := NULL]
  list(data = dt, groups = grp)
}

grz_activity_smooth_state_runs <- function(states, min_run_n = 1L) {
  if (is.null(states) || length(states) == 0L || min_run_n <= 1L) {
    return(states)
  }

  x <- as.character(states)
  r <- rle(x)
  nr <- length(r$lengths)
  if (nr <= 1L) {
    return(x)
  }

  starts <- cumsum(c(1L, utils::head(r$lengths, -1L)))
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

grz_activity_logsumexp <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(-Inf)
  }
  m <- max(x)
  m + log(sum(exp(x - m)))
}

grz_activity_hmm_log_emission_diag <- function(x, means, vars, min_var = 1e-06) {
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

grz_activity_hmm_forward_backward <- function(logb, a, pi) {
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
        log_alpha[t, s] <- logb[t, s] + grz_activity_logsumexp(log_alpha[t - 1L, ] + loga[, s])
      }
    }
  }

  loglik <- grz_activity_logsumexp(log_alpha[n, ])

  log_beta[n, ] <- 0
  if (n >= 2L) {
    for (t in (n - 1L):1L) {
      for (s in seq_len(k)) {
        log_beta[t, s] <- grz_activity_logsumexp(loga[s, ] + logb[t + 1L, ] + log_beta[t + 1L, ])
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
      denom <- grz_activity_logsumexp(as.vector(mat))
      xi_sum <- xi_sum + exp(mat - denom)
    }
  }

  list(logLik = loglik, gamma = gamma, xi_sum = xi_sum)
}

grz_activity_hmm_viterbi <- function(logb, a, pi) {
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

grz_activity_hmm_fit_diag <- function(
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
    logb <- grz_activity_hmm_log_emission_diag(x, means, vars, min_var = min_var)
    fb <- grz_activity_hmm_forward_backward(logb, a, pi)
    gamma <- fb$gamma
    nk <- colSums(gamma) + 1e-08

    pi <- gamma[1, ]
    pi <- pi / sum(pi)

    a_num <- fb$xi_sum + prior_mat
    a <- a_num / rowSums(a_num)

    for (s in seq_len(n_states)) {
      w <- gamma[, s]
      means[s, ] <- colSums(x * w) / nk[s]
      centred <- sweep(x, 2, means[s, ], "-")
      v <- colSums(centred^2 * w) / nk[s]
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
    logLik = as.numeric(utils::tail(ll_trace, 1L)),
    ll_trace = ll_trace,
    iterations = iter_used
  )
}

grz_activity_gmm_fit_diag <- function(
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
    logb <- grz_activity_hmm_log_emission_diag(x, means, vars, min_var = min_var)
    logw <- log(pmax(weights, 1e-12))

    log_joint <- sweep(logb, 2, logw, FUN = "+")
    row_ll <- apply(log_joint, 1L, grz_activity_logsumexp)
    log_gamma <- log_joint - row_ll
    gamma <- exp(log_gamma)
    gamma <- gamma / rowSums(gamma)

    nk <- colSums(gamma) + 1e-08
    weights <- nk / sum(nk)

    for (k in seq_len(n_components)) {
      w <- gamma[, k]
      means[k, ] <- colSums(x * w) / nk[k]
      centred <- sweep(x, 2, means[k, ], "-")
      v <- colSums(centred^2 * w) / nk[k]
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
    logLik = as.numeric(utils::tail(ll_trace, 1L)),
    ll_trace = ll_trace,
    iterations = iter_used
  )
}

grz_activity_gmm_predict_diag <- function(x, model, min_var = 1e-06) {
  x <- as.matrix(x)
  logb <- grz_activity_hmm_log_emission_diag(x, model$means, model$vars, min_var = min_var)
  logw <- log(pmax(as.numeric(model$weights), 1e-12))
  log_joint <- sweep(logb, 2, logw, FUN = "+")
  row_ll <- apply(log_joint, 1L, grz_activity_logsumexp)
  log_post <- log_joint - row_ll
  post <- exp(log_post)
  post <- post / rowSums(post)
  comp <- max.col(post, ties.method = "first")
  list(component = as.integer(comp), posterior = post, logLik = sum(row_ll))
}

grz_activity_roll_median <- function(x, k = 5L) {
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
  out[ok] <- stats::runmed(y[ok], k = kk, endrule = "median")
  out
}

grz_activity_adaptive_window_track <- function(lon, lat, datetime, step_m, window_mins) {
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

grz_activity_add_adaptive_features <- function(
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

  dt[, c(".grz_activity_net_disp_w_m", ".grz_activity_path_len_w_m", ".grz_activity_straightness_w", ".grz_activity_window_mins") := {
    if (is.character(adaptive_window_mins)) {
      base <- grz_round_to_base_min(as.numeric(diff(datetime), units = "mins"))
      win <- max(adaptive_window_min_mins, adaptive_window_mult * base)
    } else {
      win <- as.numeric(adaptive_window_mins)
    }

    res <- grz_activity_adaptive_window_track(
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

grz_activity_feature_data <- function(
  data,
  groups = NULL,
  step_col = "step_m",
  turn_col = "turn_rad",
  feature_set = c("adaptive", "step_turn"),
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30,
  fun_name = "gps_activity_state()"
) {
  feature_set <- match.arg(feature_set)
  prep <- grz_activity_prepare_dt(data, groups = groups, ensure_features = TRUE)
  dt <- prep$data
  grp <- prep$groups
  grz_require_cols(dt, c("datetime", step_col, turn_col), fun_name = fun_name)

  dt[, .grz_activity_step := suppressWarnings(as.numeric(get(step_col)))]
  dt[, .grz_activity_turn := suppressWarnings(as.numeric(get(turn_col)))]
  dt[, .grz_activity_feat_step := log1p(.grz_activity_step)]
  dt[, .grz_activity_feat_turn := abs(.grz_activity_turn)]

  feature_cols <- c(".grz_activity_feat_step", ".grz_activity_feat_turn")
  feature_names <- c(step_col, turn_col)
  feature_transforms <- c("log1p", "abs")

  if (feature_set == "adaptive") {
    dt <- grz_activity_add_adaptive_features(
      dt = dt,
      groups = grp,
      step_col = step_col,
      adaptive_window_mins = adaptive_window_mins,
      adaptive_window_mult = adaptive_window_mult,
      adaptive_window_min_mins = adaptive_window_min_mins
    )
    dt[, .grz_activity_feat_net := log1p(.grz_activity_net_disp_w_m)]
    dt[, .grz_activity_feat_straight := pmin(pmax(.grz_activity_straightness_w, 0), 1)]

    feature_cols <- c(feature_cols, ".grz_activity_feat_net", ".grz_activity_feat_straight")
    feature_names <- c(feature_names, "net_displacement_window_m", "straightness_window")
    feature_transforms <- c(feature_transforms, "log1p", "identity")
  }

  valid <- !is.na(dt$datetime) &
    is.finite(dt$.grz_activity_step) &
    dt$.grz_activity_step >= 0 &
    is.finite(dt$.grz_activity_turn)
  for (fc in feature_cols) {
    valid <- valid & is.finite(dt[[fc]])
  }

  list(
    data = dt,
    groups = grp,
    feature_cols = feature_cols,
    feature_names = feature_names,
    feature_transforms = feature_transforms,
    valid = valid
  )
}

grz_activity_drop_tmp <- function(dt) {
  drop_tmp <- c(
    ".grz_activity_step",
    ".grz_activity_turn",
    ".grz_activity_feat_step",
    ".grz_activity_feat_turn",
    ".grz_activity_feat_net",
    ".grz_activity_feat_straight",
    ".grz_activity_net_disp_w_m",
    ".grz_activity_path_len_w_m",
    ".grz_activity_straightness_w",
    ".grz_activity_window_mins"
  )
  drop_tmp <- intersect(drop_tmp, names(dt))
  if (length(drop_tmp) > 0L) {
    dt[, (drop_tmp) := NULL]
  }
  dt
}

grz_activity_counts <- function(dt, state_col) {
  states <- as.character(dt[[state_col]])
  list(
    inactive = sum(states == "inactive", na.rm = TRUE),
    active = sum(states == "active", na.rm = TRUE)
  )
}

grz_activity_assign_feature_names <- function(model, feature_names) {
  colnames(model$means) <- feature_names
  colnames(model$vars) <- feature_names
  model
}

grz_activity_classify_hmm <- function(
  data,
  groups = NULL,
  step_col = "step_m",
  turn_col = "turn_rad",
  feature_set = c("adaptive", "step_turn"),
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30,
  state_col = "activity_state",
  state_id_col = "activity_state_id",
  inactive_prob_col = "inactive_prob",
  fit_max_rows = 200000L,
  max_iter = 100L,
  tol = 1e-04,
  min_var = 1e-04,
  transition_prior = 1,
  self_transition_prior = 5,
  min_run_n = 2L,
  seed = 1
) {
  feature_set <- match.arg(feature_set)
  if (!is.numeric(fit_max_rows) || length(fit_max_rows) != 1L || fit_max_rows < 100) {
    stop("`fit_max_rows` must be a number >= 100.", call. = FALSE)
  }
  if (!is.numeric(min_run_n) || length(min_run_n) != 1L || min_run_n < 1) {
    stop("`min_run_n` must be a positive integer.", call. = FALSE)
  }

  features <- grz_activity_feature_data(
    data = data,
    groups = groups,
    step_col = step_col,
    turn_col = turn_col,
    feature_set = feature_set,
    adaptive_window_mins = adaptive_window_mins,
    adaptive_window_mult = adaptive_window_mult,
    adaptive_window_min_mins = adaptive_window_min_mins
  )
  dt <- features$data
  grp <- features$groups
  feature_cols <- features$feature_cols
  valid <- features$valid
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
  model <- grz_activity_hmm_fit_diag(
    x = x_fit,
    n_states = 2L,
    max_iter = as.integer(max_iter),
    tol = tol,
    min_var = min_var,
    transition_prior = transition_prior,
    self_transition_prior = self_transition_prior,
    seed = seed
  )
  model <- grz_activity_assign_feature_names(model, features$feature_names)

  step_centres <- pmax(expm1(model$means[, 1L]), 0)
  inactive_id <- as.integer(which.min(step_centres))
  active_id <- as.integer(setdiff(seq_len(nrow(model$means)), inactive_id)[1L])
  state_by_id <- rep(NA_character_, 2L)
  state_by_id[inactive_id] <- "inactive"
  state_by_id[active_id] <- "active"

  dt[, (state_col) := NA_character_]
  dt[, (state_id_col) := NA_integer_]
  dt[, (inactive_prob_col) := NA_real_]

  group_index <- interaction(dt[, ..grp], drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(dt)), group_index)

  for (idx_all in split_idx) {
    idx <- idx_all[valid[idx_all]]
    if (length(idx) == 0L) {
      next
    }

    xg <- as.matrix(dt[idx, ..feature_cols])
    logb <- grz_activity_hmm_log_emission_diag(xg, model$means, model$vars, min_var = min_var)
    fb <- grz_activity_hmm_forward_backward(logb, model$a, model$pi)
    vit <- grz_activity_hmm_viterbi(logb, model$a, model$pi)

    sid <- as.integer(vit$path)
    dt[idx, (state_id_col) := sid]
    dt[idx, (state_col) := state_by_id[sid]]
    dt[idx, (inactive_prob_col) := fb$gamma[, inactive_id]]
  }

  if (as.integer(min_run_n) > 1L) {
    dt[!is.na(get(state_col)), (state_col) := grz_activity_smooth_state_runs(get(state_col), min_run_n = as.integer(min_run_n)), by = grp]
    dt[get(state_col) == "inactive", (state_id_col) := inactive_id]
    dt[get(state_col) == "active", (state_id_col) := active_id]
  }

  counts <- grz_activity_counts(dt, state_col)
  dt <- grz_activity_drop_tmp(dt)

  metadata <- list(
    method = "hmm",
    feature_set = feature_set,
    features = features$feature_names,
    transforms = features$feature_transforms,
    groups = grp,
    valid_rows = as.integer(n_valid),
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
    iterations = model$iterations,
    min_run_n = as.integer(min_run_n),
    state_counts = counts
  )

  list(data = dt, model = metadata)
}

grz_activity_classify_gmm <- function(
  data,
  groups = NULL,
  step_col = "step_m",
  turn_col = "turn_rad",
  feature_set = c("adaptive", "step_turn"),
  adaptive_window_mins = "auto",
  adaptive_window_mult = 4,
  adaptive_window_min_mins = 30,
  state_col = "activity_state",
  component_col = "activity_state_component",
  inactive_prob_col = "inactive_prob",
  fit_max_rows = 200000L,
  max_iter = 200L,
  tol = 1e-05,
  min_var = 1e-06,
  smoothing = c("none", "hmm", "median"),
  median_window_n = 5L,
  hmm_self_transition = 0.98,
  seed = 1,
  method_label = "gmm"
) {
  feature_set <- match.arg(feature_set)
  smoothing <- match.arg(smoothing)

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

  features <- grz_activity_feature_data(
    data = data,
    groups = groups,
    step_col = step_col,
    turn_col = turn_col,
    feature_set = feature_set,
    adaptive_window_mins = adaptive_window_mins,
    adaptive_window_mult = adaptive_window_mult,
    adaptive_window_min_mins = adaptive_window_min_mins
  )
  dt <- features$data
  grp <- features$groups
  feature_cols <- features$feature_cols
  valid <- features$valid
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
  model <- grz_activity_gmm_fit_diag(
    x = x_fit,
    n_components = 2L,
    max_iter = as.integer(max_iter),
    tol = tol,
    min_var = min_var,
    seed = seed
  )
  model <- grz_activity_assign_feature_names(model, features$feature_names)

  step_centres <- pmax(expm1(model$means[, 1L]), 0)
  if (feature_set == "adaptive") {
    net_centres <- pmax(expm1(model$means[, 3L]), 0)
    movement_score <- step_centres + net_centres
  } else {
    movement_score <- step_centres
  }

  inactive_comp <- as.integer(which.min(movement_score))
  active_comp <- as.integer(setdiff(seq_len(nrow(model$means)), inactive_comp)[1L])

  dt[, (state_col) := NA_character_]
  dt[, (component_col) := NA_integer_]
  dt[, (inactive_prob_col) := NA_real_]

  valid_idx <- which(valid)
  x_all <- as.matrix(dt[valid_idx, ..feature_cols])
  pred <- grz_activity_gmm_predict_diag(x_all, model, min_var = min_var)
  inactive_prob_values <- pred$posterior[, inactive_comp]
  dt[valid_idx, (component_col) := as.integer(pred$component)]
  dt[valid_idx, (inactive_prob_col) := inactive_prob_values]
  dt[valid_idx, (state_col) := ifelse(inactive_prob_values >= 0.5, "inactive", "active")]
  dt[get(state_col) == "inactive", (component_col) := inactive_comp]
  dt[get(state_col) == "active", (component_col) := active_comp]

  if (smoothing == "median") {
    dt[!is.na(get(inactive_prob_col)), (inactive_prob_col) := grz_activity_roll_median(get(inactive_prob_col), k = as.integer(median_window_n)), by = grp]
    dt[!is.na(get(inactive_prob_col)), (state_col) := ifelse(get(inactive_prob_col) >= 0.5, "inactive", "active")]
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

    dt[, c(".grz_activity_hmm_state", ".grz_activity_hmm_prob") := {
      p <- suppressWarnings(as.numeric(get(inactive_prob_col)))
      n_local <- length(p)
      out_state <- rep(NA_character_, n_local)
      out_prob <- rep(NA_real_, n_local)

      local_valid <- which(is.finite(p))
      if (length(local_valid) > 0L) {
        run_id <- cumsum(c(1L, diff(local_valid) != 1L))
        split_runs <- split(local_valid, run_id)

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

          fb <- grz_activity_hmm_forward_backward(logb, a, pi_vec)
          vit <- grz_activity_hmm_viterbi(logb, a, pi_vec)

          out_prob[idx] <- fb$gamma[, 1L]
          out_state[idx] <- ifelse(vit$path == 1L, "inactive", "active")
        }
      }

      list(out_state, out_prob)
    }, by = grp]

    dt[!is.na(.grz_activity_hmm_prob), (inactive_prob_col) := .grz_activity_hmm_prob]
    dt[!is.na(.grz_activity_hmm_state), (state_col) := .grz_activity_hmm_state]
    dt[get(state_col) == "inactive", (component_col) := inactive_comp]
    dt[get(state_col) == "active", (component_col) := active_comp]
    dt[, c(".grz_activity_hmm_state", ".grz_activity_hmm_prob") := NULL]
  }

  counts <- grz_activity_counts(dt, state_col)
  dt <- grz_activity_drop_tmp(dt)

  metadata <- list(
    method = method_label,
    feature_set = feature_set,
    features = features$feature_names,
    transforms = features$feature_transforms,
    groups = grp,
    valid_rows = as.integer(n_valid),
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
    hmm_self_transition = as.numeric(hmm_self_transition),
    state_counts = counts
  )

  list(data = dt, model = metadata)
}

grz_activity_check_output_cols <- function(state_col, inactive_prob_col) {
  if (!is.character(state_col) || length(state_col) != 1L || trimws(state_col) == "") {
    stop("`state_col` must be a single non-empty name.", call. = FALSE)
  }
  if (!is.character(inactive_prob_col) || length(inactive_prob_col) != 1L || trimws(inactive_prob_col) == "") {
    stop("`inactive_prob_col` must be a single non-empty name.", call. = FALSE)
  }
  invisible(NULL)
}

grz_activity_call <- function(fun, defaults, dots, reserved) {
  bad <- intersect(names(dots), reserved)
  if (length(bad) > 0L) {
    stop("These arguments are controlled by `gps_activity_state()`: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  do.call(fun, utils::modifyList(defaults, dots, keep.null = TRUE))
}

grz_activity_verbose <- function(model, state_col) {
  step_centres <- pmax(expm1(model$means[, 1L]), 0)
  state_ids <- if (!is.null(model$component_map)) model$component_map$component else model$state_map$state_id
  state_names <- if (!is.null(model$component_map)) model$component_map$state else model$state_map$state
  inactive_id <- state_ids[match("inactive", state_names)]
  active_id <- state_ids[match("active", state_names)]
  turn_centres <- model$means[, 2L]

  if (model$feature_set == "adaptive") {
    net_centres <- pmax(expm1(model$means[, 3L]), 0)
    straight_centres <- model$means[, 4L]
    cat(
      sprintf(
        "[gps_activity_state] method=%s feature_set=%s valid=%s inactive=%s active=%s centre_step_m(inactive)=%.2f centre_step_m(active)=%.2f centre_abs_turn(inactive)=%.3f centre_abs_turn(active)=%.3f centre_net_disp_w_m(inactive)=%.2f centre_net_disp_w_m(active)=%.2f centre_straight_w(inactive)=%.3f centre_straight_w(active)=%.3f\n",
        model$method,
        model$feature_set,
        format(model$valid_rows, big.mark = ","),
        format(model$state_counts$inactive, big.mark = ","),
        format(model$state_counts$active, big.mark = ","),
        step_centres[inactive_id],
        step_centres[active_id],
        turn_centres[inactive_id],
        turn_centres[active_id],
        net_centres[inactive_id],
        net_centres[active_id],
        straight_centres[inactive_id],
        straight_centres[active_id]
      )
    )
  } else {
    cat(
      sprintf(
        "[gps_activity_state] method=%s feature_set=%s valid=%s inactive=%s active=%s centre_step_m(inactive)=%.2f centre_step_m(active)=%.2f centre_abs_turn(inactive)=%.3f centre_abs_turn(active)=%.3f\n",
        model$method,
        model$feature_set,
        format(model$valid_rows, big.mark = ","),
        format(model$state_counts$inactive, big.mark = ","),
        format(model$state_counts$active, big.mark = ","),
        step_centres[inactive_id],
        step_centres[active_id],
        turn_centres[inactive_id],
        turn_centres[active_id]
      )
    )
  }
  invisible(NULL)
}

#' Classify GPS activity state
#'
#' Classifies GPS-derived movement state as `active` or `inactive`. The default
#' `gmm_hmm` method fits a two-component Gaussian mixture model to movement
#' features, then applies HMM smoothing to reduce short label flicker. The
#' `"gmm"` method uses the same mixture model without HMM smoothing. The `"hmm"`
#' method fits a direct two-state HMM.
#'
#' @param data Input GPS data.
#' @param method Activity-state method. `"gmm_hmm"` is the default. `"gmm"` uses
#'   mixture-model classification without HMM smoothing. `"hmm"` fits a direct
#'   HMM.
#' @param groups Grouping columns used for track-wise features and decoding.
#' @param state_col Output state column.
#' @param inactive_prob_col Output inactive-probability column.
#' @param feature_set Feature set. `"adaptive"` augments step distance and turn
#'   angle with rolling net displacement and straightness. `"step_turn"` uses
#'   only step distance and turn angle.
#' @param fit_max_rows Maximum rows used for model fitting. Rows are sampled
#'   when there are more valid rows than this value.
#' @param seed Random seed for reproducible fitting or subsampling.
#' @param verbose Logical; print summary output.
#' @param return_class Output class: `"data.frame"` (default) or `"data.table"`.
#' @param ... Additional model arguments, such as `step_col`, `turn_col`,
#'   `adaptive_window_mins`, `max_iter`, `tol`, `min_var`, or
#'   `hmm_self_transition`.
#'
#' @return Input data with activity-state columns appended. Model diagnostics
#'   are stored in the `gps_activity_state` attribute.
#' @export
gps_activity_state <- function(
  data,
  method = c("gmm_hmm", "gmm", "hmm"),
  groups = NULL,
  state_col = "activity_state",
  inactive_prob_col = "inactive_prob",
  feature_set = c("adaptive", "step_turn"),
  fit_max_rows = 200000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
) {
  method <- match.arg(method)
  feature_set <- match.arg(feature_set)
  rc <- grz_match_output_class(return_class)
  grz_activity_check_output_cols(state_col, inactive_prob_col)
  dots <- list(...)

  if (method %in% c("gmm_hmm", "gmm")) {
    result <- grz_activity_call(
      grz_activity_classify_gmm,
      defaults = list(
        data = data,
        groups = groups,
        feature_set = feature_set,
        state_col = state_col,
        component_col = paste0(state_col, "_component"),
        inactive_prob_col = inactive_prob_col,
        fit_max_rows = fit_max_rows,
        smoothing = if (method == "gmm_hmm") "hmm" else "none",
        seed = seed,
        method_label = method
      ),
      dots = dots,
      reserved = c(
        "data", "groups", "feature_set", "state_col", "component_col",
        "inactive_prob_col", "fit_max_rows", "smoothing", "seed",
        "method_label"
      )
    )
  } else {
    result <- grz_activity_call(
      grz_activity_classify_hmm,
      defaults = list(
        data = data,
        groups = groups,
        feature_set = feature_set,
        state_col = state_col,
        state_id_col = paste0(state_col, "_id"),
        inactive_prob_col = inactive_prob_col,
        fit_max_rows = fit_max_rows,
        seed = seed
      ),
      dots = dots,
      reserved = c(
        "data", "groups", "feature_set", "state_col", "state_id_col",
        "inactive_prob_col", "fit_max_rows", "seed"
      )
    )
  }

  if (isTRUE(verbose)) {
    grz_activity_verbose(result$model, state_col = state_col)
  }

  out <- grz_as_output(result$data, rc)
  attr(out, "gps_activity_state") <- result$model
  out
}
