# Classify Active/Inactive States Using HMM

Fits a 2-state Gaussian HMM on transformed movement features and decodes
each track into `inactive`/`active` states. This is useful when simple
thresholding is unstable due to GPS jitter in low-movement periods.

## Usage

``` r
grz_classify_activity_hmm(
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
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input GPS data.

- groups:

  Grouping columns used for track-wise decoding.

- step_col:

  Step-distance column (meters).

- turn_col:

  Turn-angle column (radians).

- state_col:

  Output state column name.

- state_id_col:

  Output numeric state id column.

- inactive_prob_col:

  Output posterior probability column for inactive state.

- fit_max_rows:

  Maximum rows used to fit the HMM (sampled if needed).

- max_iter:

  Maximum EM iterations.

- tol:

  EM convergence tolerance on log-likelihood.

- min_var:

  Minimum per-state feature variance for numerical stability.

- min_run_n:

  Optional run-length smoothing threshold.

- seed:

  Random seed for reproducible fitting/subsampling.

- verbose:

  Logical; print summary output.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input data with appended HMM activity columns.
