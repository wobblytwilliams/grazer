# Classify Active/Inactive States Using GMM

Fits a 2-component Gaussian Mixture Model (GMM) on transformed movement
features and decodes each row into `inactive`/`active` states using
posterior probabilities. Optional median or HMM smoothing can be applied
to reduce label flicker.

## Usage

``` r
grz_classify_activity_gmm(
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

- feature_set:

  GMM feature set. `"adaptive"` (default) augments step and turn with
  adaptive-window displacement features. `"legacy"` uses only step and
  turn.

- adaptive_window_mins:

  Adaptive feature window size in minutes. Use `"auto"` (default) to
  scale window length from each track's sampling interval.

- adaptive_window_mult:

  Multiplier applied to base sampling interval when
  `adaptive_window_mins = "auto"`.

- adaptive_window_min_mins:

  Lower bound for auto window size (minutes).

- state_col:

  Output state column name.

- component_col:

  Output numeric component id column.

- inactive_prob_col:

  Output posterior probability column for inactive state.

- fit_max_rows:

  Maximum rows used to fit the GMM (sampled if needed).

- max_iter:

  Maximum EM iterations.

- tol:

  EM convergence tolerance on log-likelihood.

- min_var:

  Minimum per-component feature variance for numerical stability.

- smoothing:

  Posterior smoothing method: `"none"`, `"median"`, or `"hmm"`.

- median_window_n:

  Window size (number of rows) for median smoothing when
  `smoothing = "median"`.

- hmm_self_transition:

  Self-transition probability used when `smoothing = "hmm"`.

- seed:

  Random seed for reproducible fitting/subsampling.

- verbose:

  Logical; print summary output.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input data with appended GMM activity columns.
