# Classify GPS activity state

Classifies GPS-derived movement state as `active` or `inactive`. The
default `gmm_hmm` method fits a two-component Gaussian mixture model to
movement features, then applies HMM smoothing to reduce short label
flicker. The `"gmm"` method uses the same mixture model without HMM
smoothing. The `"hmm"` method fits a direct two-state HMM.

## Usage

``` r
gps_activity_state(
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
)
```

## Arguments

- data:

  Input GPS data.

- method:

  Activity-state method. `"gmm_hmm"` is the default. `"gmm"` uses
  mixture-model classification without HMM smoothing. `"hmm"` fits a
  direct HMM.

- groups:

  Grouping columns used for track-wise features and decoding.

- state_col:

  Output state column.

- inactive_prob_col:

  Output inactive-probability column.

- feature_set:

  Feature set. `"adaptive"` augments step distance and turn angle with
  rolling net displacement and straightness. `"step_turn"` uses only
  step distance and turn angle.

- fit_max_rows:

  Maximum rows used for model fitting. Rows are sampled when there are
  more valid rows than this value.

- seed:

  Random seed for reproducible fitting or subsampling.

- verbose:

  Logical; print summary output.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

- ...:

  Additional model arguments, such as `step_col`, `turn_col`,
  `adaptive_window_mins`, `max_iter`, `tol`, `min_var`, or
  `hmm_self_transition`.

## Value

Input data with activity-state columns appended. Model diagnostics are
stored in the `gps_activity_state` attribute.
