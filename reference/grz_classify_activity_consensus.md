# Combine HMM and Spatial Activity Classifications

Runs both HMM and spatial clustering methods, then combines them into a
final `inactive`/`active` decision using either strict agreement,
lenient union, or a weighted score.

## Usage

``` r
grz_classify_activity_consensus(
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
  spatial_radius_m = 20,
  spatial_min_dwell_mins = 20,
  spatial_min_points = 3L,
  spatial_max_gap_mins = 60,
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

  Grouping columns used for per-track classification.

- decision_col:

  Output final state column name.

- score_col:

  Output combined inactivity score column name.

- decision_rule:

  Combination rule: `"weighted"` (default), `"both"`, or `"either"`.

- hmm_weight:

  Weight for HMM inactivity probability in weighted rule.

- spatial_weight:

  Weight for spatial inactivity indicator in weighted rule.

- inactive_threshold:

  Threshold on weighted score for inactive label.

- hmm_state_col:

  Output column name for HMM state labels.

- hmm_prob_col:

  Output column name for HMM inactive probabilities.

- spatial_state_col:

  Output column name for spatial state labels.

- spatial_cluster_col:

  Output spatial cluster id column name.

- spatial_dwell_col:

  Output spatial dwell duration column name.

- hmm_fit_max_rows:

  Maximum rows for HMM fitting.

- spatial_radius_m:

  Radius for spatial clustering (meters).

- spatial_min_dwell_mins:

  Minimum dwell time for spatial inactivity.

- spatial_min_points:

  Minimum points for spatial inactivity cluster.

- spatial_max_gap_mins:

  Maximum allowable gap within spatial clusters.

- min_run_n:

  Optional run-length smoothing threshold.

- seed:

  Random seed.

- verbose:

  Logical; print summary output.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input data with appended HMM, spatial, and final consensus columns.
