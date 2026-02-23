# Validate behavior-state assignments

Generates first-pass validation diagnostics for state assignments:
distribution summaries, transitions, bout statistics, and optional PCA
view.

## Usage

``` r
grz_validate_behavior(
  data,
  state_col = "behavior_state",
  groups = NULL,
  feature_cols = c("speed_mps", "step_m", "turn_rad"),
  pca = TRUE,
  pca_max_n = 50000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data with movement features and state labels.

- state_col:

  Behavior-state column name.

- groups:

  Grouping columns for transitions and bouts.

- feature_cols:

  Feature columns used in diagnostics and PCA.

- pca:

  Logical; compute PCA diagnostic.

- pca_max_n:

  Maximum rows used for PCA (random sample).

- seed:

  Random seed used for PCA sampling.

- verbose:

  Logical; print validation summary.

- return_class:

  Table output class (`"data.frame"` or `"data.table"`).

## Value

A list of validation tables and optional PCA objects/plot.
