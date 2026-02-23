# Classify Active/Inactive States Using Spatial Clustering

Detects inactive bouts using a staypoint-style spatial clustering
approach: consecutive points that remain within a radius for at least a
minimum dwell time are labelled `inactive`; other points are `active`.

## Usage

``` r
grz_classify_activity_spatial(
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
)
```

## Arguments

- data:

  Input GPS data.

- groups:

  Grouping columns used for per-track clustering.

- state_col:

  Output state column name.

- cluster_col:

  Output cluster id column name.

- dwell_col:

  Output dwell duration column name (minutes).

- radius_m:

  Spatial radius for cluster membership (meters).

- min_dwell_mins:

  Minimum dwell time for an inactive cluster.

- min_points:

  Minimum consecutive points required in a cluster.

- max_gap_mins:

  Maximum allowed gap between consecutive fixes within a cluster.

- min_run_n:

  Optional run-length smoothing threshold.

- verbose:

  Logical; print summary output.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input data with appended spatial activity columns.
