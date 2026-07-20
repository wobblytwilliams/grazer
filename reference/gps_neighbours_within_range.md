# Count GPS neighbours within distance thresholds

Counts the number of other fixes within each supplied threshold at each
timestamp and herd partition. Counts only use finite distances, so
missing fixes do not contribute as neighbours.

## Usage

``` r
gps_neighbours_within_range(
  data,
  thresholds_m,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- thresholds_m:

  Distance thresholds in metres.

- herd_groups:

  Herd partition columns. Defaults to available `deployment_id` and
  `paddock`, so animals are only compared within those partitions.

- interpolate:

  Logical; if `TRUE`, regularise and interpolate fixes before comparing
  animals.

- align_interval_mins:

  Alignment interval in minutes, or `"base"` to use the median observed
  interval.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input rows with `social_group_size`, `n_valid_fixes`, and one
`n_neighbours_within_*m` and `any_neighbour_within_*m` column per
threshold.
