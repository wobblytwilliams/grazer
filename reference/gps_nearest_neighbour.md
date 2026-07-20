# Calculate GPS nearest-neighbour distance

Calculates the nearest other animal or sensor at each timestamp within
each herd partition. Rows with missing timestamps or coordinates are
retained with missing nearest-neighbour metrics.

## Usage

``` r
gps_nearest_neighbour(
  data,
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

Input rows with `social_group_size`, `n_valid_fixes`,
`nearest_neighbour_m`, `nearest_neighbour_sensor_id`, and, when present,
`nearest_neighbour_animal_id`.
