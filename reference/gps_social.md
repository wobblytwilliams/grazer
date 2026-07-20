# Calculate standard GPS social proximity metrics

Convenience wrapper around nearest-neighbour distance and neighbour
counts. The output remains row-level so it can be summarised by animal,
sensor, datetime, or user-defined epochs. Use
[`gps_proximity()`](https://wobblytwilliams.github.io/grazer/reference/gps_proximity.md)
when pair-level distances are required and
[`gps_contacts()`](https://wobblytwilliams.github.io/grazer/reference/gps_contacts.md)
when association events are required.

## Usage

``` r
gps_social(
  data,
  thresholds_m = c(25, 30, 50, 100),
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

Input rows with nearest-neighbour, mean-distance, group-size, and
threshold count metrics.
