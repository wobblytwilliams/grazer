# Summarise GPS social metrics by epoch

Converts row-level social proximity metrics into one row per animal or
sensor and epoch. If social columns are not present,
[`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md)
is run first using the supplied thresholds and alignment settings.

## Usage

``` r
gps_social_summary(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
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

  Data frame with GPS rows or output from
  [`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md),
  [`gps_nearest_neighbour()`](https://wobblytwilliams.github.io/grazer/reference/gps_nearest_neighbour.md),
  or
  [`gps_neighbours_within_range()`](https://wobblytwilliams.github.io/grazer/reference/gps_neighbours_within_range.md).

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- groups:

  Grouping columns for summaries. Defaults to available `deployment_id`,
  `animal_id`, and `sensor_id`.

- thresholds_m:

  Distance thresholds used when social metrics must be calculated from
  raw GPS rows.

- herd_groups:

  Herd partition columns passed to
  [`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md).

- interpolate:

  Logical; passed to
  [`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md)
  when needed.

- align_interval_mins:

  Alignment interval passed to
  [`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md).

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Epoch social summary table with stable identifier, epoch, nearest
neighbour, group-size, and threshold columns.
