# Summarise GPS movement by epoch

Summarises step-level movement within calendar or fixed-duration epochs.
Coordinates are assumed to be WGS84 longitude and latitude in decimal
degrees. Distances come from
[`gps_steps()`](https://wobblytwilliams.github.io/grazer/reference/gps_steps.md),
which uses haversine distances on a spherical earth. For epoch
summaries, the first fix in each epoch has no within-epoch step, so
steps crossing an epoch boundary are not counted in `total_distance_m`
or speed summaries.

## Usage

``` r
gps_movement_summary(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with GPS rows or output from
  [`gps_steps()`](https://wobblytwilliams.github.io/grazer/reference/gps_steps.md).

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- groups:

  Grouping columns for step calculations. Defaults to `deployment_id`
  and `sensor_id` when `deployment_id` is present, otherwise
  `sensor_id`. When `segment_id` is present it is always used to prevent
  steps across segment boundaries. `cum_distance_m` carries forward
  between segments within the requested track groups without adding a
  gap step.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Epoch summary table with movement metrics.
