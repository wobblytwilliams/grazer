# Summarise GPS metrics by animal or sensor

Produces one modelling-ready row per animal or sensor and epoch. By
default the function groups by available `deployment_id`, `animal_id`,
and `sensor_id`, then delegates to
[`gps_epoch()`](https://wobblytwilliams.github.io/grazer/reference/gps_epoch.md).

## Usage

``` r
gps_animal_summary(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  include = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
)
```

## Arguments

- data:

  Optional GPS data or a GPS-derived metric table.

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- include:

  Metric blocks to include. Values are `"movement"`, `"social"`,
  `"spatial"`, and `"resource_use"`. If `NULL`, blocks are inferred from
  supplied tables and columns in `data`.

- groups:

  Grouping columns for summaries and joins.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

- ...:

  Additional arguments passed to
  [`gps_epoch()`](https://wobblytwilliams.github.io/grazer/reference/gps_epoch.md).

## Value

Animal or sensor epoch summary table.
