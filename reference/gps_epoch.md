# Join GPS epoch summaries

Builds or joins modelling-ready GPS epoch summaries. Raw GPS rows can be
summarised directly for movement and social metrics. Pre-computed
outputs from
[`gps_movement_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_movement_summary.md),
[`gps_social_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_social_summary.md),
[`gps_spatial()`](https://wobblytwilliams.github.io/grazer/reference/gps_spatial.md),
and
[`gps_resource_use()`](https://wobblytwilliams.github.io/grazer/reference/gps_resource_use.md)
can also be supplied and will be joined by their common identifier and
epoch columns.

## Usage

``` r
gps_epoch(
  data = NULL,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  include = NULL,
  groups = NULL,
  movement = NULL,
  social = NULL,
  spatial = NULL,
  resource_use = NULL,
  thresholds_m = c(25, 30, 50, 100),
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  min_fixes = 5,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
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

- movement:

  Optional output from
  [`gps_steps()`](https://wobblytwilliams.github.io/grazer/reference/gps_steps.md)
  or
  [`gps_movement_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_movement_summary.md).

- social:

  Optional output from
  [`gps_social()`](https://wobblytwilliams.github.io/grazer/reference/gps_social.md)
  or
  [`gps_social_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_social_summary.md).

- spatial:

  Optional output from
  [`gps_spatial()`](https://wobblytwilliams.github.io/grazer/reference/gps_spatial.md).

- resource_use:

  Optional output from
  [`gps_resource_use()`](https://wobblytwilliams.github.io/grazer/reference/gps_resource_use.md).

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

- min_fixes:

  Minimum number of fixes required before an MCP is computed.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Joined epoch summary table.
