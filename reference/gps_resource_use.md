# Summarise GPS use near resources

Summarises fixes assigned to their nearest resource and counts fixes
within `radius_m`. Use `radius_m = 0` for polygon zones where only fixes
inside or touching the resource should count as near.

## Usage

``` r
gps_resource_use(
  data,
  resources,
  radius_m = 25,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- resources:

  `sf` object containing point, line, or polygon resources.

- radius_m:

  Distance threshold in metres for near-resource use.

- resource_id_col:

  Column in `resources` used as the resource identifier. If `NULL`, row
  numbers are used.

- resource_type_col:

  Optional column in `resources` describing resource type, such as
  water, shade, or supplement.

- resource_crs:

  CRS to assign when `resources` has no CRS.

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- groups:

  Grouping columns for summaries. Defaults to `deployment_id` and
  `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

Resource-use summary table by stream, epoch, and resource.
