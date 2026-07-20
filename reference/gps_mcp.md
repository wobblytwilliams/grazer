# Calculate GPS minimum convex polygon summaries

Builds minimum convex polygons for each animal or sensor stream within
an epoch. GPS fixes are read as WGS84 longitude and latitude, then
transformed to `metric_crs` for area calculation. When
`metric_crs = NULL`, a UTM CRS is selected from the centre of the GPS
fixes.

## Usage

``` r
gps_mcp(
  data,
  percent = c(100, 95),
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  min_fixes = 5,
  metric_crs = NULL,
  return_geometry = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- percent:

  MCP percentage or percentages. Values must be \> 0 and \<= 100.

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- groups:

  Grouping columns for summaries. Defaults to `deployment_id` and
  `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.

- min_fixes:

  Minimum number of fixes required before an MCP is computed.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- return_geometry:

  Logical; return `sf` polygon geometry instead of a plain data frame.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

A data frame with MCP area summaries, or an `sf` object when
`return_geometry = TRUE`.
