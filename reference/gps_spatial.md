# Summarise GPS spatial use by epoch

Produces basic spatial summaries and MCP area fields for each stream and
epoch. Coordinates are WGS84 GPS fixes; area calculations are made after
transforming to `metric_crs`.

## Usage

``` r
gps_spatial(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  groups = NULL,
  min_fixes = 5,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

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

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

Spatial summary table with fix counts, time span, coordinate bounds,
centroid coordinates, and MCP area fields.
