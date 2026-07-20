# Identify GPS high-use grid cells

Counts fixes in square grid cells after transforming GPS fixes to a
metric CRS. Cells at or above `hotspot_quantile` of the cell-count
distribution are returned by default.

## Usage

``` r
gps_hotspots(
  data,
  cell_size_m = 50,
  hotspot_quantile = 0.9,
  keep_all = FALSE,
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

- cell_size_m:

  Grid cell size in metres.

- hotspot_quantile:

  Quantile of cell counts used as the hotspot cutoff.

- keep_all:

  Logical; return all cells rather than only hotspot cells.

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

Data frame of grid-cell use summaries.
