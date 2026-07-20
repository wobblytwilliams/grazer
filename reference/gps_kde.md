# Calculate GPS kernel-density space use

Estimates two-dimensional kernel-density use surfaces for each animal or
sensor stream within an epoch. The output is a grid of high-use cells
for requested utilisation percentages, ranked from highest to lowest
density.

## Usage

``` r
gps_kde(
  data,
  bandwidth_m = NULL,
  cell_size_m = NULL,
  percent = c(95, 50),
  keep_all = FALSE,
  max_cells = 10000,
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

- bandwidth_m:

  Kernel bandwidth in metres. Use `NULL` to estimate separate x and y
  bandwidths from the data, one number for both axes, or two numbers for
  x and y.

- cell_size_m:

  Grid cell size in metres. If `NULL`, a cell size is chosen from the
  estimated bandwidth.

- percent:

  Utilisation percentages to return, such as `95` or `50`.

- keep_all:

  Logical; return all density cells for each requested percentage and
  flag membership, rather than only cells inside the utilisation
  percentage.

- max_cells:

  Maximum grid cells per group before the cell size is increased
  automatically.

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

  Minimum number of fixes required before a KDE surface is computed.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- return_geometry:

  Logical; return `sf` grid-cell polygons instead of a plain data frame.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

A data frame of KDE grid cells, or an `sf` object of grid-cell polygons
when `return_geometry = TRUE`.
