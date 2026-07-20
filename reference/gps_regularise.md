# Regularise GPS fixes to expected times

Creates a regular time grid for each animal or sensor stream without
filling missing coordinates. Observed fixes are assigned to the nearest
grid time when they fall within `tolerance_mins`; missing expected fixes
have `NA` coordinates.

## Usage

``` r
gps_regularise(
  data,
  interval_mins = "base",
  tolerance_mins = NULL,
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- interval_mins:

  Target interval in minutes, or `"base"` to infer the median positive
  observed interval.

- tolerance_mins:

  Tolerance in minutes for assigning observed fixes to the nearest grid
  time. `NULL` uses half of `interval_mins`. Use `0` for strict exact
  timestamp matching.

- groups:

  Grouping columns for independent streams. Defaults to available
  `deployment_id`, `animal_id`, and `sensor_id`.

- keep_extra:

  Logical; keep non-core metadata where exact observations exist and
  fill columns that are constant within a stream.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Regularised GPS data with `is_observed`, `observed_datetime`, and
`time_offset_s`. A `gps_reg` attribute summarises expected fixes,
observed fixes, gaps, grid offsets, and achieved sampling interval.
