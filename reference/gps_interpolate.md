# Interpolate GPS fixes on a regular time grid

Regularises each animal or sensor stream onto a time grid, assigns
observed fixes to nearby grid times using `tolerance_mins`, and linearly
interpolates longitude and latitude for remaining missing expected
fixes. Interpolation is done within groups only. Use
[`gps_append_segments()`](https://wobblytwilliams.github.io/grazer/reference/gps_append_segments.md)
before interpolation when large gaps should split a track.

## Usage

``` r
gps_interpolate(
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

Interpolated GPS data with `is_observed`, `is_interpolated`, and
`interpolation_gap_s`, `observed_datetime`, and `time_offset_s`. A
`gps_reg` attribute summarises expected fixes, observed fixes,
interpolated fixes, gaps, grid offsets, and achieved sampling interval.
