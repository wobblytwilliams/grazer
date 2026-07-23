# Interpolate GPS fixes on a regular time grid

Evaluates each animal or sensor stream on a common-phase regular time
grid. Longitude and latitude are interpolated directly from the
immediately preceding and following valid raw observations using
elapsed-time weights. Observations are not snapped to nearby grid times
and positions are never extrapolated. Interpolation is done within
groups only. Use
[`gps_append_segments()`](https://wobblytwilliams.github.io/grazer/reference/gps_append_segments.md)
before interpolation when large gaps should split a track.

## Usage

``` r
gps_interpolate(
  data,
  interval_mins = "base",
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with raw observation rows and `sensor_id`, `datetime`,
  `lon`, and `lat`. Output from
  [`gps_regularise()`](https://wobblytwilliams.github.io/grazer/reference/gps_regularise.md)
  or `gps_interpolate()` is not accepted.

- interval_mins:

  Target interval in minutes, or `"base"` to infer the median positive
  observed interval.

- groups:

  Grouping columns for independent streams. Defaults to available
  `deployment_id`, `animal_id`, `sensor_id`, and `segment_id`. An
  available `segment_id` is always included so interpolation cannot
  cross segments.

- keep_extra:

  Logical; retain non-core metadata on exact observations and fill
  columns that are constant within a stream.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Interpolated GPS data with `is_observed`, `is_interpolated`, and
`interpolation_gap_s`, `observed_datetime`, and `time_offset_s`. A
`gps_reg` attribute summarises raw observations, valid anchors, exact
grid observations, interpolated and unfilled grid rows, gaps, and
achieved sampling interval.

## Examples

``` r
gps_interpolate(
  data.frame(
    sensor_id = "A",
    datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + c(2, 17, 32) * 60,
    lon = c(150, 150.001, 150.002),
    lat = c(-30, -30.001, -30.002)
  ),
  interval_mins = 15,
  verbose = FALSE
)
#>   sensor_id            datetime      lon       lat is_observed is_interpolated
#> 1         A 2024-01-01 00:00:00       NA        NA       FALSE           FALSE
#> 2         A 2024-01-01 00:15:00 150.0009 -30.00087       FALSE            TRUE
#> 3         A 2024-01-01 00:30:00 150.0019 -30.00187       FALSE            TRUE
#> 4         A 2024-01-01 00:45:00       NA        NA       FALSE           FALSE
#>   observed_datetime time_offset_s interpolation_gap_s
#> 1              <NA>            NA                  NA
#> 2              <NA>            NA                 900
#> 3              <NA>            NA                 900
#> 4              <NA>            NA                  NA
```
