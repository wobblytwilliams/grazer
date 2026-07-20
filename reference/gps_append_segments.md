# Append continuous GPS segment identifiers

Adds a `segment_id` column for continuous pieces of each sensor track
after a large time gap or a negative time interval. Zero-length
intervals stay in the same segment because they are usually duplicate
fixes rather than a true break in the track. The default `segment_id`
combines `sensor_id` with the local segment number, for example
`C001_seg001`.

## Usage

``` r
gps_append_segments(
  data,
  large_gap_mins = 60,
  groups = NULL,
  segment_col = "segment_id",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data frame with standard GPS columns.

- large_gap_mins:

  Positive number. A new segment starts after intervals longer than this
  value.

- groups:

  Optional grouping columns. Defaults to `deployment_id` and `sensor_id`
  when `deployment_id` is present, otherwise `sensor_id`.

- segment_col:

  Name of the output segment column.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

GPS data with a `segment_id` column appended by default.
