# Check GPS track gaps

Finds large gaps and non-positive intervals between successive fixes
within each GPS stream. Use this before appending continuous
`segment_id` values.

## Usage

``` r
gps_check_gaps(
  data,
  large_gap_mins = 60,
  groups = NULL,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data frame with standard GPS columns.

- large_gap_mins:

  Positive number. Intervals longer than this are flagged as large gaps.

- groups:

  Optional grouping columns. Defaults to `deployment_id` and `sensor_id`
  when `deployment_id` is present, otherwise `sensor_id`.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

A data frame with one row per large gap or non-positive interval.
