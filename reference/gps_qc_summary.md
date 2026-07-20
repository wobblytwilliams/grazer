# Summarise GPS quality control

Produces a structured quality-control object for row-level issues,
duplicated records, time intervals, and large gaps.

## Usage

``` r
gps_qc_summary(
  data,
  large_gap_mins = 60,
  groups = NULL,
  check_zero_zero = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data frame with standard GPS columns.

- large_gap_mins:

  Positive number. Intervals longer than this are counted as large gaps.

- groups:

  Optional grouping columns. Defaults to `deployment_id` and `sensor_id`
  when `deployment_id` is present, otherwise `sensor_id`.

- check_zero_zero:

  Logical; flag `(0, 0)` coordinates as invalid.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

A list with class `grz_qc` containing a printed summary and issue
tables.
