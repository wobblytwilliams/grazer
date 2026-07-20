# Validate GPS data

Checks that the required GPS columns are present and that the core
fields are parseable. Row-level data quality checks are handled by
[`gps_qc_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_qc_summary.md)
and cleaning functions.

## Usage

``` r
gps_validate(
  data,
  drop_invalid = FALSE,
  large_gap_mins = 60,
  groups = NULL,
  check_zero_zero = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data frame with standard GPS columns.

- drop_invalid:

  Logical; if `TRUE`, rows with invalid required fields are removed from
  returned `data`.

- large_gap_mins:

  Positive number. Intervals longer than this are counted as large gaps.

- groups:

  Optional grouping columns used for ordering, duplicates, and interval
  checks. Defaults to `deployment_id` and `sensor_id` when
  `deployment_id` is present, otherwise `sensor_id`.

- check_zero_zero:

  Logical; flag `(0, 0)` coordinates as invalid.

- return_class:

  Output class for returned tables: `"data.frame"` (default) or
  `"data.table"`.

## Value

A list with class `grz_validation` containing the validation status,
message, typed data when valid, and compatibility tables for older
workflows.
