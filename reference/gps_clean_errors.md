# Clean row-level GPS data errors

Removes or flags invalid datetime, identifier, coordinate, and
deployment window rows.

## Usage

``` r
gps_clean_errors(
  data,
  remove_invalid_datetime = TRUE,
  remove_invalid_coords = TRUE,
  remove_zero_zero = TRUE,
  window_start = NULL,
  window_end = NULL,
  deployment_windows = NULL,
  deployment_groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_gps_error",
  reason_col = "gps_error_reason",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- remove_invalid_datetime:

  Logical; drop invalid datetimes.

- remove_invalid_coords:

  Logical; drop invalid coordinate rows.

- remove_zero_zero:

  Logical; drop `(0,0)` rows.

- window_start:

  Optional global deployment start datetime.

- window_end:

  Optional global deployment end datetime.

- deployment_windows:

  Optional data frame with deployment group columns, `start_datetime`,
  and `end_datetime`.

- deployment_groups:

  Optional columns used to join `deployment_windows`.

- action:

  Either `"drop"` to remove rows or `"flag"` to keep all rows and add
  flag and reason columns.

- flag_col:

  Name of the error flag column when `action = "flag"`.

- reason_col:

  Name of the error reason column when `action = "flag"`.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned GPS data with cleaning audit attributes.
