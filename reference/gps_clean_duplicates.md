# Drop or flag duplicate GPS fixes

Identifies duplicate fixes from user-selected key columns. By default
the first matching row is retained and later duplicate rows are removed.

## Usage

``` r
gps_clean_duplicates(
  data,
  keys = c("sensor_id", "datetime", "lon", "lat"),
  action = c("drop", "flag"),
  flag_col = "is_duplicate_fix",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- keys:

  Columns used to identify duplicates.

- action:

  Either `"drop"` to remove duplicate rows or `"flag"` to keep all rows
  and add a logical flag column.

- flag_col:

  Name of the duplicate flag column when `action = "flag"`.

- verbose:

  Logical; print drop or flag counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned GPS data. Attributes `cleaning_summary`, `removed_rows`, and
`flagged_rows` contain audit information where relevant.
