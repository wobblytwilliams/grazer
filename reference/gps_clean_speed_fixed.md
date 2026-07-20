# Clean GPS speed outliers using a fixed threshold

Clean GPS speed outliers using a fixed threshold

## Usage

``` r
gps_clean_speed_fixed(
  data,
  max_speed_mps = 4,
  groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_speed_outlier",
  keep_speed_cols = FALSE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- max_speed_mps:

  Maximum biologically plausible speed (m/s).

- groups:

  Grouping columns for step/speed calculation.

- action:

  Either `"drop"` to remove rows above the speed threshold or `"flag"`
  to keep all rows and add a logical flag column.

- flag_col:

  Name of the speed flag column when `action = "flag"`.

- keep_speed_cols:

  Keep `step_dt_s`, `step_m`, and `speed_mps` columns.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned GPS data with cleaning audit attributes.
