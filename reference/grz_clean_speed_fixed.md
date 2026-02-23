# Clean speed outliers using fixed threshold

Clean speed outliers using fixed threshold

## Usage

``` r
grz_clean_speed_fixed(
  data,
  max_speed_mps = 4,
  groups = NULL,
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

- keep_speed_cols:

  Keep `step_dt_s`, `step_m`, and `speed_mps` columns.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned data.
