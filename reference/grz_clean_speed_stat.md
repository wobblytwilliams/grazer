# Clean speed outliers using data-driven threshold

Clean speed outliers using data-driven threshold

## Usage

``` r
grz_clean_speed_stat(
  data,
  method = c("mad", "quantile"),
  k = 4,
  prob = 0.995,
  min_threshold_mps = 4,
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

- method:

  Threshold method: `"mad"` or `"quantile"`.

- k:

  MAD multiplier (used when `method = "mad"`).

- prob:

  Quantile probability (used when `method = "quantile"`).

- min_threshold_mps:

  Lower bound for threshold.

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
