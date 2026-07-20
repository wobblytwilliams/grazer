# Clean GPS speed outliers using a data-driven threshold

Clean GPS speed outliers using a data-driven threshold

## Usage

``` r
gps_clean_speed_stat(
  data,
  method = c("mad", "quantile"),
  k = 4,
  prob = 0.995,
  min_threshold_mps = 4,
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

- method:

  Threshold method: `"mad"` or `"quantile"`. The MAD method is fitted to
  the upper quartile of positive `log1p(speed_mps)` values so resting
  and near-resting fixes do not dominate the threshold.

- k:

  MAD multiplier (used when `method = "mad"`).

- prob:

  Quantile probability (used when `method = "quantile"`).

- min_threshold_mps:

  Lower bound for threshold.

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
