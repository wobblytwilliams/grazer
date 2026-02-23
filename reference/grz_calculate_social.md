# Calculate row-level social metrics

Calculate row-level social metrics

## Usage

``` r
grz_calculate_social(
  data,
  thresholds_m = c(25, 30, 50, 100),
  align_interval_mins = "base",
  interpolate = TRUE,
  herd_groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- thresholds_m:

  Distance thresholds for neighbour counts.

- align_interval_mins:

  Alignment interval in minutes or `"base"`.

- interpolate:

  Logical; align/interpolate before social calculations.

- herd_groups:

  Herd partition columns for pairwise calculations. Defaults to
  available `deployment_id` and `paddock`.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Data with appended row-level social fields.
