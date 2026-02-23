# Calculate spatial/home-range metrics by epoch

Calculate spatial/home-range metrics by epoch

## Usage

``` r
grz_calculate_spatial(
  data,
  epoch = c("day", "week", "month"),
  groups = NULL,
  min_fixes = 5,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with GPS rows.

- epoch:

  Epoch level: `"day"` (default), `"week"`, `"month"`.

- groups:

  Grouping columns for summaries.

- min_fixes:

  Minimum fixes to compute metrics.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Spatial summary table.
