# Calculate row-level movement metrics

Calculate row-level movement metrics

## Usage

``` r
grz_calculate_movement(
  data,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- groups:

  Grouping columns for movement calculations.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Data with appended row-level movement fields.
