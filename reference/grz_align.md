# Align/interpolate to a regular interval

Align/interpolate to a regular interval

## Usage

``` r
grz_align(
  data,
  interval_mins = "base",
  groups = NULL,
  keep_extra = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- interval_mins:

  Target interval in minutes, or `"base"` to infer from median observed
  interval (rounded to nearest minute).

- groups:

  Grouping columns for independent alignment.

- keep_extra:

  Logical; carry extra columns where exact timestamps exist.

- verbose:

  Logical; print alignment summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Aligned data with `aligned_from_observation` flag.
