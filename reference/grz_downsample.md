# Downsample to lower frequency

Downsample to lower frequency

## Usage

``` r
grz_downsample(
  data,
  target_mins,
  method = c("representative", "rigid"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- target_mins:

  Target interval in minutes.

- method:

  Downsample mode: `"rigid"` (first in window) or `"representative"`
  (closest to window center).

- groups:

  Grouping columns for independent downsampling.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Downsampled data.
