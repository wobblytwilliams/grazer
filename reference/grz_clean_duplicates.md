# Drop duplicate rows

Drop duplicate rows

## Usage

``` r
grz_clean_duplicates(
  data,
  keys = c("sensor_id", "datetime", "lon", "lat"),
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

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

De-duplicated data.
