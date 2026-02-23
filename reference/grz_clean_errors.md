# Clean row-level data errors

Removes invalid datetime/sensor/coordinate rows and optional `(0,0)`
rows.

## Usage

``` r
grz_clean_errors(
  data,
  remove_invalid_datetime = TRUE,
  remove_invalid_coords = TRUE,
  remove_zero_zero = TRUE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- remove_invalid_datetime:

  Logical; drop invalid datetimes.

- remove_invalid_coords:

  Logical; drop invalid coordinate rows.

- remove_zero_zero:

  Logical; drop `(0,0)` rows.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned data.
