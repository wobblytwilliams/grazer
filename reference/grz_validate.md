# Validate GPS schema compliance

Checks required columns and row-level validity. Designed as a
lightweight pipeline compliance check before cleaning.

## Usage

``` r
grz_validate(
  data,
  drop_invalid = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame containing GPS data.

- drop_invalid:

  Logical; if `TRUE`, invalid rows are removed.

- verbose:

  Logical; print summary to console.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Validated data as a data.frame by default. Validation summary is
attached in attributes `validation_qc` and `invalid_rows`.
