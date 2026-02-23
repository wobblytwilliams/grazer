# Validate canonical GPS data

Validates required GPS columns and row values, returning cleaned/typed
data plus QC and invalid-row details.

## Usage

``` r
grz_validate_gps(data, drop_invalid = FALSE)
```

## Arguments

- data:

  Input data frame with canonical GPS columns.

- drop_invalid:

  Logical; if `TRUE`, invalid rows are removed from returned `data`.

## Value

A list with class `grz_validation` containing:

- data:

  Typed data (optionally with invalid rows removed).

- qc:

  QC summary table (`data.table`).

- invalid_rows:

  Rows flagged as invalid with reasons.
