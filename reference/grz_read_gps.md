# Read and standardise GPS file input

Reads CSV or parquet, maps columns to canonical names, and optionally
runs validation.

## Usage

``` r
grz_read_gps(
  path,
  source = c("auto", "csv", "parquet"),
  colmap = NULL,
  deployment_id = NULL,
  keep_extra = TRUE,
  n_max = Inf,
  validate = TRUE,
  drop_invalid = FALSE
)
```

## Arguments

- path:

  File path.

- source:

  Input source type (`"auto"`, `"csv"`, `"parquet"`).

- colmap:

  Optional named mapping for canonical columns.

- deployment_id:

  Optional deployment identifier override.

- keep_extra:

  Logical; keep non-core source columns.

- n_max:

  Optional row limit.

- validate:

  Logical; run
  [`grz_validate_gps()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate_gps.md)
  after standardisation.

- drop_invalid:

  Logical; if validating, drop invalid rows.

## Value

A canonical GPS `data.table`. When `validate = TRUE`, QC attributes are
attached: `qc_summary` and `invalid_rows`.
