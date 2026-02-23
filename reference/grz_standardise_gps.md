# Standardise GPS column names and core types

Maps input headings to canonical names (`sensor_id`, `datetime`, `lon`,
`lat`), coerces core types, and carries optional metadata columns.

## Usage

``` r
grz_standardise_gps(
  data,
  colmap = NULL,
  deployment_id = NULL,
  keep_extra = TRUE
)
```

## Arguments

- data:

  Input data frame.

- colmap:

  Optional named character vector mapping canonical names to source
  column names.

- deployment_id:

  Optional deployment identifier (single value or vector of length
  `nrow(data)`).

- keep_extra:

  Logical; keep non-core source columns.

## Value

A `data.table` with canonical GPS columns.
