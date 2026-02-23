# Cleaning pipeline wrapper

Applies selected cleaning steps and returns cleaned data. By design,
cleaning steps drop rows and print row-count changes when
`verbose = TRUE`.

## Usage

``` r
grz_clean(
  data,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  paddocks_sf = NULL,
  max_speed_mps = 4,
  speed_stat_method = c("mad", "quantile"),
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  groups = NULL,
  snapshot = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- steps:

  Steps to apply. Any of: `"duplicates"`, `"errors"`, `"speed_fixed"`,
  `"speed_stat"`, `"spatial"`, `"denoise"`.

- paddocks_sf:

  Optional paddock polygons (`sf`) required for `"spatial"`.

- max_speed_mps:

  Fixed speed threshold (m/s).

- speed_stat_method:

  Statistical speed method.

- buffer_m:

  Paddock buffer in meters.

- append_paddock:

  Logical; append paddock name column.

- paddock_col:

  Output paddock column name.

- groups:

  Grouping columns for speed/denoise/modal paddock operations.

- snapshot:

  Logical; print snapshots after each step.

- verbose:

  Logical; print details.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned data.
