# Spatial cleaning using paddock or boundary polygons

Spatial cleaning using paddock or boundary polygons

## Usage

``` r
gps_clean_spatial(
  data,
  paddocks_sf,
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  groups = NULL,
  action = c("drop", "flag"),
  flag_col = "is_outside_boundary",
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- paddocks_sf:

  `sf` paddock polygons.

- buffer_m:

  Paddock buffer in meters.

- append_paddock:

  Logical; append paddock column.

- paddock_col:

  Name of paddock output column.

- groups:

  Grouping columns for modal paddock assignment.

- action:

  Either `"drop"` to remove rows outside polygons or `"flag"` to keep
  all rows and add a logical flag column.

- flag_col:

  Name of the outside-polygon flag column when `action = "flag"`.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Spatially cleaned GPS data with cleaning audit attributes.
