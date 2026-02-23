# Append paddock names by point-in-polygon intersection

Uses buffered paddock polygons and assigns a daily modal paddock per
animal. Rows not intersecting the modal paddock are dropped.

## Usage

``` r
grz_append_paddock_names(
  data,
  paddocks_sf,
  buffer_m = 100,
  paddock_col = "paddock",
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `lon`, `lat`, `datetime`, and `sensor_id`.

- paddocks_sf:

  `sf` polygon object.

- buffer_m:

  Buffer distance in meters.

- paddock_col:

  Output paddock column name.

- groups:

  Grouping columns used for modal paddock assignment (default
  deployment + sensor when available).

- verbose:

  Logical; print diagnostics.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Data with appended paddock column and dropped out-of-paddock rows.
