# Append paddock names to GPS fixes

Assigns paddock or area names to GPS fixes using a point-in-polygon
overlay. GPS fixes are treated as WGS84 longitude and latitude. Paddock
polygons must have a CRS, or are assumed to be WGS84 when the CRS is
missing.

## Usage

``` r
gps_append_paddock_names(
  data,
  paddocks_sf,
  name_col = NULL,
  paddock_col = "assigned_paddock",
  buffer_m = 0,
  metric_crs = NULL,
  epoch = c("day", "hour"),
  epoch_mins = NULL,
  groups = NULL,
  min_prop = 0.7,
  min_fixes = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- paddocks_sf:

  `sf` polygon object containing paddock or area polygons.

- name_col:

  Column in `paddocks_sf` containing paddock names. If `NULL`, a
  complete `NAME`, `Name`, `name`, or `Description` column is selected.

- paddock_col:

  Output paddock column name.

- buffer_m:

  Optional buffer distance in metres. Use `0` for a direct
  point-in-polygon overlay.

- metric_crs:

  Projected CRS used when `buffer_m > 0`. `NULL` selects a UTM CRS from
  the GPS coordinates.

- epoch:

  Epoch level for assignment: `"day"` or `"hour"`. Supplying
  `epoch_mins` uses fixed-duration intervals.

- epoch_mins:

  Optional fixed epoch duration in minutes.

- groups:

  Grouping columns used for animal or sensor tracks.

- min_prop:

  Minimum proportion of valid GPS fixes in an animal-epoch that must
  fall in one paddock before it is assigned.

- min_fixes:

  Minimum valid GPS fixes required per animal-epoch.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

GPS data with an `assigned_paddock` column appended by default.
