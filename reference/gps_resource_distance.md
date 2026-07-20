# Calculate distance from GPS fixes to resources

Appends the nearest resource and distance in metres for each GPS fix.
GPS fixes are treated as WGS84 longitude and latitude. Resource
geometries must have a CRS, or `resource_crs` must be supplied
explicitly.

## Usage

``` r
gps_resource_distance(
  data,
  resources,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  metric_crs = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- resources:

  `sf` object containing point, line, or polygon resources.

- resource_id_col:

  Column in `resources` used as the resource identifier. If `NULL`, row
  numbers are used.

- resource_type_col:

  Optional column in `resources` describing resource type, such as
  water, shade, or supplement.

- resource_crs:

  CRS to assign when `resources` has no CRS.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

Input GPS rows with nearest-resource fields appended.
