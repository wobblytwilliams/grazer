# Detect GPS visits to resources

Detects runs of consecutive fixes within `radius_m` of the nearest
resource. A new visit starts when the resource changes, the animal moves
outside the radius, or the gap between near-resource fixes exceeds
`max_gap_mins`.

## Usage

``` r
gps_resource_visits(
  data,
  resources,
  radius_m = 25,
  max_gap_mins = 30,
  min_fixes = 1,
  min_duration_mins = 0,
  resource_id_col = NULL,
  resource_type_col = NULL,
  resource_crs = NULL,
  groups = NULL,
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

- radius_m:

  Distance threshold in metres for near-resource use.

- max_gap_mins:

  Maximum gap in minutes allowed within a visit.

- min_fixes:

  Minimum fixes required for a visit.

- min_duration_mins:

  Minimum visit duration in minutes.

- resource_id_col:

  Column in `resources` used as the resource identifier. If `NULL`, row
  numbers are used.

- resource_type_col:

  Optional column in `resources` describing resource type, such as
  water, shade, or supplement.

- resource_crs:

  CRS to assign when `resources` has no CRS.

- groups:

  Grouping columns for summaries. Defaults to `deployment_id` and
  `sensor_id` when `deployment_id` is present, otherwise `sensor_id`.

- metric_crs:

  Projected CRS used for area calculations. `NULL` selects a UTM CRS
  from the GPS coordinates.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class for tabular output: `"data.frame"` (default) or
  `"data.table"`. Ignored when `return_geometry = TRUE`.

## Value

Visit summary table.
