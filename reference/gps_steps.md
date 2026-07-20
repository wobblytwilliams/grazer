# Calculate GPS step-level movement metrics

Builds row-level step metrics independently within each animal or sensor
stream. Coordinates are assumed to be WGS84 longitude and latitude in
decimal degrees. Step distances are great-circle haversine distances
using a spherical earth radius of 6,371,000 m.

## Usage

``` r
gps_steps(
  data,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, `lat`.

- groups:

  Grouping columns for step calculations. Defaults to `deployment_id`
  and `sensor_id` when `deployment_id` is present, otherwise
  `sensor_id`.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Data with appended `step_dt_s`, `step_m`, `speed_mps`, `bearing_deg`,
`turn_rad`, `cum_distance_m`, and `net_displacement_m` fields.
