# Calculate GPS turning angles

Thin wrapper around
[`gps_steps()`](https://wobblytwilliams.github.io/grazer/reference/gps_steps.md)
for workflows that need bearing and turning fields. Turning angles are
absolute changes between consecutive step bearings within each group.

## Usage

``` r
gps_turning(
  data,
  groups = NULL,
  unit = c("radians", "degrees", "both"),
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
  `sensor_id`. When `segment_id` is present it is always used to prevent
  steps across segment boundaries. `cum_distance_m` carries forward
  between segments within the requested track groups without adding a
  gap step.

- unit:

  Turning angle unit to return. `"radians"` returns `turn_rad`,
  `"degrees"` returns `turn_deg`, and `"both"` returns both columns.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Data with step bearings and turning angle fields.
