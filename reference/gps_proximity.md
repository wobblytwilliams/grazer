# Calculate pairwise GPS proximity by timestamp

Returns one row for each unordered pair of fixes within the same
timestamp and herd partition. Distances are haversine distances in
metres from WGS84 longitude and latitude. Pairwise output grows as
`n * (n - 1) / 2` within each timestamp, so large herds and fine
sampling intervals can produce large tables. For nearest-neighbour
distances or range counts, use
[`gps_nearest_neighbour()`](https://wobblytwilliams.github.io/grazer/reference/gps_nearest_neighbour.md)
or
[`gps_neighbours_within_range()`](https://wobblytwilliams.github.io/grazer/reference/gps_neighbours_within_range.md)
to avoid keeping all pair rows in memory at once.

## Usage

``` r
gps_proximity(
  data,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- herd_groups:

  Herd partition columns. Defaults to available `deployment_id` and
  `paddock`, so animals are only compared within those partitions.

- interpolate:

  Logical; if `TRUE`, regularise and interpolate fixes before comparing
  animals.

- align_interval_mins:

  Alignment interval in minutes, or `"base"` to use the median observed
  interval.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Pairwise proximity table with identifiers, `datetime`, `pair_id`, and
`distance_m`.
