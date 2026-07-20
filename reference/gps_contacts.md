# Calculate GPS contact events

Detects runs of pairwise proximity where distance is less than or equal
to a contact threshold. Events are built separately for each pair and
herd partition. A single contact fix has `duration_s = 0`. For irregular
timestamps, use `max_gap_mins` to decide how far apart contact fixes can
be while still belonging to the same event.

## Usage

``` r
gps_contacts(
  data,
  contact_distance_m,
  herd_groups = NULL,
  interpolate = FALSE,
  align_interval_mins = "base",
  max_gap_mins = NULL,
  min_duration_mins = 0,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- contact_distance_m:

  Contact threshold in metres.

- herd_groups:

  Herd partition columns. Defaults to available `deployment_id` and
  `paddock`, so animals are only compared within those partitions.

- interpolate:

  Logical; if `TRUE`, regularise and interpolate fixes before comparing
  animals.

- align_interval_mins:

  Alignment interval in minutes, or `"base"` to use the median observed
  interval.

- max_gap_mins:

  Maximum gap between contact fixes in the same event. If `NULL`, the
  median positive timestamp interval in the pairwise data is used.

- min_duration_mins:

  Minimum event duration to retain.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Contact event table with pair identifiers, event timing, duration,
contact-fix counts, distance summaries, and `contact_distance_m`.
