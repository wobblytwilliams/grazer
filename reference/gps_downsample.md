# Downsample GPS fixes to a lower frequency

Selects one observed fix per target time within each animal or sensor
stream. `method = "rigid"` keeps the first fix in each target interval,
while `method = "representative"` keeps the fix closest to each target
time.

## Usage

``` r
gps_downsample(
  data,
  target_mins,
  method = c("representative", "rigid"),
  phase_mins = 0,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with `sensor_id`, `datetime`, `lon`, and `lat`.

- target_mins:

  Target interval in minutes.

- method:

  Downsample mode: `"representative"` or `"rigid"`.

- phase_mins:

  Offset from the first timestamp in each group before the first target
  time is created.

- groups:

  Grouping columns for independent streams. Defaults to available
  `deployment_id`, `animal_id`, and `sensor_id`.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Downsampled GPS data. A `gps_reg` attribute summarises input fixes,
retained fixes, gaps, and achieved sampling interval.
