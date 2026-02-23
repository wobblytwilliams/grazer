# Tune idle/graze thresholds with clearer diagnostics

Builds practical threshold-tuning outputs for `rest` vs `graze`
separation: a data-driven starting cutoff from a two-component mixture
on `log1p(step_m)`, a threshold sweep score surface, and visual
diagnostics.

## Usage

``` r
grz_tune_thresholds(
  data,
  groups = NULL,
  cohort_col = NULL,
  tz_local = "UTC",
  step_col = "step_m",
  speed_col = "speed_mps",
  turn_col = "turn_rad",
  rest_step_grid = seq(3, 12, by = 1),
  rest_speed_grid = seq(0.03, 0.09, by = 0.02),
  graze_speed_max = 0.6,
  travel_speed_min = 0.6,
  travel_turn_max = 0.6,
  min_run_n = 2L,
  short_bout_mins = 10,
  night_hours = c(20:23, 0:5),
  max_rows = 25000L,
  max_points_plot = 150000L,
  seed = 1,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data containing GPS rows.

- groups:

  Group columns used for transitions and bout calculations.

- cohort_col:

  Optional cohort column for faceting.

- tz_local:

  Time zone used to derive hour-of-day.

- step_col:

  Step-distance column (m).

- speed_col:

  Speed column (m/s).

- turn_col:

  Turn-angle column (radians).

- rest_step_grid:

  Candidate `rest_step_max` values for sweep.

- rest_speed_grid:

  Candidate `rest_speed_max` values for sweep.

- graze_speed_max:

  Passed into classification rule during sweep.

- travel_speed_min:

  Passed into classification rule during sweep.

- travel_turn_max:

  Passed into classification rule during sweep.

- min_run_n:

  Run smoothing threshold used during sweep.

- short_bout_mins:

  Bout duration threshold for instability metric.

- night_hours:

  Night hours used for night-rest metric.

- max_rows:

  Maximum rows used in threshold sweep (sampled if needed).

- max_points_plot:

  Maximum points used for density/CDF plots.

- seed:

  Random seed for reproducible subsampling.

- verbose:

  Logical; print sweep summary.

- return_class:

  Output class for returned tables.

## Value

A list with `suggested`, `sweep`, `best`, `mixture`, and `plots`.
