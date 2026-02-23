# Classify behavior states from movement metrics

Classifies each row into `rest`, `graze`, or `travel` using transparent
rule thresholds.

## Usage

``` r
grz_classify_behavior(
  data,
  method = c("rules"),
  state_col = "behavior_state",
  groups = NULL,
  rest_speed_max = 0.05,
  rest_step_max = 5,
  graze_speed_max = 0.6,
  travel_speed_min = 0.6,
  travel_turn_max = 0.6,
  min_run_n = 1L,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data containing GPS rows.

- method:

  Classification method. Currently only `"rules"` is supported.

- state_col:

  Name of output state column.

- groups:

  Grouping columns used for run-order and optional smoothing.

- rest_speed_max:

  Maximum speed for `rest` state (m/s).

- rest_step_max:

  Maximum step distance for `rest` state (m).

- graze_speed_max:

  Maximum speed for `graze` state (m/s).

- travel_speed_min:

  Minimum speed for `travel` state (m/s).

- travel_turn_max:

  Maximum absolute turning angle for `travel` (radians).

- min_run_n:

  Optional run-length smoothing threshold. Runs shorter than this value
  are replaced by neighbouring states.

- verbose:

  Logical; print summary counts.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Input data with appended state column.
