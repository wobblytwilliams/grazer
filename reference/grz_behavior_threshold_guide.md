# Build threshold guidance summaries for behavior classification

Produces overall and hourly quantile summaries (plus diagnostic plots)
for selected metrics to help users choose rule-based behavior
thresholds.

## Usage

``` r
grz_behavior_threshold_guide(
  data,
  metrics = c("step_m", "turn_rad"),
  cohort_col = NULL,
  tz_local = "UTC",
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  max_points_plot = 150000L,
  seed = 1,
  include_tuning = TRUE,
  tuning_rest_step_grid = seq(3, 12, by = 1),
  tuning_rest_speed_grid = seq(0.03, 0.09, by = 0.02),
  tuning_max_rows = 25000L,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input data containing at least `sensor_id`, `datetime`, `lon`, `lat`.

- metrics:

  Metrics used for threshold guidance.

- cohort_col:

  Optional cohort column used in summaries and facets.

- tz_local:

  Time zone used to derive hour-of-day.

- probs:

  Quantile probabilities used in summary tables.

- max_points_plot:

  Maximum points used in density plot subsample.

- seed:

  Random seed used for plotting subsample.

- include_tuning:

  Logical; run threshold tuning diagnostics.

- tuning_rest_step_grid:

  Candidate `rest_step_max` values for tuning.

- tuning_rest_speed_grid:

  Candidate `rest_speed_max` values for tuning.

- tuning_max_rows:

  Maximum rows used in tuning sweep.

- return_class:

  Output class for returned tables.

## Value

A list with `overall`, `hourly`, `plots` (ggplot objects), and optional
`tuning` diagnostics.
