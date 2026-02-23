# Plot diurnal heatmaps for key movement metrics

Creates cohort/group-level diurnal heatmaps to support threshold tuning
before behavior-state classification.

## Usage

``` r
grz_plot_diurnal_metrics(
  data,
  metrics = c("step_m", "turn_rad"),
  group_col = "sensor_id",
  cohort_col = NULL,
  tz_local = "UTC",
  agg_fun = c("median", "mean"),
  scale = c("none", "zscore"),
  return_data = FALSE
)
```

## Arguments

- data:

  Input data containing at least `sensor_id`, `datetime`, `lon`, `lat`.

- metrics:

  Metrics to visualise (default speed/step/turn).

- group_col:

  Column used for y-axis grouping.

- cohort_col:

  Optional cohort column for faceting.

- tz_local:

  Time zone used to derive hour-of-day.

- agg_fun:

  Aggregation function for hourly metric values.

- scale:

  Optional scaling for fill values (`"none"` or `"zscore"`).

- return_data:

  Logical; return plotting data along with plot.

## Value

A ggplot object, or list with `plot` and `data` when
`return_data = TRUE`.
