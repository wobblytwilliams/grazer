# Plot diurnal state proportions

Visualises hour-of-day state composition after behavior classification.

## Usage

``` r
grz_plot_diurnal_states(
  data,
  state_col = "behavior_state",
  group_col = NULL,
  cohort_col = NULL,
  tz_local = "UTC",
  plot_type = c("line", "heatmap"),
  return_data = FALSE
)
```

## Arguments

- data:

  Input data with GPS rows and state column.

- state_col:

  Behavior-state column name.

- group_col:

  Optional grouping column for per-group facets.

- cohort_col:

  Optional cohort column for faceting.

- tz_local:

  Time zone used to derive hour-of-day.

- plot_type:

  Plot type (`"line"` or `"heatmap"`).

- return_data:

  Logical; return plotting data with plot.

## Value

A ggplot object, or list with `plot` and `data` when
`return_data = TRUE`.
