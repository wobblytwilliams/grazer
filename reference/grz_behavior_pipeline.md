# Behavior interpretation pipeline

End-to-end helper for iterative behavior interpretation: diurnal metric
plot -\> classify states -\> diurnal state plot -\> validation.

## Usage

``` r
grz_behavior_pipeline(
  data,
  groups = NULL,
  cohort_col = NULL,
  metrics = c("step_m", "turn_rad"),
  classification_method = c("rules"),
  threshold_guide = TRUE,
  metrics_plot = TRUE,
  states_plot = TRUE,
  validate = TRUE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Input GPS data.

- groups:

  Grouping columns used in movement/state calculations.

- cohort_col:

  Optional cohort column used in faceted plots.

- metrics:

  Metrics for diurnal metric visualisation.

- classification_method:

  Behavior classification method.

- threshold_guide:

  Logical; compute threshold guidance summaries and plots.

- metrics_plot:

  Logical; generate diurnal metric plot.

- states_plot:

  Logical; generate diurnal state plot.

- validate:

  Logical; run behavior validation.

- verbose:

  Logical; print step summaries.

- return_class:

  Output class for returned tables.

## Value

A list containing classified data, plots, and validation outputs.
