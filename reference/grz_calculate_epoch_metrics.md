# Calculate merged epoch metrics

Wrapper that merges movement/social/spatial epoch summaries. Daily epoch
is the default.

## Usage

``` r
grz_calculate_epoch_metrics(
  data,
  epoch = c("day", "week", "month"),
  include = c("movement", "social", "spatial"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame with GPS rows.

- epoch:

  Epoch level: `"day"` (default), `"week"`, `"month"`.

- include:

  Which metric blocks to include.

- groups:

  Grouping columns for summaries.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Merged epoch metrics table.
