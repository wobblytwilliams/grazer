# Summarise GPS metrics by group or herd metadata

Aggregates animal or sensor epoch summaries to group-level rows, such as
deployment, paddock, treatment, herd, or user-supplied grouping columns.

## Usage

``` r
gps_group_summary(
  data,
  epoch = c("day", "hour", "week", "month", "interval"),
  epoch_mins = NULL,
  group_cols = NULL,
  include = NULL,
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table"),
  ...
)
```

## Arguments

- data:

  Optional GPS data or a GPS-derived metric table.

- epoch:

  Epoch level: `"day"` (default), `"hour"`, `"week"`, `"month"`, or
  `"interval"`.

- epoch_mins:

  Positive epoch duration in minutes. Supplying this uses fixed-duration
  `"interval"` epochs anchored to Unix time in UTC.

- group_cols:

  Group or herd metadata columns. If `NULL`, available `deployment_id`,
  `herd_id`, `group_id`, `paddock`, and `treatment` columns are used.

- include:

  Metric blocks to include. Values are `"movement"`, `"social"`,
  `"spatial"`, and `"resource_use"`. If `NULL`, blocks are inferred from
  supplied tables and columns in `data`.

- groups:

  Grouping columns for summaries and joins.

- verbose:

  Logical; print a short summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

- ...:

  Additional arguments passed to
  [`gps_epoch()`](https://wobblytwilliams.github.io/grazer/reference/gps_epoch.md).

## Value

Group-level epoch summary table.
