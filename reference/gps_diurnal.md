# Summarise GPS metrics by date and hour

Creates UTC hourly summaries with explicit `date` and `hour` columns for
diurnal analyses. The output is based on
[`gps_animal_summary()`](https://wobblytwilliams.github.io/grazer/reference/gps_animal_summary.md)
with `epoch = "hour"`.

## Usage

``` r
gps_diurnal(
  data,
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

Hourly GPS summary table with `date` and `hour` columns.
