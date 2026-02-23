# Summarise row-level social metrics by epoch

Summarise row-level social metrics by epoch

## Usage

``` r
grz_summarise_social(
  data,
  epoch = c("day", "week", "month"),
  groups = NULL,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame (preferably with social columns).

- epoch:

  Epoch level: `"day"` (default), `"week"`, `"month"`.

- groups:

  Grouping columns for summaries.

- verbose:

  Logical; print summary.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Epoch summary table.
