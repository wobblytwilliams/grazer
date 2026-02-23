# Denoise static GPS jitter

Drops near-duplicate static fixes by retaining the first point and then
retaining additional points only when movement exceeds `radius_m` or
elapsed time exceeds `max_gap_mins`.

## Usage

``` r
grz_denoise(
  data,
  radius_m = 8,
  max_gap_mins = 20,
  groups = NULL,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- radius_m:

  Spatial jitter tolerance in meters.

- max_gap_mins:

  Maximum retained gap while static.

- groups:

  Grouping columns for denoise run.

- verbose:

  Logical; print drop counts.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Denoised data.
