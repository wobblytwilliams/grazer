# Denoise GPS jitter using statistical or state-aware smoothing

Uses statistical smoothing to reduce coordinate noise without dropping
rows. If active/inactive state labels are available, a state-aware
method can be used where inactive runs are collapsed to a robust
centroid and active runs are smoothed statistically.

## Usage

``` r
grz_denoise(
  data,
  method = c("auto", "state_aware", "statistical"),
  state_col = NULL,
  inactive_states = c("inactive", "rest", "resting", "idle", "stationary", "lying",
    "ruminating"),
  groups = NULL,
  keep_raw_coords = TRUE,
  verbose = TRUE,
  snapshot = FALSE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- method:

  Denoise method: `"auto"`, `"state_aware"`, or `"statistical"`.
  `"auto"` uses state-aware denoising when a usable state column is
  present; otherwise statistical smoothing is used.

- state_col:

  Optional state column used for `"state_aware"` mode.

- inactive_states:

  Character values treated as inactive in state-aware mode.

- groups:

  Grouping columns for denoise run.

- keep_raw_coords:

  Logical; when `TRUE`, adds `lon_raw` and `lat_raw` columns before
  replacing `lon` and `lat`.

- verbose:

  Logical; print processing details.

- snapshot:

  Logical; print quick snapshot after step.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Denoised data (row count unchanged).
