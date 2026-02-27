# Cleaning pipeline wrapper

Applies selected cleaning steps and returns cleaned data. By design,
cleaning steps report row-count changes when `verbose = TRUE`.

## Usage

``` r
grz_clean(
  data,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  paddocks_sf = NULL,
  max_speed_mps = 4,
  speed_stat_method = c("mad", "quantile"),
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  denoise_method = c("auto", "state_aware", "statistical"),
  denoise_state_col = NULL,
  denoise_inactive_states = c("inactive", "rest", "resting", "idle", "stationary",
    "lying", "ruminating"),
  denoise_keep_raw_coords = TRUE,
  groups = NULL,
  snapshot = FALSE,
  verbose = TRUE,
  return_class = c("data.frame", "data.table")
)
```

## Arguments

- data:

  Data frame of GPS rows.

- steps:

  Steps to apply. Any of: `"duplicates"`, `"errors"`, `"speed_fixed"`,
  `"speed_stat"`, `"spatial"`, `"denoise"`.

- paddocks_sf:

  Optional paddock polygons (`sf`) required for `"spatial"`.

- max_speed_mps:

  Fixed speed threshold (m/s).

- speed_stat_method:

  Statistical speed method.

- buffer_m:

  Paddock buffer in meters.

- append_paddock:

  Logical; append paddock name column.

- paddock_col:

  Output paddock column name.

- denoise_method:

  Denoise method passed to
  [`grz_denoise()`](https://wobblytwilliams.github.io/grazer/reference/grz_denoise.md).

- denoise_state_col:

  Optional state column for state-aware denoise.

- denoise_inactive_states:

  Inactive state labels for state-aware denoise.

- denoise_keep_raw_coords:

  Logical; keep `lon_raw` and `lat_raw`.

- groups:

  Grouping columns for speed/denoise/modal paddock operations.

- snapshot:

  Logical; print snapshots after each step.

- verbose:

  Logical; print details.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned data.
