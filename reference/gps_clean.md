# Clean GPS data using a selected sequence of steps

Applies selected cleaning steps and returns cleaned data. Each step
attaches an audit summary, and `gps_clean()` combines those summaries
into the final `cleaning_summary` attribute.

## Usage

``` r
gps_clean(
  data,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  action = c("drop", "flag"),
  paddocks_sf = NULL,
  max_speed_mps = 4,
  speed_stat_method = c("mad", "quantile"),
  window_start = NULL,
  window_end = NULL,
  deployment_windows = NULL,
  deployment_groups = NULL,
  buffer_m = 100,
  append_paddock = TRUE,
  paddock_col = "paddock",
  denoise_method = c("statistical", "state_aware"),
  denoise_state_col = NULL,
  denoise_inactive_states = c("inactive", "rest", "resting", "idle", "stationary",
    "lying", "ruminating"),
  denoise_keep_raw_coords = TRUE,
  step_args = list(),
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

- action:

  Either `"drop"` to remove rows in row-filtering steps or `"flag"` to
  keep rows and add step-specific flag columns.

- paddocks_sf:

  Optional paddock polygons (`sf`) required for `"spatial"`.

- max_speed_mps:

  Fixed speed threshold (m/s).

- speed_stat_method:

  Statistical speed method.

- window_start:

  Optional global deployment start datetime.

- window_end:

  Optional global deployment end datetime.

- deployment_windows:

  Optional deployment window table passed to
  [`gps_clean_errors()`](https://wobblytwilliams.github.io/grazer/reference/gps_clean_errors.md).

- deployment_groups:

  Optional join columns for `deployment_windows`.

- buffer_m:

  Paddock buffer in meters.

- append_paddock:

  Logical; append paddock name column.

- paddock_col:

  Output paddock column name.

- denoise_method:

  Denoise method passed to
  [`gps_denoise()`](https://wobblytwilliams.github.io/grazer/reference/gps_denoise.md).

- denoise_state_col:

  Optional state column for state-aware denoise.

- denoise_inactive_states:

  Inactive state labels for state-aware denoise.

- denoise_keep_raw_coords:

  Logical; keep `lon_raw` and `lat_raw`.

- step_args:

  Optional named list of step-specific arguments. Names must match
  requested steps.

- groups:

  Grouping columns for speed/denoise/modal paddock operations.

- snapshot:

  Logical; print snapshots after each step.

- verbose:

  Logical; print details.

- return_class:

  Output class: `"data.frame"` (default) or `"data.table"`.

## Value

Cleaned GPS data with combined cleaning audit attributes.
