# grazer

`grazer` provides tools to ingest, clean, map, and analyse GPS data from
cattle grazing experiments.

The package is designed around a practical pipeline: data import -\>
validation -\> cleaning -\> metric calculation -\> behavior
interpretation.

Website (pkgdown): <https://wobblytwilliams.github.io/grazer/>

## Installation

Install the released version from CRAN (once available):

``` r
# install.packages("grazer")
```

Install the development version from GitHub:

``` r
install.packages("devtools")
devtools::install_github("wobblytwilliams/grazer")
```

## Input Data Requirements

Minimum required columns are:

| Column      | Description                                        |
|-------------|----------------------------------------------------|
| `sensor_id` | Device or animal identifier                        |
| `datetime`  | Timestamp (`POSIXct` or parseable datetime string) |
| `lon`       | Longitude (decimal degrees)                        |
| `lat`       | Latitude (decimal degrees)                         |

Additional columns are treated as metadata and retained through the
workflow where possible.

## Quick Start

``` r
library(grazer)

# 1) Read and validate
gps <- grz_read_gps("path/to/gps.csv", validate = FALSE)
gps <- grz_validate(gps, drop_invalid = TRUE)

# 2) Clean
gps_clean <- grz_clean(
  gps,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4
)

# 3) Row-level metrics
gps_move <- grz_calculate_movement(gps_clean)
gps_social <- grz_calculate_social(gps_clean)

# 4) Epoch summaries
daily_metrics <- grz_calculate_epoch_metrics(gps_clean, epoch = "day")

head(daily_metrics)
```

## Main Function Groups

Ingest and validation: -
[`grz_read_gps()`](https://wobblytwilliams.github.io/grazer/reference/grz_read_gps.md) -
[`grz_standardise_gps()`](https://wobblytwilliams.github.io/grazer/reference/grz_standardise_gps.md) -
[`grz_validate()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate.md) -
[`grz_validate_gps()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate_gps.md)

Cleaning: -
[`grz_clean_duplicates()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_duplicates.md) -
[`grz_clean_errors()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_errors.md) -
[`grz_clean_speed_fixed()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_speed_fixed.md) -
[`grz_clean_speed_stat()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_speed_stat.md) -
[`grz_append_paddock_names()`](https://wobblytwilliams.github.io/grazer/reference/grz_append_paddock_names.md) -
[`grz_clean_spatial()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_spatial.md) -
[`grz_denoise()`](https://wobblytwilliams.github.io/grazer/reference/grz_denoise.md) -
[`grz_clean()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean.md)

Row-level metrics: -
[`grz_calculate_movement()`](https://wobblytwilliams.github.io/grazer/reference/grz_calculate_movement.md) -
[`grz_calculate_social()`](https://wobblytwilliams.github.io/grazer/reference/grz_calculate_social.md)

Epoch-level summaries: -
[`grz_summarise_movement()`](https://wobblytwilliams.github.io/grazer/reference/grz_summarise_movement.md) -
[`grz_summarise_social()`](https://wobblytwilliams.github.io/grazer/reference/grz_summarise_social.md) -
[`grz_calculate_spatial()`](https://wobblytwilliams.github.io/grazer/reference/grz_calculate_spatial.md) -
[`grz_calculate_epoch_metrics()`](https://wobblytwilliams.github.io/grazer/reference/grz_calculate_epoch_metrics.md)

Behavior tools: -
[`grz_plot_diurnal_metrics()`](https://wobblytwilliams.github.io/grazer/reference/grz_plot_diurnal_metrics.md) -
[`grz_behavior_threshold_guide()`](https://wobblytwilliams.github.io/grazer/reference/grz_behavior_threshold_guide.md) -
[`grz_tune_thresholds()`](https://wobblytwilliams.github.io/grazer/reference/grz_tune_thresholds.md) -
[`grz_classify_activity_hmm()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_activity_hmm.md) -
[`grz_classify_activity_spatial()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_activity_spatial.md) -
[`grz_classify_activity_consensus()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_activity_consensus.md) -
[`grz_classify_behavior()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_behavior.md) -
[`grz_plot_diurnal_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_plot_diurnal_states.md) -
[`grz_validate_behavior()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate_behavior.md) -
[`grz_behavior_pipeline()`](https://wobblytwilliams.github.io/grazer/reference/grz_behavior_pipeline.md)

## Interactive Tools

- [`grz_map()`](https://wobblytwilliams.github.io/grazer/reference/grz_map.md)
  for interactive GPS mapping.
- [`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md)
  for manual `ACTIVE`/`INACTIVE` state labelling in a timeline app.

Example:

``` r
labelled <- grz_label_gps_states(
  data = gps_clean,
  lon = "lon",
  lat = "lat",
  time = "datetime",
  color_by = "sensor_id",
  initial_label_col = "label"
)
```

## Output Conventions

- Functions return `data.frame` by default.
- Set `return_class = "data.table"` where supported for data.table
  output.
- Cleaning functions are drop-based and report dropped row counts when
  `verbose = TRUE`.

## License

MIT License.
