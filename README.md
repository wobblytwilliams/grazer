# grazer

`grazer` provides tools to ingest, clean, map, and analyse GPS data from cattle grazing experiments.

The package is designed around a practical pipeline:
data import -> validation -> cleaning -> metric calculation -> behavior interpretation.

Website (pkgdown): <https://wobblytwilliams.github.io/grazer/>

## Installation

Install the released version from CRAN (once available):

```r
# install.packages("grazer")
```

Install the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("wobblytwilliams/grazer")
```

## Input Data Requirements

Minimum required columns are:

| Column | Description |
|---|---|
| `sensor_id` | Device or animal identifier |
| `datetime` | Timestamp (`POSIXct` or parseable datetime string) |
| `lon` | Longitude (decimal degrees) |
| `lat` | Latitude (decimal degrees) |

Additional columns are treated as metadata and retained through the workflow where possible.

## Quick Start

```r
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

Ingest and validation:
- `grz_read_gps()`
- `grz_standardise_gps()`
- `grz_validate()`
- `grz_validate_gps()`

Cleaning:
- `grz_clean_duplicates()`
- `grz_clean_errors()`
- `grz_clean_speed_fixed()`
- `grz_clean_speed_stat()`
- `grz_append_paddock_names()`
- `grz_clean_spatial()`
- `grz_denoise()`
- `grz_clean()`

Row-level metrics:
- `grz_calculate_movement()`
- `grz_calculate_social()`

Epoch-level summaries:
- `grz_summarise_movement()`
- `grz_summarise_social()`
- `grz_calculate_spatial()`
- `grz_calculate_epoch_metrics()`

Behavior tools:
- `grz_plot_diurnal_metrics()`
- `grz_behavior_threshold_guide()`
- `grz_tune_thresholds()`
- `grz_classify_activity_hmm()`
- `grz_classify_activity_spatial()`
- `grz_classify_activity_consensus()`
- `grz_classify_behavior()`
- `grz_plot_diurnal_states()`
- `grz_validate_behavior()`
- `grz_behavior_pipeline()`

## Interactive Tools

- `grz_map()` for interactive GPS mapping.
- `grz_label_gps_states()` for manual `ACTIVE`/`INACTIVE` state labelling in a timeline app.

Example:

```r
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
- Set `return_class = "data.table"` where supported for data.table output.
- Cleaning functions are drop-based and report dropped row counts when `verbose = TRUE`.

## License

MIT License.
