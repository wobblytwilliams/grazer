# grazer

`grazer` provides practical tools for livestock GPS analysis in grazing
systems. The current package covers validation, quality control,
cleaning, track segmentation, movement, spatial use, resource use,
social proximity, mapping, and GPS-derived activity states.

The public API uses short `gps_*` function names. Future livestock
sensor modules may use the `acc_*`, `vf_*`, `rs_*`, and `fusion_*`
prefixes when those workflows are ready.

## Installation

``` r

install.packages("devtools")
devtools::install_github("wobblytwilliams/grazer")
```

## GPS data

Most functions expect a data frame with four standard columns:

| Column      | Meaning                                              |
|-------------|------------------------------------------------------|
| `sensor_id` | Collar, sensor, or data-stream identifier            |
| `datetime`  | Fix time as `POSIXct` or a parseable datetime string |
| `lon`       | Longitude in decimal degrees, WGS84                  |
| `lat`       | Latitude in decimal degrees, WGS84                   |

Additional study fields such as `animal_id`, `deployment_id`, `paddock`,
and `treatment` are retained where possible.

## Basic workflow

``` r

library(grazer)
library(dplyr)
library(readr)

# Import the collar data, then rename the required fields once.
gps_raw <- read_csv("path/to/gps.csv", show_col_types = FALSE) |>
  rename(
    sensor_id = collar_id,
    datetime = fix_time,
    lon = longitude,
    lat = latitude
  )

# Confirm that the required columns and values have the expected format.
validation <- gps_validate(
  gps_raw,
  drop_invalid = TRUE,
  large_gap_mins = 60,
  groups = "sensor_id"
)
print(validation)

gps_valid <- validation$data

# Inspect row-level errors, duplicates, sampling gaps, and time intervals.
qc <- gps_qc_summary(
  gps_valid,
  large_gap_mins = 60,
  groups = "sensor_id"
)
print(qc)

# Split each sensor track where a large time gap occurs. The resulting
# segment_id combines the sensor identifier and continuous segment number.
gps_segmented <- gps_append_segments(
  gps_valid,
  large_gap_mins = 60,
  groups = "sensor_id"
)

# Apply the selected cleaning steps within continuous track segments.
gps_cleaned <- gps_clean(
  gps_segmented,
  steps = c("duplicates", "errors", "speed_stat", "denoise"),
  action = "drop",
  groups = "segment_id"
)

# Calculate daily movement metrics. Step calculations remain separated by
# segment_id, so distances are not calculated across large gaps.
daily_movement <- gps_movement_summary(
  gps_cleaned,
  epoch = "day",
  groups = c("sensor_id", "animal_id")
)

daily_movement |>
  select(sensor_id, animal_id, epoch_start, n_fixes, total_distance_m)
```

Functions return `data.frame` by default. Cleaning, regularisation, and
model functions retain useful diagnostics in documented attributes. The
complete development walkthrough is available in
[`dev/public-function-walkthrough.R`](https://wobblytwilliams.github.io/grazer/dev/public-function-walkthrough.R).

## Licence

MIT Licence.
