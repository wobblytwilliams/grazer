# End-to-End GPS Workflow

## Overview

This tutorial walks through a full `grazer` workflow:

1.  Create/import GPS data.
2.  Validate schema and row quality.
3.  Clean obvious data issues.
4.  Calculate movement and social metrics.
5.  Summarise metrics by epoch.
6.  Generate activity states for interpretation.

The tutorial uses a small synthetic dataset so it runs quickly and is
fully reproducible.

``` r
library(grazer)
```

## 1) Build an Example Dataset

In practice, most projects start with `grz_read_gps("your_file.csv")`.
Here we generate data directly to keep the tutorial self-contained.

``` r
set.seed(42)

n_animals <- 3L
n_fix <- 24L * 6L * 3L # 3 days at 10-minute intervals

timestamps <- seq(
  as.POSIXct("2024-09-01 00:00:00", tz = "UTC"),
  by = "10 min",
  length.out = n_fix
)

make_track <- function(sensor_id, lon0, lat0) {
  data.frame(
    sensor_id = sensor_id,
    datetime = timestamps,
    lon = lon0 + cumsum(rnorm(n_fix, mean = 0, sd = 0.00018)),
    lat = lat0 + cumsum(rnorm(n_fix, mean = 0, sd = 0.00014)),
    stringsAsFactors = FALSE
  )
}

gps <- rbind(
  make_track("A01", 132.305, -14.474),
  make_track("A02", 132.300, -14.471),
  make_track("A03", 132.309, -14.469)
)

# Add a few realistic data issues so cleaning steps are visible.
gps <- rbind(gps, gps[1:5, ])         # duplicates
gps$lon[12] <- 0                      # invalid 0,0 coordinate
gps$lat[12] <- 0
gps$datetime[25] <- NA                # invalid datetime
gps$lon[300] <- gps$lon[300] + 0.5    # extreme jump
gps$lat[300] <- gps$lat[300] + 0.5

dim(gps)
#> [1] 1301    4
head(gps)
#>   sensor_id            datetime      lon       lat
#> 1       A01 2024-09-01 00:00:00 132.3052 -14.47407
#> 2       A01 2024-09-01 00:10:00 132.3051 -14.47428
#> 3       A01 2024-09-01 00:20:00 132.3052 -14.47429
#> 4       A01 2024-09-01 00:30:00 132.3053 -14.47416
#> 5       A01 2024-09-01 00:40:00 132.3054 -14.47445
#> 6       A01 2024-09-01 00:50:00 132.3054 -14.47449
```

## 2) Validate Inputs

[`grz_validate()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate.md)
checks required columns and row-level validity.

``` r
gps_valid <- grz_validate(gps, drop_invalid = FALSE, verbose = TRUE)
#> [validate] rows=1,301 invalid=1 drop_invalid=FALSE
```

If you want invalid rows removed immediately, use `drop_invalid = TRUE`.

## 3) Clean the GPS Data

[`grz_clean()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean.md)
applies selected cleaning steps in sequence.

``` r
gps_clean <- grz_clean(
  gps_valid,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  verbose = TRUE
)
#> [clean] start_rows=1,301
#> [clean_duplicates] dropped: 5 | rows: 1,301 -> 1,296
#> [clean_errors] dropped_sensor=0 dropped_datetime=1 dropped_coord=0 dropped_zero_zero=1
#> [clean_errors] dropped: 2 | rows: 1,296 -> 1,294
#> [clean_speed_fixed] threshold=4.000 m/s
#> [clean_speed_fixed] dropped: 2 | rows: 1,294 -> 1,292
#> [denoise] radius_m=8.0 max_gap_mins=20.0
#> [denoise] dropped: 147 | rows: 1,292 -> 1,145
#> [clean] final_rows=1,145

dim(gps_clean)
#> [1] 1145    4
```

## 4) Calculate Row-Level Metrics

Movement metrics are appended per row:

``` r
gps_move <- grz_calculate_movement(gps_clean, verbose = FALSE)

head(gps_move[, c("sensor_id", "datetime", "step_m", "speed_mps", "turn_rad")])
#>   sensor_id            datetime   step_m  speed_mps  turn_rad
#> 1       A01 2024-09-01 00:00:00       NA         NA        NA
#> 2       A01 2024-09-01 00:10:00 26.43748 0.04406246        NA
#> 3       A01 2024-09-01 00:30:00 23.40070 0.01950058 2.5984382
#> 4       A01 2024-09-01 00:40:00 33.18402 0.05530670 1.9333044
#> 5       A01 2024-09-01 01:00:00 35.19329 0.02932774 0.6466382
#> 6       A01 2024-09-01 01:10:00 22.52189 0.03753648 2.3381507
```

Social metrics are calculated from pairwise distances at matching
timestamps:

``` r
gps_social <- grz_calculate_social(
  gps_clean,
  interpolate = FALSE,
  verbose = FALSE
)

head(gps_social[, c("sensor_id", "datetime", "nearest_neighbor_m", "mean_dist_to_others_m")])
#>   sensor_id            datetime nearest_neighbor_m mean_dist_to_others_m
#> 1       A01 2024-09-01 00:00:00           658.6774              697.3500
#> 2       A01 2024-09-01 00:10:00           674.1047              703.9899
#> 3       A01 2024-09-01 00:30:00           653.9589              653.9589
#> 4       A01 2024-09-01 00:40:00           677.9667              693.3137
#> 5       A01 2024-09-01 01:00:00           669.3390              681.2625
#> 6       A01 2024-09-01 01:10:00           651.3377              672.1177
```

## 5) Summarise by Epoch

Create daily summaries across movement, social, and spatial metric
blocks:

``` r
daily_metrics <- grz_calculate_epoch_metrics(
  gps_clean,
  epoch = "day",
  include = c("movement", "social", "spatial"),
  verbose = FALSE
)

head(daily_metrics)
#>   sensor_id      epoch n_fixes total_distance_m mean_step_m median_step_m
#> 1       A01 2024-09-01     124         3049.627    24.79372      23.25731
#> 2       A01 2024-09-02     130         2981.152    22.93193      21.47594
#> 3       A01 2024-09-03     120         2769.693    23.08077      20.59701
#> 4       A02 2024-09-01     133         3274.149    24.80416      21.44471
#> 5       A02 2024-09-02     129         2979.671    23.09822      20.86086
#> 6       A02 2024-09-03     123         3044.434    24.75150      22.19985
#>   p95_step_m mean_speed_mps p95_speed_mps max_speed_mps mean_abs_turn_rad
#> 1   46.42561     0.03787735    0.07686938    0.09678620          1.570044
#> 2   40.38573     0.03625706    0.06577168    0.08103584          1.601261
#> 3   41.37209     0.03505623    0.06895348    0.08552444          1.658756
#> 4   45.91862     0.03965457    0.07559741    0.10891191          1.547781
#> 5   43.68859     0.03636858    0.07281431    0.09406478          1.567149
#> 6   47.93585     0.03725534    0.07414606    0.09361049          1.534920
#>   social_n_fixes mean_nn_m p50_nn_m mean_dist_to_others_m
#> 1            144  427.2408 440.1242              527.4449
#> 2            144  232.8264 234.1117              579.9951
#> 3            144  748.4558 716.2365             1009.2021
#> 4            144  437.3016 440.1242              546.8217
#> 5            144  232.8264 234.1117              552.0137
#> 6            144  486.6880 507.6488              677.3645
#>   mean_n_within_           25m mean_n_within_           30m
#> 1                            0                            0
#> 2                            0                            0
#> 3                            0                            0
#> 4                            0                            0
#> 5                            0                            0
#> 6                            0                            0
#>   mean_n_within_           50m mean_n_within_          100m
#> 1                   0.00000000                   0.04861111
#> 2                   0.01388889                   0.05555556
#> 3                   0.00000000                   0.00000000
#> 4                   0.00000000                   0.04861111
#> 5                   0.01388889                   0.05555556
#> 6                   0.00000000                   0.00000000
#>   prop_any_within_           25m prop_any_within_           30m
#> 1                              0                              0
#> 2                              0                              0
#> 3                              0                              0
#> 4                              0                              0
#> 5                              0                              0
#> 6                              0                              0
#>   prop_any_within_           50m prop_any_within_          100m spatial_n_fixes
#> 1                     0.00000000                     0.04861111             124
#> 2                     0.01388889                     0.05555556             130
#> 3                     0.00000000                     0.00000000             120
#> 4                     0.00000000                     0.04861111             133
#> 5                     0.01388889                     0.05555556             129
#> 6                     0.00000000                     0.00000000             123
#>   span_hours mcp100_area_ha mcp95_area_ha  kde95_ha
#> 1   23.83333       3.585967      3.109282  5.761878
#> 2   23.83333       3.868435      3.366676  5.101731
#> 3   23.83333       6.283721      5.918231 11.753870
#> 4   23.83333       8.605469      8.262302 15.347603
#> 5   23.83333       4.781225      4.518734  7.996646
#> 6   23.83333       7.260353      6.353417 10.453869
```

## 6) Activity State Interpretation

[`grz_classify_activity_consensus()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_activity_consensus.md)
combines HMM and spatial signals into a final `active`/`inactive` state.

``` r
gps_states <- grz_classify_activity_consensus(
  gps_clean,
  groups = "sensor_id",
  verbose = FALSE
)

table(gps_states$activity_state_consensus, useNA = "ifany")
#> 
#>   active inactive 
#>      924      221
```

For additional diagnostics and threshold tuning, see:

- [`grz_plot_diurnal_metrics()`](https://wobblytwilliams.github.io/grazer/reference/grz_plot_diurnal_metrics.md)
- [`grz_behavior_threshold_guide()`](https://wobblytwilliams.github.io/grazer/reference/grz_behavior_threshold_guide.md)
- [`grz_tune_thresholds()`](https://wobblytwilliams.github.io/grazer/reference/grz_tune_thresholds.md)
- [`grz_validate_behavior()`](https://wobblytwilliams.github.io/grazer/reference/grz_validate_behavior.md)

## 7) Interactive QA and Manual Labelling

The two interactive tools are:

- [`grz_map()`](https://wobblytwilliams.github.io/grazer/reference/grz_map.md)
  for exploratory mapping.
- [`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md)
  for manual `ACTIVE`/`INACTIVE` labels.

Use these in an interactive R session:

``` r
# Interactive map
grz_map(
  data = gps_states,
  timeline = TRUE,
  group = "sensor_id",
  state_col = "activity_state_consensus"
)

# Interactive manual state labelling app
labelled <- grz_label_gps_states(
  data = gps_states,
  lon = "lon",
  lat = "lat",
  time = "datetime",
  color_by = "sensor_id",
  initial_label_col = "label"
)
```

## 8) Save Outputs

``` r
# Example export pattern
# data.table::fwrite(daily_metrics, "daily_metrics.csv")
# data.table::fwrite(gps_states, "gps_states.csv")
```

## Next Steps

- Replace synthetic data with your project CSV files.
- Add paddock polygons with
  [`grz_clean_spatial()`](https://wobblytwilliams.github.io/grazer/reference/grz_clean_spatial.md)
  and
  [`grz_append_paddock_names()`](https://wobblytwilliams.github.io/grazer/reference/grz_append_paddock_names.md).
- Use
  [`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md)
  labels to benchmark and tune consensus settings.
