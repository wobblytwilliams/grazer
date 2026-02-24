# GPS 101: Core Workflow

## Overview

This tutorial is a practical introduction to the `grazer` core pipeline:

1.  Start from GPS rows in canonical columns (`sensor_id`, `datetime`,
    `lon`, `lat`).
2.  Validate required schema and data types.
3.  Clean obvious errors and noise.
4.  Calculate movement, social, and spatial metrics.
5.  Summarise results to daily metrics for interpretation.

## Why This Workflow?

A structured workflow helps avoid common analysis problems:

- You detect data issues before model fitting.
- You apply cleaning rules consistently across experiments.
- You generate comparable metrics across paddocks, cohorts, and
  deployments.
- You keep row-level and day-level outputs aligned for QA and reporting.

## 1) Load the package

``` r
library(grazer)
```

## 2) Create an example GPS table

This vignette builds synthetic data so it is fully reproducible.  
In real projects you would typically call
`grz_read_gps("your_file.csv")`.

``` r
set.seed(101)

timestamps <- seq(
  from = as.POSIXct("2024-05-01 00:00:00", tz = "UTC"),
  by = "10 min",
  length.out = 3 * 24 * 6
)

animal_ids <- c("A01", "A02", "A03")
gps_raw <- data.frame()

for (i in seq_along(animal_ids)) {
  one_animal <- data.frame(
    sensor_id = animal_ids[i],
    datetime = timestamps,
    lon = 132.30 + (i * 0.004) + cumsum(rnorm(length(timestamps), 0, 0.00018)),
    lat = -14.47 - (i * 0.002) + cumsum(rnorm(length(timestamps), 0, 0.00015)),
    stringsAsFactors = FALSE
  )
  gps_raw <- rbind(gps_raw, one_animal)
}

# Add a few realistic issues so validation and cleaning have visible effects.
gps_raw <- rbind(gps_raw, gps_raw[1:5, ])   # duplicate rows
gps_raw$lon[20] <- 0                         # invalid location (0,0)
gps_raw$lat[20] <- 0
gps_raw$datetime[33] <- NA                   # invalid datetime

nrow(gps_raw)
#> [1] 1301
head(gps_raw)
#>   sensor_id            datetime      lon       lat
#> 1       A01 2024-05-01 00:00:00 132.3039 -14.47195
#> 2       A01 2024-05-01 00:10:00 132.3040 -14.47191
#> 3       A01 2024-05-01 00:20:00 132.3039 -14.47179
#> 4       A01 2024-05-01 00:30:00 132.3040 -14.47194
#> 5       A01 2024-05-01 00:40:00 132.3040 -14.47172
#> 6       A01 2024-05-01 00:50:00 132.3042 -14.47183
```

## 3) Validate required columns and row values

``` r
gps_valid <- grz_validate(
  data = gps_raw,
  drop_invalid = FALSE,
  verbose = TRUE
)
#> [validate] rows=1,301 invalid=1 drop_invalid=FALSE

validation_qc <- attr(gps_valid, "validation_qc")
invalid_rows <- attr(gps_valid, "invalid_rows")

validation_qc
#>                 metric count   proportion
#>                 <char> <int>        <num>
#> 1:              n_rows  1301 1.0000000000
#> 2:      n_invalid_rows     1 0.0007686395
#> 3: n_missing_sensor_id     0 0.0000000000
#> 4:  n_invalid_datetime     1 0.0007686395
#> 5:       n_invalid_lon     0 0.0000000000
#> 6:       n_invalid_lat     0 0.0000000000
head(invalid_rows)
#>    sensor_id datetime      lon       lat   invalid_reason
#>       <char>   <POSc>    <num>     <num>           <char>
#> 1:       A01     <NA> 132.3039 -14.47294 invalid_datetime
```

## 4) Clean the GPS rows

This run applies four common cleaning steps:

- `duplicates`: removes repeated fixes.
- `errors`: drops invalid coordinates and malformed rows.
- `speed_fixed`: removes biologically implausible moves.
- `denoise`: reduces local GPS bounce in static periods.

``` r
gps_clean <- grz_clean(
  data = gps_valid,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  verbose = TRUE
)
#> [clean] start_rows=1,301
#> [clean_duplicates] dropped: 5 | rows: 1,301 -> 1,296
#> [clean_errors] dropped_sensor=0 dropped_datetime=1 dropped_coord=0 dropped_zero_zero=1
#> [clean_errors] dropped: 2 | rows: 1,296 -> 1,294
#> [clean_speed_fixed] threshold=4.000 m/s
#> [clean_speed_fixed] dropped: 0 | rows: 1,294 -> 1,294
#> [denoise] radius_m=8.0 max_gap_mins=20.0
#> [denoise] dropped: 108 | rows: 1,294 -> 1,186
#> [clean] final_rows=1,186

nrow(gps_clean)
#> [1] 1186
head(gps_clean)
#>   sensor_id            datetime      lon       lat
#> 1       A01 2024-05-01 00:00:00 132.3039 -14.47195
#> 2       A01 2024-05-01 00:10:00 132.3040 -14.47191
#> 3       A01 2024-05-01 00:20:00 132.3039 -14.47179
#> 4       A01 2024-05-01 00:30:00 132.3040 -14.47194
#> 5       A01 2024-05-01 00:40:00 132.3040 -14.47172
#> 6       A01 2024-05-01 00:50:00 132.3042 -14.47183
```

## 5) Calculate row-level movement and social metrics

``` r
gps_movement <- grz_calculate_movement(gps_clean, verbose = FALSE)
gps_metrics <- grz_calculate_social(gps_movement, interpolate = FALSE, verbose = FALSE)

head(gps_metrics[, c(
  "sensor_id", "datetime", "step_m", "speed_mps", "turn_rad",
  "nearest_neighbor_m", "mean_dist_to_others_m"
)])
#>   sensor_id            datetime   step_m  speed_mps turn_rad nearest_neighbor_m
#> 1       A01 2024-05-01 00:00:00       NA         NA       NA           507.0182
#> 2       A01 2024-05-01 00:10:00 11.52811 0.01921351       NA           502.9769
#> 3       A01 2024-05-01 00:20:00 18.13259 0.03022098 1.996812           502.3627
#> 4       A01 2024-05-01 00:30:00 17.06946 0.02844910 2.581649           503.8102
#> 5       A01 2024-05-01 00:40:00 25.51189 0.04251981 2.657424           513.6946
#> 6       A01 2024-05-01 00:50:00 25.77840 0.04296399 1.821947           540.2254
#>   mean_dist_to_others_m
#> 1              740.5702
#> 2              502.9769
#> 3              726.3539
#> 4              736.5660
#> 5              754.7735
#> 6              762.4748
```

## 6) Summarise to day-level movement, social, and spatial outputs

``` r
daily_metrics <- grz_calculate_epoch_metrics(
  data = gps_metrics,
  epoch = "day",
  include = c("movement", "social", "spatial"),
  verbose = FALSE
)

daily_metrics
#>   sensor_id      epoch n_fixes total_distance_m mean_step_m median_step_m
#> 1       A01 2024-05-01     134         3132.873    23.55544      20.81554
#> 2       A01 2024-05-02     122         2650.702    21.72707      19.68335
#> 3       A01 2024-05-03     134         3243.565    24.20571      23.53400
#> 4       A02 2024-05-01     131         3455.125    26.57789      26.15696
#> 5       A02 2024-05-02     138         3369.007    24.41309      22.22398
#> 6       A02 2024-05-03     131         3459.795    26.41065      24.56899
#> 7       A03 2024-05-01     135         3370.273    25.15129      22.91217
#> 8       A03 2024-05-02     132         2989.238    22.64574      21.35027
#> 9       A03 2024-05-03     129         3059.136    23.71423      21.51194
#>   p95_step_m mean_speed_mps p95_speed_mps max_speed_mps mean_abs_turn_rad
#> 1   42.30171     0.03740605    0.06877027    0.07968321          1.528619
#> 2   42.95296     0.03330009    0.07030039    0.09528840          1.592599
#> 3   42.28486     0.03894213    0.07047477    0.10514065          1.603855
#> 4   47.72869     0.04214338    0.07485512    0.11071979          1.678684
#> 5   43.08025     0.03988782    0.07119755    0.09133846          1.545299
#> 6   49.24512     0.04236014    0.08207520    0.10820711          1.538815
#> 7   47.28117     0.04002761    0.07659921    0.10995727          1.568127
#> 8   41.62788     0.03608192    0.06734519    0.08752319          1.542835
#> 9   44.33423     0.03689340    0.07215069    0.11369144          1.672228
#>   social_n_fixes mean_nn_m p50_nn_m mean_dist_to_others_m
#> 1            134  769.2329 815.6995              863.8375
#> 2            122  902.9607 900.5049              957.4817
#> 3            134  870.5431 860.7784             1095.2608
#> 4            131  366.0400 296.3314              559.0056
#> 5            138  323.1694 287.9136              604.4207
#> 6            131  635.6847 680.8530              761.0322
#> 7            135  399.2312 298.6138              654.6165
#> 8            132  302.3545 285.7099              563.0773
#> 9            129  735.4497 740.3027             1035.5359
#>   mean_n_within_           25m mean_n_within_           30m
#> 1                            0                            0
#> 2                            0                            0
#> 3                            0                            0
#> 4                            0                            0
#> 5                            0                            0
#> 6                            0                            0
#> 7                            0                            0
#> 8                            0                            0
#> 9                            0                            0
#>   mean_n_within_           50m mean_n_within_          100m
#> 1                            0                            0
#> 2                            0                            0
#> 3                            0                            0
#> 4                            0                            0
#> 5                            0                            0
#> 6                            0                            0
#> 7                            0                            0
#> 8                            0                            0
#> 9                            0                            0
#>   prop_any_within_           25m prop_any_within_           30m
#> 1                              0                              0
#> 2                              0                              0
#> 3                              0                              0
#> 4                              0                              0
#> 5                              0                              0
#> 6                              0                              0
#> 7                              0                              0
#> 8                              0                              0
#> 9                              0                              0
#>   prop_any_within_           50m prop_any_within_          100m spatial_n_fixes
#> 1                              0                              0             134
#> 2                              0                              0             122
#> 3                              0                              0             134
#> 4                              0                              0             131
#> 5                              0                              0             138
#> 6                              0                              0             131
#> 7                              0                              0             135
#> 8                              0                              0             132
#> 9                              0                              0             129
#>   span_hours mcp100_area_ha mcp95_area_ha  kde95_ha
#> 1   23.83333       4.808943      4.321158  8.048117
#> 2   23.83333       5.521516      5.315512  7.818676
#> 3   23.83333       5.171636      4.911483  8.014663
#> 4   23.83333       6.443654      6.013205 10.647315
#> 5   23.83333       8.997431      8.077120 17.619046
#> 6   23.83333       4.651171      4.355067  7.709534
#> 7   23.83333       6.926231      6.625744 11.165264
#> 8   23.66667       6.765488      6.352339 12.957912
#> 9   23.83333       8.796936      8.537822 14.950112
```

## 7) Plot key features for quick QA

Use quick plots to check whether values are biologically reasonable.

``` r
par(mfrow = c(1, 2))

hist(
  gps_metrics$step_m,
  breaks = 40,
  main = "Step Distance (m)",
  xlab = "step_m",
  col = "grey80",
  border = "white"
)

hist(
  gps_metrics$speed_mps,
  breaks = 40,
  main = "Speed (m/s)",
  xlab = "speed_mps",
  col = "grey80",
  border = "white"
)
```

![](gps-101-core-workflow_files/figure-html/unnamed-chunk-8-1.png)

``` r

par(mfrow = c(1, 1))
```

## Next Step

Continue to the 201 tutorial for:

- active/inactive classification,
- manual state labelling with
  [`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md),
- and prediction-vs-label validation.
