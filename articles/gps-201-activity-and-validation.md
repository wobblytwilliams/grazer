# GPS 201: Activity Classification and Validation

## Overview

This tutorial covers the next stage after cleaning and metric
generation:

1.  classify each GPS row as `active` or `inactive`,
2.  manually label rows in an interactive app,
3.  compare model predictions against labels,
4.  rank parameter settings by agreement.

## Why This Workflow?

State classification is iterative. A clear validation loop gives better
decisions:

- You can inspect how model assumptions map to real movement behaviour.
- You can use manual labels as a transparent ground-truth reference.
- You can tune parameters with a reproducible and auditable process.
- You can report uncertainty and model agreement directly.

## 1) Build a small reproducible example

``` r
library(grazer)

set.seed(202)

timestamps <- seq(
  from = as.POSIXct("2024-06-01 00:00:00", tz = "UTC"),
  by = "10 min",
  length.out = 2 * 24 * 6
)

animal_ids <- c("A01", "A02")
gps <- data.frame()

for (i in seq_along(animal_ids)) {
  one_animal <- data.frame(
    sensor_id = animal_ids[i],
    datetime = timestamps,
    lon = 132.31 + (i * 0.003) + cumsum(rnorm(length(timestamps), 0, 0.00020)),
    lat = -14.46 - (i * 0.002) + cumsum(rnorm(length(timestamps), 0, 0.00016)),
    stringsAsFactors = FALSE
  )
  gps <- rbind(gps, one_animal)
}

gps <- grz_clean(
  data = gps,
  steps = c("errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  verbose = FALSE
)
```

## 2) Run consensus active/inactive classification

[`grz_classify_activity_consensus()`](https://wobblytwilliams.github.io/grazer/reference/grz_classify_activity_consensus.md)
combines HMM and spatial signals into one final state column.

``` r
gps_states <- grz_classify_activity_consensus(
  data = gps,
  groups = "sensor_id",
  verbose = FALSE
)

table(gps_states$activity_state_consensus, useNA = "ifany")
#> 
#>   active inactive 
#>      363      167
```

## 3) Add manual labels (interactive app)

Use this in an interactive R session to create or edit a `label` column.

``` r
gps_states$point_id <- paste(
  gps_states$sensor_id,
  format(gps_states$datetime, "%Y%m%d%H%M%S", tz = "UTC"),
  seq_len(nrow(gps_states)),
  sep = "_"
)

labelled <- grz_label_gps_states(
  data = gps_states,
  lon = "lon",
  lat = "lat",
  time = "datetime",
  id = "point_id",
  color_by = "sensor_id",
  initial_label_col = "label",
  start_day_offset = 0L,
  time_window = "week",
  n_animals = 1L,
  animal_col = "sensor_id"
)
```

If you save that output to CSV, you can reload it later and continue
editing:

``` r
# data.table::fwrite(labelled, "labelled_states.csv")
# labelled <- data.table::fread("labelled_states.csv")
# labelled$datetime <- as.POSIXct(labelled$datetime, tz = "UTC")
```

## 4) Demonstrate a prediction-vs-label comparison

For a fully reproducible vignette, we create a synthetic `label` column
by copying predictions and flipping a random subset of rows.  
In your project, replace this with manual labels from
[`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md).

``` r
labelled <- gps_states
labelled$label <- labelled$activity_state_consensus

set.seed(303)
flip_n <- floor(0.1 * nrow(labelled))
flip_idx <- sample(seq_len(nrow(labelled)), size = flip_n)

labelled$label[flip_idx] <- ifelse(
  labelled$label[flip_idx] == "active",
  "inactive",
  "active"
)

table(labelled$label, useNA = "ifany")
#> 
#>   active inactive 
#>      342      188
```

## 5) Tune parameters and rank the top 3 settings

``` r
grid <- expand.grid(
  inactive_threshold = c(0.50, 0.55, 0.60),
  spatial_radius_m = c(15, 20),
  spatial_min_dwell_mins = c(10, 20),
  stringsAsFactors = FALSE
)

results <- data.frame(
  inactive_threshold = numeric(0),
  spatial_radius_m = numeric(0),
  spatial_min_dwell_mins = numeric(0),
  n_compared = integer(0),
  accuracy = numeric(0)
)

for (i in seq_len(nrow(grid))) {
  pred <- grz_classify_activity_consensus(
    data = labelled,
    groups = "sensor_id",
    decision_rule = "weighted",
    inactive_threshold = grid$inactive_threshold[i],
    spatial_radius_m = grid$spatial_radius_m[i],
    spatial_min_dwell_mins = grid$spatial_min_dwell_mins[i],
    spatial_min_points = 3L,
    min_run_n = 2L,
    verbose = FALSE
  )

  truth <- tolower(trimws(as.character(labelled$label)))
  pred_state <- tolower(trimws(as.character(pred$activity_state_consensus)))
  keep <- truth %in% c("active", "inactive") & pred_state %in% c("active", "inactive")

  n_compared <- sum(keep)
  accuracy <- if (n_compared == 0L) NA_real_ else mean(truth[keep] == pred_state[keep])

  results <- rbind(
    results,
    data.frame(
      inactive_threshold = grid$inactive_threshold[i],
      spatial_radius_m = grid$spatial_radius_m[i],
      spatial_min_dwell_mins = grid$spatial_min_dwell_mins[i],
      n_compared = n_compared,
      accuracy = accuracy
    )
  )
}

top3 <- results[order(-results$accuracy, -results$n_compared), ][1:3, ]
top3$accuracy_percent <- round(100 * top3$accuracy, 2)
top3
#>    inactive_threshold spatial_radius_m spatial_min_dwell_mins n_compared
#> 6                 0.6               20                     10        530
#> 12                0.6               20                     20        530
#> 3                 0.6               15                     10        530
#>     accuracy accuracy_percent
#> 6  0.9000000            90.00
#> 12 0.9000000            90.00
#> 3  0.7943396            79.43
```

## 6) Optional advanced diagnostics

These helpers are useful once you have enough labelled data:

``` r
# Diurnal feature patterns for threshold setting
grz_plot_diurnal_metrics(
  data = labelled,
  metrics = c("step_m", "turn_rad"),
  group_col = "sensor_id"
)

# Behaviour diagnostics (feature summary, transitions, bouts, PCA)
diagnostics <- grz_validate_behavior(
  data = labelled,
  state_col = "activity_state_consensus",
  groups = "sensor_id",
  pca = TRUE
)
```

## Recommended Practice

1.  Label a representative subset (for example, one animal for one
    week).
2.  Tune parameters on that subset.
3.  Validate performance metrics.
4.  Freeze parameters and run full-cohort processing.
5.  Re-check performance when deployment conditions change.
