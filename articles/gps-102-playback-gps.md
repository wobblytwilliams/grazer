# GPS 102: Interactive Playback with grz_playback_gps()

## Overview

This tutorial shows how to build an interactive playback map for visual
QA using
[`grz_playback_gps()`](https://wobblytwilliams.github.io/grazer/reference/grz_playback_gps.md).
The playback map is useful immediately after data validation, before
downstream summaries and modelling.

In this tutorial you will:

1.  create a small reproducible GPS example,
2.  validate and lightly clean it,
3.  render a leaflet playback map with timeline + tails,
4.  tune performance options for larger datasets.

## 1) Load packages

``` r
library(grazer)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tibble)
```

## 2) Build example GPS data

``` r
set.seed(102)

timestamps <- seq(
  from = as.POSIXct("2024-07-01 00:00:00", tz = "UTC"),
  by = "10 min",
  length.out = 2 * 24 * 6
)

animals <- tibble(
  sensor_id = c("A01", "A02", "A03"),
  lon0 = c(132.305, 132.312, 132.298),
  lat0 = c(-14.472, -14.468, -14.476)
)

gps_parts <- list()
for (i in seq_len(nrow(animals))) {
  gps_parts[[i]] <- tibble(
    sensor_id = animals$sensor_id[i],
    datetime = timestamps,
    lon = animals$lon0[i] + cumsum(rnorm(length(timestamps), 0, 0.0002)),
    lat = animals$lat0[i] + cumsum(rnorm(length(timestamps), 0, 0.00016))
  )
}

gps_raw <- bind_rows(gps_parts)
gps_raw <- bind_rows(gps_raw, gps_raw %>% slice(1:3)) # add a few duplicates
```

## 3) Validate and lightly clean

``` r
gps_valid <- grz_validate(gps_raw, drop_invalid = TRUE, verbose = FALSE)

gps_clean <- grz_clean(
  data = gps_valid,
  steps = c("duplicates", "errors", "speed_fixed"),
  max_speed_mps = 4,
  verbose = FALSE
)
```

## 4) Render playback map (embedded html widget)

The widget below is rendered directly in the tutorial, so it also
appears in the GitHub Pages site after pkgdown build/deploy.

``` r
if (playback_pkgs_ok) {
  playback_map <- grz_playback_gps(
    data = gps_clean,
    group = "sensor_id",
    align = TRUE,
    align_interval_mins = 10,
    tail_points = 30,
    show_points = TRUE,
    point_size_slider = TRUE,
    point_size_min = 1,
    point_size_max = 25,
    playback_steps = 900,
    playback_duration_ms = 18000,
    popup_fields = c("sensor_id", "datetime"),
    provider = "Esri.WorldImagery",
    warnings = FALSE,
    progress = FALSE,
    show_loading_overlay = TRUE
  )

  playback_map
} else {
  cat(
    "Playback widget skipped because required packages are not available: ",
    paste(playback_pkgs, collapse = ", "),
    ".\n",
    sep = ""
  )
  NULL
}
#> Playback widget skipped because required packages are not available: leaflet, leaftime, htmlwidgets.
#> NULL
```

## 5) Practical options for larger data

Use these options when rendering speed becomes a bottleneck:

- `max_rows`: caps rows passed to playback.
- `render_every_n`: keeps every n-th row per group.
- `align_interval_mins`: coarsen temporal resolution.
- `tail_points`: shorten visible trail depth.

``` r
m_fast <- grz_playback_gps(
  data = gps_clean,
  group = "sensor_id",
  max_rows = 15000,
  render_every_n = 2,
  align_interval_mins = 15,
  tail_points = 20
)
```

## Next Step

Continue to GPS 103 for activity classification and label-based
validation:

- GMM + HMM state classification
- manual labelling with
  [`grz_label_gps_states()`](https://wobblytwilliams.github.io/grazer/reference/grz_label_gps_states.md)
- prediction-vs-label comparison and parameter tuning
