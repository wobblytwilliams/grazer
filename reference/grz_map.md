# Plot Cattle GPS Fixes on an Interactive Leaflet Map

Creates a leaflet map of GPS fixes with optional grouping by a blocking
variable and optional timeline playback via `leaflet.extras2`.

## Usage

``` r
grz_map(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  block = NULL,
  group = NULL,
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
  state_legend_title = "State",
  timeline = FALSE,
  popup_fields = c("sensor_id", "datetime"),
  provider = "Esri.WorldImagery",
  point_radius = 3,
  point_opacity = 0.7,
  max_points = NULL,
  max_blocks = NULL,
  sample_n = NULL,
  max_block = NULL,
  seed = 1,
  large_n = 5000L,
  block_n = 20L,
  warnings = TRUE
)
```

## Arguments

- data:

  Data frame containing GPS points.

- lon:

  Name of longitude column.

- lat:

  Name of latitude column.

- datetime:

  Name of datetime column.

- block:

  Deprecated alias for `group`.

- group:

  Optional grouping column(s) for color/layer separation.

- state_col:

  Optional state column for fixed state coloring (for example
  `activity_state_gmm`).

- state_colors:

  Named colors for state levels. Defaults to red (`inactive`) and green
  (`active`).

- state_legend_title:

  Legend title used when `state_col` is provided.

- timeline:

  Logical; if `TRUE`, render points with an interactive time slider.

- popup_fields:

  Character vector of fields to show in marker popups.

- provider:

  Tile provider name passed to `leaflet::addProviderTiles()`.

- point_radius:

  Marker radius.

- point_opacity:

  Marker opacity.

- max_points:

  Optional max number of points to render (random sample).

- max_blocks:

  Optional maximum number of block groups to plot.

- sample_n:

  Deprecated alias for `max_points`.

- max_block:

  Deprecated alias for `max_blocks`.

- seed:

  Random seed used for sampling.

- large_n:

  Threshold for large map warning/confirmation.

- block_n:

  Threshold for number of block groups warning/confirmation.

- warnings:

  Logical; if `FALSE`, bypasses map-size warning prompts.

## Value

A `leaflet` htmlwidget.
