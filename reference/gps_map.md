# Map GPS fixes interactively

Creates a leaflet map of GPS fixes with optional grouping and optional
timeline playback via `leaflet.extras2`.

## Usage

``` r
gps_map(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  groups = NULL,
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
  state_legend_title = "State",
  timeline = FALSE,
  popup_fields = c("sensor_id", "datetime"),
  provider = "Esri.WorldImagery",
  point_radius = 3,
  point_opacity = 0.7,
  max_points = NULL,
  max_groups = NULL,
  seed = 1,
  point_warning_n = 5000L,
  group_warning_n = 20L,
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

- groups:

  Optional grouping columns for colour and layer separation.

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

  Tile provider name passed to
  [`leaflet::addProviderTiles()`](https://rstudio.github.io/leaflet/reference/addProviderTiles.html).

- point_radius:

  Marker radius.

- point_opacity:

  Marker opacity.

- max_points:

  Optional max number of points to render (random sample).

- max_groups:

  Optional maximum number of complete groups to plot.

- seed:

  Random seed used for sampling.

- point_warning_n:

  Point count that triggers a large-map warning.

- group_warning_n:

  Group count that triggers a large-map warning.

- warnings:

  Logical; if `FALSE`, bypasses map-size confirmations.

## Value

A `leaflet` htmlwidget.
