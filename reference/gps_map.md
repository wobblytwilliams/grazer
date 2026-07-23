# Map GPS fixes interactively

Creates a leaflet map of GPS fixes with optional switchable group
layers, polygon overlays, and timeline playback.

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
  polygons_sf = NULL,
  polygon_label_col = NULL,
  polygon_group = "Polygons",
  polygon_color = "#03F",
  polygon_weight = 5,
  polygon_opacity = 0.5,
  polygon_fill = TRUE,
  polygon_fill_opacity = 0.2,
  layer_control = TRUE,
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

  Optional grouping columns for colour and layer separation. Multiple
  columns are combined into labels separated by `" | "`.

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

- polygons_sf:

  Optional `sf` object containing polygon or multipolygon geometries.
  Any declared CRS is accepted and transformed to EPSG:4326.

- polygon_label_col:

  Optional column in `polygons_sf` used for labels.

- polygon_group:

  Layer-control name for all polygon features.

- polygon_color:

  Polygon border colour.

- polygon_weight:

  Polygon border weight.

- polygon_opacity:

  Polygon border opacity.

- polygon_fill:

  Logical; fill polygons.

- polygon_fill_opacity:

  Polygon fill opacity.

- layer_control:

  Logical; add on/off controls for grouped animals and polygon overlays.

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

## Examples

``` r
if (FALSE) { # \dontrun{
gps_map(
  gps_data,
  groups = c("animal_id", "treatment"),
  polygons_sf = paddocks,
  polygon_label_col = "paddock_name",
  polygon_group = "Paddocks"
)
} # }
```
