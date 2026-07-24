# Playback GPS tracks on a leaflet timeline with tails

Creates an interactive leaflet htmlwidget with timeline playback and
track tails inspired by moveVis-style animations. This function is
presentation-oriented and avoids Shiny.

## Usage

``` r
gps_playback(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  groups = NULL,
  color_by = c("group", "state"),
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
  state_legend_title = "State",
  polygons_sf = NULL,
  polygon_label_col = NULL,
  polygon_group = "Polygons",
  polygon_color = "#03F",
  polygon_weight = 5,
  polygon_opacity = 0.5,
  polygon_fill = TRUE,
  polygon_fill_opacity = 0.2,
  layer_control = TRUE,
  smooth_movement = TRUE,
  align = TRUE,
  align_interval_mins = "base",
  align_keep_extra = TRUE,
  tail_points = 19L,
  tail_minutes = NULL,
  show_points = TRUE,
  point_radius = 4,
  point_opacity = 0.9,
  point_size_slider = TRUE,
  point_size_min = 1,
  point_size_max = 25,
  slider_position = "bottomleft",
  playback_controls = TRUE,
  playback_speed_slider = TRUE,
  playback_steps = 1000L,
  playback_duration_ms = 10000L,
  show_ticks = FALSE,
  enable_keyboard_controls = TRUE,
  wait_to_update_map = FALSE,
  provider = "Esri.WorldImagery",
  popup_fields = c("sensor_id", "datetime"),
  show_legend = TRUE,
  group_palette = "Dark 3",
  max_groups = NULL,
  max_rows = NULL,
  render_every_n = 1L,
  seed = 1,
  warnings = TRUE,
  progress = TRUE,
  show_loading_overlay = TRUE
)
```

## Arguments

- data:

  Data frame containing GPS rows.

- lon:

  Name of longitude column.

- lat:

  Name of latitude column.

- datetime:

  Name of datetime column.

- groups:

  Optional grouping columns for track identity and colouring. If NULL,
  `sensor_id` is used when present.

- color_by:

  Color mode. `"group"` (default) colors by track; `"state"` colors by
  `state_col`.

- state_col:

  Optional state column used when `color_by = "state"`.

- state_colors:

  Named colors for state levels.

- state_legend_title:

  Legend title used when `color_by = "state"`.

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

  Logical; add on/off controls for individual tracks and polygon
  overlays. All track timelines share one playback control.

- smooth_movement:

  Logical; interpolate marker positions and progressively draw the
  current tail segment between observed fixes in the browser. This
  changes only playback rendering, not the input data.

- align:

  Logical; if `TRUE`, calls
  [`gps_interpolate()`](https://wobblytwilliams.github.io/grazer/reference/gps_interpolate.md)
  before playback.

- align_interval_mins:

  Interval passed to `gps_interpolate(interval_mins = )`.

- align_keep_extra:

  Logical; passed to `gps_interpolate(keep_extra = )`.

- tail_points:

  Tail length expressed as number of aligned points. Ignored when
  `tail_minutes` is supplied.

- tail_minutes:

  Tail length in minutes. If NULL, inferred from `tail_points`.

- show_points:

  Logical; draw moving point markers.

- point_radius:

  Point marker radius.

- point_opacity:

  Point marker opacity.

- point_size_slider:

  Logical; show an on-map slider to control point radius during
  playback.

- point_size_min:

  Minimum value for the point-size slider.

- point_size_max:

  Maximum value for the point-size slider.

- slider_position:

  Position of timeline controls.

- playback_controls:

  Logical; show play/pause/step controls.

- playback_speed_slider:

  Logical; show a 0.25x to 4x playback-speed slider beneath the
  point-size control. The slider is omitted when
  `playback_controls = FALSE`.

- playback_steps:

  Number of playback steps for the timeline.

- playback_duration_ms:

  Minimum playback duration in milliseconds.

- show_ticks:

  Logical; show timeline ticks.

- enable_keyboard_controls:

  Logical; allow keyboard control of playback.

- wait_to_update_map:

  Logical; if `TRUE`, redraw map only after slider interaction ends.

- provider:

  Tile provider for
  [`leaflet::addProviderTiles()`](https://rstudio.github.io/leaflet/reference/addProviderTiles.html).

- popup_fields:

  Fields to include in popups.

- show_legend:

  Logical; add legend.

- group_palette:

  HCL palette name used for group colours.

- max_groups:

  Optional maximum number of complete groups to animate.

- max_rows:

  Optional maximum rows to render after alignment. If set and exceeded,
  rows are sampled across groups.

- render_every_n:

  Keep every n-th aligned row per group (\>= 1) before building timeline
  features. Useful for faster rendering.

- seed:

  Random seed for sampling.

- warnings:

  Logical; print warnings for large payloads.

- progress:

  Logical; print stage progress and loop progress bars while building
  playback features.

- show_loading_overlay:

  Logical; show an on-map loading overlay until timeline layers are
  ready.

## Value

A `leaflet` htmlwidget.

## Details

Inspiration: the playback/tail design was informed by the moveVis
package (<https://github.com/16EAGLE/moveVis>).

## Examples

``` r
if (FALSE) { # \dontrun{
gps_playback(
  gps_data,
  groups = c("animal_id", "treatment"),
  polygons_sf = paddocks,
  polygon_label_col = "paddock_name",
  polygon_group = "Paddocks"
)
} # }
```
