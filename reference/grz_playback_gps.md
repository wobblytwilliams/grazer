# Playback GPS tracks on a leaflet timeline with tails

Creates an interactive leaflet htmlwidget with timeline playback and
track tails inspired by moveVis-style animations. This function is
presentation-oriented and avoids Shiny.

Inspiration: the playback/tail design was informed by the moveVis
package (<https://github.com/16EAGLE/moveVis>).

## Usage

``` r
grz_playback_gps(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  group = NULL,
  color_by = c("group", "state"),
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
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
  playback_steps = 1000L,
  playback_duration_ms = 10000L,
  show_ticks = FALSE,
  enable_keyboard_controls = TRUE,
  wait_to_update_map = FALSE,
  provider = "Esri.WorldImagery",
  popup_fields = c("sensor_id", "datetime"),
  show_legend = TRUE,
  group_palette = "Dark 3",
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

- group:

  Optional grouping column(s) for track identity and coloring. If NULL,
  `sensor_id` is used when present.

- color_by:

  Color mode. `"group"` (default) colors by track; `"state"` colors by
  `state_col`.

- state_col:

  Optional state column used when `color_by = "state"`.

- state_colors:

  Named colors for state levels.

- align:

  Logical; if `TRUE`, calls
  [`grz_align()`](https://wobblytwilliams.github.io/grazer/reference/grz_align.md)
  before playback.

- align_interval_mins:

  Interval passed to `grz_align(interval_mins = )`.

- align_keep_extra:

  Logical; passed to `grz_align(keep_extra = )`.

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

  Tile provider for `leaflet::addProviderTiles()`.

- popup_fields:

  Fields to include in popups.

- show_legend:

  Logical; add legend.

- group_palette:

  HCL palette name used for group colors.

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
