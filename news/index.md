# Changelog

## grazer 0.1.0

- [`gps_interpolate()`](https://wobblytwilliams.github.io/grazer/reference/gps_interpolate.md)
  now evaluates raw tracks at exact common grid times using
  time-weighted linear interpolation between valid bracketing
  observations. It no longer snaps observations or accepts
  `tolerance_mins`.
- [`gps_interpolate()`](https://wobblytwilliams.github.io/grazer/reference/gps_interpolate.md)
  always respects `segment_id`, does not extrapolate, and reports
  interpolation-specific diagnostics in the `gps_reg` attribute.
- [`gps_steps()`](https://wobblytwilliams.github.io/grazer/reference/gps_steps.md)
  now prevents step calculations across `segment_id` boundaries while
  carrying cumulative distance forward within each track without adding
  distance for a data gap.
- [`gps_map()`](https://wobblytwilliams.github.io/grazer/reference/gps_map.md)
  now creates switchable animal layers from one or more `groups` columns
  for static and timeline maps.
- [`gps_map()`](https://wobblytwilliams.github.io/grazer/reference/gps_map.md)
  now accepts projected or geographic polygon `sf` layers through
  `polygons_sf`, transforms them to WGS84, and includes them in map
  bounds and layer controls.
- Timeline maps now position the layers control below the scrubber’s
  datetime label so the controls do not overlap.
- Layer controls now include a `Deselect all` button for hiding every
  animal and polygon overlay while retaining the base map.
- [`gps_map()`](https://wobblytwilliams.github.io/grazer/reference/gps_map.md)
  legends now appear on the left so they do not overlap an expanded
  layers control on the right.
