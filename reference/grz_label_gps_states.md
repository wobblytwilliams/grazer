# Label GPS States in an Interactive Leaflet Timeline App

Launches an interactive Shiny + Leaflet app for manual labelling of GPS
points as ACTIVE or INACTIVE. Labels are stored in a column and returned
to the R session when the user clicks Finish and return.

## Usage

``` r
grz_label_gps_states(
  data,
  lon = "lon",
  lat = "lat",
  time = "time",
  id = NULL,
  color_by = NULL,
  initial_label_col = "label",
  tz = "UTC",
  display_tz = tz,
  start_day_offset = 0L,
  time_window = c("all", "day", "week"),
  n_animals = NULL,
  animal_col = NULL,
  max_points_display = NULL,
  downsample_display = FALSE,
  launch_browser = TRUE
)
```

## Arguments

- data:

  Input data frame or tibble.

- lon:

  Longitude column name.

- lat:

  Latitude column name.

- time:

  Datetime column name.

- id:

  Optional unique row id column. If NULL, an internal id is created.

- color_by:

  Optional column used for filtering groups in the sidebar.

- initial_label_col:

  Label column name to read/write.

- tz:

  Time zone used to parse time.

- display_tz:

  Time zone used for display in the app.

- start_day_offset:

  Integer number of days since the minimum date.

- time_window:

  One of "all", "day", or "week".

- n_animals:

  Optional integer to limit number of animals shown.

- animal_col:

  Optional animal identifier column used with n_animals.

- max_points_display:

  Optional maximum number of points displayed.

- downsample_display:

  Logical; if TRUE, deterministic thinning is used when
  max_points_display is set.

- launch_browser:

  Logical; passed to shiny::runApp().

## Value

A data frame with updated initial_label_col values (ACTIVE, INACTIVE, or
NA).

## Details

The app supports timeline scrubbing, click/shape selection, and
row-level state labelling.
