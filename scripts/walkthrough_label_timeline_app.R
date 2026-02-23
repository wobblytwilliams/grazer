#!/usr/bin/env Rscript

# Walkthrough: build a small labelling dataset and launch the package
# Leaflet timeline labelling app.
#
# Usage (from project root):
#   source("scripts/walkthrough_label_timeline_app.R")

# -------------------------------------------------------------------
# 1) Load package code
# -------------------------------------------------------------------
pkgload::load_all(".")

# -------------------------------------------------------------------
# 2) Configure input and output
#    Keep this small for manual labelling sessions.
# -------------------------------------------------------------------
input_csv <- file.path("test_data", "ManbullooToSimulate.csv")
output_dir <- file.path("experimental", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------------------------
# 3) Read + lightly clean GPS data
#    This keeps columns and ordering consistent before labelling.
# -------------------------------------------------------------------
raw <- grz_read_gps(input_csv, n_max = 50000, validate = FALSE)
clean <- grz_clean(
  raw,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  verbose = FALSE
)

# -------------------------------------------------------------------
# 4) Add a stable point key for labelling
#    Keep key creation outside the app so labels can be merged back
#    into downstream analyses without ambiguity.
# -------------------------------------------------------------------
clean <- clean[order(clean$datetime), , drop = FALSE]
clean$point_id <- paste(
  clean$sensor_id,
  format(clean$datetime, "%Y%m%d%H%M%S", tz = "UTC"),
  seq_len(nrow(clean)),
  sep = "_"
)

# -------------------------------------------------------------------
# 5) Launch timeline labelling app
#    Click/drag-select points and assign ACTIVE / INACTIVE labels.
#    If you draw polygons/rectangles, click "Select drawn area(s)"
#    before applying labels.
#    Timeline subsetting is now handled in the function call.
#    Use sidebar controls to adjust:
#    - Show history / window minutes for visible points
#    - Point size scrubber for marker radius
#    Use \"Finish and return\" to close app and continue script.
# -------------------------------------------------------------------
labelled <- grz_label_gps_states(
  data = clean,
  lon = "lon",
  lat = "lat",
  time = "datetime",
  id = "point_id",
  color_by = "sensor_id",
  initial_label_col = "label",
  tz = "UTC",
  start_day_offset = 15L,
  time_window = "week",
  n_animals = 1L,
  animal_col = "sensor_id",
  max_points_display = 12000,
  downsample_display = FALSE
)

# -------------------------------------------------------------------
# 6) Save labelled data for model training / QA
# -------------------------------------------------------------------
labelled <- labelled[order(labelled$datetime), , drop = FALSE]
start_time <- min(labelled$datetime, na.rm = TRUE)
end_time <- max(labelled$datetime, na.rm = TRUE)
target_sensor <- unique(labelled$sensor_id)
target_sensor <- target_sensor[!is.na(target_sensor) & trimws(target_sensor) != ""]
target_sensor <- if (length(target_sensor) > 0L) target_sensor[[1]] else "unknown_sensor"

out_file <- file.path(
  output_dir,
  paste0("labels_", target_sensor, "_", format(start_time, "%Y%m%d"), "_to_", format(end_time, "%Y%m%d"), ".csv")
)
data.table::fwrite(labelled, out_file)

# Minimal run summary.
print(table(labelled$label, useNA = "ifany"))
print(out_file)
