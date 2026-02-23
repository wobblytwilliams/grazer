#!/usr/bin/env Rscript

# -----------------------------------------------------------------------------
# Grazer Walkthrough: Label GPS data, then tune active/inactive consensus
# -----------------------------------------------------------------------------
#
# This script is written as a boilerplate tutorial format that can be adapted
# for documentation/blog use.
#
# What this walkthrough does:
# 1) Load GPS data (assumes input data is already correct).
# 2) Open the interactive timeline labelling app.
# 3) Save labelled data to CSV.
# 4) Read labelled data back in (typical next-session workflow).
# 5) Run a simple consensus tuning grid.
# 6) Print the 3 most accurate parameter settings.
#
# Run from the project root:
#   source("scripts/walkthrough_label_consensus_compare.R")

# -----------------------------------------------------------------------------
# 1) Load package code and required libraries
# -----------------------------------------------------------------------------
pkgload::load_all(".")

library(dplyr)
library(readr)
library(tidyr)
library(tibble)

# -----------------------------------------------------------------------------
# 2) Set input/output paths
# -----------------------------------------------------------------------------
# Raw GPS input for this walkthrough.
input_csv <- file.path("test_data", "ManbullooToSimulate.csv")

# Where labelled outputs and tuning outputs will be written.
output_dir <- file.path("experimental", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Label file path used for save + reload demonstration.
labels_csv <- file.path(output_dir, "labelled_manbuloo_for_walkthrough.csv")

# -----------------------------------------------------------------------------
# 3) Read GPS data and create a stable point ID
# -----------------------------------------------------------------------------
# We assume incoming data is already valid/correct for this tutorial.
gps <- grz_read_gps(input_csv, validate = FALSE)

# A stable point ID is useful so labels can be joined/reused reliably later.
gps <- gps %>%
  arrange(datetime) %>%
  mutate(
    point_id = paste(
      sensor_id,
      format(datetime, "%Y%m%d%H%M%S", tz = "UTC"),
      row_number(),
      sep = "_"
    )
  )

# -----------------------------------------------------------------------------
# 4) Open the interactive timeline labelling app
# -----------------------------------------------------------------------------
# In the app:
# - scrub through time
# - click points (or select multiple with rectangle/polygon tools)
# - mark ACTIVE / INACTIVE
# - click "Finish and return" to continue this script
labelled_now <- grz_label_gps_states(
  data = gps,
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

# -----------------------------------------------------------------------------
# 5) Save labels to CSV
# -----------------------------------------------------------------------------
# This is the key step for repeatability: label once, reuse many times.
readr::write_csv(labelled_now, labels_csv)
cat("\nSaved labelled data to:\n")
print(labels_csv)

# -----------------------------------------------------------------------------
# 6) Reload labelled data (typical next-session workflow)
# -----------------------------------------------------------------------------
# In practice, many users stop after labelling and continue another day.
# This step shows how to restart from saved labels.
labelled <- readr::read_csv(labels_csv, show_col_types = FALSE) %>%
  mutate(
    datetime = as.POSIXct(datetime, tz = "UTC"),
    label = toupper(trimws(as.character(label)))
  )

# Confirm labels are present.
if (!"label" %in% names(labelled)) {
  stop("Expected a `label` column after reloading labelled data.", call. = FALSE)
}
print(table(labelled$label, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 7) Choose grouping column for consensus
# -----------------------------------------------------------------------------
# Most grazing datasets use sensor_id as the tracking unit.
if ("sensor_id" %in% names(labelled)) {
  group_col <- "sensor_id"
} else if ("animal_id" %in% names(labelled)) {
  group_col <- "animal_id"
} else {
  stop("Need a grouping column: `sensor_id` or `animal_id`.", call. = FALSE)
}

# -----------------------------------------------------------------------------
# 8) Define a simple tuning grid
# -----------------------------------------------------------------------------
# Keep this intentionally small and interpretable for first-pass tuning.
grid <- tidyr::expand_grid(
  inactive_threshold = c(0.50, 0.55, 0.60, 0.65, 0.70),
  spatial_radius_m = c(15, 20, 25),
  spatial_min_dwell_mins = c(10, 20)
)

results <- tibble(
  inactive_threshold = numeric(),
  spatial_radius_m = numeric(),
  spatial_min_dwell_mins = numeric(),
  n_compared = integer(),
  accuracy = numeric()
)

# -----------------------------------------------------------------------------
# 9) Run consensus for each setting and compute accuracy
# -----------------------------------------------------------------------------
for (i in seq_len(nrow(grid))) {
  setting <- grid[i, ]

  predicted <- grz_classify_activity_consensus(
    data = labelled,
    groups = group_col,
    decision_rule = "weighted",
    inactive_threshold = setting$inactive_threshold[[1]],
    spatial_radius_m = setting$spatial_radius_m[[1]],
    spatial_min_dwell_mins = setting$spatial_min_dwell_mins[[1]],
    spatial_min_points = 3L,
    min_run_n = 2L,
    verbose = FALSE
  )

  # Compare only rows that have both truth and prediction as active/inactive.
  comparison <- predicted %>%
    as_tibble() %>%
    mutate(
      truth = tolower(trimws(as.character(label))),
      pred = tolower(trimws(as.character(activity_state_consensus)))
    ) %>%
    mutate(
      truth = ifelse(truth %in% c("active", "inactive"), truth, NA_character_),
      pred = ifelse(pred %in% c("active", "inactive"), pred, NA_character_)
    ) %>%
    filter(!is.na(truth), !is.na(pred))

  n_compared <- nrow(comparison)
  accuracy <- if (n_compared == 0) NA_real_ else mean(comparison$truth == comparison$pred)

  results <- dplyr::bind_rows(
    results,
    tibble(
      inactive_threshold = setting$inactive_threshold[[1]],
      spatial_radius_m = setting$spatial_radius_m[[1]],
      spatial_min_dwell_mins = setting$spatial_min_dwell_mins[[1]],
      n_compared = n_compared,
      accuracy = accuracy
    )
  )
}

# -----------------------------------------------------------------------------
# 10) Rank results and print top 3 settings
# -----------------------------------------------------------------------------
top_3 <- results %>%
  arrange(desc(accuracy), desc(n_compared)) %>%
  slice_head(n = 3) %>%
  mutate(accuracy_percent = round(accuracy * 100, 2)) %>%
  select(
    inactive_threshold,
    spatial_radius_m,
    spatial_min_dwell_mins,
    n_compared,
    accuracy_percent
  )

cat("\nTop 3 settings by accuracy (manual labels vs consensus):\n")
print(top_3)

# -----------------------------------------------------------------------------
# 11) Save full tuning table
# -----------------------------------------------------------------------------
results_file <- file.path(output_dir, "consensus_tuning_results_simple.csv")
readr::write_csv(results %>% arrange(desc(accuracy), desc(n_compared)), results_file)
cat("\nSaved full tuning table to:\n")
print(results_file)
