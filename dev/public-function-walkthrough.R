# grazer public function walkthrough
#
# 00. Packages ---------------------------------------------------------------

# Run this script from the grazer repository root.
# In RStudio: Session > Set Working Directory > To Project Directory.

# For local package development, load the package from the current repo.

devtools::load_all(".", quiet = TRUE)

library(grazer)

# Tidyverse-style tools are used for the analysis code outside grazer calls.
library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)


# Two functions use leaflet to plot and inspect information. These we're deliberately kept out of the package to reduce install complexity. There are several alternatives to using grazer_map to visualise data.

has_leaflet <- requireNamespace("leaflet", quietly = TRUE)
has_playback <- all(vapply(
  c("leaflet", "leaftime", "htmlwidgets"),
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
))

# 01. Example GPS data -------------------------------------------------------

# In a real analysis this object would usually come from readr::read_csv().
# This synthetic dataset is large enough to exercise the public API, but small
# enough to run quickly while developing the package.

set.seed(20260515)

fix_times <- tibble(
  fix_index = seq_len(144),
  timestamp = seq(
    from = as.POSIXct("2024-05-01 00:00:00", tz = "UTC"),
    by = "20 min",
    length.out = 144
  )
)

animal_lookup <- tibble(
  deployment_id = "trial_2024_wet",
  herd_id = "H1",
  paddock = "north",
  sensor_id = c("C001", "C002", "C003", "C004"),
  animal_id = c("cow_001", "cow_002", "cow_003", "cow_004"),
  treatment = c("control", "shade", "shade", "control"),
  lon_start = c(150.0000, 150.0007, 150.0014, 150.0021),
  lat_start = c(-30.0000, -30.0004, -30.0008, -30.0012)
)

gps_base <- tidyr::expand_grid(animal_lookup, fix_times) |>
  arrange(sensor_id, timestamp) |>
  group_by(sensor_id) |>
  mutate(
    # These tracks have a gentle shared drift plus individual noise.
    # The result is a grazing-like path, not a biological simulation.
    longitude = first(lon_start) + cumsum(rnorm(n(), mean = 0.000010, sd = 0.000060)),
    latitude = first(lat_start) + cumsum(rnorm(n(), mean = 0.000006, sd = 0.000055)),
    hdop = round(runif(n(), min = 0.8, max = 2.6), 2)
  ) |>
  ungroup() |>
  select(
    deployment_id, herd_id, paddock, sensor_id, animal_id, treatment,
    fix_index, timestamp, longitude, latitude, hdop
  )

# Add a few deliberate data problems.
# These let the validation and cleaning functions show their behaviour.
gps_imported <- gps_base |>
  filter(!(sensor_id == "C002" & fix_index == 25)) |>
  mutate(
    longitude = if_else(sensor_id == "C001" & row_number() == 35, longitude + 0.25, longitude),
    latitude = if_else(sensor_id == "C001" & row_number() == 35, latitude + 0.25, latitude)
  ) |>
  bind_rows(
    gps_base |> slice(25),
    gps_base |> slice(80) |> mutate(longitude = 0, latitude = 0),
    gps_base |> slice(140) |> mutate(sensor_id = "", animal_id = "")
  ) |>
  arrange(sensor_id, timestamp) |>
  group_by(sensor_id) |>
  mutate(
    # Give one animal a real break in its record so the gap and segment
    # examples have something visible to inspect.
    timestamp = timestamp + if_else(sensor_id == "C003" & row_number() >= 72, 3 * 3600, 0)
  ) |>
  ungroup() |>
  arrange(sensor_id, timestamp)

groups_animal <- c("deployment_id", "sensor_id")
groups_summary <- c("deployment_id", "sensor_id", "animal_id", "treatment")
herd_groups <- "deployment_id"
metric_crs <- 32756

raw_overview <- gps_imported |>
  summarise(
    n_rows = n(),
    n_sensors = n_distinct(sensor_id),
    n_animals = n_distinct(animal_id),
    first_fix = min(timestamp, na.rm = TRUE),
    last_fix = max(timestamp, na.rm = TRUE)
  )

raw_overview

gps_imported |>
  count(sensor_id, animal_id, treatment, name = "n_raw_fixes") |>
  arrange(sensor_id)


# 02. Spatial fixtures used later -------------------------------------------

# All spatial functions should be reviewed with real paddock and resource data
# later, but these simple geometries are enough to inspect API behaviour.

paddock_coords <- matrix(
  c(
    149.9960, -30.0060,
    150.0080, -30.0060,
    150.0080, -29.9960,
    149.9960, -29.9960,
    149.9960, -30.0060
  ),
  ncol = 2,
  byrow = TRUE
)

paddocks_sf <- sf::st_sf(
  NAME = "north",
  paddock_name = "north",
  geometry = sf::st_sfc(sf::st_polygon(list(paddock_coords)), crs = 4326)
)

resource_points <- sf::st_sf(
  resource_id = c("water_1", "shade_1"),
  resource_type = c("water", "shade"),
  geometry = sf::st_sfc(
    sf::st_point(c(150.0000, -30.0000)),
    sf::st_point(c(150.0030, -30.0020)),
    crs = 4326
  )
)

water_zone_coords <- matrix(
  c(
    149.9995, -30.0005,
    150.0008, -30.0005,
    150.0008, -29.9995,
    149.9995, -29.9995,
    149.9995, -30.0005
  ),
  ncol = 2,
  byrow = TRUE
)

resource_zone <- sf::st_sf(
  resource_id = "water_zone_1",
  resource_type = "water_zone",
  geometry = sf::st_sfc(sf::st_polygon(list(water_zone_coords)), crs = 4326)
)


# 03. Validation functions ---------------------------------------------------

# gps_validate()
# Start with validation before dropping anything. This is a format check. Here
# the imported data still use common raw column names, so validation should tell
# us that grazer expects `datetime`, `lon`, and `lat`.
validation_imported <- gps_validate(
  gps_imported,
  drop_invalid = FALSE,
  large_gap_mins = 45,
  groups = groups_animal,
  check_zero_zero = TRUE
)

validation_imported

# Rename once the validation message makes the required column names clear.
gps_raw <- gps_imported |>
  rename(
    datetime = timestamp,
    lon = longitude,
    lat = latitude
  )

validation <- gps_validate(
  gps_raw,
  drop_invalid = FALSE,
  large_gap_mins = 45,
  groups = groups_animal,
  check_zero_zero = TRUE
)

validation

# Validation is the format gate. If names or core formats are wrong, it should
# explain the required structure.

# gps_qc_summary()
# This prints a short summary and stores the issue tables for inspection.
qc_raw <- gps_qc_summary(
  gps_raw,
  large_gap_mins = 45,
  groups = groups_animal,
  check_zero_zero = TRUE
)

qc_raw

qc_raw$summary
qc_raw$invalid_rows
qc_raw$duplicates
qc_raw$gaps
qc_raw$non_positive_intervals

# A useful QC habit is to look at the tables before cleaning. The summary tells
# you the size of the issue, but the tables show whether the rows make sense.
qc_raw$gaps |>
  as_tibble() |>
  arrange(gap_id, gap_side)

# gps_check_gaps()
# Use this to inspect large gaps and non-positive intervals before deciding
# where continuous track segments should start and end.
gaps_raw <- gps_check_gaps(
  gps_raw,
  large_gap_mins = 45,
  groups = groups_animal
)

gaps_raw |>
  as_tibble() |>
  select(any_of(c(
    "deployment_id", "sensor_id", "datetime",
    "previous_datetime", "interval_mins",
    "interval_type", "is_large_gap", "is_non_positive_interval"
  )))

# Once you have chosen an acceptable gap threshold, append continuous segments.
# `segment_id` combines the sensor id and local segment number, such as
# C001_seg001. Use it when a function should work within continuous track
# pieces and avoid calculating across known large gaps.
gps_segmented <- gps_append_segments(
  gps_raw,
  large_gap_mins = 45,
  groups = groups_animal,
  verbose = FALSE
)

gps_segmented |>
  as_tibble() |>
  count(sensor_id, animal_id, segment_id, name = "n_fixes")

# Later calls use `groups = "segment_id"` when they should stay within
# continuous track pieces and avoid calculating across known large gaps.

# 04. Individual cleaning functions ----------------------------------------

# gps_clean_duplicates()
# First run with action = "flag" so we can inspect what would be removed.
duplicates_flagged <- gps_clean_duplicates(
  gps_segmented,
  action = "flag",
  verbose = FALSE
)

table(duplicates_flagged$is_duplicate_fix)

duplicates_flagged |>
  as_tibble() |>
  filter(is_duplicate_fix) |>
  select(sensor_id, animal_id, datetime, lon, lat, is_duplicate_fix)

duplicates_dropped <- gps_clean_duplicates(
  gps_segmented,
  action = "drop",
  verbose = FALSE
)
class(duplicates_dropped)

attributes(duplicates_dropped)

attr(duplicates_dropped, "cleaning_summary")


# The cleaning audit is stored in attributes so the data remain easy to analyse and we record the cleaning steps.

# gps_clean_errors()
# This catches missing identifiers, invalid datetimes, invalid coordinates, and
# zero-zero fixes. Again, flag first if you are unsure.

errors_flagged <- gps_clean_errors(
  duplicates_dropped,
  action = "flag",
  verbose = FALSE
)

errors_flagged |>
  as_tibble() |>
  filter(is_gps_error) |>
  select(any_of(c(
    "sensor_id", "animal_id", "datetime",
    "lon", "lat", "is_gps_error", "gps_error_reason"
  )))

errors_dropped <- gps_clean_errors(
  duplicates_dropped,
  action = "drop",
  verbose = FALSE
)

attributes(errors_dropped)
attr(errors_dropped, "cleaning_summary")

# The summary now accumulates when standalone cleaning steps are chained.

# gps_clean_speed_fixed()
# Fixed speed thresholds are easy to explain. The risk is that one value may be
# too strict for some studies and too loose for others.

speed_fixed_flagged <- gps_clean_speed_fixed(
  errors_dropped,
  max_speed_mps = 4,
  groups = "segment_id",
  action = "flag",
  keep_speed_cols = TRUE,
  verbose = FALSE
)

speed_fixed_flagged |>
  as_tibble() |>
  filter(is_speed_outlier) |>
  select(sensor_id, animal_id, datetime, step_m, speed_mps, is_speed_outlier)

speed_fixed_dropped <- gps_clean_speed_fixed(
  errors_dropped,
  max_speed_mps = 4,
  groups = "segment_id",
  action = "drop",
  keep_speed_cols = FALSE,
  verbose = FALSE
)

attr(speed_fixed_dropped, "cleaning_summary")

# The audit continues to accumulate through the stepwise cleaning workflow.

# gps_clean_speed_stat()
# Statistical filters are useful as a comparison because they estimate an
# autonomous threshold from the upper part of the speed distribution. For a
# small repeatable analysis, a fixed biological threshold is usually easier to
# explain, so we inspect the statistical result but continue with fixed speed
# cleaning below.

speed_stat_flagged <- gps_clean_speed_stat(
  errors_dropped,
  method = "mad",
  k = 4,
  min_threshold_mps = 4,
  groups = "segment_id",
  action = "flag",
  keep_speed_cols = TRUE,
  verbose = FALSE
)

speed_stat_flagged |>
  as_tibble() |>
  count(is_speed_outlier, name = "n_rows")

attr(speed_stat_flagged, "cleaning_summary")

speed_compare <- tibble(
  method = c("fixed", "statistical"),
  threshold_mps = c(4, attr(speed_stat_flagged, "speed_threshold_mps")),
  n_flagged = c(
    sum(speed_fixed_flagged$is_speed_outlier, na.rm = TRUE),
    sum(speed_stat_flagged$is_speed_outlier, na.rm = TRUE)
  )
)

speed_compare

# gps_activity_state()
# This is an optional model-based step. It can be useful before state-aware
# denoising, but it is not the same as removing duplicates or impossible speeds.
# Check the output visually before relying on it.
gps_states <- gps_activity_state(
  speed_fixed_dropped,
  method = "gmm_hmm",
  groups = "segment_id",
  state_col = "activity_state",
  inactive_prob_col = "inactive_prob",
  verbose = FALSE
)

gps_states |>
  as_tibble() |>
  count(activity_state, name = "n_rows")

gps_states |>
  as_tibble() |>
  ggplot(aes(x = datetime, y = speed_mps, colour = activity_state)) +
  geom_point(alpha = 0.6, size = 1) +
  facet_wrap(vars(sensor_id), scales = "free_x") +
  labs(x = "Datetime", y = "Speed (m/s)", colour = "Activity state") +
  theme_minimal()

# gps_denoise()
# Statistical denoising is explicit. State-aware denoising is also explicit and
# requires the state column you want it to use.
gps_denoised <- gps_denoise(
  gps_states,
  method = "state_aware",
  state_col = "activity_state",
  groups = "segment_id",
  keep_raw_coords = TRUE,
  verbose = FALSE
)

gps_denoised |>
  as_tibble() |>
  transmute(
    sensor_id,
    animal_id,
    datetime,
    moved_lon_maybe = lon != lon_raw,
    moved_lat_maybe = lat != lat_raw
  ) |>
  summarise(
    n_rows = n(),
    n_rows_with_adjusted_lon = sum(moved_lon_maybe, na.rm = TRUE),
    n_rows_with_adjusted_lat = sum(moved_lat_maybe, na.rm = TRUE)
  )


# gps_append_paddock_names()
# Assign paddock names at an animal-epoch level. The threshold helps avoid
# declaring a paddock when the GPS evidence is split or noisy.
gps_with_paddock <- gps_append_paddock_names(
  speed_fixed_dropped,
  paddocks_sf = paddocks_sf,
  name_col = "paddock_name",
  paddock_col = "assigned_paddock",
  buffer_m = 0,
  metric_crs = metric_crs,
  epoch = "day",
  min_prop = 0.7,
  min_fixes = 1,
  groups = groups_animal,
  verbose = FALSE
)

gps_with_paddock |>
  as_tibble() |>
  count(assigned_paddock, name = "n_fixes")

# gps_clean_spatial()
# This can flag or drop fixes outside an expected paddock boundary.
spatial_flagged <- gps_clean_spatial(
  speed_fixed_dropped,
  paddocks_sf = paddocks_sf,
  buffer_m = 50,
  append_paddock = TRUE,
  paddock_col = "paddock_from_boundary",
  groups = "segment_id",
  action = "flag",
  verbose = FALSE
)

spatial_flagged |>
  as_tibble() |>
  count(is_outside_boundary, name = "n_fixes")

table(spatial_flagged$is_outside_boundary)

# gps_clean()
# This is the convenience wrapper for common cleaning steps. Use `step_args`
# when a particular step needs its own settings.
gps_cleaned <- gps_clean(
  gps_segmented,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  groups = "segment_id",
  step_args = list(
    speed_fixed = list(max_speed_mps = 4),
    denoise = list(method = "statistical", keep_raw_coords = TRUE)
  ),
  verbose = FALSE
)

attr(gps_cleaned, "cleaning_summary")

attr(gps_cleaned, "removed_rows")

cleaning_effect <- tibble(
  stage = c("raw", "cleaned"),
  n_rows = c(nrow(gps_raw), nrow(gps_cleaned)),
  n_sensors = c(n_distinct(gps_raw$sensor_id), n_distinct(gps_cleaned$sensor_id)),
  n_animals = c(n_distinct(gps_raw$animal_id), n_distinct(gps_cleaned$animal_id))
)

cleaning_effect


# 05. Time alignment functions ----------------------------------------------

# gps_regularise()
# Builds the expected 20-minute time grid for each continuous track segment.
# Observed fixes are matched to nearby grid times within `tolerance_mins`.
# Missing expected fixes remain visible as rows with missing coordinates.
gps_regularised <- gps_regularise(
  speed_fixed_dropped,
  interval_mins = 20,
  tolerance_mins = NULL,
  groups = "segment_id",
  keep_extra = TRUE,
  verbose = FALSE
)

gps_regularised |>
  as_tibble() |>
  count(is_observed, name = "n_rows")

attr(gps_regularised, "gps_reg")

# gps_interpolate()
# First matches observed fixes to the same kind of regular grid used by
# gps_regularise(). It then fills missing grid rows with straight-line
# interpolation between observed fixes inside each segment.
gps_interpolated <- gps_interpolate(
  speed_fixed_dropped,
  interval_mins = 20,
  tolerance_mins = NULL,
  groups = "segment_id",
  keep_extra = TRUE,
  verbose = FALSE
)

gps_interpolated |>
  as_tibble() |>
  count(is_observed, is_interpolated, name = "n_rows")

gps_interpolated |>
  as_tibble() |>
  filter(is_interpolated | !is_observed) |>
  select(sensor_id, segment_id, datetime, observed_datetime, lon, lat, is_observed, is_interpolated) |>
  head(12)

attr(gps_interpolated, "gps_reg")

# gps_downsample()
# Reduces data frequency by keeping one observed fix per coarser target
# interval. `representative` keeps the fix closest to each target time. `rigid`
# keeps the first fix in each target interval. It does not create missing rows
# or interpolate coordinates.
gps_40min <- gps_downsample(
  gps_cleaned,
  target_mins = 40,
  method = "representative",
  groups = "segment_id",
  verbose = FALSE
)

tibble(
  cleaned_rows = nrow(gps_cleaned),
  downsampled_rows = nrow(gps_40min)
)

attr(gps_40min, "gps_reg")

# 06. Movement functions -----------------------------------------------------

# gps_steps()
# Row-level movement: step distance, speed, bearing, turn angle.
# A step is the line formed between two gps fixes.

gps_step_rows <- gps_steps(
  gps_cleaned,
  groups = "segment_id",
  verbose = FALSE
) 

gps_step_rows |>
  as_tibble() |>
  select(sensor_id, animal_id, datetime, step_dt_s, step_m, speed_mps) |>
  head(12)


# gps_turning()
# A focused helper when turning angle is the main thing being reviewed.
gps_turning_rows <- gps_turning(
  gps_cleaned,
  groups = "segment_id",
  unit = "both",
  verbose = FALSE
)

gps_turning_rows |>
  as_tibble() |>
  select(sensor_id, animal_id, datetime, bearing_deg, turn_rad, turn_deg) |>
  head(12)


# gps_movement_summary()
# Epoch-level movement summaries.
movement_day <- gps_movement_summary(
  gps_cleaned,
  epoch = "day",
  groups = groups_summary,
  verbose = FALSE
)

movement_day |>
  as_tibble() |>
  select(sensor_id, animal_id, treatment, epoch_start, n_fixes, total_distance_m)

movement_day |>
  as_tibble() |>
  ggplot(aes(x = as.Date(epoch_start), y = total_distance_m, colour = treatment)) +
  geom_point(size = 2) +
  facet_wrap(vars(sensor_id)) +
  labs(x = "Date", y = "Total distance (m/day)", colour = "Treatment") +
  theme_minimal()


# 07. Social and proximity functions ----------------------------------------

# Movement summaries above used the cleaned observed fixes. Social and
# proximity metrics are different because animals need to be compared at shared
# timestamps. Here we align once, then use that aligned object for the social
# functions.
gps_social_aligned <- gps_interpolate(
  gps_cleaned,
  interval_mins = 20,
  tolerance_mins = NULL,
  groups = "segment_id",
  keep_extra = TRUE,
  verbose = FALSE
)

gps_social_aligned |>
  as_tibble() |>
  count(is_observed, is_interpolated, name = "n_rows")

# gps_proximity()
# Pair-level distances at shared timestamps.
proximity_rows <- gps_proximity(
  gps_social_aligned,
  herd_groups = herd_groups,
  interpolate = FALSE,
  verbose = FALSE
)

proximity_rows |>
  as_tibble() |>
  select(deployment_id, datetime, pair_id, distance_m) |>
  head(12)


# gps_nearest_neighbour()
# Row-level nearest animal at each timestamp.
nearest_rows <- gps_nearest_neighbour(
  gps_social_aligned,
  herd_groups = herd_groups,
  interpolate = FALSE,
  verbose = FALSE
)

nearest_rows |>
  as_tibble() |>
  select(
    sensor_id, animal_id, datetime,
    nearest_neighbour_sensor_id,
    nearest_neighbour_animal_id,
    nearest_neighbour_m
  ) |>
  head(12)


# gps_neighbours_within_range()
# Row-level count of neighbours within one or more distance thresholds.
range_rows <- gps_neighbours_within_range(
  gps_social_aligned,
  thresholds_m = c(25, 50, 100),
  herd_groups = herd_groups,
  interpolate = FALSE,
  verbose = FALSE
)

range_rows |>
  as_tibble() |>
  select(
    sensor_id, animal_id, datetime,
    n_neighbours_within_25m,
    n_neighbours_within_50m,
    any_neighbour_within_100m
  ) |>
  head(12)


# gps_contacts()
# Contact events summarised by animal pair.
contact_events <- gps_contacts(
  gps_social_aligned,
  contact_distance_m = 75,
  herd_groups = herd_groups,
  interpolate = FALSE,
  max_gap_mins = 40,
  min_duration_mins = 0,
  verbose = FALSE
)

contact_events |>
  as_tibble() |>
  select(any_of(c(
    "deployment_id", "pair_id", "sensor_id_a", "sensor_id_b",
    "start_datetime", "end_datetime", "n_contact_fixes", "duration_mins"
  ))) |>
  head(12)


# gps_social()
# Convenience row-level social metrics.
social_rows <- gps_social(
  gps_social_aligned,
  thresholds_m = c(25, 50, 100),
  herd_groups = herd_groups,
  interpolate = FALSE,
  verbose = FALSE
)

social_rows |>
  as_tibble() |>
  group_by(sensor_id, animal_id) |>
  summarise(
    median_nearest_neighbour_m = median(nearest_neighbour_m, na.rm = TRUE),
    prop_any_neighbour_50m = mean(any_neighbour_within_50m, na.rm = TRUE),
    .groups = "drop"
  )


# gps_social_summary()
# Epoch-level social summaries.
social_day <- gps_social_summary(
  social_rows,
  epoch = "day",
  groups = groups_summary,
  thresholds_m = c(25, 50, 100),
  verbose = FALSE
)

social_day |>
  as_tibble() |>
  select(
    sensor_id, animal_id, treatment, epoch_start,
    mean_nearest_neighbour_m,
    prop_any_neighbour_within_50m
  )


# 08. Spatial use functions --------------------------------------------------

# gps_spatial()
# Broad spatial summary including centroid and MCP area fields.
spatial_day <- gps_spatial(
  gps_cleaned,
  epoch = "day",
  groups = groups_summary,
  min_fixes = 10,
  metric_crs = metric_crs,
  verbose = FALSE
)

spatial_day |>
  as_tibble() |>
  select(
    sensor_id, animal_id, treatment, epoch_start,
    n_fixes, centroid_lon, centroid_lat, mcp95_area_ha
  )


# gps_mcp()
# Specific minimum convex polygon output.
mcp_day <- gps_mcp(
  gps_cleaned,
  percent = c(100, 95),
  epoch = "day",
  groups = groups_summary,
  min_fixes = 10,
  metric_crs = metric_crs,
  return_geometry = FALSE,
  verbose = FALSE
)

mcp_day |>
  as_tibble() |>
  select(sensor_id, animal_id, epoch_start, mcp_percent, n_fixes, mcp_area_ha)


# gps_kde()
# Kernel-density space-use grid cells.
kde_day <- gps_kde(
  gps_cleaned,
  percent = c(95, 50),
  cell_size_m = 50,
  epoch = "day",
  groups = groups_summary,
  min_fixes = 10,
  metric_crs = metric_crs,
  return_geometry = FALSE,
  verbose = FALSE
)

kde_day |>
  as_tibble() |>
  group_by(sensor_id, animal_id, epoch_start, kde_percent) |>
  summarise(
    kde_area_ha = first(kde_area_ha),
    n_grid_cells = n(),
    .groups = "drop"
  )


# gps_hotspots()
# High-use grid cells based on fix counts.
hotspots_day <- gps_hotspots(
  gps_cleaned,
  cell_size_m = 50,
  hotspot_quantile = 0.90,
  keep_all = FALSE,
  epoch = "day",
  groups = groups_summary,
  metric_crs = metric_crs,
  verbose = FALSE
)

hotspots_day |>
  as_tibble() |>
  count(sensor_id, animal_id, epoch_start, name = "n_hotspot_cells")


# 09. Resource-use functions -------------------------------------------------

# gps_resource_distance()
# Appends nearest-resource distance fields to each GPS row.
resource_distance_rows <- gps_resource_distance(
  gps_cleaned,
  resources = resource_points,
  resource_id_col = "resource_id",
  resource_type_col = "resource_type",
  metric_crs = metric_crs,
  verbose = FALSE
)

resource_distance_rows |>
  as_tibble() |>
  select(
    sensor_id, animal_id, datetime,
    nearest_resource_id, nearest_resource_type, resource_distance_m
  ) |>
  head(12)


# gps_resource_use()
# Summarises fixes near point resources or inside polygon resources.
resource_use_day <- gps_resource_use(
  gps_cleaned,
  resources = resource_points,
  radius_m = 75,
  resource_id_col = "resource_id",
  resource_type_col = "resource_type",
  epoch = "day",
  groups = groups_summary,
  metric_crs = metric_crs,
  verbose = FALSE
)

resource_use_day |>
  as_tibble() |>
  select(
    sensor_id, animal_id, resource_id, resource_type,
    epoch_start, n_fixes, n_fixes_near, prop_fixes_near
  ) |>
  head(12)

resource_zone_use <- gps_resource_use(
  gps_cleaned,
  resources = resource_zone,
  radius_m = 0,
  resource_id_col = "resource_id",
  resource_type_col = "resource_type",
  epoch = "day",
  groups = groups_summary,
  metric_crs = metric_crs,
  verbose = FALSE
)

resource_zone_use |>
  as_tibble() |>
  select(sensor_id, animal_id, resource_id, epoch_start, n_fixes_near, prop_fixes_near)


# gps_resource_visits()
# Detects bouts near a resource.
resource_visits <- gps_resource_visits(
  gps_cleaned,
  resources = resource_points,
  radius_m = 75,
  max_gap_mins = 40,
  min_fixes = 2,
  min_duration_mins = 0,
  resource_id_col = "resource_id",
  resource_type_col = "resource_type",
  groups = groups_summary,
  metric_crs = metric_crs,
  verbose = FALSE
)

resource_visits |>
  as_tibble() |>
  select(any_of(c(
    "sensor_id", "animal_id", "resource_id", "resource_type",
    "visit_start", "visit_end", "n_fixes", "duration_mins"
  ))) |>
  head(12)


# 10. Joined summaries -------------------------------------------------------

daily_include <- c("movement", "social", "spatial")

# gps_epoch()
# Joins selected summary blocks into one modelling-friendly table.
daily_metrics <- gps_epoch(
  gps_cleaned,
  epoch = "day",
  include = daily_include,
  groups = groups_summary,
  thresholds_m = c(25, 50, 100),
  herd_groups = herd_groups,
  min_fixes = 10,
  metric_crs = metric_crs,
  verbose = FALSE
)

daily_metrics |>
  as_tibble() |>
  select(any_of(c(
    "sensor_id", "animal_id", "treatment", "epoch_start",
    "n_fixes", "total_distance_m",
    "mean_nearest_neighbour_m", "mcp95_area_ha"
  )))


# gps_animal_summary()
# A convenience wrapper around gps_epoch() for animal or sensor rows.
animal_day <- gps_animal_summary(
  gps_cleaned,
  epoch = "day",
  include = daily_include,
  groups = groups_summary,
  thresholds_m = c(25, 50, 100),
  herd_groups = herd_groups,
  min_fixes = 10,
  metric_crs = metric_crs,
  verbose = FALSE
)

animal_day |>
  as_tibble() |>
  select(any_of(c(
    "sensor_id", "animal_id", "treatment", "epoch_start",
    "total_distance_m", "mean_nearest_neighbour_m", "mcp95_area_ha"
  )))


# gps_group_summary()
# Aggregates animal summaries to treatment, paddock, herd, or other metadata.
group_day <- gps_group_summary(
  gps_cleaned,
  epoch = "day",
  group_cols = "treatment",
  include = daily_include,
  groups = groups_summary,
  thresholds_m = c(25, 50, 100),
  herd_groups = herd_groups,
  min_fixes = 10,
  metric_crs = metric_crs,
  verbose = FALSE
)

group_day |>
  as_tibble() |>
  select(any_of(c(
    "treatment", "epoch_start", "n_animals",
    "total_distance_m", "mean_total_distance_m",
    "mean_nearest_neighbour_m"
  )))


# gps_diurnal()
# Hourly summaries with explicit date and hour columns.
diurnal_movement <- gps_diurnal(
  gps_cleaned,
  include = "movement",
  groups = groups_summary,
  verbose = FALSE
)

diurnal_movement |>
  as_tibble() |>
  select(sensor_id, animal_id, date, hour, n_fixes, total_distance_m) |>
  head(12)


# 11. Mapping functions ------------------------------------------------------

# These return htmlwidgets for interactive inspection in RStudio.
# They are optional because leaflet-related packages may not be installed.

if (has_leaflet) {
  # gps_map()
  # Use a subset first. Full collar datasets can be large in the viewer.
  map_widget <- gps_map(
    gps_cleaned |> filter(datetime <= min(datetime, na.rm = TRUE) + as.difftime(12, units = "hours")),
    groups = "sensor_id",
    popup_fields = c("sensor_id", "animal_id", "datetime"),
    max_points = 500,
    warnings = FALSE
  )

  map_widget
} else {
  message("Skipping gps_map(): leaflet is not installed.")
}

if (has_playback) {
  # gps_playback()
  # Playback is most useful for a short period and a small number of animals.
  playback_widget <- gps_playback(
    gps_cleaned |>
      filter(sensor_id %in% c("C001", "C002")) |>
      filter(datetime <= min(datetime, na.rm = TRUE) + as.difftime(8, units = "hours")),
    groups = "sensor_id",
    align = FALSE,
    tail_points = 12,
    show_points = TRUE,
    point_size_slider = FALSE,
    playback_steps = 80,
    playback_duration_ms = 8000,
    progress = FALSE
  )

  playback_widget
} else {
  message("Skipping gps_playback(): leaflet, leaftime, or htmlwidgets is not installed.")
}


# 12. Final public API coverage check ---------------------------------------

# This checks whether every exported gps_* function appears in this script.
# It is deliberately simple so you can rerun it after adding or renaming public
# functions.

namespace_lines <- readLines("NAMESPACE")

exported_gps_functions <- tibble(line = namespace_lines) |>
  filter(grepl("^export\\(gps_", line)) |>
  mutate(function_name = sub("^export\\((.*)\\)$", "\\1", line)) |>
  pull(function_name) |>
  sort()

script_text <- readLines("dev/public-function-walkthrough.R") |>
  paste(collapse = "\n")

script_function_matches <- gregexpr("gps_[A-Za-z0-9_]+\\s*\\(", script_text)

used_gps_functions <- regmatches(script_text, script_function_matches) |>
  unlist() |>
  sub("\\s*\\($", "", x = _) |>
  unique() |>
  sort()

missing_from_script <- setdiff(exported_gps_functions, used_gps_functions)

tibble(
  n_exported_gps_functions = length(exported_gps_functions),
  n_used_in_this_script = length(intersect(exported_gps_functions, used_gps_functions)),
  missing = if (length(missing_from_script) == 0) "none" else paste(missing_from_script, collapse = ", ")
)

# Review questions:
# If a function feels hard to explain in this script, that is useful feedback.
# It may mean the function needs clearer defaults, clearer output columns, or a
# more explicit name before the API settles.

