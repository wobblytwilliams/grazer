# grazer

`grazer` is an R package for cleaning, calculating, and summarising GPS data from cattle grazing experiments.

## 1. Core Design Decisions

- Required minimum input columns: `sensor_id`, `datetime`, `lon`, `lat`.
- Default return type for pipeline functions: `data.frame`.
- Internal processing uses `data.table` for speed.
- Cleaning functions are drop-based (not flag-only) and print:
  - rows dropped
  - before/after row counts
- `verbose = TRUE` by default.
- Optional `snapshot = TRUE` prints a quick state snapshot after each clean step.
- Terminology preference: `group` (not `block`).

## 2. Pipeline Framework

### Step A: Ingest (optional helper)
- `grz_read_gps()`
- `grz_standardise_gps()`

These are kept available, but ingestion can be done outside the package.

### Step B: Validate
- `grz_validate()`

Checks schema compliance and row validity (sensor/date/coords), prints a summary, and returns data.

### Step C: Clean
- `grz_clean_duplicates()`
- `grz_clean_errors()`
- `grz_clean_speed_fixed()` (default threshold `4 m/s`)
- `grz_clean_speed_stat()` (data-driven threshold)
- `grz_clean_spatial()` (polygon-filtering with paddock assignment)
- `grz_denoise()` (first draft static-jitter reduction)
- `grz_clean()` wrapper (multi-step pipeline)

Spatial cleaning rules implemented:
- `(0,0)` rows are dropped.
- Invalid datetime rows are dropped in cleaning.
- Default paddock buffer is `100 m`.
- If no paddock intersection: drop point.
- For overlap ambiguity: assign daily modal paddock per animal/group; drop points outside that modal paddock.
- Paddock naming checks `NAME` and `Description`; requires complete naming in one column; `NAME` is preferred.

### Step D: Calculate (row-level)
- `grz_calculate_movement()`
- `grz_calculate_social()`

Default intent:
- metrics are appended to row-level data first.

Social alignment default:
- `align_interval_mins = "base"`:
  - median observed time difference
  - rounded to nearest minute

### Step E: Summarise (epoch-level)
- `grz_summarise_movement()`
- `grz_summarise_social()`
- `grz_calculate_spatial()` (epoch-level spatial/home-range metrics)
- `grz_calculate_epoch_metrics()` wrapper

Default epoch:
- `day`

### Step F: Behavior Interpretation Loop
- `grz_plot_diurnal_metrics()` (cohort/group metric heatmaps for threshold setting)
- `grz_behavior_threshold_guide()` (hourly quantiles + distribution plots for threshold tuning)
- `grz_classify_behavior()` (rule-based `rest` / `graze` / `travel`)
- `grz_plot_diurnal_states()` (diurnal state proportions)
- `grz_validate_behavior()` (state counts, transitions, bouts, PCA diagnostic)
- `grz_behavior_pipeline()` wrapper

## 3. Optional Utility Functions

- `grz_align()` (interpolate/align at explicit or base interval)
- `grz_append_pdk_names()` (used by spatial cleaning)
- `grz_downsample()` (representative or rigid downsampling)

## 4. Mapping

- `grz_map()` is currently retained as-is.
- It now accepts preferred `group` terminology (with `block` retained for backward compatibility).

## 5. Current Implemented Function Set

Ingest/validation:
- `grz_read_gps()`
- `grz_standardise_gps()`
- `grz_validate_gps()`
- `grz_validate()`

Cleaning:
- `grz_clean_duplicates()`
- `grz_clean_errors()`
- `grz_clean_speed_fixed()`
- `grz_clean_speed_stat()`
- `grz_append_pdk_names()`
- `grz_clean_spatial()`
- `grz_denoise()`
- `grz_clean()`

Utilities:
- `grz_align()`
- `grz_downsample()`

Row-level calculations:
- `grz_calculate_movement()`
- `grz_calculate_social()`

Epoch-level calculations:
- `grz_summarise_movement()`
- `grz_summarise_social()`
- `grz_calculate_spatial()`
- `grz_calculate_epoch_metrics()`

Behavior interpretation:
- `grz_plot_diurnal_metrics()`
- `grz_behavior_threshold_guide()`
- `grz_classify_behavior()`
- `grz_plot_diurnal_states()`
- `grz_validate_behavior()`
- `grz_behavior_pipeline()`

Legacy draft metrics (retained for compatibility):
- `grz_remove_duplicates()`
- `grz_flag_track_qc()`
- `grz_regularise_grid()`
- `grz_downsample_phases()`
- `grz_downsample_async()`
- `grz_movement_metrics()`
- `grz_social_metrics()`
- `grz_home_range_metrics()`
- `grz_activity_metrics()`
- `grz_qc_sampling()`
- `grz_qc_report()`
- `grz_per_entity_summary()`

Mapping:
- `grz_map()`

## 6. Quick Run Script

Use:

```r
source("scripts/run_draft_checks.R")
```

to execute draft checks on `test_data/ManbullooToSimulate.csv`.

## 7. Open Questions

Remaining unresolved choices are tracked in:

- `questions.txt`
