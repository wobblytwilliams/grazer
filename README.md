# grazer <img src="man/figures/logo.png" align="right" height="140" alt="grazer logo" />

`grazer` simplifies the process of working with GPS data from livestock grazing systems. It centralises commonly used cleaning methods and metric calculations, providing a consistent workflow for preparing movement, social, and basic spatial summaries.

The package also includes quick interactive mapping and tools for visually annotating GPS data, supporting the creation of labelled datasets for behavioural interpretation.

Outputs are analysis-ready datasets and summaries suitable for visualisation, statistical analysis, and inclusion in scientific publications. `grazer` is not intended to cover every telemetry workflow; rather, it supports scientists entering the precision livestock research space by providing a practical and reproducible starting point.

For tutorials and updates visit: <https://wobblytwilliams.github.io/grazer/>

## Installation

Install the development version from GitHub:

```r
install.packages("devtools")
devtools::install_github("wobblytwilliams/grazer")
```

## Input Data Requirements

`grazer` operates on time-ordered GPS observations representing individual animals or devices. To ensure consistent processing and reproducible metrics, the input dataset must contain the following minimum fields:

| Column | Description |
|--------|-------------|
| `sensor_id` | Unique identifier for the animal, collar, or device. Used to group trajectories and compute individual metrics. |
| `datetime` | Timestamp of the GPS fix. Must be `POSIXct` or a parseable datetime string. |
| `lon` | Longitude in decimal degrees (WGS84). |
| `lat` | Latitude in decimal degrees (WGS84). |

If your source file uses different headings, rename them to these canonical names before calling `grz_validate()`.

### Why these fields are required

These variables allow `grazer` to reconstruct movement paths through space and time. From them, the package derives:

- step lengths and speeds  
- movement persistence and activity patterns  
- proximity and social interactions  
- spatial use and utilisation patterns  
- time-based summaries (hourly, daily, seasonal)

Using a consistent schema ensures calculations are reproducible across studies and comparable between deployments, properties, and experiments.

A standardised structure also supports **data sharing and collaboration**, enabling datasets from different projects, institutions, and sensor platforms to be combined without extensive reformatting. This promotes interoperability and reduces preprocessing overhead when integrating multi-site or multi-year studies.

### Additional columns

`grazer` assumes that your dataset contains additional contextual information you may wish to retain, such as paddock, treatment group, herd, collar type, pasture condition, or environmental covariates.

These fields are treated as metadata and are preserved throughout cleaning, metric calculation, and summarisation steps wherever possible.

## Quick Start

```r
library(grazer)

# 1) Read (outside grazer), then rename to required schema and validate
gps_raw <- readr::read_csv("path/to/gps.csv", show_col_types = FALSE)

# Required names for grazer are: sensor_id, datetime, lon, lat
# Example rename if your source columns are named differently:
gps <- dplyr::rename(
  gps_raw,
  sensor_id = device_id,
  datetime = Time,
  lon = Longitude,
  lat = Latitude
)

gps <- grz_validate(gps, drop_invalid = TRUE)

# 2) Quick playback inspection (after validate)
m_preview <- grz_playback_gps(
  data = gps,
  group = "sensor_id",
  align = TRUE,
  align_interval_mins = "base",
  tail_points = 20,
  show_points = TRUE,
  point_size_slider = TRUE,
  warnings = FALSE,
  progress = FALSE
)
print(m_preview)

# 3) Clean
gps_clean <- grz_clean(
  gps,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4
)

# 4) Row-level metrics
gps_move <- grz_calculate_movement(gps_clean)
gps_social <- grz_calculate_social(gps_clean)
gps_states <- grz_classify_activity_gmm(
  gps_clean,
  groups = "sensor_id",
  smoothing = "hmm",
  hmm_self_transition = 0.98
)

# 5) Epoch summaries
daily_metrics <- grz_calculate_epoch_metrics(gps_clean, epoch = "day")

head(daily_metrics)
```

## Main Function Groups

`grazer` functions follow a typical telemetry workflow: validate inputs -> clean tracks -> derive metrics -> summarise patterns -> interpret behaviour.

### Ingest & validation

| Function | Purpose |
|----------|---------|
| `grz_validate()` | Checks required fields, timestamps, coordinate validity, and record ordering. |
| `grz_validate_gps()` | Performs GPS-specific checks (missing fixes, impossible coordinates, duplicates, irregular sampling). |


---

### Cleaning

Cleaning functions remove or flag records that may bias movement and behaviour metrics.

| Function | Purpose |
|----------|---------|
| `grz_clean_duplicates()` | Removes duplicate fixes. |
| `grz_clean_errors()` | Removes missing or impossible values (coordinates, timestamps). |
| `grz_clean_speed_fixed()` | Removes biologically implausible steps using a fixed speed threshold. |
| `grz_clean_speed_stat()` | Identifies speed outliers using data-driven thresholds. |
| `grz_append_paddock_names()` | Assigns paddock or area identifiers via spatial overlay. |
| `grz_clean_spatial()` | Removes fixes outside expected spatial boundaries. |
| `grz_denoise()` | Reduces GPS jitter using statistical smoothing, with optional state-aware denoise when active/inactive labels are available. |
| `grz_clean()` | Runs selected cleaning steps as a reproducible pipeline. |

---

### Row-level metrics

| Function | Purpose |
|----------|---------|
| `grz_calculate_movement()` | Computes step length, speed, turning, and related movement metrics. |
| `grz_calculate_social()` | Calculates proximity and nearest-neighbour metrics between animals. |

---

### Epoch-level summaries

| Function | Purpose |
|----------|---------|
| `grz_summarise_movement()` | Aggregates movement metrics by time window (e.g. hour, day). |
| `grz_summarise_social()` | Aggregates social metrics across epochs. |
| `grz_calculate_spatial()` | Summarises spatial use (e.g. paddock utilisation, time in areas). |
| `grz_calculate_epoch_metrics()` | Produces a combined set of movement, social, and spatial summaries. |

---

### Behaviour tools

| Function | Purpose |
|----------|---------|
| `grz_plot_diurnal_metrics()` | Visualises diurnal patterns in movement and social metrics. |
| `grz_behavior_threshold_guide()` | Suggests candidate thresholds for activity classification. |
| `grz_tune_thresholds()` | Tunes thresholds using labelled data or heuristics. |
| `grz_classify_activity_gmm()` | Classifies activity using a 2-component Gaussian mixture model with optional HMM smoothing. |
| `grz_classify_behavior()` | Produces final behavioural state labels. |
| `grz_plot_diurnal_states()` | Visualises behavioural states across time-of-day. |
| `grz_validate_behavior()` | Performs QA checks on behavioural outputs. |
| `grz_behavior_pipeline()` | Runs an end-to-end behaviour classification workflow. |

## Interactive Tools

- `grz_map()` for interactive static GPS mapping (low overheads).
- `grz_playback_gps()` for timeline playback with animated track tails on a leaflet map (high overheads).
- `grz_label_gps_states()` for manual `ACTIVE`/`INACTIVE` state labelling in a timeline app. For use in the GMM activity-state workflow.

## Output Conventions

- Functions return `data.frame` by default.
- Set `return_class = "data.table"` where supported for data.table output.
- Cleaning functions are drop-based and report dropped row counts when `verbose = TRUE`.

## License

MIT License.


