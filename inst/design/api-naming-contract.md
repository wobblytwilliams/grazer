# grazer API Naming Contract

## Purpose

This document records the naming rules for the public `grazer` API. The package
is being built as a central ecosystem package for livestock sensor and grazing
system workflows, with GPS as the first implemented module.

Public functions should be short, data-source-first, and easy to read in an
analysis script.

## Public Prefixes

Use these public prefixes:

| Prefix | Scope |
|---|---|
| `gps_` | GPS fixes, tracks, cleaning, movement, spatial use, social proximity, resource use, maps, playback, and GPS-derived activity state |
| `acc_` | Future accelerometer workflows |
| `vf_` | Future virtual fencing workflows |
| `rs_` | Future remote sensing workflows |
| `fusion_` | Future workflows that align or combine streams |

Only the `gps_` surface is implemented in the current build. The other prefixes
are reserved design space and should not be implemented until requested.

## Public Naming Rules

1. Put the data source first, then the action, metric, or output.
2. Prefer short names that read naturally in a script.
3. Avoid `calculate` in public function names unless no clearer verb exists.
4. Avoid broad names such as `gps_do_everything()`.
5. Use British English in documentation, comments, messages, and output wording
   where a spelling choice is needed.
6. Use one function for one clear level of work. Wrappers can orchestrate tested
   smaller functions, but should not hide important assumptions.
7. Every exported function needs roxygen documentation and tests.

Good examples:

- `gps_validate()`
- `gps_clean()`
- `gps_steps()`
- `gps_movement_summary()`
- `gps_social()`
- `gps_spatial()`
- `gps_activity_state()`

Poor examples:

- `grz_gps_calculate_movement()`
- `gps_calculate_all_metrics()`
- `gps_do_everything()`

## Internal Naming Rules

Use `grz_` for internal helpers, validators, constructors, classes, and shared
infrastructure.

Examples:

- `grz_require_cols()`
- `grz_as_output()`
- `grz_default_group_cols()`
- `grz_haversine_m()`
- `validate_grz_gps()`

Internal rules:

1. Internal helpers stay unexported unless they become a real user workflow.
2. Temporary columns should use a `.grz_` prefix and must not leak into user
   outputs.
3. Public functions should accept ordinary data frames where possible.
4. `data.table` can be used internally, but user-facing defaults should remain
   `data.frame`.

## Current GPS Public API

The current public surface is `gps_*` only.

Validation and QC:

- `gps_validate()`
- `gps_qc_summary()`
- `gps_check_gaps()`

Cleaning and track preparation:

- `gps_append_segments()`
- `gps_clean_duplicates()`
- `gps_clean_errors()`
- `gps_clean_speed_fixed()`
- `gps_clean_speed_stat()`
- `gps_append_paddock_names()`
- `gps_clean_spatial()`
- `gps_denoise()`
- `gps_clean()`

Temporal alignment:

- `gps_regularise()`
- `gps_interpolate()`
- `gps_downsample()`

Movement and summaries:

- `gps_steps()`
- `gps_turning()`
- `gps_movement_summary()`
- `gps_epoch()`
- `gps_animal_summary()`
- `gps_group_summary()`
- `gps_diurnal()`

Social metrics:

- `gps_proximity()`
- `gps_nearest_neighbour()`
- `gps_neighbours_within_range()`
- `gps_contacts()`
- `gps_social()`
- `gps_social_summary()`

Spatial and resource use:

- `gps_mcp()`
- `gps_kde()`
- `gps_hotspots()`
- `gps_spatial()`
- `gps_resource_distance()`
- `gps_resource_use()`
- `gps_resource_visits()`

Activity state and inspection:

- `gps_activity_state()`
- `gps_map()`
- `gps_playback()`

## Activity State Naming

`gps_activity_state()` is the public entry point for GPS-derived active or
inactive states.

Current methods:

- `method = "gmm_hmm"` fits a two-component Gaussian mixture model and applies
  HMM smoothing.
- `method = "gmm"` fits the same mixture model without HMM smoothing.
- `method = "hmm"` fits a direct two-state HMM.

Current feature sets:

- `feature_set = "adaptive"` uses step, turn, rolling net displacement, and
  rolling straightness.
- `feature_set = "step_turn"` uses step and turn only.

Model diagnostics are stored in the `gps_activity_state` attribute. Older
model-specific attributes should not be used.

## Fresh Build Decisions

This build should not maintain old public `grz_*` compatibility wrappers.
Useful implementation ideas can remain as internal `grz_` helpers, but user
workflows should use the `gps_*` API directly.

The manual GPS state labelling app is not part of the current package surface.
If manual labelling returns later, it should be designed as a fresh workflow
with clear dependencies and a `gps_` public name.

## Output Naming

User-facing outputs should be predictable:

1. Return `data.frame` by default.
2. Use snake_case column names.
3. Put identifiers before metric columns.
4. Include units in names where units are not obvious.
5. Use UTC datetimes unless local time is explicitly requested.
6. Keep important QC and cleaning audit information visible through returned
   tables or documented attributes.

Common suffixes:

| Pattern | Meaning |
|---|---|
| `_id` | Identifier |
| `_m` | Metres |
| `_mps` | Metres per second |
| `_s` | Seconds |
| `_mins` | Minutes |
| `_ha` | Hectares |
| `_deg` | Degrees |
| `_rad` | Radians |
| `n_` | Count |
| `prop_` | Proportion |
| `is_`, `has_`, `any_` | Logical flag |
