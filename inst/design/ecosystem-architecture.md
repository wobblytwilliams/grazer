# grazer Ecosystem Architecture

## Direction

`grazer` is intended to be a central R package for livestock sensor and grazing
system workflows. The first implemented module is GPS. Future modules may cover
accelerometers, virtual fencing, remote sensing, and fused data streams, but
those modules should remain design-only until they are deliberately scoped.

The package should feel like a coherent analysis toolkit rather than a set of
unrelated scripts.

## API Layers

Public functions use data-source-first prefixes:

| Prefix | Module status | Intended scope |
|---|---|---|
| `gps_` | Active build | GPS validation, QC, cleaning, movement, social proximity, spatial use, resource use, activity state, maps, playback |
| `acc_` | Reserved | Accelerometer epochs, features, and behaviour model support |
| `vf_` | Reserved | Virtual fencing events, cues, pulses, boundary interactions, and welfare checks |
| `rs_` | Reserved | Remote sensing extraction, patch summaries, and exposure layers |
| `fusion_` | Reserved | Alignment and joins across sensor or environmental streams |

Internal helpers use `grz_`.

## Current GPS Workflow

A typical GPS workflow should be possible with ordinary data frames:

1. Validate schema and row-level values with `gps_validate()`.
2. Inspect quality-control tables with `gps_qc_summary()` and gap structure with
   `gps_check_gaps()`.
3. Add continuous track segments with `gps_append_segments()` when large gaps
   should separate movement calculations.
4. Clean duplicates, row errors, speed outliers, spatial errors, or GPS jitter
   using the `gps_clean_*()` functions or the `gps_clean()` wrapper.
5. Regularise, interpolate, or downsample only when the analysis needs an even
   time base.
6. Add row-level movement fields with `gps_steps()`.
7. Summarise movement, social proximity, spatial use, or resource use with the
   relevant `gps_*_summary()` or `gps_epoch()` functions.
8. Use `gps_activity_state()` when GPS-derived active or inactive state is part
   of the analysis.
9. Inspect tracks with `gps_map()` or `gps_playback()` when optional mapping
   packages are installed.

## Current GPS Components

Validation and QC:

- `gps_validate()`
- `gps_qc_summary()`
- `gps_check_gaps()`

Cleaning and preparation:

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

Metrics and summaries:

- `gps_steps()`
- `gps_turning()`
- `gps_movement_summary()`
- `gps_social()`
- `gps_social_summary()`
- `gps_spatial()`
- `gps_epoch()`
- `gps_animal_summary()`
- `gps_group_summary()`
- `gps_diurnal()`

Spatial, resource, and social detail:

- `gps_proximity()`
- `gps_nearest_neighbour()`
- `gps_neighbours_within_range()`
- `gps_contacts()`
- `gps_mcp()`
- `gps_kde()`
- `gps_hotspots()`
- `gps_resource_distance()`
- `gps_resource_use()`
- `gps_resource_visits()`

Activity and inspection:

- `gps_activity_state()`
- `gps_map()`
- `gps_playback()`

## Activity State Design

`gps_activity_state()` is a fresh public interface for GPS-derived active or
inactive states. It uses the model mechanics developed in the MVP work but uses
current naming and output conventions.

Current methods:

- `gmm_hmm`: fit a two-component Gaussian mixture model and smooth the posterior
  state sequence with an HMM.
- `gmm`: fit the same mixture model without HMM smoothing.
- `hmm`: fit a direct two-state HMM.

Current feature sets:

- `adaptive`: step distance, turn angle, rolling net displacement, and rolling
  straightness.
- `step_turn`: step distance and turn angle only.

The returned data has activity columns appended. Model diagnostics are stored in
the `gps_activity_state` attribute.

## Dependency Boundaries

Keep the core GPS package light:

- `data.table` can support internal speed.
- `sf` is required for spatial operations.
- Mapping dependencies remain optional and are checked at runtime.
- Manual labelling with Shiny is not part of the current package surface.

Avoid adding heavy dependencies unless a module cannot be implemented sensibly
without them.

## Fresh Build Decisions

This build should not keep old public compatibility wrappers. Old `grz_*`
workflow names are not part of the user-facing API. Internal `grz_` helpers are
still appropriate where they support the current public functions.

The manual labelling app has been removed from the current package surface. If a
labelling workflow is needed later, it should be designed separately with clear
scope, dependencies, tests, and documentation.

Build artefacts, generated site files, and vignette refreshes are separate
cleanup tasks.

## Future Modules

Future module work should start with design and fixture data before public
functions are exported.

Possible future entry points:

- `acc_epoch()`
- `acc_features()`
- `vf_events()`
- `rs_extract()`
- `fusion_align()`

These should not be implemented as stubs unless there is a real workflow and
test plan behind them.
