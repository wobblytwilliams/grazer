# grazer ecosystem roadmap

This document sketches the possible shape of the `grazer` ecosystem for collaborators. The aim is to build a coherent, reusable set of tools for precision livestock research in extensive grazing systems, starting with GPS workflows and expanding into other sensor streams and analysis modules.

The long-term ambition is that this becomes a go-to R ecosystem for researchers working with livestock sensor data, built around a small, deliberate object system, consistent naming, interoperable workflows, and practical modules for common research questions.

## Phase 1: GPS-focused CRAN package

Phase 1 will focus on getting `grazer` onto CRAN as a strong GPS workflow package.

The first release, as a minimum, should cover:

- GPS data validation and schema checks.
- Cleaning and quality control.
- Movement metrics.
- Basic social/proximity metrics.
- Spatial summaries.
- Distance to points of interest such as water, shade, supplements, fences, or infrastructure.
- Home-range and spatial-use summaries.
- Activity-state classification from GPS movement features.
- Epoch Summaries
- Visualisation and map-based exploration.
- A compact example dataset and vignettes that show a complete workflow.

This gives the project a clear first identity: `grazer` is the practical package for GPS-based livestock grazing workflows.

```mermaid
flowchart LR
  A[Raw GPS data] ==> B[Validate]
  B ==> C[Clean]
  C ==> D[Quality control]

  D ==> E[Movement metrics]
  D ==> F[Social metrics]
  D ==> G[Spatial and POI metrics]
  D ==> H[Activity states]

  E ==> I[Epoch summaries]
  F ==> I
  G ==> I
  H ==> I

  E --> J[Analysis-ready outputs]
  F --> J
  G --> J
  H --> J

  I ==> J
```

## Ecosystem vision

The broader ecosystem should be organised around research workflows, not only sensor hardware. Sensors are the inputs, but researchers usually care about questions such as:

- Where did animals spend time?
- How far did they move?
- How did they respond to boundaries, water, shade, pasture, heat, or management?
- What behaviours were they performing?
- How reliable were the sensors?
- How can collar event streams and other animal-linked sensor streams be standardised?
- How can multiple sensor streams be combined into analysis-ready data?

```mermaid
mindmap
  root((grazer ecosystem))
    GPS
      cleaning
      movement
      proximity
      POI distance
      spatial interaction
      activity states
    Virtual fencing
      boundary events
      cue and pulse summaries
      learning curves
      compliance
      welfare checks
      sensor data standardisation
    Accelerometers
      handling noise
      applying common filter methods
      feature windows
      behaviour labels
      prediction models
      validation
      transfer across herds
    Remote sensing
      indices in raster and patch configurations
      water and shade layers
      biomass proxies
      raster extraction
      exposure summaries
    Environmental context
      weather
      heat load
      terrain
      paddock metadata
    Auxiliary sensor streams
      time linked individual streams (boli, greenfeed, liveweight)
    Data fusion
      aligned timelines
      shared object system
      multi-sensor features
      analysis datasets
```

## Possible modules

| Area | What it covers | Example outputs |
|---|---|---|
| GPS core | Validation, cleaning, movement, proximity, POI distance, spatial interaction, activity states, epoch summaries, maps | Clean tracks, speed, distance, turning, paddock use, proximity metrics, activity-state summaries |
| Virtual fencing | Response to virtual boundaries and collar event streams, including standardisation of raw vendor outputs | Cue counts, pulse counts, boundary approaches, compliance, learning curves, welfare summaries |
| Accelerometers | Cleaning noisy signals, applying common filter methods, extracting windows, and building behaviour prediction workflows | Filtered signals, feature tables, behaviour labels, prediction outputs, validation summaries |
| Remote sensing | Linking animals to raster- and patch-based landscape products and management layers | Vegetation index exposure, biomass proxies, water/shade availability, patch-use summaries |
| Environmental context | Weather, heat load, terrain, and paddock metadata linked to animals or epochs | Heat exposure summaries, rainfall windows, terrain-linked movement summaries, paddock context tables |
| Auxiliary sensor streams | Animal-linked temporal streams in written/tabular formats such as bolus data, GreenFeed data, liveweight, or similar non-image, non-audio sensor outputs | Time-aligned methane, rumen temperature, intake, visit events, liveweight changes, derived temporal summaries |
| Sensor QC | Data quality across devices and deployments | Missing fixes, irregular intervals, dropout diagnostics, stream completeness summaries |
| Multi-sensor fusion | Joining GPS, virtual fence, accelerometer, auxiliary sensor, environment, and remote-sensing products | Aligned timelines, modelling tables, reproducible analysis datasets, fused feature sets |

## What to include or omit as features

One of design decisions is not what to build, but what not to build.

The package should add value where livestock workflows are genuinely awkward, repetitive, or hard to standardise. It should avoid recreating tools that are already well solved elsewhere in the R ecosystem.

Principles:

- If a strong, widely used package already solves a problem cleanly, use it rather than wrapping it unnecessarily.
- Add grazer-specific functions where domain knowledge, repeated workflow pain points, or standardisation needs justify them.
- Prefer outputs that remain easy for users to inspect, modify, and extend in their own analyses.

Examples of what this means in practice:

- Spatial data should use `sf` directly. Grazer should work with `sf` objects, but it should not create a parallel spatial class or large wrapper layer around `sf`.
- Plotting should generally use `ggplot2` conventions. It is often better to point users back to `ggplot` rather than to create a large family of rigid plotting wrappers. If a visual is specialised enough to justify package support, it may be better implemented as a `geom_`, a helper that returns plotting data, or a function that returns a modifiable `ggplot` object.
- Data import functions should be kept minimal at this stage. `readr::read_csv()` and `data.table::fread()` are already sufficient in most cases. Instead of building ingest helpers, the start of the workflow should be a validation step that confirms the required fields, types, ordering, and metadata are present.

Tabular design principles:

- Return `data.frame` objects by default so outputs remain familiar and widely compatible.
- Use `data.table` internally where it provides clear speed and memory benefits for large livestock datasets.
- Keep the tabular backend fast, but keep the user-facing outputs simple.

This should help grazer stay focused on domain-specific workflow value rather than expanding into a general-purpose reimplementation of the wider R ecosystem.

## Shared object system

Returning information in R data frames reduces the barrier for researchers who already work in tidyverse, data.table, or base R workflows. The intent should be to keep the object system as small as possible. Where a good class already exists, it should be used directly rather than wrapped in a new grazer-specific class. In particular, spatial layers should use `sf` directly rather than introducing a separate spatial object.

Core object concepts:

- `grz_track`: row-level animal/sensor observations (gps,acc, methane, liveweight, etc.).
- `grz_epoch`: summarised data by time window.
- `grz_qc`: validation and sensor-quality summaries.
- `grz_model`: fitted classification or prediction objects (if built into grazer).

It's not clear at this stage how important it will be to define the `grz_epoch`, and `grz_qc` as standalone defined schemas; however, having a defined `grz_track` object should simplify data fusion modules in future.

Notes on object scope:
- Do not invent the wheel, if it's already simple in base R, or tidyverse, there is no need for additional schemas
- Spatial layers such as paddocks, POIs, fences, water, and shade should use `sf`.
- Auxiliary sensor streams such as bolus, GreenFeed, or liveweight should usually still fit within `grz_track`, with a clear schema for `sensor_type`, timestamps, animal linkage, and measured variables. However, there is a potential that movement and behaviour data are separated from other temporal sensor data.

## Package structure options

There are two ways the ecosystem could develop. At this stage, it's not clear what the best option it. And, there is potential that this changes as the function suite becomes more complex/sophisticated

### Option 1: One package to rule them all.

Everything stays inside `grazer`.

Advantages:

- Easier for users to install and learn at the start.
- One documentation site and one public package identity.
- Less coordination across packages and fewer dependency decisions for users.
- A simpler development path while the scope is still settling.

Risks:

- Heavy dependencies from remote sensing, modelling, accelerometer workflows, or specialised stream-processing tools could make installation and maintenance harder. Although, in theory, this can be tested by dev team.
- The package may become conceptually crowded as more specialist workflows are added.
- GPS, accelerometer, virtual fencing, remote sensing, weight, weather, and metadata streams may need distinct release cycles and different levels of maintenance attention.

### Option 2: Multiple smaller packages plus an aggregation package

If the workflows become sufficiently distinct, the ecosystem could move towards multiple smaller specialist packages with an aggregation package sitting above them.

In that model:

- stream-specific packages would handle specialist workflows such as GPS, accelerometers, virtual fencing, liveweight, or environmental data
- the aggregation package would not need to contain all workflow logic itself
- interoperability would occur mainly through standardised epoch summaries and analysis-ready tables

One possible shape could look like this:

```mermaid
flowchart LR
  A[Raw GPS data] ==> B[grazer.gps]
  C[Raw accelerometer data] ==> D[grazer.acc]
  E[Virtual fencing data] ==> F[grazer.vf]
  G[Weight data] ==> H[grazer.weight]
  I[Remote sensing and weather] ==> J[grazer.env]

  B ==> K[GPS epoch summaries]
  D ==> L[Behaviour epoch summaries]
  F ==> M[Virtual fencing epoch summaries]
  H ==> N[Weight epoch summaries]
  J ==> O[Environmental exposure summaries]

  K ==> P[grazer.fusion]
  L ==> P
  M ==> P
  N ==> P
  O ==> P

  P ==> Q[Analysis-ready outputs]
```

Preferred strategy:

Start with one practical package, `grazer`, focused on the GPS and movement workflow for extensively grazed livestock. Only move towards multiple smaller packages plus an aggregation package if there is a clear maintenance, dependency, or user-group reason to do so.

Maintain detailed documentation and vignette's can ensure the applications of the package are clear.

## Naming convention

The naming should be continuous and predictable across the ecosystem.

### Package names

- Use `grazer` for the CRAN GPS package.
- If the ecosystem later grows into multiple packages, keep the `grazer.*` family rather than introducing a second naming system.
- Do not rename the package family away from `grazer`.

### Function names

Keep the current `grz_` prefix.

Recommended naming pattern:

- Use `grz_<stream>_<verb>()` where the stream comes before the action.
- This makes related functions easier to discover through autocomplete. For example, typing `grz_vf_` should surface the virtual fencing workflow.
- Use shared verbs across streams where possible, such as `read`, `standardise`, `validate`, `clean`, `align`, `annotate`, `calculate`, `summarise`, `classify`, `tune`, and `plot`.

Example names across major workflows:

| Workflow | Example names |
|---|---|
| GPS | `grz_gps_read_csv()`, `grz_gps_standardise()`, `grz_gps_validate()`, `grz_gps_clean()`, `grz_gps_align()`, `grz_gps_calculate_movement()`, `grz_gps_summarise_epoch()`, `grz_gps_plot_map()` |
| Accelerometer | `grz_acc_read_csv()`, `grz_acc_standardise()`, `grz_acc_validate()`, `grz_acc_clean()`, `grz_acc_align()`, `grz_acc_calculate_features()`, `grz_acc_classify_behaviour()`, `grz_acc_plot_features()` |
| Virtual fencing | `grz_vf_read_events()`, `grz_vf_standardise()`, `grz_vf_validate()`, `grz_vf_clean()`, `grz_vf_align()`, `grz_vf_calculate_events()`, `grz_vf_summarise_epochs()`, `grz_vf_classify_response()`, `grz_vf_plot_events()` |

This naming convention results in long function names which might not be a preferred method. Alternatively, the `grz_` prefix could be dropped in favour of the `sensor_verb` naming style.

## Preferred direction

1. Get `grazer` onto CRAN as a polished GPS package.
2. Use phase 1 to establish the shared schema, naming style, and pipe-friendly workflow, with a view to expanding.
3. Treat virtual fencing, accelerometers, remote sensing, auxiliary sensor streams, and fusion as future modules.
4. Keep the package structure under review, and only move to multiple smaller packages if the ecosystem genuinely needs it.

# GPS phase 1 delivery plan

To achieve the CRAN goal, the GPS package should be treated as a scoped delivery project with clear task ownership. The most practical way to organise this is to start from what already exists in `grazer`, stabilise the function set, and then divide the remaining work into parallel streams.

The core idea is:

- define what the GPS package is responsible for in phase 1
- review the existing function set given the broader ecosystem and identify anything missing
- then split work into implementation and documentation, and testing and examples.

## Suggested order of work

1. Confirm the phase 1 function set from the current package.
2. Group those functions into a clear workflow for users.
3. Identify gaps, overlaps, and functions that are still experimental.
4. Assign tasks across function refinement & documentation; and, tests, and release preparation.
5. revise vignettes
6. Integrate, check, and prepare for CRAN submission.

## Proposed phase 1 function groups

The majority of these functions have some level of development in the current version of the package.

| Work area | Likely functions or outputs |
|---|---|
| Input and validation | `grz_validate()`, `grz_validate_gps()`, schema checks, canonical column expectations |
| Cleaning and QC | duplicate removal, error cleaning, speed filtering, denoising, fix-performance summaries |
| Alignment and preprocessing | alignment, downsampling, standard GPS preparation steps |
| Movement and proximity metrics | movement metrics, social/proximity summaries, POI distance summaries |
| Spatial summaries | spatial summaries, paddock annotation, home range, home-range change |
| Activity interpretation | activity-state classification, threshold tuning, validation, labelled workflows |
| Visualisation | maps, playback, summary plots?, diagnostic plots? |
| Example workflows | small package dataset, vignettes, end-to-end examples |
