grz_make_popup <- function(data, fields) {
  if (length(fields) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  present <- fields[fields %in% names(data)]
  if (length(present) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  parts <- lapply(present, function(field) {
    value <- as.character(data[[field]])
    value[is.na(value)] <- ""
    paste0("<b>", field, ":</b> ", value)
  })

  do.call(paste, c(parts, sep = "<br/>"))
}

grz_make_group_id <- function(data, groups) {
  parts <- lapply(groups, function(col) {
    value <- as.character(data[[col]])
    value[is.na(value) | trimws(value) == ""] <- "(missing)"
    value
  })

  do.call(paste, c(parts, sep = " | "))
}

grz_make_group_palette <- function(values) {
  n <- length(unique(values))
  cols <- grDevices::hcl.colors(max(3L, n), palette = "Dark 3")
  leaflet::colorFactor(palette = cols, domain = values)
}

grz_guess_group_cols <- function(data, groups = NULL) {
  if (!is.null(groups)) {
    if (!is.character(groups) || length(groups) < 1L || any(is.na(groups)) || any(trimws(groups) == "")) {
      stop("`groups` must be NULL or a character vector of one or more column names.", call. = FALSE)
    }
    missing <- setdiff(groups, names(data))
    if (length(missing) > 0L) {
      stop("`groups` column(s) not found: ", paste(missing, collapse = ", "), call. = FALSE)
    }
    return(unique(groups))
  }

  candidates <- c("sensor_id", "animal_id", "device_id", "deployment_id")
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) > 0L) {
    return(hit[[1L]])
  }
  NULL
}

grz_make_state_palette <- function(values, state_colors = c(inactive = "#d7191c", active = "#1a9641")) {
  if (!is.character(state_colors) || length(state_colors) < 1L) {
    stop("`state_colors` must be a named character vector of colors.", call. = FALSE)
  }
  if (is.null(names(state_colors)) || any(is.na(names(state_colors))) || any(trimws(names(state_colors)) == "")) {
    stop("`state_colors` must be named (e.g., c(inactive = '#d7191c', active = '#1a9641')).", call. = FALSE)
  }

  vals <- as.character(values)
  vals[is.na(vals) | trimws(vals) == ""] <- "(missing)"
  levels <- unique(vals)

  named_cols <- stats::setNames(unname(state_colors), tolower(names(state_colors)))
  if (!("(missing)" %in% names(named_cols))) {
    named_cols["(missing)"] <- "#808080"
  }

  assigned <- rep(NA_character_, length(levels))
  for (i in seq_along(levels)) {
    key <- tolower(levels[[i]])
    if (key %in% names(named_cols)) {
      assigned[[i]] <- named_cols[[key]]
    }
  }

  unknown <- which(is.na(assigned))
  if (length(unknown) > 0L) {
    assigned[unknown] <- grDevices::hcl.colors(length(unknown), palette = "Dark 3")
  }

  names(assigned) <- levels
  list(
    values = vals,
    colors = unname(assigned[vals]),
    legend_labels = names(assigned),
    legend_colors = unname(assigned)
  )
}

grz_select_groups <- function(data, group_col, max_groups, seed) {
  groups <- unique(as.character(data[[group_col]]))
  if (length(groups) <= max_groups) {
    return(data)
  }

  set.seed(seed)
  keep_groups <- sample(groups, max_groups)
  data[data[[group_col]] %in% keep_groups, , drop = FALSE]
}

grz_sample_points <- function(data, max_points, seed) {
  if (nrow(data) <= max_points) {
    return(data)
  }

  set.seed(seed)
  idx <- sample.int(nrow(data), max_points)
  data[idx, , drop = FALSE]
}

grz_sample_points_by_group <- function(data, group_col, max_points, seed) {
  if (nrow(data) <= max_points) {
    return(data)
  }

  groups <- split(seq_len(nrow(data)), as.character(data[[group_col]]), drop = TRUE)
  group_names <- names(groups)
  n_groups <- length(group_names)
  if (n_groups == 0L) {
    return(grz_sample_points(data, max_points = max_points, seed = seed))
  }

  set.seed(seed)
  order_for_remainder <- sample(group_names, n_groups)

  base <- floor(max_points / n_groups)
  rem <- max_points %% n_groups
  quota <- stats::setNames(rep(base, n_groups), group_names)
  if (rem > 0L) {
    quota[order_for_remainder[seq_len(rem)]] <- quota[order_for_remainder[seq_len(rem)]] + 1L
  }

  selected <- integer(0)
  remaining <- groups

  for (g in group_names) {
    idx <- groups[[g]]
    take <- min(length(idx), quota[[g]])
    if (take > 0L) {
      picked <- sample(idx, take)
      selected <- c(selected, picked)
      remaining[[g]] <- setdiff(idx, picked)
    }
  }

  deficit <- max_points - length(selected)
  while (deficit > 0L) {
    available <- names(remaining)[vapply(remaining, length, integer(1)) > 0L]
    if (length(available) == 0L) {
      break
    }

    pass_order <- sample(available, length(available))
    for (g in pass_order) {
      if (deficit == 0L) {
        break
      }
      pool <- remaining[[g]]
      if (length(pool) == 0L) {
        next
      }
      picked <- sample(pool, 1L)
      selected <- c(selected, picked)
      remaining[[g]] <- setdiff(pool, picked)
      deficit <- deficit - 1L
    }
  }

  data[selected, , drop = FALSE]
}

grz_validate_map_inputs <- function(data, lon, lat, datetime, groups, state_col = NULL) {
  needed <- c(lon, lat, datetime)
  if (!is.null(groups)) {
    needed <- c(needed, groups)
  }
  if (!is.null(state_col)) {
    needed <- c(needed, state_col)
  }

  missing <- setdiff(unique(needed), names(data))
  if (length(missing) > 0) {
    stop("Missing required columns for mapping: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

grz_prepare_map_polygons <- function(polygons_sf, polygon_label_col) {
  if (is.null(polygons_sf)) {
    return(NULL)
  }
  if (!inherits(polygons_sf, "sf")) {
    stop("`polygons_sf` must be an sf object.", call. = FALSE)
  }
  if (nrow(polygons_sf) < 1L) {
    stop("`polygons_sf` must contain at least one polygon.", call. = FALSE)
  }
  if (is.na(sf::st_crs(polygons_sf))) {
    stop("`polygons_sf` must have a declared coordinate reference system (CRS).", call. = FALSE)
  }
  geometry_types <- unique(as.character(sf::st_geometry_type(polygons_sf, by_geometry = TRUE)))
  unsupported <- setdiff(geometry_types, c("POLYGON", "MULTIPOLYGON"))
  if (length(unsupported) > 0L) {
    stop(
      "`polygons_sf` must contain only POLYGON or MULTIPOLYGON geometries; found: ",
      paste(unsupported, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (any(sf::st_is_empty(polygons_sf))) {
    stop("`polygons_sf` contains empty geometries.", call. = FALSE)
  }
  geometry_valid <- sf::st_is_valid(polygons_sf)
  if (any(is.na(geometry_valid)) || any(!geometry_valid)) {
    stop("`polygons_sf` contains invalid polygon geometries.", call. = FALSE)
  }
  if (!is.null(polygon_label_col)) {
    if (!is.character(polygon_label_col) || length(polygon_label_col) != 1L ||
        is.na(polygon_label_col) || trimws(polygon_label_col) == "") {
      stop("`polygon_label_col` must be NULL or a single non-empty column name.", call. = FALSE)
    }
    if (!polygon_label_col %in% names(polygons_sf)) {
      stop("`polygon_label_col` was not found in `polygons_sf`.", call. = FALSE)
    }
  }

  tryCatch(
    sf::st_transform(polygons_sf, 4326),
    error = function(e) {
      stop("`polygons_sf` could not be transformed to WGS84 (EPSG:4326): ", conditionMessage(e), call. = FALSE)
    }
  )
}

grz_grouped_timeslider_dependency <- function() {
  htmltools::htmlDependency(
    name = "grazer-grouped-timeslider",
    version = "0.1.0",
    src = c(file = "htmlwidgets/grazer-grouped-timeslider"),
    package = "grazer",
    script = "grazer-grouped-timeslider.js"
  )
}

grz_add_timeslider_dependency <- function(map) {
  dependency_names <- vapply(
    map$dependencies,
    function(dependency) dependency$name,
    character(1)
  )
  if (!"grazer-grouped-timeslider" %in% dependency_names) {
    map$dependencies <- c(map$dependencies, list(grz_grouped_timeslider_dependency()))
  }
  map
}

grz_add_grouped_timeslider <- function(map, times, layer_ids, groups) {
  map <- grz_add_timeslider_dependency(map)
  leaflet::invokeMethod(
    map,
    NULL,
    "addGrazerGroupedTimeslider",
    as.character(times),
    as.character(layer_ids),
    as.character(groups),
    list(position = "topright", range = TRUE, showAllOnStart = TRUE)
  )
}

grz_offset_timeline_layer_control <- function(map, margin_top_px = 44) {
  map <- grz_add_timeslider_dependency(map)
  leaflet::invokeMethod(
    map,
    NULL,
    "offsetGrazerTimelineLayerControl",
    as.numeric(margin_top_px)
  )
}

grz_add_layer_deselect_all <- function(map, overlay_groups) {
  map <- grz_add_timeslider_dependency(map)
  leaflet::invokeMethod(
    map,
    NULL,
    "addGrazerLayerDeselectAll",
    as.character(overlay_groups),
    "Deselect all"
  )
}

grz_confirm_continue <- function(stop_message) {
  if (interactive()) {
    proceed <- readline("Do you wish to continue? [y/N]: ")
    is_yes <- tolower(trimws(proceed)) %in% c("y", "yes")
    if (!is_yes) {
      stop("Map creation cancelled by user.", call. = FALSE)
    }
  } else {
    stop(stop_message, call. = FALSE)
  }
}

#' Map GPS fixes interactively
#'
#' Creates a leaflet map of GPS fixes with optional switchable group layers,
#' polygon overlays, and timeline playback.
#'
#' @param data Data frame containing GPS points.
#' @param lon Name of longitude column.
#' @param lat Name of latitude column.
#' @param datetime Name of datetime column.
#' @param groups Optional grouping columns for colour and layer separation.
#'   Multiple columns are combined into labels separated by `" | "`.
#' @param state_col Optional state column for fixed state coloring (for
#'   example `activity_state_gmm`).
#' @param state_colors Named colors for state levels. Defaults to red
#'   (`inactive`) and green (`active`).
#' @param state_legend_title Legend title used when `state_col` is provided.
#' @param timeline Logical; if `TRUE`, render points with an interactive time slider.
#' @param polygons_sf Optional `sf` object containing polygon or multipolygon
#'   geometries. Any declared CRS is accepted and transformed to EPSG:4326.
#' @param polygon_label_col Optional column in `polygons_sf` used for labels.
#' @param polygon_group Layer-control name for all polygon features.
#' @param polygon_color Polygon border colour.
#' @param polygon_weight Polygon border weight.
#' @param polygon_opacity Polygon border opacity.
#' @param polygon_fill Logical; fill polygons.
#' @param polygon_fill_opacity Polygon fill opacity.
#' @param layer_control Logical; add on/off controls for grouped animals and
#'   polygon overlays.
#' @param popup_fields Character vector of fields to show in marker popups.
#' @param provider Tile provider name passed to `leaflet::addProviderTiles()`.
#' @param point_radius Marker radius.
#' @param point_opacity Marker opacity.
#' @param max_points Optional max number of points to render (random sample).
#' @param max_groups Optional maximum number of complete groups to plot.
#' @param seed Random seed used for sampling.
#' @param point_warning_n Point count that triggers a large-map warning.
#' @param group_warning_n Group count that triggers a large-map warning.
#' @param warnings Logical; if `FALSE`, bypasses map-size confirmations.
#'
#' @return A `leaflet` htmlwidget.
#' @examples
#' \dontrun{
#' gps_map(
#'   gps_data,
#'   groups = c("animal_id", "treatment"),
#'   polygons_sf = paddocks,
#'   polygon_label_col = "paddock_name",
#'   polygon_group = "Paddocks"
#' )
#' }
#' @export
gps_map <- function(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  groups = NULL,
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
  state_legend_title = "State",
  timeline = FALSE,
  polygons_sf = NULL,
  polygon_label_col = NULL,
  polygon_group = "Polygons",
  polygon_color = "#03F",
  polygon_weight = 5,
  polygon_opacity = 0.5,
  polygon_fill = TRUE,
  polygon_fill_opacity = 0.2,
  layer_control = TRUE,
  popup_fields = c("sensor_id", "datetime"),
  provider = "Esri.WorldImagery",
  point_radius = 3,
  point_opacity = 0.7,
  max_points = NULL,
  max_groups = NULL,
  seed = 1,
  point_warning_n = 5000L,
  group_warning_n = 20L,
  warnings = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("`gps_map()` requires the `leaflet` package.", call. = FALSE)
  }

  if (!is.character(lon) || length(lon) != 1L) {
    stop("`lon` must be a single column name.", call. = FALSE)
  }
  if (!is.character(lat) || length(lat) != 1L) {
    stop("`lat` must be a single column name.", call. = FALSE)
  }
  if (!is.character(datetime) || length(datetime) != 1L) {
    stop("`datetime` must be a single column name.", call. = FALSE)
  }
  if (!is.null(groups)) {
    if (!is.character(groups) || length(groups) < 1L || any(is.na(groups)) || any(trimws(groups) == "")) {
      stop("`groups` must be NULL or a character vector of one or more column names.", call. = FALSE)
    }
    groups <- unique(groups)
  }
  if (!is.null(state_col)) {
    if (!is.character(state_col) || length(state_col) != 1L || is.na(state_col) || trimws(state_col) == "") {
      stop("`state_col` must be NULL or a single non-empty column name.", call. = FALSE)
    }
  }
  if (!is.character(state_legend_title) || length(state_legend_title) != 1L || is.na(state_legend_title)) {
    stop("`state_legend_title` must be a single string.", call. = FALSE)
  }
  if (!is.character(state_colors) || length(state_colors) < 1L) {
    stop("`state_colors` must be a named character vector of colors.", call. = FALSE)
  }
  if (!is.character(polygon_group) || length(polygon_group) != 1L || is.na(polygon_group) || trimws(polygon_group) == "") {
    stop("`polygon_group` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.character(polygon_color) || length(polygon_color) != 1L || is.na(polygon_color) || trimws(polygon_color) == "") {
    stop("`polygon_color` must be a single colour value.", call. = FALSE)
  }
  if (!is.numeric(polygon_weight) || length(polygon_weight) != 1L || !is.finite(polygon_weight) || polygon_weight < 0) {
    stop("`polygon_weight` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(polygon_opacity) || length(polygon_opacity) != 1L || !is.finite(polygon_opacity) || polygon_opacity < 0 || polygon_opacity > 1) {
    stop("`polygon_opacity` must be in [0, 1].", call. = FALSE)
  }
  if (!is.logical(polygon_fill) || length(polygon_fill) != 1L || is.na(polygon_fill)) {
    stop("`polygon_fill` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(polygon_fill_opacity) || length(polygon_fill_opacity) != 1L || !is.finite(polygon_fill_opacity) || polygon_fill_opacity < 0 || polygon_fill_opacity > 1) {
    stop("`polygon_fill_opacity` must be in [0, 1].", call. = FALSE)
  }
  if (!is.logical(layer_control) || length(layer_control) != 1L || is.na(layer_control)) {
    stop("`layer_control` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(names(state_colors)) || any(is.na(names(state_colors))) || any(trimws(names(state_colors)) == "")) {
    stop("`state_colors` must be named (e.g., c(inactive = '#d7191c', active = '#1a9641')).", call. = FALSE)
  }

  if (!is.null(max_points) && (!is.numeric(max_points) || length(max_points) != 1L || max_points < 1)) {
    stop("`max_points` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_groups) && (!is.numeric(max_groups) || length(max_groups) != 1L || max_groups < 1)) {
    stop("`max_groups` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_groups) && is.null(groups)) {
    stop("`max_groups` requires `groups` to be supplied.", call. = FALSE)
  }

  if (!is.numeric(point_warning_n) || length(point_warning_n) != 1L || point_warning_n < 1) {
    stop("`point_warning_n` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(group_warning_n) || length(group_warning_n) != 1L || group_warning_n < 1) {
    stop("`group_warning_n` must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(warnings) || length(warnings) != 1L) {
    stop("`warnings` must be TRUE or FALSE.", call. = FALSE)
  }

  grz_validate_map_inputs(data, lon = lon, lat = lat, datetime = datetime, groups = groups, state_col = state_col)
  polygons <- grz_prepare_map_polygons(polygons_sf, polygon_label_col = polygon_label_col)

  dat <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  dat[[lon]] <- suppressWarnings(as.numeric(dat[[lon]]))
  dat[[lat]] <- suppressWarnings(as.numeric(dat[[lat]]))
  dat[[datetime]] <- grz_parse_datetime_utc(dat[[datetime]])

  valid <- !is.na(dat[[datetime]]) &
    is.finite(dat[[lon]]) & dat[[lon]] >= -180 & dat[[lon]] <= 180 &
    is.finite(dat[[lat]]) & dat[[lat]] >= -90 & dat[[lat]] <= 90

  if (!all(valid)) {
    warning(sum(!valid), " rows removed due to invalid datetime/lon/lat values.", call. = FALSE)
  }
  dat <- dat[valid, , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("No valid rows available for mapping.", call. = FALSE)
  }

  dat <- dat[order(dat[[datetime]]), , drop = FALSE]
  group_col <- NULL
  group_title <- NULL
  state_info <- NULL

  if (!is.null(groups)) {
    dat$.grz_group <- grz_make_group_id(dat, groups)
    group_col <- ".grz_group"
    group_title <- paste(groups, collapse = " + ")
  }
  if (!is.null(state_col)) {
    state_info <- grz_make_state_palette(dat[[state_col]], state_colors = state_colors)
    dat$.grz_state <- state_info$values
  }

  if (!is.null(max_groups) && !is.null(group_col)) {
    dat <- grz_select_groups(
      data = dat,
      group_col = group_col,
      max_groups = as.integer(max_groups),
      seed = seed
    )
  }

  if (!is.null(max_points) && nrow(dat) > as.integer(max_points)) {
    if (!is.null(group_col)) {
      dat <- grz_sample_points_by_group(
        data = dat,
        group_col = group_col,
        max_points = as.integer(max_points),
        seed = seed
      )
    } else {
      dat <- grz_sample_points(
        data = dat,
        max_points = as.integer(max_points),
        seed = seed
      )
    }
  }

  if (isTRUE(warnings) && nrow(dat) > as.integer(point_warning_n)) {
    warning(
      "gps_map() will render ", format(nrow(dat), big.mark = ","),
      " points. This may take some time. ",
      "Use `max_groups =` or `max_points =` to subset your data and reduce the number of points.",
      call. = FALSE,
      immediate. = TRUE
    )
    grz_confirm_continue(
      "Map creation halted for large point count. Set `warnings = FALSE` to continue."
    )
  }

  if (!is.null(group_col)) {
    n_groups <- length(unique(dat[[group_col]]))
    if (isTRUE(warnings) && n_groups > as.integer(group_warning_n)) {
      warning(
        "gps_map() will render ", format(n_groups, big.mark = ","),
        " group levels. This may take some time and may be hard to interpret. ",
        "Use `max_groups =` or `max_points =` to subset your data and reduce the number of points.",
        call. = FALSE,
        immediate. = TRUE
      )
      grz_confirm_continue(
        "Map creation halted for high group count. Set `warnings = FALSE` to continue."
      )
    }
  }

  dat <- dat[order(dat[[datetime]]), , drop = FALSE]
  animal_layers <- if (is.null(group_col)) character() else unique(as.character(dat[[group_col]]))
  if (!is.null(polygons) && polygon_group %in% animal_layers) {
    stop("`polygon_group` must not duplicate an animal group layer name.", call. = FALSE)
  }
  if (!is.null(state_col)) {
    state_info <- grz_make_state_palette(dat$.grz_state, state_colors = state_colors)
  }
  popup <- grz_make_popup(dat, fields = popup_fields)

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addProviderTiles(map, provider = provider)

  if (!is.null(polygons)) {
    polygon_labels <- if (is.null(polygon_label_col)) NULL else as.character(polygons[[polygon_label_col]])
    map <- leaflet::addPolygons(
      map,
      data = polygons,
      group = polygon_group,
      stroke = TRUE,
      color = polygon_color,
      weight = polygon_weight,
      opacity = polygon_opacity,
      fill = polygon_fill,
      fillColor = polygon_color,
      fillOpacity = polygon_fill_opacity,
      label = polygon_labels
    )
  }

  if (!is.null(state_col)) {
    point_colors <- state_info$colors
    map <- leaflet::addLegend(
      map,
      position = "bottomright",
      colors = state_info$legend_colors,
      labels = state_info$legend_labels,
      title = state_legend_title,
      opacity = point_opacity
    )
  } else if (is.null(group_col)) {
    point_colors <- rep("#2b8cbe", nrow(dat))
  } else {
    pal <- grz_make_group_palette(dat[[group_col]])
    point_colors <- pal(dat[[group_col]])
    map <- leaflet::addLegend(
      map,
      position = "bottomright",
      pal = pal,
      values = dat[[group_col]],
      title = group_title
    )
  }

  if (isTRUE(timeline)) {
    if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
      stop("`timeline = TRUE` requires the `leaflet.extras2` package.", call. = FALSE)
    }
    if (!requireNamespace("yyjsonr", quietly = TRUE)) {
      stop("`timeline = TRUE` requires the `yyjsonr` package.", call. = FALSE)
    }
    sf_dat <- sf::st_as_sf(dat, coords = c(lon, lat), crs = 4326, remove = FALSE)
    sf_dat$time <- format(dat[[datetime]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    sf_dat$.grz_popup <- popup
    sf_dat$.grz_color <- point_colors

    map <- leaflet.extras2::addTimeslider(
      map,
      data = sf_dat,
      radius = point_radius,
      stroke = FALSE,
      fill = TRUE,
      fillOpacity = point_opacity,
      color = ~.grz_color,
      popup = ~.grz_popup,
      options = leaflet.extras2::timesliderOptions(
        position = "topright",
        timeAttribute = "time",
        range = TRUE,
        minValue = 0,
        maxValue = -1,
        showAllOnStart = TRUE
      )
    )

    if (!is.null(group_col)) {
      if (!requireNamespace("htmltools", quietly = TRUE)) {
        stop("Grouped timelines require the `htmltools` package.", call. = FALSE)
      }
      timeline_layer_ids <- paste0("grz_timeline_", seq_len(nrow(dat)))
      map <- leaflet::addCircleMarkers(
        map,
        lng = dat[[lon]],
        lat = dat[[lat]],
        layerId = timeline_layer_ids,
        group = dat[[group_col]],
        radius = point_radius,
        stroke = FALSE,
        fillColor = point_colors,
        fillOpacity = point_opacity,
        popup = popup
      )
      map <- grz_add_grouped_timeslider(
        map,
        times = sf_dat$time,
        layer_ids = timeline_layer_ids,
        groups = dat[[group_col]]
      )
    }
  } else {
    map <- leaflet::addCircleMarkers(
      map,
      lng = dat[[lon]],
      lat = dat[[lat]],
      group = if (is.null(group_col)) NULL else dat[[group_col]],
      radius = point_radius,
      stroke = FALSE,
      fillColor = point_colors,
      fillOpacity = point_opacity,
      popup = popup
    )
  }

  overlay_layers <- unique(c(animal_layers, if (!is.null(polygons)) polygon_group else character()))
  if (isTRUE(layer_control) && length(overlay_layers) > 0L) {
    map <- leaflet::addLayersControl(
      map,
      overlayGroups = overlay_layers,
      options = leaflet::layersControlOptions(collapsed = TRUE)
    )
    map <- grz_add_layer_deselect_all(map, overlay_groups = overlay_layers)
    if (isTRUE(timeline)) {
      map <- grz_offset_timeline_layer_control(map)
    }
  }

  bounds_lon <- dat[[lon]]
  bounds_lat <- dat[[lat]]
  if (!is.null(polygons)) {
    polygon_bounds <- sf::st_bbox(polygons)
    bounds_lon <- c(bounds_lon, unname(polygon_bounds[c("xmin", "xmax")]))
    bounds_lat <- c(bounds_lat, unname(polygon_bounds[c("ymin", "ymax")]))
  }
  map <- leaflet::fitBounds(
    map,
    lng1 = min(bounds_lon, na.rm = TRUE),
    lat1 = min(bounds_lat, na.rm = TRUE),
    lng2 = max(bounds_lon, na.rm = TRUE),
    lat2 = max(bounds_lat, na.rm = TRUE)
  )

  map
}

#' Playback GPS tracks on a leaflet timeline with tails
#'
#' Creates an interactive leaflet htmlwidget with timeline playback and
#' track tails inspired by moveVis-style animations. This function is
#' presentation-oriented and avoids Shiny.
#'
#' Inspiration: the playback/tail design was informed by the moveVis package
#' (\url{https://github.com/16EAGLE/moveVis}).
#'
#' @param data Data frame containing GPS rows.
#' @param lon Name of longitude column.
#' @param lat Name of latitude column.
#' @param datetime Name of datetime column.
#' @param groups Optional grouping columns for track identity and colouring.
#'   If NULL, `sensor_id` is used when present.
#' @param color_by Color mode. `"group"` (default) colors by track; `"state"`
#'   colors by `state_col`.
#' @param state_col Optional state column used when `color_by = "state"`.
#' @param state_colors Named colors for state levels.
#' @param align Logical; if `TRUE`, calls `gps_interpolate()` before playback.
#' @param align_interval_mins Interval passed to `gps_interpolate(interval_mins = )`.
#' @param align_keep_extra Logical; passed to `gps_interpolate(keep_extra = )`.
#' @param tail_points Tail length expressed as number of aligned points.
#'   Ignored when `tail_minutes` is supplied.
#' @param tail_minutes Tail length in minutes. If NULL, inferred from
#'   `tail_points`.
#' @param show_points Logical; draw moving point markers.
#' @param point_radius Point marker radius.
#' @param point_opacity Point marker opacity.
#' @param point_size_slider Logical; show an on-map slider to control point
#'   radius during playback.
#' @param point_size_min Minimum value for the point-size slider.
#' @param point_size_max Maximum value for the point-size slider.
#' @param slider_position Position of timeline controls.
#' @param playback_controls Logical; show play/pause/step controls.
#' @param playback_steps Number of playback steps for the timeline.
#' @param playback_duration_ms Minimum playback duration in milliseconds.
#' @param show_ticks Logical; show timeline ticks.
#' @param enable_keyboard_controls Logical; allow keyboard control of playback.
#' @param wait_to_update_map Logical; if `TRUE`, redraw map only after slider
#'   interaction ends.
#' @param provider Tile provider for `leaflet::addProviderTiles()`.
#' @param popup_fields Fields to include in popups.
#' @param show_legend Logical; add legend.
#' @param group_palette HCL palette name used for group colours.
#' @param max_rows Optional maximum rows to render after alignment. If set and
#'   exceeded, rows are sampled across groups.
#' @param render_every_n Keep every n-th aligned row per group (>= 1) before
#'   building timeline features. Useful for faster rendering.
#' @param seed Random seed for sampling.
#' @param warnings Logical; print warnings for large payloads.
#' @param progress Logical; print stage progress and loop progress bars while
#'   building playback features.
#' @param show_loading_overlay Logical; show an on-map loading overlay until
#'   timeline layers are ready.
#'
#' @return A `leaflet` htmlwidget.
#' @export
gps_playback <- function(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  groups = NULL,
  color_by = c("group", "state"),
  state_col = NULL,
  state_colors = c(inactive = "#d7191c", active = "#1a9641"),
  align = TRUE,
  align_interval_mins = "base",
  align_keep_extra = TRUE,
  tail_points = 19L,
  tail_minutes = NULL,
  show_points = TRUE,
  point_radius = 4,
  point_opacity = 0.9,
  point_size_slider = TRUE,
  point_size_min = 1,
  point_size_max = 25,
  slider_position = "bottomleft",
  playback_controls = TRUE,
  playback_steps = 1000L,
  playback_duration_ms = 10000L,
  show_ticks = FALSE,
  enable_keyboard_controls = TRUE,
  wait_to_update_map = FALSE,
  provider = "Esri.WorldImagery",
  popup_fields = c("sensor_id", "datetime"),
  show_legend = TRUE,
  group_palette = "Dark 3",
  max_rows = NULL,
  render_every_n = 1L,
  seed = 1,
  warnings = TRUE,
  progress = TRUE,
  show_loading_overlay = TRUE
) {
  color_by <- match.arg(color_by)
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  needed_pkgs <- c("leaflet", "leaftime", "htmlwidgets")
  missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    stop(
      "Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
      ". Install with install.packages().",
      call. = FALSE
    )
  }

  if (!is.character(lon) || length(lon) != 1L || is.na(lon) || trimws(lon) == "") {
    stop("`lon` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.character(lat) || length(lat) != 1L || is.na(lat) || trimws(lat) == "") {
    stop("`lat` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.character(datetime) || length(datetime) != 1L || is.na(datetime) || trimws(datetime) == "") {
    stop("`datetime` must be a single non-empty column name.", call. = FALSE)
  }
  if (!is.logical(align) || length(align) != 1L) {
    stop("`align` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(align_keep_extra) || length(align_keep_extra) != 1L) {
    stop("`align_keep_extra` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(tail_points) || length(tail_points) != 1L || !is.finite(tail_points) || tail_points < 1) {
    stop("`tail_points` must be a single number >= 1.", call. = FALSE)
  }
  if (!is.null(tail_minutes) && (!is.numeric(tail_minutes) || length(tail_minutes) != 1L || !is.finite(tail_minutes) || tail_minutes <= 0)) {
    stop("`tail_minutes` must be NULL or a single number > 0.", call. = FALSE)
  }
  if (!is.numeric(point_radius) || length(point_radius) != 1L || point_radius <= 0) {
    stop("`point_radius` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(point_opacity) || length(point_opacity) != 1L || point_opacity < 0 || point_opacity > 1) {
    stop("`point_opacity` must be in [0, 1].", call. = FALSE)
  }
  if (!is.logical(point_size_slider) || length(point_size_slider) != 1L) {
    stop("`point_size_slider` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(point_size_min) || length(point_size_min) != 1L || !is.finite(point_size_min) || point_size_min <= 0) {
    stop("`point_size_min` must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(point_size_max) || length(point_size_max) != 1L || !is.finite(point_size_max) || point_size_max <= 0) {
    stop("`point_size_max` must be a positive number.", call. = FALSE)
  }
  if (point_size_min >= point_size_max) {
    stop("`point_size_min` must be smaller than `point_size_max`.", call. = FALSE)
  }
  if (point_radius < point_size_min || point_radius > point_size_max) {
    stop("`point_radius` must be within [`point_size_min`, `point_size_max`].", call. = FALSE)
  }
  if (!is.character(slider_position) || length(slider_position) != 1L || is.na(slider_position)) {
    stop("`slider_position` must be a single string.", call. = FALSE)
  }
  slider_position <- match.arg(slider_position, c("topleft", "topright", "bottomleft", "bottomright"))
  if (!is.logical(playback_controls) || length(playback_controls) != 1L) {
    stop("`playback_controls` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(playback_steps) || length(playback_steps) != 1L || !is.finite(playback_steps) || playback_steps < 10) {
    stop("`playback_steps` must be a single number >= 10.", call. = FALSE)
  }
  if (!is.numeric(playback_duration_ms) || length(playback_duration_ms) != 1L || !is.finite(playback_duration_ms) || playback_duration_ms <= 0) {
    stop("`playback_duration_ms` must be a single number > 0.", call. = FALSE)
  }
  if (!is.logical(show_ticks) || length(show_ticks) != 1L) {
    stop("`show_ticks` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(enable_keyboard_controls) || length(enable_keyboard_controls) != 1L) {
    stop("`enable_keyboard_controls` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(wait_to_update_map) || length(wait_to_update_map) != 1L) {
    stop("`wait_to_update_map` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_points) || length(show_points) != 1L) {
    stop("`show_points` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_legend) || length(show_legend) != 1L) {
    stop("`show_legend` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(group_palette) || length(group_palette) != 1L || is.na(group_palette) || trimws(group_palette) == "") {
    stop("`group_palette` must be a single palette name.", call. = FALSE)
  }
  if (!is.null(max_rows) && (!is.numeric(max_rows) || length(max_rows) != 1L || !is.finite(max_rows) || max_rows < 100)) {
    stop("`max_rows` must be NULL or a single number >= 100.", call. = FALSE)
  }
  if (!is.numeric(render_every_n) || length(render_every_n) != 1L || !is.finite(render_every_n) || render_every_n < 1 || render_every_n %% 1 != 0) {
    stop("`render_every_n` must be a single integer >= 1.", call. = FALSE)
  }
  render_every_n <- as.integer(render_every_n)
  if (!is.logical(warnings) || length(warnings) != 1L) {
    stop("`warnings` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(progress) || length(progress) != 1L) {
    stop("`progress` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_loading_overlay) || length(show_loading_overlay) != 1L) {
    stop("`show_loading_overlay` must be TRUE or FALSE.", call. = FALSE)
  }
  if (color_by == "state") {
    if (is.null(state_col) || !is.character(state_col) || length(state_col) != 1L || is.na(state_col) || trimws(state_col) == "") {
      stop("`state_col` must be a single column name when `color_by = \"state\"`.", call. = FALSE)
    }
  }

  stage_total <- if (isTRUE(align)) 9L else 8L
  stage_i <- 0L
  stage_msg <- function(text) {
    if (!isTRUE(progress)) {
      return(invisible(NULL))
    }
    stage_i <<- stage_i + 1L
    cat("[gps_playback] Stage ", stage_i, "/", stage_total, ": ", text, "\n", sep = "")
    utils::flush.console()
    invisible(NULL)
  }

  stage_msg("Preparing input data")
  dat <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  group_cols <- grz_guess_group_cols(dat, groups = groups)
  if (is.null(group_cols)) {
    dat$.grz_group <- "track_1"
    group_cols <- ".grz_group"
  } else if (length(group_cols) == 1L) {
    dat$.grz_group <- as.character(dat[[group_cols]])
  } else {
    dat$.grz_group <- grz_make_group_id(dat, group_cols)
  }
  dat$.grz_group[is.na(dat$.grz_group) | trimws(dat$.grz_group) == ""] <- "(missing)"

  grz_validate_map_inputs(
    data = dat,
    lon = lon,
    lat = lat,
    datetime = datetime,
    groups = NULL,
    state_col = if (color_by == "state") state_col else NULL
  )

  dat[[lon]] <- suppressWarnings(as.numeric(dat[[lon]]))
  dat[[lat]] <- suppressWarnings(as.numeric(dat[[lat]]))
  dat[[datetime]] <- grz_parse_datetime_utc(dat[[datetime]])

  valid <- !is.na(dat[[datetime]]) &
    is.finite(dat[[lon]]) & dat[[lon]] >= -180 & dat[[lon]] <= 180 &
    is.finite(dat[[lat]]) & dat[[lat]] >= -90 & dat[[lat]] <= 90
  if (!all(valid) && isTRUE(warnings)) {
    warning(sum(!valid), " rows removed due to invalid datetime/lon/lat values.", call. = FALSE)
  }
  dat <- dat[valid, , drop = FALSE]
  if (nrow(dat) < 2L) {
    stop("Need at least 2 valid rows to build playback.", call. = FALSE)
  }

  if (isTRUE(align)) {
    stage_msg("Aligning timestamps")
    if (!("sensor_id" %in% names(dat))) {
      dat$sensor_id <- dat$.grz_group
    }
    dat <- gps_interpolate(
      data = dat,
      interval_mins = align_interval_mins,
      groups = ".grz_group",
      keep_extra = align_keep_extra,
      verbose = FALSE,
      return_class = "data.frame"
    )
    dat[[datetime]] <- grz_parse_datetime_utc(dat[[datetime]])
  }

  stage_msg("Sorting and filtering aligned rows")
  dat <- dat[order(dat$.grz_group, dat[[datetime]]), , drop = FALSE]
  dat <- dat[!is.na(dat[[datetime]]) & is.finite(dat[[lon]]) & is.finite(dat[[lat]]), , drop = FALSE]
  if (nrow(dat) < 2L) {
    stop("Need at least 2 valid rows after alignment/filtering.", call. = FALSE)
  }

  stage_msg("Applying sampling and thinning")
  if (!is.null(max_rows) && nrow(dat) > as.integer(max_rows)) {
    dat <- grz_sample_points_by_group(
      data = dat,
      group_col = ".grz_group",
      max_points = as.integer(max_rows),
      seed = seed
    )
    dat <- dat[order(dat$.grz_group, dat[[datetime]]), , drop = FALSE]
    if (isTRUE(warnings)) {
      warning("Playback data sampled to ", nrow(dat), " rows via `max_rows`.", call. = FALSE)
    }
  }

  if (render_every_n > 1L) {
    idx_keep <- unlist(
      lapply(split(seq_len(nrow(dat)), dat$.grz_group, drop = TRUE), function(idx) {
        idx[seq(1L, length(idx), by = render_every_n)]
      }),
      use.names = FALSE
    )
    idx_keep <- sort(unique(idx_keep))
    dat <- dat[idx_keep, , drop = FALSE]
    if (isTRUE(warnings)) {
      warning("Playback data thinned to ", nrow(dat), " rows via `render_every_n = ", render_every_n, "`.", call. = FALSE)
    }
  }

  stage_msg("Computing timing and color mappings")
  diff_secs <- as.numeric(diff(sort(unique(as.numeric(dat[[datetime]])))))
  diff_secs <- diff_secs[is.finite(diff_secs) & diff_secs > 0]
  base_sec <- if (length(diff_secs) == 0L) 60 else as.numeric(stats::median(diff_secs, na.rm = TRUE))
  if (!is.finite(base_sec) || base_sec <= 0) {
    base_sec <- 60
  }

  tail_ms <- if (!is.null(tail_minutes)) {
    as.numeric(tail_minutes) * 60 * 1000
  } else {
    as.numeric(tail_points) * base_sec * 1000
  }
  if (!is.finite(tail_ms) || tail_ms <= 0) {
    tail_ms <- as.numeric(tail_points) * base_sec * 1000
  }
  tail_weight_fixed <- 3
  tail_opacity_fixed <- 0.85
  point_hold_ms <- max(1, as.integer(round(base_sec * 1000)))

  if (color_by == "state") {
    state_info <- grz_make_state_palette(dat[[state_col]], state_colors = state_colors)
    dat$.grz_color <- state_info$colors
    legend_colors <- state_info$legend_colors
    legend_labels <- state_info$legend_labels
    legend_title <- state_col
  } else {
    group_levels <- unique(dat$.grz_group)
    cols <- grDevices::hcl.colors(max(3L, length(group_levels)), palette = group_palette)
    pal <- leaflet::colorFactor(cols, domain = group_levels)
    dat$.grz_color <- pal(dat$.grz_group)
    legend_labels <- group_levels
    legend_colors <- pal(group_levels)
    legend_title <- if (!is.null(group_cols)) paste(group_cols, collapse = " + ") else "Group"
  }

  popup <- grz_make_popup(dat, fields = popup_fields)
  time_ms <- as.numeric(dat[[datetime]]) * 1000

  # Persist each point until the next observation in the same group to reduce
  # visual flicker during timeline playback.
  point_end_ms <- rep(NA_real_, nrow(dat))
  split_idx <- split(seq_len(nrow(dat)), dat$.grz_group, drop = TRUE)
  for (idx in split_idx) {
    if (length(idx) == 0L) {
      next
    }
    idx <- idx[order(dat[[datetime]][idx])]
    if (length(idx) == 1L) {
      point_end_ms[idx] <- time_ms[idx] + point_hold_ms
      next
    }
    for (j in seq_len(length(idx) - 1L)) {
      i_cur <- idx[[j]]
      i_next <- idx[[j + 1L]]
      point_end_ms[[i_cur]] <- max(time_ms[[i_next]], time_ms[[i_cur]] + point_hold_ms)
    }
    i_last <- idx[[length(idx)]]
    point_end_ms[[i_last]] <- time_ms[[i_last]] + point_hold_ms
  }

  stage_msg("Building point features")
  point_features <- list()
  if (isTRUE(show_points)) {
    n_points <- nrow(dat)
    point_features <- vector("list", n_points)
    pb_points <- NULL
    pb_tick <- max(1L, as.integer(round(n_points / 200)))
    if (isTRUE(progress) && n_points >= 1000L) {
      pb_points <- utils::txtProgressBar(min = 0, max = n_points, style = 3)
    }

    for (i in seq_len(n_points)) {
      point_features[[i]] <- list(
        type = "Feature",
        properties = list(
          start = time_ms[[i]],
          end = point_end_ms[[i]],
          color = dat$.grz_color[[i]],
          radius = as.numeric(point_radius),
          fillOpacity = as.numeric(point_opacity),
          popup = popup[[i]]
        ),
        geometry = list(
          type = "Point",
          coordinates = list(as.numeric(dat[[lon]][[i]]), as.numeric(dat[[lat]][[i]]))
        )
      )
      if (!is.null(pb_points) && ((i %% pb_tick) == 0L || i == n_points)) {
        utils::setTxtProgressBar(pb_points, i)
      }
    }
    if (!is.null(pb_points)) {
      close(pb_points)
    }
  }

  stage_msg("Building tail features")
  line_features <- list()
  edge_count <- sum(vapply(split_idx, function(i) max(length(i) - 1L, 0L), integer(1)))
  pb_lines <- NULL
  pb_tick_lines <- max(1L, as.integer(round(max(edge_count, 1L) / 200)))
  if (isTRUE(progress) && edge_count >= 1000L) {
    pb_lines <- utils::txtProgressBar(min = 0, max = edge_count, style = 3)
  }
  line_capacity <- max(1L, edge_count)
  line_features <- vector("list", line_capacity)
  line_i <- 0L
  edge_i <- 0L

  for (idx in split_idx) {
    if (length(idx) < 2L) {
      next
    }
    idx <- idx[order(dat[[datetime]][idx])]
    for (j in 2:length(idx)) {
      edge_i <- edge_i + 1L
      i_prev <- idx[[j - 1L]]
      i_cur <- idx[[j]]
      if (!all(is.finite(c(dat[[lon]][[i_prev]], dat[[lat]][[i_prev]], dat[[lon]][[i_cur]], dat[[lat]][[i_cur]])))) {
        if (!is.null(pb_lines) && ((edge_i %% pb_tick_lines) == 0L || edge_i == edge_count)) {
          utils::setTxtProgressBar(pb_lines, edge_i)
        }
        next
      }
      seg_start <- time_ms[[i_cur]]
      seg_end <- time_ms[[i_cur]] + tail_ms
      if (!is.finite(seg_start) || !is.finite(seg_end) || seg_end <= seg_start) {
        if (!is.null(pb_lines) && ((edge_i %% pb_tick_lines) == 0L || edge_i == edge_count)) {
          utils::setTxtProgressBar(pb_lines, edge_i)
        }
        next
      }

      line_i <- line_i + 1L
      line_features[[line_i]] <- list(
        type = "Feature",
        properties = list(
          start = seg_start,
          end = seg_end,
          color = dat$.grz_color[[i_cur]],
          weight = tail_weight_fixed,
          opacity = tail_opacity_fixed
        ),
        geometry = list(
          type = "LineString",
          coordinates = list(
            list(as.numeric(dat[[lon]][[i_prev]]), as.numeric(dat[[lat]][[i_prev]])),
            list(as.numeric(dat[[lon]][[i_cur]]), as.numeric(dat[[lat]][[i_cur]]))
          )
        )
      )

      if (!is.null(pb_lines) && ((edge_i %% pb_tick_lines) == 0L || edge_i == edge_count)) {
        utils::setTxtProgressBar(pb_lines, edge_i)
      }
    }
  }
  if (!is.null(pb_lines)) {
    close(pb_lines)
  }
  if (line_i == 0L) {
    line_features <- list()
  } else if (line_i < length(line_features)) {
    line_features <- line_features[seq_len(line_i)]
  }

  features <- c(line_features, point_features)
  if (length(features) == 0L) {
    stop("No drawable playback features were created.", call. = FALSE)
  }
  geojson <- list(type = "FeatureCollection", features = features)

  timeline_point_to_layer <- htmlwidgets::JS(
    "function(feature, latlng) {
       var p = feature && feature.properties ? feature.properties : {};
       var col = p.color || '#2b8cbe';
       var rad = parseFloat(p.radius);
       if (!isFinite(rad)) rad = 4;
       var op = parseFloat(p.fillOpacity);
       if (!isFinite(op)) op = 0.9;
        return L.circleMarker(latlng, {
          radius: rad,
          stroke: true,
          weight: 1,
          color: col,
          fillColor: col,
          fillOpacity: op,
          opacity: op,
          playbackPoint: true
        });
      }"
  )
  timeline_style <- htmlwidgets::JS(
    "function(feature) {
       var p = feature && feature.properties ? feature.properties : {};
       var w = parseFloat(p.weight);
       if (!isFinite(w)) w = 3;
       var o = parseFloat(p.opacity);
       if (!isFinite(o)) o = 0.85;
       return {
         color: p.color || '#2b8cbe',
         weight: w,
         opacity: o,
         lineCap: 'round',
         lineJoin: 'round',
         playbackTail: true
       };
     }"
  )
  timeline_on_each <- htmlwidgets::JS(
    "function(feature, layer) {
       if (feature && feature.properties && feature.properties.popup) {
         layer.bindPopup(feature.properties.popup);
       }
     }"
  )
  timeline_format_output <- htmlwidgets::JS(
    "function(ms) {
       var d = new Date(ms);
       try {
         return d.toLocaleString();
       } catch (err) {
         return d.toISOString();
       }
     }"
  )

  slider_opts <- leaftime::sliderOptions(
    position = slider_position,
    formatOutput = timeline_format_output,
    enablePlayback = isTRUE(playback_controls),
    enableKeyboardControls = isTRUE(enable_keyboard_controls),
    steps = as.integer(round(playback_steps)),
    duration = as.integer(round(playback_duration_ms)),
    waitToUpdateMap = isTRUE(wait_to_update_map),
    showTicks = isTRUE(show_ticks)
  )
  timeline_opts <- leaftime::timelineOptions(
    pointToLayer = timeline_point_to_layer,
    style = timeline_style,
    drawOnSetTime = TRUE
  )
  timeline_opts$onEachFeature <- timeline_on_each

  stage_msg("Building leaflet widget")
  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(provider = provider) |>
    leaftime::addTimeline(
      geojson,
      timelineOpts = timeline_opts,
      sliderOpts = slider_opts,
      width = "100%"
    ) |>
    leaflet::fitBounds(
      lng1 = min(dat[[lon]], na.rm = TRUE),
      lat1 = min(dat[[lat]], na.rm = TRUE),
      lng2 = max(dat[[lon]], na.rm = TRUE),
      lat2 = max(dat[[lat]], na.rm = TRUE)
    )

  if (isTRUE(show_legend)) {
    map <- leaflet::addLegend(
      map,
      position = "topright",
      colors = legend_colors,
      labels = legend_labels,
      title = legend_title,
      opacity = 1
    )
  }

  stage_msg("Attaching interactive map controls")
  if (isTRUE(show_loading_overlay)) {
    loading_id <- paste0("grz_loading_", as.integer(stats::runif(1, 1, 1e9)))
    js_loading_overlay <- sprintf(
      "function(el, x) {
         var map = (this && typeof this.eachLayer === 'function')
           ? this
           : (this && typeof this.getMap === 'function' ? this.getMap() : null);
         if (!map) return;
         var root = el;
         if (!root) return;
         if (window.getComputedStyle(root).position === 'static') {
           root.style.position = 'relative';
         }

         var overlay = document.createElement('div');
         overlay.id = '%s';
         overlay.style.position = 'absolute';
         overlay.style.left = '0';
         overlay.style.top = '0';
         overlay.style.right = '0';
         overlay.style.bottom = '0';
         overlay.style.zIndex = '9999';
         overlay.style.display = 'flex';
         overlay.style.alignItems = 'center';
         overlay.style.justifyContent = 'center';
         overlay.style.background = 'rgba(0, 0, 0, 0.35)';
         overlay.style.pointerEvents = 'none';
         overlay.style.transition = 'opacity 180ms ease';
         overlay.innerHTML = '<div style=\"background:#111;color:#fff;padding:10px 14px;border-radius:6px;font-size:13px;font-weight:600;box-shadow:0 1px 4px rgba(0,0,0,0.4);\">Loading playback layers...</div>';
         root.appendChild(overlay);

         var hidden = false;
         function hideOverlay() {
           if (hidden) return;
           hidden = true;
           overlay.style.opacity = '0';
           window.setTimeout(function() {
             if (overlay && overlay.parentNode) {
               overlay.parentNode.removeChild(overlay);
             }
           }, 220);
         }

         function hasPlaybackLayers() {
           var found = false;
           map.eachLayer(function(layer) {
             var o = layer && layer.options ? layer.options : null;
             if (o && (o.playbackPoint === true || o.playbackTail === true)) {
               found = true;
             }
           });
           return found;
         }

         map.on('layeradd', function(e) {
           var o = e.layer && e.layer.options ? e.layer.options : null;
           if (o && (o.playbackPoint === true || o.playbackTail === true)) {
             hideOverlay();
           }
         });

         var t0 = Date.now();
         var timer = window.setInterval(function() {
           if (hasPlaybackLayers()) {
             window.clearInterval(timer);
             hideOverlay();
             return;
           }
           if ((Date.now() - t0) > 15000) {
             window.clearInterval(timer);
             hideOverlay();
           }
         }, 200);

         map.whenReady(function() {
           window.setTimeout(function() {
             if (hasPlaybackLayers()) {
               hideOverlay();
             }
           }, 120);
         });
       }",
      loading_id
    )
    map <- htmlwidgets::onRender(map, js_loading_overlay)
  }

  if (isTRUE(point_size_slider) && isTRUE(show_points)) {
    slider_id <- paste0("grz_point_size_", as.integer(stats::runif(1, 1, 1e9)))
    slider_wrap_id <- paste0(slider_id, "_wrap")

    slider_html <- paste0(
      "<div id='", slider_wrap_id, "' class='leaflet-bar' ",
      "style='background:white;padding:6px 8px;min-width:170px;font-size:12px;'>",
      "<div style='margin-bottom:4px;font-weight:600;'>Point size</div>",
      "<input id='", slider_id, "' type='range' min='", format(as.numeric(point_size_min), scientific = FALSE),
      "' max='", format(as.numeric(point_size_max), scientific = FALSE),
      "' step='1' value='", format(as.numeric(point_radius), scientific = FALSE),
      "' style='width:150px;'/>",
      "</div>"
    )
    map <- leaflet::addControl(
      map,
      html = slider_html,
      position = "topleft"
    )

    js_point_slider <- sprintf(
      "function(el, x) {
         var map = (this && typeof this.eachLayer === 'function')
           ? this
           : (this && typeof this.getMap === 'function' ? this.getMap() : null);
         if (!map) return;
         var currentRadius = %s;
         var sliderId = '%s';
         var wrapId = '%s';

         function applyRadius(r) {
           currentRadius = r;
           map.eachLayer(function(layer) {
             if (layer && layer.options && layer.options.playbackPoint === true && typeof layer.setRadius === 'function') {
               layer.setRadius(currentRadius);
             }
           });
         }

         map.on('layeradd', function(e) {
           var layer = e.layer;
           if (layer && layer.options && layer.options.playbackPoint === true && typeof layer.setRadius === 'function') {
             layer.setRadius(currentRadius);
           }
         });

         function bindSlider() {
           var input = document.getElementById(sliderId);
           if (!input) {
             setTimeout(bindSlider, 150);
             return;
           }
           var wrap = document.getElementById(wrapId);
           if (wrap && window.L && L.DomEvent) {
             L.DomEvent.disableClickPropagation(wrap);
             L.DomEvent.disableScrollPropagation(wrap);
           }
           input.addEventListener('input', function(ev) {
             var v = parseFloat(ev.target.value);
             if (!isFinite(v)) return;
             applyRadius(v);
           });
           applyRadius(currentRadius);
         }
         bindSlider();
       }",
      format(as.numeric(point_radius), scientific = FALSE),
      slider_id,
      slider_wrap_id
    )
    map <- htmlwidgets::onRender(map, js_point_slider)
  }

  map
}
