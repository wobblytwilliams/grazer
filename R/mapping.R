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

grz_make_block_group <- function(data, block_cols) {
  parts <- lapply(block_cols, function(col) {
    value <- as.character(data[[col]])
    value[is.na(value) | trimws(value) == ""] <- "(missing)"
    value
  })

  do.call(paste, c(parts, sep = " | "))
}

grz_make_block_palette <- function(values) {
  n <- length(unique(values))
  cols <- grDevices::hcl.colors(max(3L, n), palette = "Dark 3")
  leaflet::colorFactor(palette = cols, domain = values)
}

grz_select_block_groups <- function(data, block_col, max_blocks, seed) {
  groups <- unique(as.character(data[[block_col]]))
  if (length(groups) <= max_blocks) {
    return(data)
  }

  set.seed(seed)
  keep_groups <- sample(groups, max_blocks)
  data[data[[block_col]] %in% keep_groups, , drop = FALSE]
}

grz_sample_points <- function(data, max_points, seed) {
  if (nrow(data) <= max_points) {
    return(data)
  }

  set.seed(seed)
  idx <- sample.int(nrow(data), max_points)
  data[idx, , drop = FALSE]
}

grz_sample_points_by_block <- function(data, block_col, max_points, seed) {
  if (nrow(data) <= max_points) {
    return(data)
  }

  groups <- split(seq_len(nrow(data)), as.character(data[[block_col]]), drop = TRUE)
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

grz_validate_map_inputs <- function(data, lon, lat, datetime, block) {
  needed <- c(lon, lat, datetime)
  if (!is.null(block)) {
    needed <- c(needed, block)
  }

  missing <- setdiff(unique(needed), names(data))
  if (length(missing) > 0) {
    stop("Missing required columns for mapping: ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

grz_prompt_continue <- function(stop_message) {
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

#' Plot Cattle GPS Fixes on an Interactive Leaflet Map
#'
#' Creates a leaflet map of GPS fixes with optional grouping by a blocking
#' variable and optional timeline playback via `leaflet.extras2`.
#'
#' @param data Data frame containing GPS points.
#' @param lon Name of longitude column.
#' @param lat Name of latitude column.
#' @param datetime Name of datetime column.
#' @param block Deprecated alias for `group`.
#' @param group Optional grouping column(s) for colour/layer separation.
#' @param timeline Logical; if `TRUE`, render points with an interactive time slider.
#' @param popup_fields Character vector of fields to show in marker popups.
#' @param provider Tile provider name passed to `leaflet::addProviderTiles()`.
#' @param point_radius Marker radius.
#' @param point_opacity Marker opacity.
#' @param max_points Optional max number of points to render (random sample).
#' @param max_blocks Optional maximum number of block groups to plot.
#' @param sample_n Deprecated alias for `max_points`.
#' @param max_block Deprecated alias for `max_blocks`.
#' @param seed Random seed used for sampling.
#' @param large_n Threshold for large map warning/confirmation.
#' @param block_n Threshold for number of block groups warning/confirmation.
#' @param warnings Logical; if `FALSE`, bypasses map-size warning prompts.
#'
#' @return A `leaflet` htmlwidget.
#' @export
grz_map <- function(
  data,
  lon = "lon",
  lat = "lat",
  datetime = "datetime",
  block = NULL,
  group = NULL,
  timeline = FALSE,
  popup_fields = c("sensor_id", "datetime"),
  provider = "Esri.WorldImagery",
  point_radius = 3,
  point_opacity = 0.7,
  max_points = NULL,
  max_blocks = NULL,
  sample_n = NULL,
  max_block = NULL,
  seed = 1,
  large_n = 5000L,
  block_n = 20L,
  warnings = TRUE
) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("`grz_map()` requires the `leaflet` package.", call. = FALSE)
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
  if (!is.null(group)) {
    if (!is.character(group) || length(group) < 1L || any(is.na(group)) || any(trimws(group) == "")) {
      stop("`group` must be NULL or a character vector of one or more column names.", call. = FALSE)
    }
    if (!is.null(block) && !identical(unique(block), unique(group))) {
      stop("Supply either `group` (preferred) or `block`, not both with different values.", call. = FALSE)
    }
    block <- group
  }
  if (!is.null(block)) {
    if (!is.character(block) || length(block) < 1L || any(is.na(block)) || any(trimws(block) == "")) {
      stop("`block` must be NULL or a character vector of one or more column names.", call. = FALSE)
    }
    block <- unique(block)
  }

  if (!is.null(sample_n) && (!is.numeric(sample_n) || length(sample_n) != 1L || sample_n < 1)) {
    stop("`sample_n` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_points) && (!is.numeric(max_points) || length(max_points) != 1L || max_points < 1)) {
    stop("`max_points` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_points) && !is.null(sample_n) && as.integer(max_points) != as.integer(sample_n)) {
    stop("Use either `max_points` or `sample_n` (deprecated), not both with different values.", call. = FALSE)
  }
  if (is.null(max_points) && !is.null(sample_n)) {
    max_points <- sample_n
  }

  if (!is.null(max_block) && (!is.numeric(max_block) || length(max_block) != 1L || max_block < 1)) {
    stop("`max_block` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_blocks) && (!is.numeric(max_blocks) || length(max_blocks) != 1L || max_blocks < 1)) {
    stop("`max_blocks` must be NULL or a positive integer.", call. = FALSE)
  }
  if (!is.null(max_blocks) && !is.null(max_block) && as.integer(max_blocks) != as.integer(max_block)) {
    stop("Use either `max_blocks` or `max_block` (deprecated), not both with different values.", call. = FALSE)
  }
  if (is.null(max_blocks) && !is.null(max_block)) {
    max_blocks <- max_block
  }
  if (!is.null(max_blocks) && is.null(block)) {
    stop("`max_blocks` requires `block` to be supplied.", call. = FALSE)
  }

  if (!is.numeric(large_n) || length(large_n) != 1L || large_n < 1) {
    stop("`large_n` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(block_n) || length(block_n) != 1L || block_n < 1) {
    stop("`block_n` must be a positive integer.", call. = FALSE)
  }
  if (!is.logical(warnings) || length(warnings) != 1L) {
    stop("`warnings` must be TRUE or FALSE.", call. = FALSE)
  }

  grz_validate_map_inputs(data, lon = lon, lat = lat, datetime = datetime, block = block)

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
  block_col <- NULL
  block_title <- NULL

  if (!is.null(block)) {
    dat$.grz_block <- grz_make_block_group(dat, block)
    block_col <- ".grz_block"
    block_title <- paste(block, collapse = " + ")
  }

  if (!is.null(max_blocks) && !is.null(block_col)) {
    dat <- grz_select_block_groups(
      data = dat,
      block_col = block_col,
      max_blocks = as.integer(max_blocks),
      seed = seed
    )
  }

  if (!is.null(max_points) && nrow(dat) > as.integer(max_points)) {
    if (!is.null(block_col)) {
      dat <- grz_sample_points_by_block(
        data = dat,
        block_col = block_col,
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

  if (isTRUE(warnings) && nrow(dat) > as.integer(large_n)) {
    warning(
      "grz_map() will render ", format(nrow(dat), big.mark = ","),
      " points. This may take some time. ",
      "Use `max_blocks =` or `max_points =` to subset your data and reduce the number of points.",
      call. = FALSE,
      immediate. = TRUE
    )
    grz_prompt_continue(
      "Map creation halted for large point count. Set `warnings = FALSE` to continue."
    )
  }

  if (!is.null(block_col)) {
    n_groups <- length(unique(dat[[block_col]]))
    if (isTRUE(warnings) && n_groups > as.integer(block_n)) {
      warning(
        "grz_map() will render ", format(n_groups, big.mark = ","),
        " group levels. This may take some time and may be hard to interpret. ",
        "Use `max_blocks =` or `max_points =` to subset your data and reduce the number of points.",
        call. = FALSE,
        immediate. = TRUE
      )
      grz_prompt_continue(
        "Map creation halted for high group count. Set `warnings = FALSE` to continue."
      )
    }
  }

  dat <- dat[order(dat[[datetime]]), , drop = FALSE]
  popup <- grz_make_popup(dat, fields = popup_fields)

  map <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE))
  map <- leaflet::addProviderTiles(map, provider = provider)

  if (isTRUE(timeline)) {
    if (!requireNamespace("leaflet.extras2", quietly = TRUE)) {
      stop("`timeline = TRUE` requires the `leaflet.extras2` package.", call. = FALSE)
    }
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("`timeline = TRUE` requires the `sf` package.", call. = FALSE)
    }

    sf_dat <- sf::st_as_sf(dat, coords = c(lon, lat), crs = 4326, remove = FALSE)
    sf_dat$time <- format(dat[[datetime]], "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    sf_dat$.grz_popup <- popup

    if (is.null(block_col)) {
      sf_dat$.grz_color <- "#2b8cbe"
    } else {
      pal <- grz_make_block_palette(sf_dat[[block_col]])
      sf_dat$.grz_color <- pal(sf_dat[[block_col]])
      map <- leaflet::addLegend(
        map,
        position = "bottomright",
        pal = pal,
        values = sf_dat[[block_col]],
        title = block_title
      )
    }

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
  } else if (is.null(block_col)) {
    map <- leaflet::addCircleMarkers(
      map,
      lng = dat[[lon]],
      lat = dat[[lat]],
      radius = point_radius,
      stroke = FALSE,
      fillColor = "#2b8cbe",
      fillOpacity = point_opacity,
      popup = popup
    )
  } else {
    pal <- grz_make_block_palette(dat[[block_col]])
    map <- leaflet::addCircleMarkers(
      map,
      lng = dat[[lon]],
      lat = dat[[lat]],
      radius = point_radius,
      stroke = FALSE,
      fillColor = pal(dat[[block_col]]),
      fillOpacity = point_opacity,
      popup = popup
    )
    map <- leaflet::addLegend(
      map,
      position = "bottomright",
      pal = pal,
      values = dat[[block_col]],
      title = block_title
    )
  }

  map <- leaflet::fitBounds(
    map,
    lng1 = min(dat[[lon]], na.rm = TRUE),
    lat1 = min(dat[[lat]], na.rm = TRUE),
    lng2 = max(dat[[lon]], na.rm = TRUE),
    lat2 = max(dat[[lat]], na.rm = TRUE)
  )

  map
}
