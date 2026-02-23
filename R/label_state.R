#' Label GPS States in an Interactive Leaflet Timeline App
#'
#' Launches an interactive Shiny + Leaflet app for manual labelling of GPS
#' points as ACTIVE or INACTIVE. Labels are stored in a column and returned
#' to the R session when the user clicks Finish and return.
#'
#' The app supports timeline scrubbing, click/shape selection, and row-level
#' state labelling.
#'
#' @param data Input data frame or tibble.
#' @param lon Longitude column name.
#' @param lat Latitude column name.
#' @param time Datetime column name.
#' @param id Optional unique row id column. If NULL, an internal id is created.
#' @param colour_by Optional column used for filtering groups in the sidebar.
#' @param initial_label_col Label column name to read/write.
#' @param tz Time zone used to parse time.
#' @param display_tz Time zone used for display in the app.
#' @param start_day_offset Integer number of days since the minimum date.
#' @param time_window One of "all", "day", or "week".
#' @param n_animals Optional integer to limit number of animals shown.
#' @param animal_col Optional animal identifier column used with n_animals.
#' @param max_points_display Optional maximum number of points displayed.
#' @param downsample_display Logical; if TRUE, deterministic thinning is used
#'   when max_points_display is set.
#' @param launch_browser Logical; passed to shiny::runApp().
#'
#' @return A data frame with updated initial_label_col values (ACTIVE,
#'   INACTIVE, or NA).
#' @export
label_state <- function(
  data,
  lon = "lon",
  lat = "lat",
  time = "time",
  id = NULL,
  colour_by = NULL,
  initial_label_col = "label",
  tz = "UTC",
  display_tz = tz,
  start_day_offset = 0L,
  time_window = c("all", "day", "week"),
  n_animals = NULL,
  animal_col = NULL,
  max_points_display = NULL,
  downsample_display = FALSE,
  launch_browser = TRUE
) {
  needed_pkgs <- c("shiny", "leaflet", "leaflet.extras", "leaftime", "sf", "jsonlite", "htmlwidgets")
  missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0L) {
    stop(
      "Missing required package(s): ", paste(missing_pkgs, collapse = ", "),
      ". Install with install.packages().",
      call. = FALSE
    )
  }

  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.", call. = FALSE)
  }
  df <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) == 0L) {
    stop("`data` has zero rows.", call. = FALSE)
  }

  must_have <- c(lon, lat, time)
  missing_cols <- setdiff(must_have, names(df))
  if (length(missing_cols) > 0L) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.null(id) && !id %in% names(df)) {
    stop("`id` column not found: ", id, call. = FALSE)
  }
  if (!is.null(colour_by) && !colour_by %in% names(df)) {
    stop("`colour_by` column not found: ", colour_by, call. = FALSE)
  }
  if (!is.null(animal_col) && !animal_col %in% names(df)) {
    stop("`animal_col` column not found: ", animal_col, call. = FALSE)
  }

  if (!is.numeric(start_day_offset) || length(start_day_offset) != 1L || is.na(start_day_offset)) {
    stop("`start_day_offset` must be a single non-missing integer >= 0.", call. = FALSE)
  }
  if (start_day_offset < 0 || start_day_offset %% 1 != 0) {
    stop("`start_day_offset` must be a single non-missing integer >= 0.", call. = FALSE)
  }
  start_day_offset <- as.integer(start_day_offset)

  time_window <- match.arg(time_window)

  if (!is.null(n_animals)) {
    if (!is.numeric(n_animals) || length(n_animals) != 1L || is.na(n_animals) || n_animals < 1 || n_animals %% 1 != 0) {
      stop("`n_animals` must be NULL or a single integer >= 1.", call. = FALSE)
    }
    n_animals <- as.integer(n_animals)
  }

  df[[lon]] <- suppressWarnings(as.numeric(df[[lon]]))
  df[[lat]] <- suppressWarnings(as.numeric(df[[lat]]))
  if (any(!is.finite(df[[lon]]) | df[[lon]] < -180 | df[[lon]] > 180, na.rm = TRUE)) {
    stop("`", lon, "` must contain valid longitude values in [-180, 180].", call. = FALSE)
  }
  if (any(!is.finite(df[[lat]]) | df[[lat]] < -90 | df[[lat]] > 90, na.rm = TRUE)) {
    stop("`", lat, "` must contain valid latitude values in [-90, 90].", call. = FALSE)
  }

  dt <- as.POSIXct(df[[time]], tz = tz)
  if (anyNA(dt)) {
    stop("`", time, "` could not be parsed to POSIXct for all rows.", call. = FALSE)
  }

  min_date <- as.Date(min(dt, na.rm = TRUE), tz = tz)
  window_start <- as.POSIXct(min_date + start_day_offset, tz = tz)
  window_end <- switch(
    time_window,
    day = window_start + as.difftime(1, units = "days"),
    week = window_start + as.difftime(7, units = "days"),
    all = NULL
  )

  keep_time <- dt >= window_start
  if (!is.null(window_end)) {
    keep_time <- keep_time & dt < window_end
  }
  df <- df[keep_time, , drop = FALSE]
  dt <- dt[keep_time]
  if (nrow(df) == 0L) {
    stop(
      "No rows remain after applying `start_day_offset = ", start_day_offset,
      "` and `time_window = '", time_window, "'`.",
      call. = FALSE
    )
  }

  if (!is.null(n_animals)) {
    if (is.null(animal_col)) {
      candidates <- unique(c("sensor_id", "animal_id", "device_id", colour_by))
      candidates <- candidates[!is.na(candidates) & candidates %in% names(df)]
      if (length(candidates) == 0L) {
        stop(
          "`n_animals` was provided but no animal identifier column was found. ",
          "Provide `animal_col`.",
          call. = FALSE
        )
      }
      animal_col <- candidates[[1]]
    }

    animal_vals <- as.character(df[[animal_col]])
    valid_animals <- !is.na(animal_vals) & trimws(animal_vals) != ""
    animal_ids <- unique(animal_vals[valid_animals])
    if (length(animal_ids) == 0L) {
      stop("No non-missing animal ids found in `", animal_col, "` after time filtering.", call. = FALSE)
    }

    if (length(animal_ids) > n_animals) {
      selected_animals <- animal_ids[seq_len(n_animals)]
      warning(
        "Subsetting to the first ", n_animals, " animal(s) from `", animal_col, "` in the selected window.",
        call. = FALSE
      )
    } else {
      selected_animals <- animal_ids
    }

    keep_animals <- animal_vals %in% selected_animals
    df <- df[keep_animals, , drop = FALSE]
    dt <- dt[keep_animals]
    if (nrow(df) == 0L) {
      stop("No rows remain after `n_animals` filtering.", call. = FALSE)
    }
  }

  internal_id <- is.null(id)
  if (internal_id) {
    id <- ".label_row_id"
    df[[id]] <- seq_len(nrow(df))
  }

  id_key <- as.character(df[[id]])
  if (any(is.na(id_key) | trimws(id_key) == "")) {
    stop("`id` contains missing/blank values. Provide a unique id per row.", call. = FALSE)
  }
  if (anyDuplicated(id_key) > 0L) {
    stop("`id` must be unique per GPS fix.", call. = FALSE)
  }

  if (!initial_label_col %in% names(df)) {
    df[[initial_label_col]] <- NA_character_
  } else {
    x <- as.character(df[[initial_label_col]])
    x <- toupper(trimws(x))
    x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
    invalid <- !is.na(x) & !(x %in% c("ACTIVE", "INACTIVE"))
    if (any(invalid)) {
      warning(sum(invalid), " rows had non-standard labels; set to NA.", call. = FALSE)
      x[invalid] <- NA_character_
    }
    df[[initial_label_col]] <- x
  }

  df$.label_key <- id_key
  df$.label_time <- dt
  df$.label_time_num <- as.numeric(dt)

  ord <- order(df$.label_time_num)
  df <- df[ord, , drop = FALSE]

  display_keys <- df$.label_key
  if (!is.null(max_points_display)) {
    if (!is.numeric(max_points_display) || length(max_points_display) != 1L || max_points_display < 100) {
      stop("`max_points_display` must be NULL or a single integer >= 100.", call. = FALSE)
    }
    max_points_display <- as.integer(max_points_display)
    if (nrow(df) > max_points_display) {
      if (isTRUE(downsample_display)) {
        idx <- unique(round(seq(1, nrow(df), length.out = max_points_display)))
      } else {
        set.seed(1)
        idx <- sort(sample.int(nrow(df), max_points_display))
      }
      display_keys <- df$.label_key[idx]
      warning(
        "Displaying a subset of ", length(display_keys), " points (from ", nrow(df), "). ",
        "Only displayed points can be selected in this session.",
        call. = FALSE
      )
    }
  }

  min_sec <- floor(min(df$.label_time_num, na.rm = TRUE))
  max_sec <- ceiling(max(df$.label_time_num, na.rm = TRUE))

  .make_popup <- function(d) {
    t_display <- format(as.POSIXct(d$.label_time, origin = "1970-01-01", tz = "UTC"), tz = display_tz, usetz = TRUE)
    paste0(
      "<b>", id, ":</b> ", as.character(d[[id]]),
      "<br/><b>", time, ":</b> ", t_display,
      "<br/><b>", initial_label_col, ":</b> ", ifelse(is.na(d[[initial_label_col]]), "NA", d[[initial_label_col]])
    )
  }

  .label_fill_color <- function(lbl) {
    out <- rep("#9e9e9e", length(lbl)) # default unlabelled
    is_active <- !is.na(lbl) & lbl == "ACTIVE"
    is_inactive <- !is.na(lbl) & lbl == "INACTIVE"
    out[is_active] <- "#2ca25f"
    out[is_inactive] <- "#d7191c"
    out
  }

  .label_stroke_color <- function(lbl, selected) {
    out <- rep("#ffd400", length(lbl)) # default unlabelled ring
    is_active <- !is.na(lbl) & lbl == "ACTIVE"
    is_inactive <- !is.na(lbl) & lbl == "INACTIVE"
    out[is_active] <- "#1b5e20"
    out[is_inactive] <- "#7f0000"
    sel <- !is.na(selected) & selected
    out[sel] <- "#111111"
    out
  }

  .draw_feature_to_sfc <- function(feat) {
    if (is.null(feat)) {
      return(NULL)
    }

    .close_ring <- function(mat) {
      if (nrow(mat) < 3L) {
        return(NULL)
      }
      if (!all(mat[1, ] == mat[nrow(mat), ])) {
        mat <- rbind(mat, mat[1, ])
      }
      if (nrow(mat) < 4L) {
        return(NULL)
      }
      mat
    }

    .as_ring_geojson <- function(ring) {
      if (!is.list(ring) || length(ring) < 3L) {
        return(NULL)
      }
      mat <- do.call(
        rbind,
        lapply(ring, function(pt) {
          vals <- suppressWarnings(as.numeric(unlist(pt)))
          if (length(vals) < 2L) {
            return(c(NA_real_, NA_real_))
          }
          c(vals[[1]], vals[[2]])
        })
      )
      mat <- mat[is.finite(mat[, 1]) & is.finite(mat[, 2]), , drop = FALSE]
      .close_ring(mat)
    }

    .as_ring_latlng <- function(ring) {
      if (!is.list(ring) || length(ring) < 3L) {
        return(NULL)
      }
      mat <- do.call(
        rbind,
        lapply(ring, function(pt) {
          if (is.null(pt$lng) || is.null(pt$lat)) {
            return(c(NA_real_, NA_real_))
          }
          c(suppressWarnings(as.numeric(pt$lng)), suppressWarnings(as.numeric(pt$lat)))
        })
      )
      mat <- mat[is.finite(mat[, 1]) & is.finite(mat[, 2]), , drop = FALSE]
      .close_ring(mat)
    }

    .rect_from_bounds <- function(bounds) {
      if (is.null(bounds) || is.null(bounds$`_southWest`) || is.null(bounds$`_northEast`)) {
        return(NULL)
      }
      sw <- bounds$`_southWest`
      ne <- bounds$`_northEast`
      lng_min <- suppressWarnings(as.numeric(sw$lng))
      lat_min <- suppressWarnings(as.numeric(sw$lat))
      lng_max <- suppressWarnings(as.numeric(ne$lng))
      lat_max <- suppressWarnings(as.numeric(ne$lat))
      if (!all(is.finite(c(lng_min, lat_min, lng_max, lat_max)))) {
        return(NULL)
      }
      mat <- rbind(
        c(lng_min, lat_min),
        c(lng_max, lat_min),
        c(lng_max, lat_max),
        c(lng_min, lat_max),
        c(lng_min, lat_min)
      )
      sf::st_sfc(sf::st_polygon(list(mat)), crs = 4326)
    }

    geom <- NULL
    if (is.list(feat$geometry) && !is.null(feat$geometry$type)) {
      geom <- feat$geometry
    } else if (is.list(feat$feature$geometry) && !is.null(feat$feature$geometry$type)) {
      geom <- feat$feature$geometry
    }

    if (!is.null(geom) && !is.null(geom$type) && !is.null(geom$coordinates)) {
      gtype <- toupper(as.character(geom$type))
      if (gtype == "POLYGON") {
        rings <- lapply(geom$coordinates, .as_ring_geojson)
        rings <- Filter(Negate(is.null), rings)
        if (length(rings) > 0L) {
          return(sf::st_sfc(sf::st_polygon(rings), crs = 4326))
        }
      } else if (gtype == "MULTIPOLYGON") {
        polys <- list()
        for (poly in geom$coordinates) {
          rings <- lapply(poly, .as_ring_geojson)
          rings <- Filter(Negate(is.null), rings)
          if (length(rings) > 0L) {
            polys[[length(polys) + 1L]] <- rings
          }
        }
        if (length(polys) > 0L) {
          return(sf::st_sfc(sf::st_multipolygon(polys), crs = 4326))
        }
      }
    }

    bounds <- NULL
    if (is.list(feat$bounds)) {
      bounds <- feat$bounds
    } else if (is.list(feat$geometry$bounds)) {
      bounds <- feat$geometry$bounds
    } else if (is.list(feat$feature$bounds)) {
      bounds <- feat$feature$bounds
    } else if (is.list(feat$properties$bounds)) {
      bounds <- feat$properties$bounds
    }
    rect_sfc <- .rect_from_bounds(bounds)
    if (!is.null(rect_sfc)) {
      return(rect_sfc)
    }

    latlngs <- NULL
    if (is.list(feat$latlngs)) {
      latlngs <- feat$latlngs
    } else if (is.list(feat$geometry$latlngs)) {
      latlngs <- feat$geometry$latlngs
    } else if (is.list(feat$feature$latlngs)) {
      latlngs <- feat$feature$latlngs
    }
    if (is.null(latlngs)) {
      return(NULL)
    }

    rings <- NULL
    if (length(latlngs) > 0L && is.list(latlngs[[1]]) && !is.null(latlngs[[1]]$lat) && !is.null(latlngs[[1]]$lng)) {
      rings <- list(latlngs)
    } else if (
      length(latlngs) > 0L &&
      is.list(latlngs[[1]]) &&
      length(latlngs[[1]]) > 0L &&
      is.list(latlngs[[1]][[1]]) &&
      !is.null(latlngs[[1]][[1]]$lat) &&
      !is.null(latlngs[[1]][[1]]$lng)
    ) {
      rings <- latlngs
    }

    if (is.null(rings)) {
      return(NULL)
    }

    ring_mats <- lapply(rings, .as_ring_latlng)
    ring_mats <- Filter(Negate(is.null), ring_mats)
    if (length(ring_mats) == 0L) {
      return(NULL)
    }
    sf::st_sfc(sf::st_polygon(ring_mats), crs = 4326)
  }

  .make_timeline_geojson <- function(d) {
    if (nrow(d) == 0L) {
      return(list(type = "FeatureCollection", features = list()))
    }

    start_ms <- as.numeric(d$.label_time) * 1000
    feats <- vector("list", nrow(d))
    for (i in seq_len(nrow(d))) {
      feats[[i]] <- list(
        type = "Feature",
        properties = list(
          start = start_ms[[i]],
          end = start_ms[[i]]
        ),
        geometry = list(
          type = "Point",
          coordinates = list(as.numeric(d[[lon]][[i]]), as.numeric(d[[lat]][[i]]))
        )
      )
    }

    list(type = "FeatureCollection", features = feats)
  }

  display_tz_json <- jsonlite::toJSON(display_tz, auto_unbox = TRUE)
  timeline_onchange <- htmlwidgets::JS(
    "function(e) {
      var t = null;
      if (e && e.target && typeof e.target.time !== 'undefined') {
        t = e.target.time;
      }
      if (t === null || typeof t === 'undefined') {
        return;
      }
      if (typeof t === 'string') {
        t = Date.parse(t);
      }
      if (!isFinite(t)) {
        return;
      }
      Shiny.setInputValue('timeline_cursor_live', t, {priority: 'event'});
    }"
  )
  timeline_point_to_layer <- htmlwidgets::JS(
    "function(feature, latlng) {
      return L.circleMarker(latlng, {
        radius: 0,
        weight: 0,
        opacity: 0,
        fillOpacity: 0,
        interactive: false
      });
    }"
  )
  timeline_format_output <- htmlwidgets::JS(
    paste0(
      "function(ms) {",
      "  var d = new Date(ms);",
      "  try {",
      "    return d.toLocaleString(undefined, { timeZone: ", display_tz_json, " });",
      "  } catch (err) {",
      "    return d.toISOString();",
      "  }",
      "}"
    )
  )

  ui <- shiny::fluidPage(
    shiny::titlePanel("GPS Timeline Labelling App (Experimental)"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::p("Use the map timeline control (bottom-left) to scrub time."),
        shiny::p("Click points or draw shapes to select, then apply labels."),
        if (!is.null(colour_by)) {
          shiny::selectInput(
            "filter_group",
            paste0("Filter by ", colour_by),
            choices = c("All", sort(unique(as.character(df[[colour_by]])))),
            selected = "All"
          )
        },
        shiny::checkboxInput("show_history", "Show all points up to current timeline time", FALSE),
        shiny::sliderInput(
          "window_minutes",
          "Window (minutes) when history = FALSE",
          min = 1,
          max = 720,
          value = 60,
          step = 1
        ),
        shiny::sliderInput(
          "point_radius",
          "Point size",
          min = 1,
          max = 25,
          value = 5,
          step = 1
        ),
        shiny::tags$hr(),
        shiny::actionButton("mark_active", "Mark ACTIVE", class = "btn-primary"),
        shiny::actionButton("mark_inactive", "Mark INACTIVE", class = "btn-danger"),
        shiny::actionButton("clear_label", "Clear label"),
        shiny::actionButton("select_drawn", "Select drawn area(s)"),
        shiny::actionButton("clear_selection", "Clear selection"),
        shiny::tags$hr(),
        shiny::actionButton("finish_return", "Finish and return", class = "btn-success"),
        shiny::tags$hr(),
        shiny::verbatimTextOutput("status", placeholder = TRUE),
        shiny::tableOutput("label_counts")
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map", height = "760px"),
        shiny::tags$br(),
        shiny::h4("Selected / visible preview"),
        shiny::tableOutput("preview")
      )
    )
  )

  server <- function(input, output, session) {
    base_df <- df

    rv <- shiny::reactiveValues(
      df = df,
      selected = character(0),
      cursor = min_sec
    )

    display_base_df <- shiny::reactive({
      d <- base_df[base_df$.label_key %in% display_keys, , drop = FALSE]
      if (!is.null(colour_by) && !is.null(input$filter_group) && input$filter_group != "All") {
        d <- d[as.character(d[[colour_by]]) == input$filter_group, , drop = FALSE]
      }
      d
    })

    display_df <- shiny::reactive({
      d_base <- display_base_df()
      rv$df[rv$df$.label_key %in% d_base$.label_key, , drop = FALSE]
    })

    visible_df <- shiny::reactive({
      d <- display_df()
      shiny::req(nrow(d) > 0)

      cursor <- rv$cursor
      if (isTRUE(input$show_history)) {
        d <- d[d$.label_time_num <= cursor, , drop = FALSE]
      } else {
        half_window <- as.numeric(input$window_minutes) * 60
        d <- d[
          d$.label_time_num >= (cursor - half_window) & d$.label_time_num <= (cursor + half_window),
          ,
          drop = FALSE
        ]
      }
      d
    })

    refresh_markers <- function() {
      d <- visible_df()
      proxy <- leaflet::leafletProxy("map")
      proxy <- proxy |>
        leaflet::clearGroup("gps_points") |>
        leaflet::clearControls()

      if (nrow(d) == 0L) {
        return(invisible(NULL))
      }

      selected <- d$.label_key %in% rv$selected
      fill_col <- .label_fill_color(d[[initial_label_col]])
      stroke_col <- .label_stroke_color(d[[initial_label_col]], selected)
      base_radius <- if (is.null(input$point_radius) || !is.finite(as.numeric(input$point_radius))) 5 else as.numeric(input$point_radius)
      radius <- ifelse(selected, base_radius + 2, base_radius)

      proxy <- proxy |>
        leaflet::addCircleMarkers(
          lng = d[[lon]],
          lat = d[[lat]],
          layerId = d$.label_key,
          radius = radius,
          stroke = TRUE,
          weight = 1,
          color = stroke_col,
          fillColor = fill_col,
          fillOpacity = 1,
          group = "gps_points",
          popup = .make_popup(d)
        )

      proxy |>
        leaflet::addLegend(
          position = "topright",
          colors = c("#2ca25f", "#d7191c", "#9e9e9e", "#ff9800"),
          labels = c("ACTIVE", "INACTIVE", "Unlabelled (yellow ring)", "Selected"),
          title = "Label state",
          opacity = 0.9
        )

      invisible(NULL)
    }

    output$map <- leaflet::renderLeaflet({
      d <- display_base_df()

      m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles("Esri.WorldImagery") |>
        leaflet.extras::addDrawToolbar(
          targetGroup = "draw_select",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          circleMarkerOptions = FALSE,
          markerOptions = FALSE,
          polygonOptions = leaflet.extras::drawPolygonOptions(showArea = TRUE),
          rectangleOptions = leaflet.extras::drawRectangleOptions(),
          editOptions = leaflet.extras::editToolbarOptions()
        ) |>
        leaftime::addTimeline(
          .make_timeline_geojson(d),
          timelineOpts = list(
            pointToLayer = timeline_point_to_layer
          ),
          sliderOpts = list(
            waitToUpdateMap = FALSE,
            showTicks = FALSE,
            enablePlayback = TRUE,
            formatOutput = timeline_format_output
          ),
          width = "100%",
          onchange = timeline_onchange
        )

      if (nrow(d) > 0L) {
        leaflet::fitBounds(
          m,
          lng1 = min(d[[lon]], na.rm = TRUE),
          lat1 = min(d[[lat]], na.rm = TRUE),
          lng2 = max(d[[lon]], na.rm = TRUE),
          lat2 = max(d[[lat]], na.rm = TRUE)
        )
      } else {
        m
      }
    })

    shiny::observeEvent(input$timeline_cursor_live, {
      x <- suppressWarnings(as.numeric(input$timeline_cursor_live))
      if (!is.finite(x)) {
        return()
      }
      if (x > 1e12) {
        x <- x / 1000
      }
      rv$cursor <- x
    }, ignoreInit = FALSE)

    shiny::observe({
      display_base_df()
      rv$cursor
      rv$selected
      rv$df
      input$show_history
      input$window_minutes
      input$point_radius
      refresh_markers()
    })

    shiny::observeEvent(input$map_marker_click, {
      click_id <- as.character(input$map_marker_click$id)
      if (length(click_id) == 0L || is.na(click_id)) {
        return()
      }
      if (click_id %in% rv$selected) {
        rv$selected <- setdiff(rv$selected, click_id)
      } else {
        rv$selected <- c(rv$selected, click_id)
      }
    })

    select_from_draw <- function(feat) {
      poly <- .draw_feature_to_sfc(feat)
      if (is.null(poly)) {
        return(invisible(NULL))
      }

      d <- visible_df()
      if (nrow(d) == 0L) {
        return(invisible(NULL))
      }

      pts <- sf::st_as_sf(d, coords = c(lon, lat), crs = 4326, remove = FALSE)
      inside <- as.logical(sf::st_intersects(pts, poly, sparse = FALSE)[, 1])
      if (any(inside)) {
        rv$selected <- unique(c(rv$selected, d$.label_key[inside]))
      }
      invisible(NULL)
    }

    shiny::observeEvent(input$map_draw_new_feature, {
      select_from_draw(input$map_draw_new_feature)
    })

    shiny::observeEvent(input$map_draw_edited_features, {
      feats <- input$map_draw_edited_features$features
      if (is.null(feats) || length(feats) == 0L) {
        return()
      }
      for (i in seq_along(feats)) {
        select_from_draw(feats[[i]])
      }
    })

    shiny::observeEvent(input$select_drawn, {
      all_feats <- input$map_draw_all_features$features
      if (is.null(all_feats) || length(all_feats) == 0L) {
        shiny::showNotification("No drawn polygons/rectangles found.", type = "warning")
        return()
      }
      n_before <- length(rv$selected)
      for (i in seq_along(all_feats)) {
        select_from_draw(all_feats[[i]])
      }
      n_added <- length(rv$selected) - n_before
      shiny::showNotification(
        paste0("Selected ", n_added, " additional point(s) from drawn area(s)."),
        type = "message"
      )
    })

    apply_label <- function(value) {
      ids <- rv$selected
      if (length(ids) == 0L) {
        return()
      }
      idx <- match(ids, rv$df$.label_key)
      idx <- idx[!is.na(idx)]
      if (length(idx) == 0L) {
        return()
      }
      if (is.na(value)) {
        rv$df[[initial_label_col]][idx] <- NA_character_
      } else {
        rv$df[[initial_label_col]][idx] <- as.character(value)
      }
      rv$selected <- character(0)
    }

    clear_drawn_shapes <- function() {
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("draw_select")
      invisible(NULL)
    }

    shiny::observeEvent(input$mark_active, {
      apply_label("ACTIVE")
      clear_drawn_shapes()
    })
    shiny::observeEvent(input$mark_inactive, { apply_label("INACTIVE") })
    shiny::observeEvent(input$clear_label, { apply_label(NA_character_) })
    shiny::observeEvent(input$clear_selection, { rv$selected <- character(0) })

    output$status <- shiny::renderText({
      d_vis <- visible_df()
      current_time <- as.POSIXct(rv$cursor, origin = "1970-01-01", tz = "UTC")
      x <- rv$df[[initial_label_col]]
      paste0(
        "Current time: ", format(current_time, tz = display_tz, usetz = TRUE), "\n",
        "Visible points: ", format(nrow(d_vis), big.mark = ","), "\n",
        "Selected points: ", format(length(rv$selected), big.mark = ","), "\n",
        "Total rows in app: ", format(nrow(rv$df), big.mark = ","), "\n",
        "Window used: start_day_offset = ", start_day_offset, ", time_window = '", time_window, "'\n",
        "Labels: ACTIVE=", sum(x == "ACTIVE", na.rm = TRUE),
        " INACTIVE=", sum(x == "INACTIVE", na.rm = TRUE),
        " NA=", sum(is.na(x))
      )
    })

    output$label_counts <- shiny::renderTable({
      x <- rv$df[[initial_label_col]]
      data.frame(
        label = c("ACTIVE", "INACTIVE", "NA"),
        n = c(
          sum(x == "ACTIVE", na.rm = TRUE),
          sum(x == "INACTIVE", na.rm = TRUE),
          sum(is.na(x))
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, digits = 0)

    output$preview <- shiny::renderTable({
      d_vis <- visible_df()
      d <- if (length(rv$selected) > 0L) {
        rv$df[rv$df$.label_key %in% rv$selected, , drop = FALSE]
      } else {
        utils::head(d_vis, 15)
      }

      keep <- unique(c(id, time, initial_label_col, colour_by, animal_col))
      keep <- keep[!is.na(keep) & keep %in% names(d)]
      if (length(keep) == 0L) {
        keep <- names(d)
      }
      d[, keep, drop = FALSE]
    }, striped = TRUE, bordered = TRUE)

    shiny::observeEvent(input$finish_return, {
      out <- rv$df
      out$.label_key <- NULL
      out$.label_time <- NULL
      out$.label_time_num <- NULL
      if (internal_id) {
        out[[id]] <- NULL
      }
      shiny::stopApp(out)
    })
  }

  app <- shiny::shinyApp(ui = ui, server = server)
  result <- shiny::runApp(app, launch.browser = launch_browser)

  if (!is.data.frame(result)) {
    stop("Labelling app did not return a data frame.", call. = FALSE)
  }
  result
}

# -----------------------------------------------------------------------------
# Synthetic example (run manually in an interactive R session)
# -----------------------------------------------------------------------------
# set.seed(1)
# n <- 1500
# t0 <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
# toy <- data.frame(
#   animal_id = rep(c("A01", "A02"), each = n / 2),
#   timestamp = t0 + seq_len(n) * 60,
#   lon = 132.30 + cumsum(rnorm(n, 0, 0.0002)),
#   lat = -14.47 + cumsum(rnorm(n, 0, 0.0002)),
#   stringsAsFactors = FALSE
# )
# toy$point_id <- paste(toy$animal_id, format(toy$timestamp, "%Y%m%d%H%M%S"), seq_len(n), sep = "_")
#
# labelled <- label_state(
#   data = toy,
#   lon = "lon",
#   lat = "lat",
#   time = "timestamp",
#   id = "point_id",
#   colour_by = "animal_id",
#   initial_label_col = "label",
#   start_day_offset = 0L,
#   time_window = "week",
#   n_animals = 1L,
#   animal_col = "animal_id",
#   tz = "UTC"
# )
# table(labelled$label, useNA = "ifany")

