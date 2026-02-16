#!/usr/bin/env Rscript

pkgload::load_all(".")

path <- file.path("test_data", "ManbullooToSimulate.csv")

cat("Reading data...\n")
raw <- grz_read_gps(path, n_max = 50000, validate = FALSE)
cat("rows:", nrow(raw), "cols:", ncol(raw), "\n")

cat("\n1) grz_validate\n")
val <- grz_validate(raw, drop_invalid = FALSE, verbose = TRUE, return_class = "data.frame")
cat("rows:", nrow(val), "\n")

cat("\n2) grz_clean_duplicates\n")
x <- grz_clean_duplicates(val, verbose = TRUE, snapshot = TRUE)
cat("rows:", nrow(x), "\n")

cat("\n3) grz_clean_errors\n")
x <- grz_clean_errors(x, verbose = TRUE, snapshot = TRUE)
cat("rows:", nrow(x), "\n")

cat("\n4) grz_clean_speed_fixed\n")
x_speed_fixed <- grz_clean_speed_fixed(x, max_speed_mps = 4, verbose = TRUE, snapshot = TRUE)
cat("rows:", nrow(x_speed_fixed), "\n")

cat("\n5) grz_clean_speed_stat\n")
x_speed_stat <- grz_clean_speed_stat(x, method = "mad", min_threshold_mps = 4, verbose = TRUE, snapshot = TRUE)
cat("rows:", nrow(x_speed_stat), "\n")

cat("\n6) grz_denoise\n")
x_clean <- grz_denoise(x_speed_fixed, verbose = TRUE, snapshot = TRUE)
cat("rows:", nrow(x_clean), "\n")

cat("\n7) grz_align + grz_downsample\n")
x_aligned <- grz_align(x_clean, interval_mins = "base", verbose = TRUE)
x_down <- grz_downsample(x_aligned, target_mins = 60, method = "representative", verbose = TRUE)
cat("aligned rows:", nrow(x_aligned), "downsample rows:", nrow(x_down), "\n")

cat("\n8) grz_calculate_movement\n")
mv_rows <- grz_calculate_movement(x_clean, verbose = TRUE)
cat("rows:", nrow(mv_rows), "has speed:", "speed_mps" %in% names(mv_rows), "\n")

cat("\n9) grz_calculate_social\n")
soc_input <- x_clean[1:min(nrow(x_clean), 1200), , drop = FALSE]
soc_alt <- soc_input
soc_alt$sensor_id <- "SIM_B"
soc_alt$lon <- soc_alt$lon + 0.0003
soc_data <- rbind(soc_input, soc_alt)
soc_rows <- grz_calculate_social(soc_data, align_interval_mins = "base", interpolate = TRUE, verbose = TRUE)
cat("rows:", nrow(soc_rows), "has nearest_neighbor_m:", "nearest_neighbor_m" %in% names(soc_rows), "\n")

cat("\n10) Epoch summaries\n")
mv_day <- grz_summarise_movement(mv_rows, epoch = "day", verbose = TRUE)
soc_day <- grz_summarise_social(soc_rows, epoch = "day", verbose = TRUE)
sp_day <- grz_calculate_spatial(x_clean, epoch = "day", min_fixes = 5, verbose = TRUE)
epoch_all <- grz_calculate_epoch_metrics(x_clean, epoch = "day", include = c("movement", "spatial"), verbose = TRUE)
cat(
  "movement rows:", nrow(mv_day),
  "social rows:", nrow(soc_day),
  "spatial rows:", nrow(sp_day),
  "epoch wrapper rows:", nrow(epoch_all), "\n"
)

if (requireNamespace("sf", quietly = TRUE)) {
  cat("\n11) grz_clean_spatial (synthetic paddock test)\n")
  xr <- range(x_clean$lon, na.rm = TRUE)
  yr <- range(x_clean$lat, na.rm = TRUE)
  poly <- sf::st_polygon(list(matrix(
    c(
      xr[1] - 0.01, yr[1] - 0.01,
      xr[2] + 0.01, yr[1] - 0.01,
      xr[2] + 0.01, yr[2] + 0.01,
      xr[1] - 0.01, yr[2] + 0.01,
      xr[1] - 0.01, yr[1] - 0.01
    ),
    ncol = 2,
    byrow = TRUE
  )))
  paddock <- sf::st_sf(NAME = "TestPaddock", geometry = sf::st_sfc(poly, crs = 4326))
  x_sp <- grz_clean_spatial(
    x_clean[1:min(nrow(x_clean), 5000), , drop = FALSE],
    paddocks_sf = paddock,
    buffer_m = 100,
    append_pdk = TRUE,
    verbose = TRUE
  )
  cat("rows:", nrow(x_sp), "has paddock:", "paddock" %in% names(x_sp), "\n")
}

cat("\n12) behavior workflow\n")
beh_rows <- grz_classify_behavior(x_clean, verbose = TRUE)
beh_val <- grz_validate_behavior(beh_rows, verbose = TRUE, pca = TRUE)
beh_guide <- grz_behavior_threshold_guide(beh_rows)
cat(
  "classified rows:", nrow(beh_rows),
  "state levels:", paste(sort(unique(na.omit(beh_rows$behavior_state))), collapse = ","),
  "validation states:", nrow(beh_val$state_counts),
  "guide overall rows:", nrow(beh_guide$overall), "\n"
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p_metric <- grz_plot_diurnal_metrics(beh_rows)
  p_state <- grz_plot_diurnal_states(beh_rows)
  bp <- grz_behavior_pipeline(
    beh_rows,
    threshold_guide = TRUE,
    metrics_plot = TRUE,
    states_plot = TRUE,
    validate = TRUE,
    verbose = TRUE
  )
  cat(
    "diurnal_metric_plot:", inherits(p_metric, "ggplot"),
    "diurnal_state_plot:", inherits(p_state, "ggplot"),
    "pipeline_has_data:", !is.null(bp$data),
    "pipeline_has_threshold_guide:", !is.null(bp$threshold_guide), "\n"
  )
}

cat("\nDraft checks complete.\n")
