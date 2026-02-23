## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 5
)


## ----setup--------------------------------------------------------------------
library(grazer)


## ----synthetic-data-----------------------------------------------------------
set.seed(42)

n_animals <- 3L
n_fix <- 24L * 6L * 3L # 3 days at 10-minute intervals

timestamps <- seq(
  as.POSIXct("2024-09-01 00:00:00", tz = "UTC"),
  by = "10 min",
  length.out = n_fix
)

make_track <- function(sensor_id, lon0, lat0) {
  data.frame(
    sensor_id = sensor_id,
    datetime = timestamps,
    lon = lon0 + cumsum(rnorm(n_fix, mean = 0, sd = 0.00018)),
    lat = lat0 + cumsum(rnorm(n_fix, mean = 0, sd = 0.00014)),
    stringsAsFactors = FALSE
  )
}

gps <- rbind(
  make_track("A01", 132.305, -14.474),
  make_track("A02", 132.300, -14.471),
  make_track("A03", 132.309, -14.469)
)

# Add a few realistic data issues so cleaning steps are visible.
gps <- rbind(gps, gps[1:5, ])         # duplicates
gps$lon[12] <- 0                      # invalid 0,0 coordinate
gps$lat[12] <- 0
gps$datetime[25] <- NA                # invalid datetime
gps$lon[300] <- gps$lon[300] + 0.5    # extreme jump
gps$lat[300] <- gps$lat[300] + 0.5

dim(gps)
head(gps)


## ----validate-----------------------------------------------------------------
gps_valid <- grz_validate(gps, drop_invalid = FALSE, verbose = TRUE)


## ----clean--------------------------------------------------------------------
gps_clean <- grz_clean(
  gps_valid,
  steps = c("duplicates", "errors", "speed_fixed", "denoise"),
  max_speed_mps = 4,
  verbose = TRUE
)

dim(gps_clean)


## ----movement-----------------------------------------------------------------
gps_move <- grz_calculate_movement(gps_clean, verbose = FALSE)

head(gps_move[, c("sensor_id", "datetime", "step_m", "speed_mps", "turn_rad")])


## ----social-------------------------------------------------------------------
gps_social <- grz_calculate_social(
  gps_clean,
  interpolate = FALSE,
  verbose = FALSE
)

head(gps_social[, c("sensor_id", "datetime", "nearest_neighbor_m", "mean_dist_to_others_m")])


## ----epoch--------------------------------------------------------------------
daily_metrics <- grz_calculate_epoch_metrics(
  gps_clean,
  epoch = "day",
  include = c("movement", "social", "spatial"),
  verbose = FALSE
)

head(daily_metrics)


## ----behavior-----------------------------------------------------------------
gps_states <- grz_classify_activity_consensus(
  gps_clean,
  groups = "sensor_id",
  verbose = FALSE
)

table(gps_states$activity_state_consensus, useNA = "ifany")


## ----interactive-tools, eval = FALSE------------------------------------------
# # Interactive map
# grz_map(
#   data = gps_states,
#   timeline = TRUE,
#   group = "sensor_id",
#   state_col = "activity_state_consensus"
# )
# 
# # Interactive manual state labelling app
# labelled <- grz_label_gps_states(
#   data = gps_states,
#   lon = "lon",
#   lat = "lat",
#   time = "datetime",
#   color_by = "sensor_id",
#   initial_label_col = "label"
# )


## ----save---------------------------------------------------------------------
# Example export pattern
# data.table::fwrite(daily_metrics, "daily_metrics.csv")
# data.table::fwrite(gps_states, "gps_states.csv")

