# Title: Hourly validation of proposed algorithm

# Purpose: This script evaluates the hourly occupant detection part of the
# proposed occupant detection algorithm based on water use data. It also
# investigates the individual steps of the algorithm. Additionally, it compares
# the algorithm against the electricity-based algorithm of Becker and
# Kleiminger (2018).

# Becker, V., & Kleiminger, W. (2018). Exploring zero-training algorithms for
# occupancy detection based on smart meter measurements. Computer Science -
# Research and Development, 33(1–2), 25–36.
# https://doi.org/10.1007/s00450-017-0344-9

# Functions:  functions/plot_styles.R
#             functions/fn_hourly_occupancy.R
#             functions/geo_ma.cpp


# Outputs: plots/03_hourly_validation_paper.pdf (Figure 5)
#          plots/hourly_validation/BUILD-H-01.pdf ... BUILD-H-08.pdf + WM-A-11.pdf (Supplementary material)

# Author: M. Schaffer
# Contact details: msch@build.aau.dk

# Load packages and functions ---------------------------------------------

library(data.table)
library(lubridate)
library(purrr)
library(Rcpp)
library(ggplot2)
library(Hmisc, include.only = "approxExtrap")
library(patchwork)
library(yardstick)
library(forcats)
library(Rcpp)

source("functions/plot_styles.R")
source("functions/hourly_occupancy.R")
sourceCpp("functions/geo_ma.cpp")


# Download data -----------------------------------------------------------

## Data from Wilhelm et al.  ----------------------------------------------

# Wilhelm, S., Kasbauer, J., Jakob, D., Elser, B., & Ahrens, D. (2023). Smart
# Meter Water Consumption Measurements – Additional Evaluation Data. Zenodo.
# https://doi.org/10.5281/zenodo.7585347


# Download data and unzip
wm_url <- "https://zenodo.org/api/records/7585347/files-archive"
temp_dir <- tempdir()
download.file(wm_url, destfile = file.path(temp_dir, "wilhelm.zip"), mode = "wb")
unzip(file.path(temp_dir, "wilhelm.zip"), exdir = file.path(temp_dir, "wilhelm"))
unzip(file.path(temp_dir, "wilhelm", "out_of_house.zip"), exdir = file.path(temp_dir, "wilhelm"))


read_wilhelm <- function(path = file.path(temp_dir, "wilhelm", "out_of_house", "WM_A_11")) {
  absence_dt <- fread(file.path(path, "periods.csv"))
  swm_dt <- fread(file.path(path, "smartmeter.csv"))

  swm_dt[, time := with_tz(time, "Europe/Berlin")]
  absence_dt[, absent_from := with_tz(absent_from, "Europe/Berlin")]
  absence_dt[, absent_to := with_tz(absent_to, "Europe/Berlin")]
  setorder(swm_dt, time)

  # Find non cumulative values - check for
  swm_dt[, cum := total_m3 - shift(total_m3) >= 0]
  swm_dt <- swm_dt[!c(which(cum == FALSE) - 1, which(cum == FALSE))]
  swm_dt[, cum := NULL]

  # Round to full hour
  swm_dt[, time_rounded := round_date(time, "hour")]

  # Find two closest values before and after full hour
  swm_dt[, before := fifelse(time < time_rounded, yes = TRUE, no = NA)]
  swm_dt[, after := fifelse(time > time_rounded, yes = TRUE, no = NA)]
  swm_dt[!is.na(before), before_cum := cumsum(before), by = time_rounded]
  swm_dt[!is.na(after), after_cum := cumsum(after), by = time_rounded]
  swm_dt[, equal := time == time_rounded]

  # Get hourly values closest to full hour
  hourly_check <- swm_dt[, .(last_before = time[which.max(before_cum)], first_after = time[which.min(after_cum)], equal = any(equal)), by = time_rounded]

  # Check if values are within 30min of full hour
  hourly_check[, keep := equal == TRUE | ((time_rounded - last_before) < 1800 & (first_after - time_rounded) < 1800)]
  hourly_check[is.na(keep), keep := TRUE]
  hourly_check <- hourly_check[keep == TRUE]
  swm_dt_hourly <- data.table(time_rounded = hourly_check[, time_rounded])

  swm_dt_hourly[, total_m3 := approxExtrap(
    x = swm_dt[, as.numeric(time)],
    y = swm_dt[, total_m3],
    xout = as.numeric(time_rounded)
  )$y]
  swm_dt_hourly[, total_m3 := round(total_m3, 3)]

  # Combine SWM and questionnaire data
  absence_dt[, absent_from_rounded := ceiling_date(absent_from, "hour") + hours(1)]
  absence_dt[, absent_to_rounded := floor_date(absent_to, "hour")]
  absence_dt <- absence_dt[absent_to_rounded > absent_from_rounded]

  absence_dt_long <- absence_dt[, .(
    time_rounded = seq.POSIXt(absent_from_rounded, absent_to_rounded, "hour"),
    dishwasher, washing_machine,
    occupied = FALSE
  ), by = absent_from_rounded][, absent_from_rounded := NULL]

  swm_dt_hourly <- merge(swm_dt_hourly, absence_dt_long, by = "time_rounded", all.x = T)
  swm_dt_hourly[is.na(occupied), occupied := TRUE]
  swm_dt_hourly[, building := basename(path)]
  setnames(swm_dt_hourly, c("total_m3", "washing_machine", "time_rounded"), c("water_volume_m3", "washing", "date_time"))
  return(swm_dt_hourly)
}

# WM_A_08 Has a recording error !
data_wm <- read_wilhelm(file.path(temp_dir, "wilhelm", "out_of_house", "WM_A_11"))
data_wm[, water_use_l := (water_volume_m3 - shift(water_volume_m3, type = "lag", n = 1, fill = NA)) * 1000]
data_wm[, water_volume_m3 := NULL]
setnafill(data_wm, type = "nocb", cols = "water_use_l")


## Data from Schaffer er al. ----------------------------------------------

# Schaffer, M., Jensen, R. L., Larsen, T. S., Marszal-Pomianowska, A., Rohde,
# L., Rubak, E., & Vera-Valdés, J. E. (2025). Residential Household Dataset:
# Occupancy, Water, and Electricity Data (Department of Civil Engineering
# Technical Report, Issue 327). Aalborg University.
# https://doi.org/10.54337/aau780546283

download.file("https://zenodo.org/records/15180141/files/final_data.zip", destfile = file.path(temp_dir, "validation.zip"), mode = "wb")
unzip(file.path(temp_dir, "validation.zip"), exdir = file.path(temp_dir, "validation"))
data_build <- fread(file.path(temp_dir, "validation", "final_data", "hourly_data.csv"))


## Combine data -----------------------------------------------------------

validation_dt <- rbind(data_wm, data_build, fill = TRUE)
validation_dt[, occupied_fct := factor(occupied, levels = c("TRUE", "FALSE"))]


# Manual daily occupancy ------------------------------------------------

validation_dt[, hour := hour(date_time)]
validation_dt[, date := as.IDate(date_time, tz = "Europe/Copenhagen")]
validation_dt[hour < 12, date := date - 1]
validation_dt[building == "BUILD_H_03" & date %in% as.Date(c("2024-05-18", "2024-05-19")), daily_occ := FALSE]
validation_dt[building == "BUILD_H_04" & date %in% as.Date(c("2024-08-18", "2024-08-19", "2024-08-20")), daily_occ := FALSE]
validation_dt[building == "BUILD_H_06" & date == as.Date("2024-07-03"), daily_occ := FALSE]
validation_dt[building == "BUILD_H_08" & date > as.Date("2024-07-15"), daily_occ := FALSE]
validation_dt[is.na(daily_occ), daily_occ := TRUE]


# Estimate hourly occupancy based on SWM data  -------------------------------

# Define weights based on data length per house
validation_dt[, w_total := (1 / .N) / .NGRP, by = building]
validation_dt[daily_occ == TRUE, w_hour := (1 / .N) / .NGRP, by = building]

## Assuming all water equal occupancy ------------------------------------
validation_dt[, occ_water_use := water_use_l != 0]
validation_dt[daily_occ == FALSE, occ_water_use := FALSE]

# MCC per building, without weighting and including weighting
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE")), case_weights = w_total), 3)]

# MCC only for occupied days per building, without weighting and including weighting
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_water_use, levels = c("TRUE", "FALSE")), case_weights = w_hour), 3)]

## Include smoothing for occupancy ----------------------------------------
# Single not occupied hour between occupied is treated as occupied

validation_dt[, rle_encoding := rle2(occ_water_use), by = building]
validation_dt[, occ_smooth := occ_water_use]
validation_dt[occ_smooth == F & rle_encoding == 1, occ_smooth := T]
validation_dt[, rle_encoding := NULL]
validation_dt[daily_occ == FALSE, occ_smooth := FALSE]

# MCC per building, without weighting and including weighting
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE")), case_weights = w_total), 3)]

# MCC only for occupied days per building, without weighting and including weighting
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_smooth, levels = c("TRUE", "FALSE")), case_weights = w_hour), 3)]


## Include night occupancy --------------------------------------------------
# Handle the night occupancy problem.
# Set the period between the last occupancy between 20:00 and midnight and the
# last occupancy between 5:00 and 10:00 to occupied if in the evening and
# morning occupancy was detected.
validation_dt[, hour := hour(date_time)]
validation_dt_lst <- split(validation_dt, by = "building")
walk(validation_dt_lst, ~ night_occ(.x, occ_name = "occ_smooth", new_occ_name = "occ_estimated", time_name = "date_time"))
validation_dt <- validation_dt_lst |> rbindlist()
validation_dt[daily_occ == FALSE, occ_estimated := FALSE]

# MCC per building, without weighting and including weighting
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE")), case_weights = w_total), 3)]

# MCC only for occupied days per building, without weighting and including weighting
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[daily_occ == TRUE, round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE")), case_weights = w_hour), 3)]

# Manual normalized confusion matrix
# True occupied
validation_dt[daily_occ == TRUE & occupied == TRUE, round(mean(occ_estimated), 2)]
# False unoccupied
validation_dt[daily_occ == TRUE & occupied == TRUE, round(1 - mean(occ_estimated), 2)]
# FALSE occupied
validation_dt[daily_occ == TRUE & occupied == FALSE, round(mean(occ_estimated), 2)]
validation_dt[daily_occ == TRUE & occupied == FALSE, round(mean(occ_estimated), 2), by = c("dishwasher", "washing")]
# TRUE unoccupied
validation_dt[daily_occ == TRUE & occupied == FALSE, round(1 - mean(occ_estimated), 2)]
validation_dt[daily_occ == TRUE & occupied == FALSE, round(1 - mean(occ_estimated), 2), by = c("dishwasher", "washing")]


# Estimate hourly occupancy based on SEM data  ------------------------------

# Log10 + smoothing
validation_dt[, log_el := log10(electrcity_use_kwh)]
validation_dt[, log_el := frollmean(log_el, align = "center", n = 5, adaptive = FALSE, algo = "exact"), by = building]
validation_dt[, log_el := nafill(log_el, type = "nocb"), by = building]
validation_dt[, log_el := nafill(log_el, type = "locf"), by = building]

# GeoMA
validation_dt[, occ_el := geo_ma(log_el, 0.05), by = building]
validation_dt[, el_geom_avg := geo_ma_avg(log_el, 0.05), by = building]

# Second smoothing
validation_dt[, occ_el := round(frollmean(occ_el, align = "center", n = 5, adaptive = FALSE, algo = "exact")), by = building]
validation_dt[, occ_el := nafill(as.numeric(occ_el), type = "nocb"), by = building]
validation_dt[, occ_el := nafill(as.numeric(occ_el), type = "locf"), by = building]

# Use manually labelled daily occupancy
validation_dt[daily_occ == FALSE, occ_el := FALSE]

# Add night occupancy
validation_dt_lst <- split(validation_dt, by = "building")
walk(validation_dt_lst, ~ night_occ(.x, occ_name = "occ_el", new_occ_name = "occ_el_estimated", time_name = "date_time"))
validation_dt <- validation_dt_lst |> rbindlist()
validation_dt[daily_occ == FALSE, occ_el_estimated := FALSE]

# Compare SWM and SEM based algorithm -------------------------------------


## SWM based  -------------------------------------------------------------
validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE")), case_weights = w_total), 3)]

# MCC only for occupied days per building, without weighting and including weighting
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_estimated, levels = c("TRUE", "FALSE")), case_weights = w_hour), 3)]


## SEM based  -------------------------------------------------------------

validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[!building %in% c("WM_A_11", "BUILD_H_02"), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE")), case_weights = w_total), 3)]

# MCC only for occupied days per building, without weighting and including weighting
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE"))), 3), by = "building"]
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE"))), 3)]
validation_dt[daily_occ == TRUE & (!building %in% c("WM_A_11", "BUILD_H_02")), round(mcc_vec(truth = occupied_fct, estimate = factor(occ_el_estimated, levels = c("TRUE", "FALSE")), case_weights = w_hour), 3)]


# Plot results -----------------------------------------------------------

# So all dates are in English
Sys.setlocale("LC_TIME", "English")

validation_dt[, plot_truth := factor(fcase(
  daily_occ == FALSE, "daily absent",
  washing == FALSE & dishwasher == TRUE, "absent +\ndishwasher",
  washing == TRUE & dishwasher == FALSE, "absent +\nwashing machine",
  washing == TRUE & dishwasher == TRUE, "absent +\ndishwasher &\ washing machine",
  washing == FALSE & dishwasher == FALSE, "absent",
  occupied == FALSE & is.na(washing) & is.na(dishwasher), "absent + no info"
), levels = c("water use", "el. use smooth", "daily absent", "absent", "absent + no info", "absent +\ndishwasher", "absent +\nwashing machine", "absent +\ndishwasher &\ washing machine"))]

validation_dt[, building := gsub("_", "-", building)]
plot_lst <- split(validation_dt, by = "building")

p_colors <- c(
  "water use" = "#004488",
  "el. use smooth" = "#BB5566",
  "daily absent" = "#EEDD88",
  "absent" = "#BBCC33",
  "absent + no info" = "#DDDDDD",
  "absent +\ndishwasher" = "#EE8866",
  "absent +\nwashing machine" = "#FFAABB",
  "absent +\ndishwasher & washing machine" = "#44BB99"
)


## Supplementary material plots -------------------------------------------

create_plot <- function(dt) {
  lvls <- dt[, as.character(unique(plot_truth))]
  lvls <- lvls[!is.na(lvls)]

  el_exists <- dt[, !all(is.na(electrcity_use_kwh))]

  meter_colors <- c("water use")

  if (el_exists) {
    meter_colors <- c(meter_colors, "el. use smooth")
  }

  existing_colors <- p_colors[names(p_colors) %in% c(meter_colors, lvls)]

  # Estimated occupancy
  dt[, swm_occ_span := rleid(occ_estimated)]
  swm_estimated <- dt[occ_estimated == T, .(start = min(date_time - hours(1)), end = max(date_time)), by = list(swm_occ_span, building)]

  if (el_exists) {
    dt[, sem_occ_span := rleid(occ_el_estimated)]
    sem_estimated <- dt[occ_el_estimated == T, .(start = min(date_time - hours(1)), end = max(date_time)), by = list(sem_occ_span, building)]
  }

  p_truth <- ggplot() +
    geom_tile(data = dt, aes(x = date_time - 1800, y = 0.5, width = 3600, height = Inf, fill = plot_truth), show.legend = TRUE) +
    scale_fill_manual(
      values = existing_colors,
      breaks = names(existing_colors),
      name = NULL,
      na.value = "transparent",
      guide = guide_legend(
        override.aes = list(
          alpha = c(rep(0.4, length(meter_colors)), rep(1, length(lvls))),
          color = existing_colors,
          linewidth = 0.15
        ),
        nrow = 1
      ),
      drop = FALSE,
    ) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d.%b", guide = guide_axis(angle = 45), timezone = "Europe/Copenhagen", name = NULL, expand = c(0.01, 0.01)) +
    theme_nice() +
    theme(legend.box = "vertical")

  p_swm <- p_truth +
    geom_step(data = dt, aes(x = date_time, y = water_use_l), color = "#004488", direction = "vh", linewidth = 0.2) +
    geom_rect(data = dt, aes(xmin = date_time, xmax = shift(date_time), ymax = water_use_l, ymin = 0), fill = "#004488", alpha = 0.4) +
    geom_linerange(data = swm_estimated, aes(xmin = start, xmax = end, y = I(0.5)), color = "black", linewidth = 0.25) +
    geom_point(data = swm_estimated, aes(x = start, y = I(0.5)), color = "black", size = 0.2) +
    geom_point(data = swm_estimated, aes(x = end, y = I(0.5)), color = "black", size = 0.2) +
    scale_y_continuous(name = "Water use L/h") +
    theme(
      plot.margin = margin(l = 5, r = 5, unit = "mm"),
      legend.position = "none", plot.title = element_text(size = 10, hjust = 0.5)
    ) +
    ggtitle(dt[1, building])

  if (el_exists) {
    p_sem <- p_truth +
      geom_step(data = dt, aes(x = date_time, y = log_el), color = "#BB5566", direction = "vh", linewidth = 0.2) +
      geom_step(data = dt, aes(x = date_time, y = el_geom_avg, color = "el. use avg."), direction = "vh", linewidth = 0.3) +
      geom_linerange(data = sem_estimated, aes(xmin = start, xmax = end, y = I(0.5), color = "estimated\noccupancy"), linewidth = 0.25) +
      geom_point(data = sem_estimated, aes(x = start, y = I(0.5), color = "estimated\noccupancy"), size = 0.2) +
      geom_point(data = sem_estimated, aes(x = end, y = I(0.5), color = "estimated\noccupancy"), size = 0.2) +
      scale_color_manual(
        values = c("estimated\noccupancy" = "black", "el. use avg." = "#DDAA33"),
        breaks = c("estimated\noccupancy", "el. use avg."),
        name = NULL,
        na.value = "transparent",
        guide = guide_legend(override.aes = list(fill = "transparent"), order = 1)
      ) +
      scale_y_continuous(name = expression("log"[10] ~ "(el. use - kWh/h)")) +
      theme(plot.margin = margin(l = 5, r = 5, t = 1, unit = "mm"), axis.title.y = element_text(size = 5, colour = "black"))

    y_limit <- layer_scales(p_sem)$y$range$range[1]
    p_sem <- p_sem + geom_rect(data = dt, aes(xmin = date_time, xmax = shift(date_time), ymax = log_el, ymin = y_limit, fill = "el. use smooth"), alpha = 0.4)

    p_final <- p_swm / p_sem + plot_layout(axes = "collect")
  }
  if (!el_exists) {
    p_final <- p_swm / grid::textGrob("no electricity data available") +
      plot_layout(axes = "collect", guides = "collect") & theme(legend.position = "bottom")
  }
  return(p_final)
}

plots <- map(plot_lst, ~ create_plot(.x))

iwalk(plots, ~ ggsave(
  filename = paste0("plots/hourly_validation/", .y, ".pdf"),
  plot = .x,
  width = 160,
  height = 130,
  units = "mm",
  device = cairo_pdf
))


## Publication plot -------------------------------------------------------


create_plot_pub <- function(dt) {
  existing_colors <- p_colors[-c(5, 8)]

  dt[, swm_occ_span := rleid(occ_estimated)]
  dt[, sem_occ_span := rleid(occ_el_estimated)]

  swm_estimated <- dt[occ_estimated == T, .(start = min(date_time - hours(1)), end = max(date_time)), by = list(swm_occ_span, building)]
  sem_estimated <- dt[occ_el_estimated == T, .(start = min(date_time - hours(1)), end = max(date_time)), by = list(sem_occ_span, building)]


  p_truth <- ggplot() +
    geom_tile(data = dt, aes(x = date_time - 1800, y = 0.5, width = 3600, height = Inf, fill = plot_truth), show.legend = TRUE) +
    scale_fill_manual(
      values = existing_colors,
      breaks = names(existing_colors),
      name = NULL,
      na.value = "transparent",
      guide = guide_legend(
        override.aes = list(
          alpha = c(0.4, 0.4, rep(1, length(existing_colors) - 2)),
          color = existing_colors,
          linewidth = 0.15
        ),
        nrow = 1
      ),
      drop = FALSE,
    ) +
    scale_x_datetime(date_breaks = "1 day", date_labels = "%d.%b", guide = guide_axis(angle = 45), timezone = "Europe/Copenhagen", name = NULL, expand = c(0.01, 0.01)) +
    theme_nice() +
    theme(legend.box = "vertical")

  p_swm <- p_truth +
    geom_step(data = dt, aes(x = date_time, y = water_use_l), color = "#004488", direction = "vh", linewidth = 0.2) +
    geom_rect(data = dt, aes(xmin = date_time, xmax = shift(date_time), ymax = water_use_l, ymin = 0), fill = "#004488", alpha = 0.4) +
    geom_linerange(data = swm_estimated, aes(xmin = start, xmax = end, y = I(0.5)), color = "black", linewidth = 0.25) +
    geom_point(data = swm_estimated, aes(x = start, y = I(0.5)), color = "black", size = 0.2) +
    geom_point(data = swm_estimated, aes(x = end, y = I(0.5)), color = "black", size = 0.2) +
    scale_y_continuous(name = "Water use L/h") +
    theme(
      plot.margin = margin(l = 5, r = 5, unit = "mm"),
      legend.position = "none", plot.title = element_text(size = 10, hjust = 0.5)
    ) +
    ggtitle(dt[1, building])

  p_sem <- p_truth +
    geom_step(data = dt, aes(x = date_time, y = log_el), color = "#BB5566", direction = "vh", linewidth = 0.2) +
    geom_step(data = dt, aes(x = date_time, y = el_geom_avg, color = "el. use avg."), direction = "vh", linewidth = 0.3) +
    geom_linerange(data = sem_estimated, aes(xmin = start, xmax = end, y = I(0.5), color = "estimated\noccupancy"), linewidth = 0.25) +
    geom_point(data = sem_estimated, aes(x = start, y = I(0.5), color = "estimated\noccupancy"), size = 0.2) +
    geom_point(data = sem_estimated, aes(x = end, y = I(0.5), color = "estimated\noccupancy"), size = 0.2) +
    scale_color_manual(
      values = c("estimated\noccupancy" = "black", "el. use avg." = "#DDAA33"),
      breaks = c("estimated\noccupancy", "el. use avg."),
      name = NULL,
      na.value = "transparent",
      guide = guide_legend(override.aes = list(fill = "transparent"), order = 1)
    ) +
    scale_y_continuous(name = expression("log"[10] ~ "(el. use - kWh/h)")) +
    theme(plot.margin = margin(l = 5, r = 5, t = 1, unit = "mm"), axis.title.y = element_text(size = 5, colour = "black"))

  y_limit <- layer_scales(p_sem)$y$range$range[1]
  p_sem <- p_sem + geom_rect(data = dt, aes(xmin = date_time, xmax = shift(date_time), ymax = log_el, ymin = y_limit, fill = "el. use smooth"), alpha = 0.4)
  p_final <- p_swm / p_sem + plot_layout(axes = "collect")


  return(p_final)
}

pub_plots <- map(plot_lst[c(5, 6, 8)], ~ create_plot_pub(.x))
pub_plot <- (pub_plots[[1]] & theme(legend.position = "none")) /
  (pub_plots[[2]] & theme(legend.position = "none")) /
  (pub_plots[[3]] & theme(legend.key.size = unit(5, "mm"), legend.spacing = unit(1, "mm"))) +
  plot_layout(axes = "collect_x") +
  plot_annotation(tag_levels = "a", tag_suffix = ")") &
  theme(
    plot.tag.position = c(-0.02, 1),
    plot.tag = element_text(size = 8, hjust = 0, vjust = 0, face = "bold")
  )

# Figure 5
ggsave(
  filename = "plots/03_hourly_validation_paper.pdf",
  plot = pub_plot,
  width = 160,
  height = 180,
  units = "mm",
  device = cairo_pdf
)
