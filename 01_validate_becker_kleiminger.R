# Title: Re validation of electricity based occupant detection algorithm

# Purpose:  This script showcases the GeoMA algorithm developed by Becker and
# Kleiminger (2018) to detect occupants based on 30-minute electricity data. It
# uses the two accessible data sets used in their research. Additionally, it
# shows the performance of the  algorithm with 60-minute input.

# Becker, V., & Kleiminger, W. (2018). Exploring zero-training algorithms for
# occupancy detection based on smart meter measurements. Computer Science -
# Research and Development, 33(1–2), 25–36.
# https://doi.org/10.1007/s00450-017-0344-9

# Data files: data/eco/ (downloaded manually, from:
#             http://doi.org/10.5905/ethz-1007-35)

# Functions: functions/geo_ma.cpp

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages and functions ---------------------------------------------

library(data.table)
library(Rcpp)
library(lubridate)
library(ggplot2)
library(yardstick)
library(purrr)

sourceCpp("functions/geo_ma.cpp")


# Night algorithm ---------------------------------------------------------

fn_night_occ <- function(data, occ_name = "water_use_custom", new_occ_name = "occ_w_night", time_name = "time_rounded") {
  data[, hour := hour(get(time_name))]
  # Create a pseudo day that includes all hours from morning_time to < night_time.
  data[, pseudo_day := fifelse(
    hour < 20,
    yes = as.Date(get(time_name), tz = attributes(get(time_name))$tz) - 1,
    no = as.Date(get(time_name), tz = attributes(get(time_name))$tz)
  )]

  data[, idx := seq_len(.N), by = pseudo_day]

  # At least 1 h of occupancy
  # 30 minute data
  if (all(data[, minute(get(time_name))] == 0)) {
    data[hour >= 20 | hour == 0, evening_occ := sum(get(occ_name)) > 2, by = pseudo_day]
  } else { # 60 min data
    data[hour >= 20 | hour == 0, evening_occ := any(get(occ_name)), by = pseudo_day]
  }

  # Detect if anytime during night_time to midnight occupancy was detected
  # suppressWarnings is needed because max of an empty vector throws a warning
  # max of an empty vector returns - Inf as.numeric is needed because Inf values
  # work only with double. Thus Inf in an int col is NA
  data[, evening_occ_idx := suppressWarnings(as.numeric(max(idx[get(occ_name) == TRUE & hour >= 20 & evening_occ == TRUE]))), by = pseudo_day]
  # Handle first day assuming that in the unknown evening occ was detected
  data[pseudo_day == min(pseudo_day), evening_occ_idx := min(idx)]

  # Set occ true from last detected occ in the evening till 9 o'clock in the morning
  data[is.finite(evening_occ_idx) & idx >= evening_occ_idx & (hour <= 9 | hour >= 20), night_occ := TRUE]
  data[, (new_occ_name) := fifelse(night_occ == TRUE | get(occ_name) == TRUE, yes = TRUE, no = FALSE, na = FALSE)]
  data[, c("pseudo_day", "evening_occ_idx", "idx", "night_occ", "evening_occ") := NULL]
  return(NULL)
}

# Smart* (Chen’s) data --------------------------------------------------------

temp_dir <- tempdir()
download.file("https://lass.cs.umass.edu/smarttraces/2017/occupancy-niom.tar.gz", destfile = file.path(temp_dir, "occupancy-niom.tar.gz"), mode = "wb")
untar(file.path(temp_dir, "occupancy-niom.tar.gz"), exdir = file.path(temp_dir, "smart"))


fn_smart <- function(name_part) {
  el_data <- fread(file.path(temp_dir, "smart", "selected", paste0(name_part, "_energytrace.txt")))

  # Naming inconsistency
  if (name_part == "Home_B_summer") {
    occ_data1 <- fread(file.path(temp_dir, "smart", "selected", paste0(name_part, "__person_1.txt")))
    occ_data2 <- fread(file.path(temp_dir, "smart", "selected", paste0(name_part, "__person_2.txt")))
  } else {
    occ_data1 <- fread(file.path(temp_dir, "smart", "selected", paste0(name_part, "_person_1.txt")))
    occ_data2 <- fread(file.path(temp_dir, "smart", "selected", paste0(name_part, "_person_2.txt")))
  }
  setnames(occ_data1, "V4", "occ1")
  setnames(occ_data2, "V4", "occ2")
  occ_data <- merge(occ_data1[, .(V1, occ1)], occ_data2[, .(V1, occ2)], , by = "V1")
  data <- merge(occ_data, el_data, by = "V1")
  data[, building := name_part]
  return(data)
}
smart_builings <- sub("^(([^_]+_){2}[^_]+).*", "\\1", list.files(file.path(temp_dir, "smart", "selected"), pattern = "*.txt")) |> unique()

smart_dt <- map(smart_builings, ~ fn_smart(.x)) |> rbindlist()
setnames(smart_dt, c("V2", "V3"), c("datetime", "el"))
smart_dt[, datetime := parse_date_time2(datetime, "%m/%d/%y %H:%M")]
smart_dt[, occ := occ1 == 1 | occ2 == 1]


## Original 30 minutes --------------------------------------------------------
smart_dt[, datetime_round := ceiling_date(datetime, unit = "30 minute")]
smart_agg_dt <- smart_dt[, .(el = sum(el), occ = round(mean(occ))), by = list(datetime_round, building)]
smart_agg_dt[, occ := as.logical(occ)]
smart_agg_dt[, mean(occ)]

smart_agg_dt[, log_el := log10(el)]
smart_agg_dt[, log_el := frollmean(log_el, align = "center", n = 5, adaptive = FALSE, algo = "exact"), by = building]
smart_agg_dt[, log_el := nafill(log_el, type = "nocb"), by = building]
smart_agg_dt[, log_el := nafill(log_el, type = "locf"), by = building]
smart_agg_dt[, occ_el := geo_ma(log_el, 0.05), by = building]

smart_agg_dt[, occ_el := round(frollmean(occ_el, align = "center", n = 5, adaptive = FALSE, algo = "exact")), by = building]
smart_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "nocb"), by = building]
smart_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "locf"), by = building]

smart_lst <- split(smart_agg_dt, by = "building")
walk(smart_lst, ~ fn_night_occ(.x, occ_name = "occ_el", new_occ_name = "occ_el_night", time_name = "datetime_round"))
smart_agg_dt <- smart_lst |> rbindlist()


# MCC and accuracy for the original 30 minutes data
smart_agg_dt[, mcc_vec(factor(occ, levels = c("TRUE", "FALSE")), factor(occ_el_night, levels = c("TRUE", "FALSE")))] |> round(2)
smart_agg_dt[, mean(occ == occ_el_night)] |> round(2)


## New 60 minutes --------------------------------------------------------
smart_dt[, datetime_round := ceiling_date(datetime, unit = "1 hour")]
smart_agg_dt <- smart_dt[, .(el = sum(el), occ = round(mean(occ))), by = list(datetime_round, building)]
smart_agg_dt[, occ := as.logical(occ)]
smart_agg_dt[, mean(occ)]

smart_agg_dt[, log_el := log10(el)]
smart_agg_dt[, log_el := frollmean(log_el, align = "center", n = 5, adaptive = FALSE, algo = "exact"), by = building]
smart_agg_dt[, log_el := nafill(log_el, type = "nocb"), by = building]
smart_agg_dt[, log_el := nafill(log_el, type = "locf"), by = building]
smart_agg_dt[, occ_el := geo_ma(log_el, 0.05), by = building]

smart_agg_dt[, occ_el := round(frollmean(occ_el, align = "center", n = 5, adaptive = FALSE, algo = "exact")), by = building]
smart_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "nocb"), by = building]
smart_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "locf"), by = building]

smart_lst <- split(smart_agg_dt, by = "building")
walk(smart_lst, ~ fn_night_occ(.x, occ_name = "occ_el", new_occ_name = "occ_el_night", time_name = "datetime_round"))
smart_agg_dt <- smart_lst |> rbindlist()

# MCC and accuracy for the 60 minutes data
smart_agg_dt[, mcc_vec(factor(occ, levels = c("TRUE", "FALSE")), factor(occ_el_night, levels = c("TRUE", "FALSE")))] |> round(2)
smart_agg_dt[, mean(occ == occ_el_night)] |> round(2)


# Eco data ----------------------------------------------------------------

# Needed to download manually and separately as otherwise connection closes
# as the accepted copyright expired before the download is completed.
# http://doi.org/10.5905/ethz-1007-35

fn_occ <- function(file_path) {
  unzip(file_path, exdir = file.path(temp_dir, gsub(".zip", "", basename(file_path))))
  csv_files <- list.files(file.path(temp_dir, gsub(".zip", "", basename(file_path))), full.names = TRUE)
  data <- map(csv_files, ~ fread(.x))
  walk(data, ~ .x[, V1 := fast_strptime(V1, format = "%d-%b-%Y", lt = FALSE)])
  walk2(data, gsub(".csv|[0-9]|_", "", basename(csv_files)), ~ .x[, season := .y])
  data <- map(data, ~ melt(.x, id.vars = c("V1", "season"), variable.name = "time", value.name = "occ", variable.factor = FALSE))
  data <- rbindlist(data)

  setorder(data, "V1", "time")

  data[, idx := seq_len(.N), by = V1]
  data[, date_time := V1 + idx]
  data[, c("time", "V1", "idx") := NULL]

  data[, building := as.numeric(substr(basename(file_path), 1, 2))]
  return(data)
}
fn_el <- function(file_path) {
  unzip(file_path, exdir = file.path(temp_dir, gsub(".zip", "", basename(file_path))))


  if (substr(basename(file_path), 1, 2) == "04") {
    csv_files <- file.path(temp_dir, gsub(".zip", "", basename(file_path)), gsub(".zip", "", basename(file_path)), substr(basename(file_path), 1, 2)) |>
      list.files(full.names = TRUE)
  } else {
    csv_files <- file.path(temp_dir, gsub(".zip", "", basename(file_path)), substr(basename(file_path), 1, 2)) |>
      list.files(full.names = TRUE)
  }
  fn_read <- function(csv_file) {
    dt <- fread(input = csv_file, select = 1, col.names = "el")
    dt[, date := as.POSIXct(gsub(".csv", "", basename(csv_file)))]
    dt[, date_time := date + .I]
    dt[, date := NULL]
    return(dt)
  }
  data <- map(csv_files, ~ fn_read(.x)) |> rbindlist()
  data[, building := as.numeric(substr(basename(file_path), 1, 2))]
}

occ_files <- list.files("data/eco", pattern = "occupancy", full.names = TRUE)
occ_dt <- map(occ_files, ~ fn_occ(.x)) |> rbindlist()
occ_dt[, occ := as.logical(occ)]

sm_files <- list.files("data/eco", pattern = "sm", full.names = TRUE)
sm_dt <- map(sm_files, ~ fn_el(.x)) |> rbindlist()

# NA encoding
sm_dt[el == -1, el := NA]
setnafill(sm_dt, type = "nocb", cols = "el")
eco_dt <- merge(occ_dt, sm_dt, by = c("date_time", "building"))


## Original 30 minutes --------------------------------------------------------

eco_dt[, datetime_round := ceiling_date(date_time, unit = "30 minute")]
eco_agg_dt <- eco_dt[, .(el = sum(el), occ = round(mean(occ))), by = list(datetime_round, building, season)]
eco_agg_dt[, occ := as.logical(occ)]
eco_agg_dt[, mean(occ)]

setorder(eco_agg_dt, "building", "season", "datetime_round")

eco_agg_dt[, log_el := log10(el)]
eco_agg_dt[, log_el := frollmean(log_el, align = "center", n = 5, adaptive = FALSE, algo = "exact"), by = list(building, season)]
eco_agg_dt[, log_el := nafill(log_el, type = "nocb"), by = list(building, season)]
eco_agg_dt[, log_el := nafill(log_el, type = "locf"), by = list(building, season)]
eco_agg_dt[, occ_el := geo_ma(log_el, 0.05), by = list(building, season)]

eco_agg_dt[, occ_el := round(frollmean(occ_el, align = "center", n = 5, adaptive = FALSE, algo = "exact")), by = list(building, season)]
eco_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "nocb"), by = list(building, season)]
eco_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "locf"), by = list(building, season)]

eco_agg_lst <- split(eco_agg_dt, by = c("building", "season"))
walk(eco_agg_lst, ~ fn_night_occ(.x, occ_name = "occ_el", new_occ_name = "occ_el_night", time_name = "datetime_round"))
eco_agg_dt <- eco_agg_lst |> rbindlist()

# MCC and accuracy for the original 30 minutes data
eco_agg_dt[, mcc_vec(factor(occ, levels = c("TRUE", "FALSE")), factor(occ_el_night, levels = c("TRUE", "FALSE"))) |> round(2)]
eco_agg_dt[, mean(occ == occ_el_night) |> round(2)]


## New 60 minutes --------------------------------------------------------
eco_dt[, datetime_round := ceiling_date(date_time, unit = "1 hour")]
eco_agg_dt <- eco_dt[, .(el = sum(el), occ = round(mean(occ))), by = list(datetime_round, building, season)]
eco_agg_dt[, mean(occ)]
eco_agg_dt[, occ := as.logical(occ)]

eco_agg_dt[, log_el := log10(el)]
eco_agg_dt[, log_el := frollmean(log_el, align = "center", n = 5, adaptive = FALSE, algo = "exact"), by = list(building, season)]
eco_agg_dt[, log_el := nafill(log_el, type = "nocb")]
eco_agg_dt[, log_el := nafill(log_el, type = "locf")]
eco_agg_dt[, occ_el := geo_ma(log_el, 0.05), by = list(building, season)]

eco_agg_dt[, occ_el := round(frollmean(occ_el, align = "center", n = 5, adaptive = FALSE, algo = "exact")), by = list(building, season)]
eco_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "nocb"), by = list(building, season)]
eco_agg_dt[, occ_el := nafill(as.numeric(occ_el), type = "locf"), by = list(building, season)]

eco_agg_lst <- split(eco_agg_dt, by = c("building", "season"))
walk(eco_agg_lst, ~ fn_night_occ(.x, occ_name = "occ_el", new_occ_name = "occ_el_night", time_name = "datetime_round"))
eco_agg_dt <- eco_agg_lst |> rbindlist()

# MCC and accuracy for the 60 minutes data
eco_agg_dt[, mcc_vec(factor(occ, levels = c("TRUE", "FALSE")), factor(occ_el_night, levels = c("TRUE", "FALSE"))) |> round(2)]
eco_agg_dt[, mean(occ == occ_el_night) |> round(2)]
