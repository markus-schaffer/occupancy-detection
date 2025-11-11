# Title: Prepare large scale data

# Purpose:  This script prepares the large scale SWM data. The data originates
# for this data set:

# Schaffer, M., Veit, M., Marszal-Pomianowska, A., Frandsen, M., Pomianowski, M.
# Z., Dichmann, E., Sørensen, C. G., & Kragh, J. (2024). Dataset of smart heat
# and water meter data with accompanying building characteristics. Data in
# Brief, 52, 109964. https://doi.org/10.1016/j.dib.2023.109964


# The processing uses an detection of abnormal SWM use developed by:

# Ghamkhar, H., Jalili Ghazizadeh, M., Mohajeri, S. H., Moslehi, I., &
# Yousefi-Khoshqalb, E. (2023). An unsupervised method to exploit low-resolution
# water meter data for detecting end-users with abnormal consumption: Employing
# the DBSCAN and time series complexity. Sustainable Cities and Society,
# 94(December 2022), 104516. https://doi.org/10.1016/j.scs.2023.104516

# This method was implemented via python code (04a_abnormal_detection.py) which
# was provided by the authors.

# Data files: data/04_large_scale_swm.fst

# Functions: functions/plot_styles.R

# Outputs: plots/04_abnormal_profiles.pdf (Figure 3)
#          data/04_large_scale_swm_processed.fst

# Author: M. Schaffer
# Contact details: msch@build.aau.dk

# Load packages and functions ---------------------------------------------

library(data.table)
library(fst)
library(purrr)
library(lubridate)
library(ggplot2)
library(reticulate)

source("functions/plot_styles.R")
use_condaenv("fd_swm")
source_python("04a_abnormal_detection.py")


# Read SWM data and process it --------------------------------------------

water_dt <- read_fst("data/04_large_scale_swm.fst", as.data.table = TRUE)

# Calculate hourly water use
water_dt[, water_volume_m3 := round(water_volume_m3, 3)]
water_dt[, water_volume_demand_l := (water_volume_m3 - shift(water_volume_m3, type = "lag", n = 1, fill = NA)) * 1000, by = water_meter_id]

# A few meters are assigned to two customers
water_dt <- merge(water_dt, water_dt[, .(uniqueN(water_meter_id), water_meter_id = unique(water_meter_id)), by = customer_id][V1 == 1, "water_meter_id"],
  by = "water_meter_id"
)
# Customer ID  is not longer needed
water_dt[, customer_id := NULL]


# Some meters have only 8759 hours of data. Fill the one missing hour
setkey(water_dt, water_meter_id, time_rounded)
water_dt <- water_dt[CJ(water_meter_id, time_rounded = seq.POSIXt(as.POSIXct("2022-01-01", "Europe/Copenhagen"), as.POSIXct("2022-12-31 23:00:00", "Europe/Copenhagen"), by = "hour"), unique = TRUE)]
cols <- c("water_volume_m3", "water_volume_demand_l")
water_dt[, (cols) := lapply(.SD, nafill, "nocb"), .SDcols = cols, by = water_meter_id]
water_dt[, (cols) := lapply(.SD, nafill, "locf"), .SDcols = cols, by = water_meter_id]


# Leakage detection  ------------------------------------------------------

# Assume that at least once per day the water use should be 0L
water_dt[, day := yday(time_rounded)]
water_dt[, hour := hour(time_rounded)]
water_dt[, hour_min := hour[which.min(water_volume_demand_l)], by = list(day, water_meter_id)]

# Some of the smallest values are outside the night period
hour_dt <- unique(water_dt[, .(water_meter_id, day, hour_min)])
(hour_dt[hour_min > 6, .N] / hour_dt[, .N]) * 100
water_dt[, min_daily := min(water_volume_demand_l), by = list(day, water_meter_id)]

# Share of days with nonzero min value
(water_dt[min_daily != 0, uniqueN(.SD), .SDcols = c("day", "water_meter_id")] / water_dt[, uniqueN(.SD), .SDcols = c("day", "water_meter_id")]) * 100

water_dt[, water_volume_demand_l := water_volume_demand_l - min_daily]
water_dt[, water_use := water_volume_demand_l != 0]
water_dt[, c("hour_min", "min_daily", "day") := NULL]


# Exclusion based on water use frequency ----------------------------------

# Exclude data from meters which did not use water for at least 3 hours per day
# on average (1095 hours with water use) or did use water for more than 21 hours
# per day on average (7665 hours with water use)
fraction_use <- water_dt[, .(water_use = mean(water_use)), by = water_meter_id]
setorder(fraction_use, water_use)
fraction_use[, suspicious := water_use <= ((365 * 3) / 8760) | water_use > ((8760 - (365 * 3)) / 8760)]

water_dt <- merge(water_dt, fraction_use[suspicious == FALSE, "water_meter_id"], by = "water_meter_id")
water_dt[, uniqueN(water_meter_id)]

# Abnormal consumption ----------------------------------------------------

# Calculate weekly data used for the detection of abnormal customers
water_dt[, week := isoweek(time_rounded)]
water_dt[, month := month(time_rounded, label = TRUE, locale = "English_United States")]
water_dt[month == "Jan" & week > 6, week := week - week]

water_week_dt <- water_dt[, .(value = max(water_volume_m3) - min(water_volume_m3)), by = list(water_meter_id, week)]

# Normalize data to range of 0-1
fn_min_max <- function(x, maximum = 1, minimum = 0) {
  x_std <- (x - min(x)) / (max(x) - min(x))
  x_scaled <- x_std * (maximum - minimum) + minimum
  return(x_scaled)
}

water_week_dt[, value := fn_min_max(value), by = water_meter_id]
setnames(water_week_dt, "water_meter_id", "id")

# Python function
abnormal_meters <- main_fd(water_week_dt)


# Exclude identified abnormal buildings and plot exemplary profiles
# abnormal_meters <- fread("data/01a_abnormal_meters.csv")
water_dt <- water_dt[!water_meter_id %in% abnormal_meters$id]
water_dt[, uniqueN(water_meter_id)]


## Plot for publication ---------------------------------------------------

water_week_dt[, type := "typical"]
water_week_dt[id %in% abnormal_meters$id, type := "atypical"]

set.seed(876)
plot_dt <- water_week_dt[id %in% c(water_week_dt[type == "typical", unique(id)[sample(uniqueN(id), 5)]], water_week_dt[type == "atypical", unique(id)[sample(uniqueN(id), 5)]])]

plot_dt[, id := rleid(id), by = type]

p <- ggplot(plot_dt, aes(x = week, y = value, group = paste0(id, type))) +
  geom_line(linewidth = 0.1) +
  facet_grid(rows = vars(id), cols = vars(type)) +
  labs(y = "normalised weekly water use") +
  theme_nice()

# Figure 3
ggsave(
  plot = p,
  filename = "plots/04_abnormal_profiles.pdf",
  device = cairo_pdf,
  width = 88,
  height = 88,
  units = "mm"
)


# Prepare data for Occupant detection -------------------------------------

# Add date defined from noon to noon
water_dt[, date := as.IDate(time_rounded, tz = "Europe/Copenhagen")]
water_dt[hour < 12, date := date - 1]

# Remove the resulting incomplete days at the beginning and end
water_dt <- merge(water_dt, water_dt[, .N, by = c("water_meter_id", "date")][N > 12, -"N"], by = c("water_meter_id", "date"))

# Make each day 24 values long - i.e. get rid of daylight saving time as this
# would cause a shift in the data.
get_dst <- function(y = 2019, tz) {
  start <- map_chr(y, ~ paste0(.x, "-01-01"))
  end <- map_chr(y, ~ paste0(.x, "-12-31"))
  d1 <- map2(start, end, ~ seq(as.POSIXct(.x, tz = tz),
    as.POSIXct(.y, tz = tz),
    by = "hour"
  ))
  map_dfr(d1, ~ range(.x[dst(.x)]) |> setNames(c("start", "end")))
}
dst_range <- get_dst(2022, "Europe/Copenhagen")
water_dt <- rbind(water_dt, water_dt[time_rounded == dst_range$start])
water_dt <- water_dt[time_rounded != dst_range$end]
setorder(water_dt, water_meter_id, time_rounded)

water_dt[, .N, by = c("water_meter_id", "date")][N != 24]

setcolorder(water_dt, c("water_meter_id", "time_rounded", "date"))
water_dt[, c("water_volume_m3") := NULL]

setkey(water_dt, "water_meter_id", "time_rounded", "date")
write_fst(water_dt, "data/04_large_scale_swm_processed.fst", compress = 100)
