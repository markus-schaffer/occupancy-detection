# Title: Daily evaluation of proposed algorithm

# Purpose:  This script evaluates the daily occupant detection part of the
# proposed occupant detection algorithm based on water use data. It investigates
# the effect of the different thresholds for water use volume and usage events.

# The algorithm was originally proposed in: 
# van Alwon, J., Newing, A., Smith, A., Ellaway, S., Hibbert, O., & Merchant, P.
# (2022). WatPop: Inferring dwelling occupancy patterns and identifying tourist
# dwellings using high temporal resolution water metering data. In Inferring
# tourist dwelling characteristics and occupancy patterns using high temporal
# resolution water metering data. A technical report prepared for the Office for
# National Statistics as part of the ESRC funded ’WatPop: understanding seasonal
# population: Vol. 10.47389/3 (Issue No 4).
# https://eprints.whiterose.ac.uk/189680/

# And used in:
# Newing, A., Hibbert, O., Van-Alwon, J., Ellaway, S., & Smith, A. (2023). Smart
# water metering as a non-invasive tool to infer dwelling type and occupancy –
# Implications for the collection of neighbourhood-level housing and tourism
# statistics. Computers, Environment and Urban Systems, 105(August), 102028.
# https://doi.org/10.1016/j.compenvurbsys.2023.102028


# Functions: functions/000_styles.R

# Outputs: plots/02_daily_validation.pdf (Figure 4)


# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages and functions ---------------------------------------------

library(data.table)
library(forcats)
library(yardstick, include.only = "mcc")
library(purrr)
library(ggplot2)
library(patchwork)
library(scales)

source("functions/plot_styles.R")


# Download data from Schaffer et al. --------------------------------------

# Schaffer, M., Jensen, R. L., Larsen, T. S., Marszal-Pomianowska, A., Rohde,
# L., Rubak, E., & Vera-Valdés, J. E. (2025). Residential Household Dataset:
# Occupancy, Water, and Electricity Data (Department of Civil Engineering
# Technical Report, Issue 327). Aalborg University.
# https://doi.org/10.54337/aau780546283

temp_dir <- tempdir()
download.file("https://zenodo.org/records/15180141/files/final_data.zip", destfile = file.path(temp_dir, "validation.zip"), mode = "wb")
unzip(file.path(temp_dir, "validation.zip"), exdir = file.path(temp_dir, "validation"))
val_dt <- fread(file.path(temp_dir, "validation", "final_data", "daily_data.csv"))


## Majority voting --------------------------------------------------------
# Ties are ordered "Occupied","Not Occupied", "Uncertain"

val_dt[, `:=`(
  daily_mean = mean(water_use_l),
  usage_events = sum(water_use_l != 0)
), by = list(date, building)]

daily_dt <- val_dt[, -c("date_time", "water_use_l")] |> unique()

dt_melt <- melt(daily_dt[, -c("daily_mean", "usage_events")], id.vars = c("building", "date"), value.name = "truth")
dt_melt[, truth := factor(truth, levels = c("Occupied", "Not Occupied", "Uncertain"))]
dt_count <- dt_melt[, .N, by = list(truth, building, date)] |> setorder(building, date, -N, truth)
dt_count[, weight := N / sum(N), by = list(building, date)]
voting_dt <- dt_count[, .SD[1], by = list(building, date)]
voting_dt[weight != 1, .N]
voting_dt[truth == "Uncertain"]

# Exclude days with majority voting uncertain
voting_dt <- voting_dt[truth != "Uncertain"]

voting_dt[, truth := factor(truth, levels = c("Occupied", "Not Occupied"))]
voting_dt[, truth := fct_recode(truth, "TRUE" = "Occupied", "FALSE" = "Not Occupied")]
daily_dt <- merge(voting_dt, daily_dt, by = c("building", "date"))


# Evaluate occupant detection method --------------------------------------

fn_occ_detection <- function(dt, event_thres = 0.25, usage_thres = 0.25) {
  dt[, `:=`(
    event_limit = mean(usage_events) * event_thres,
    usage_limit = mean(daily_mean) * usage_thres
  ), by = building]

  dt[, estimate := daily_mean > usage_limit & usage_events > event_limit]
  dt[, estimate := factor(estimate, levels = c("TRUE", "FALSE"))]

  res <- mcc(dt, truth = truth, estimate = estimate, case_weights = weight) |> setDT()
  res[, "event_thres" := event_thres]
  res[, "usage_thres" := usage_thres]
  return(res)
}
thresholds <- expand.grid(list(event_thres = seq(0, 1, 0.01), usage_thres = seq(0, 1, 0.01)))

res <- map2(thresholds$event_thres, thresholds$usage_thres, ~ fn_occ_detection(daily_dt, event_thres = .x, usage_thres = .y)) |> rbindlist()
setorder(res, -.estimate)

# Best thresholds according to van Alwon, et al., 2022
(res[usage_thres == 0.25 & event_thres == 0.25, .estimate] - res[1, .estimate]) |> round(3)

# Difference between best and top 10%
(res[1, .estimate] - res[.N / 10, .estimate]) |> round(3)


# Plot results ------------------------------------------------------------

# Zoom into the top 10% of the results 
zoom <- res[seq_len(.N / 10), .(
  min_event_thres = min(event_thres),
  max_event_thres = max(event_thres),
  min_usage_thres = min(usage_thres),
  max_usage_thres = max(usage_thres)
)]

zoom_data <- res[usage_thres >= zoom$min_usage_thres &
  usage_thres <= zoom$max_usage_thres &
  event_thres >= zoom$min_event_thres &
  event_thres <= zoom$max_event_thres]


plots <- list()
plots[["all"]] <- ggplot(res, aes(x = event_thres, y = usage_thres, fill = .estimate)) +
  geom_raster() +
  geom_rect(data = zoom, aes(xmin = min_event_thres - 1 / 101 / 2, xmax = max_event_thres + 1 / 101 / 2, ymin = min_usage_thres - 1 / 101 / 2, ymax = max_usage_thres + 1 / 101 / 2), fill = "transparent", color = "black", linewidth = 0.1, inherit.aes = FALSE) +
  geom_point(data = data.table(x = 0.25, y = 0.25), aes(x = x, y = y), shape = 4, size = .5, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = gradient_vib, name = "MCC", limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("≤0.00", "0.25", "0.50", "0.75", "1.00"), oob = squish) +
  guides(fill = guide_colorbar(order = 1), shape = guide_legend(override.aes = c(size = 1))) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(expand = c(0, 0), name = "event threshold", labels = label_percent()) +
  scale_y_continuous(expand = c(0, 0), name = "water use threshold", labels = label_percent()) +
  labs(tag = "a)") +
  theme_nice() +
  theme(legend.position = "right", plot.margin = unit(c(0, 0, 0, 0), "mm"))

plots[["zoom"]] <- ggplot(zoom_data, aes(x = event_thres, y = usage_thres, fill = .estimate)) +
  geom_raster() +
  geom_point(data = data.table(x = 0.25, y = 0.25), aes(x = x, y = y), shape = 4, size = 1, inherit.aes = FALSE) +
  scale_fill_gradientn(colors = gradient_vib, name = "MCC", breaks = c(seq(0.75, 1, 0.05))) +
  guides(fill = guide_colorbar(order = 1)) +
  coord_fixed(ratio = 1) +
  scale_x_continuous(expand = c(0, 0), name = "event threshold", labels = label_percent()) +
  scale_y_continuous(expand = c(0, 0), name = "water use threshold", labels = label_percent(), n.breaks = 4) +
  labs(tag = "b)") +
  theme_nice() +
  theme(legend.position = "right", plot.margin = unit(c(0, 0, 0, 0), "mm"))

plots[["legend"]] <- ggplot() +
  geom_point(data = data.table(x = 0.25, y = 0.25), aes(x = x, y = y, shape = "Newing et al., 2023"), size = 0, stroke = 0, inherit.aes = FALSE) +
  scale_shape_manual(values = 4, name = NULL, guide = guide_legend(override.aes = list(size = 1, stroke = 1))) +
  theme_void() +
  theme(legend.position = "bottom", plot.margin = unit(c(0, 0, 0, 0), "mm"), legend.margin = margin(0, 0, 0, 0, "mm"))

plots[["final"]] <- plots[["all"]] / plots[["zoom"]] / plots[["legend"]] +
  plot_layout(heights  = c(1, .5, 0.001), widths = rep(1,3))

# (Figure 4)
ggsave(
  "plots/02_daily_validation.pdf",
  plot = plots[["final"]],
  width = 88,
  height = 120,
  units = "mm",
  device = cairo_pdf
)