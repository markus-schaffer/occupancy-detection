# Title: Establish and Analyse large scale occupancy

# Purpose: This script uses the prepared large scale SWM data and establishes
# daily and hourly occupancy based on the proposed algorithm. Afterwards, his
# occupancy is aggregated and plotted

# Data files: data/04_large_scale_swm_processed.fst

# Functions: functions/plot_styles.R

# Outputs: plots/05_daily_occupancy.pdf (Figure 6)
#          plots/05_hourly_occupancy.pdf(Figure 7)

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages and functions ---------------------------------------------

library(data.table)
library(fst)
library(purrr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)
# library(grid)


source("functions/plot_styles.R")
source("functions/hourly_occupancy.R")
source("functions/holidays.R")


# Load & process data -----------------------------------------------------

swm_dt <- read_fst("data/04_large_scale_swm_processed.fst", as.data.table = TRUE)

## Occupancy detection ---------------------------------------------------
### Daily occupancy  ----------------------------------------------------


swm_dt[, `:=`(
  daily_mean = mean(water_volume_demand_l),
  usage_events = sum(water_volume_demand_l != 0)
), by = list(date, water_meter_id)]

# Thresholds from validation study
swm_dt[, `:=`(
  event_limit = mean(usage_events) * 0.3,
  usage_limit = mean(daily_mean) * 0.12
), by = water_meter_id]

swm_dt[, daily_occ := daily_mean > usage_limit & usage_events > event_limit]
swm_dt[, c("daily_mean", "usage_events", "event_limit", "usage_limit") := NULL]


### Hourly occupancy -----------------------------------------------------

# Water use == Occupancy
swm_dt[, occ_water_use := water_use]
swm_dt[daily_occ == FALSE, occ_water_use := FALSE]

# Set single non occupied hours between occupied ones to occupied
swm_dt[, rle_encoding := rle2(occ_water_use), by = water_meter_id]
swm_dt[, occ_smooth := occ_water_use]
swm_dt[occ_smooth == F & rle_encoding == 1, occ_smooth := T]
swm_dt[, rle_encoding := NULL]

# Solve night occupancy
swm_dt_lst <- split(swm_dt, by = "water_meter_id")
walk(swm_dt_lst, ~ night_occ(.x, occ_name = "occ_smooth", new_occ_name = "occ_estimated", time_name = "time_rounded"), .progress = "night occupancy")
swm_dt <- swm_dt_lst |> rbindlist()

swm_dt[, c("occ_water_use", "occ_smooth") := NULL]


### Add needed information for plotting --------------------------------------

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

swm_dt[, day := yday(time_rounded)]
swm_dt[, month := month(time_rounded, label = TRUE, abbr = TRUE, locale = "English_United States")]

swm_dt[, pseudo_hour := hour(time_rounded)]
swm_dt[day == yday(dst_range$start), pseudo_hour := seq_len(.N) - 1, by = water_meter_id]
swm_dt[, pseudo_time := seq_len(.N), by = water_meter_id]
swm_dt[, weekday := wday(time_rounded, label = TRUE, week_start = 1, locale = "English_United States")]

# Daily analysis ----------------------------------------------------------

plots <- list()

holiday_dt <- swm_dt[water_meter_id == water_meter_id[1], .(time_rounded, month, weekday, week)]
holiday_dt[, holiday := holidays(time_rounded)]
holiday_dt <- unique(holiday_dt[, -"time_rounded"])
holiday_dt[, y_min := 0]
holiday_dt[, y_max := 24]
holiday_dt[, x_min := week - 0.5]
holiday_dt[, x_max := week + 0.5]

daily_mean <- swm_dt[, .(mean_occ = mean(daily_occ)), by = list(month, weekday, week, pseudo_hour)]
daily_mean[, range(mean_occ)]

plots[["daily_occ"]] <- ggplot() +
  geom_raster(data = daily_mean, aes(x = week, y = pseudo_hour, fill = mean_occ), vjust = 0) +
  geom_rect(data = holiday_dt[holiday != "No holiday"], aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, color = "black"), fill = "transparent", linewidth = 0.1) +
  facet_grid(cols = vars(month), rows = vars(weekday), scale = "free", space = "free") +
  scale_x_continuous(breaks = NULL, expand = c(0, 0), name = NULL) +
  scale_y_reverse(name = "hours", expand = c(0, 0), breaks = c(0, 12, 24), limits = c(24, 0)) +
  scale_fill_gradientn(
    colours = rev(gradient_occ),
    name = "Buildings occupied",
    na.value = "transparent",
    limits = c(0.7, 1),
    oob = scales::squish,
    breaks = c(0.7, 0.8, 0.9, 1),
    labels = paste0(c("\U2264 ", rep("", 3)), c(70, 80, 90, 100), "%")
  ) +
  scale_color_manual(values = "black", label = "", name = "Holidays") +
  guides(color = guide_legend(order = 2), fill = guide_colorbar(order = 1)) +
  theme_time() +
  theme(
    legend.position = "right",
    panel.spacing.x = unit(1, "mm"),
    panel.spacing.y = unit(1, "mm"),
    legend.title.position = "bottom",
    axis.text.y = element_text(vjust = c(1, 0.5, 0))
  )

# the pdf device leads to better alignment of the fills than cairo_pdf but
# causes problems with the ≤ sign. The publication plot was done with pdf and
# the ≤ was manually inserted.
# Figure 6
ggsave("plots/05_daily_occupancy.pdf",
  plot = plots[["daily_occ"]],
  width = 160,
  height = 80,
  units = "mm",
  device = cairo_pdf
)


# Hourly analysis  -----------------------------------------------------

## Plot holidays -------------------------------------------------------

holiday_dt <- swm_dt[water_meter_id == water_meter_id[1], .(time_rounded, month, day)]
holiday_dt[, holiday := holidays(time_rounded)]

holiday_dt <- rbind(
  holiday_dt[holiday != "No holiday" & holiday != "Christmas holiday", .(x_min = min(day), x_max = max(day)), by = list(holiday)],
  holiday_dt[holiday == "Christmas holiday" & month == "Jan", .(x_min = min(day), x_max = max(day)), by = list(holiday)],
  holiday_dt[holiday == "Christmas holiday" & month == "Dec", .(x_min = min(day), x_max = max(day)), by = list(holiday)]
)

holiday_dt[, x_min := x_min - 0.5]
holiday_dt[, x_max := x_max + 0.5]
holiday_dt[, y_min := 0]
holiday_dt[, y_max := 1]
holiday_dt[, breaks := x_min + (x_max - x_min) / 2]
holiday_dt[, label := gsub(" holiday", "", holiday)]

plots[["holidays"]] <- ggplot() +
  geom_rect(data = holiday_dt, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = I("transparent"), colour = I("black")), linewidth = 0.15) +
  scale_y_discrete(name = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = holiday_dt$breaks, minor_breaks = NULL, labels = holiday_dt$label, expand = c(0, 0), name = NULL, guide = guide_axis(angle = 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 5, colour = "black"),
    axis.ticks = element_line(colour = "black", linewidth = 0.1, linetype = 1),
    panel.grid.major = element_blank()
  )


hourly_occ <- swm_dt[, .(mean_occ = mean(occ_estimated)), by = list(month, day, pseudo_hour)]

plots[["hourly_occ"]] <- ggplot() +
  geom_raster(data = hourly_occ, aes(x = day, y = pseudo_hour, fill = mean_occ), vjust = 0) +
  scale_x_continuous(breaks = NULL, expand = c(0, 0), name = NULL) +
  scale_y_reverse(name = "hour", expand = c(0, 0), breaks = seq(0, 24)) +
  facet_grid(cols = vars(month), scale = "free", space = "free") +
  scale_fill_gradientn(
    colours = rev(gradient_occ),
    name = "Buildings occupied",
    na.value = "transparent",
    limits = c(0.4, 1),
    oob = scales::squish,
    breaks = c(0.4, 0.6, 0.8, 1),
    labels = paste0(c("\U2264 ", rep("", 3)), c(40, 60, 80, 100), "%")
  ) +
  scale_color_manual(values = "black", label = "holidays", name = NULL) +
  guides(
    color = guide_legend(override.aes = list(fill = "white", key.size = unit(4, "mm"))),
    fill = guide_colorbar(title.position = "bottom", title.hjust = 0.5)
  ) +
  theme_time() +
  theme(
    axis.text.y = element_text(size = 4),
    legend.position = "right"
  )


layout <- c(
  area(1, 1),
  area(1, 1)
)

plots[["hourly_occ_overview"]] <- plots[["hourly_occ"]] +
  plots[["holidays"]] +
  plot_layout(design = layout)

# the pdf device leads to better alignment of the fills than cairo_pdf but
# causes problems with the ≤ sign. The publication plot was done with pdf and
# the ≤ was manually inserted.
# Figure 7
ggsave("plots/05_hourly_occupancy.pdf",
  plot = plots[["hourly_occ_overview"]],
  width = 160,
  height = 80,
  units = "mm",
  device = cairo_pdf
)
