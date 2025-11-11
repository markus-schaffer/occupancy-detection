rle2 <- function(x) {
  res <- rle(x)$lengths
  return(rep(res, res))
}

night_occ <- function(data,occ_name = "water_use_custom", new_occ_name = "occ_w_night",  time_name = "time_rounded", morning_time = 10, night_time = 20) {
  # Create a pseudo day that includes all hours from morning_time to < night_time.
  data[, pseudo_day := fifelse(
    hour < night_time,
    yes = as.Date(get(time_name), tz = attributes(get(time_name))$tz) - 1,
    no = as.Date(get(time_name), tz = attributes(get(time_name))$tz)
  )]

  data[, idx := seq_len(.N), by = pseudo_day]
  # Detect if anytime during night_time to midnight occupancy was detected
  # suppressWarnings is needed because max of an empty vector throws a warning
  # max of an empty vector returns - Inf
  # as.numeric is needed because Inf values work only with double. Thus Inf in an int col is Na
  data[, evening_occ_idx := suppressWarnings(as.numeric(max(idx[get(occ_name) == TRUE & hour >= night_time]))), by = pseudo_day]
  # Handle first day assuming that in the unknown evening occ was detected
  data[pseudo_day == min(pseudo_day), evening_occ_idx := min(idx)]

  # Detect if anytime during 05:00 to morning_time occupancy was detected
  # suppressWarnings is needed because max of an empty vector throws a warning
  data[, morning_occ_idx := suppressWarnings(as.numeric(max(idx[get(occ_name) == TRUE & (hour >= 5 & hour <= morning_time)]))), by = pseudo_day]
  # Handling of the last day - assuming that in the unknown morning occ was detected
  data[pseudo_day == max(pseudo_day), morning_occ_idx := max(idx)]

  # Set occ true from last detected occ in the evening till last one in the morning
  # If evening_occ_idx or morning_occ_idx is not finite no occ was detected
  data[is.finite(evening_occ_idx) & is.finite(morning_occ_idx) &
    idx >= evening_occ_idx & idx <= morning_occ_idx, night_occ := TRUE]
  data[, (new_occ_name) := fifelse(night_occ == TRUE | get(occ_name) == TRUE, yes = TRUE, no = FALSE, na = FALSE)]


  data[, c("pseudo_day", "evening_occ_idx", "morning_occ_idx", "idx", "night_occ") := NULL]
  return(NULL)
}