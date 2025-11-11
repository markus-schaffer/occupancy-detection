# Color & plot theme definition ----------------------------------------------

gradient_vib <- c("#FEFBE9", "#FCF7D5", "#F5F3C1", "#EAF0B5", "#DDECBF", "#D0E7CA", "#C2E3D2", "#B5DDD8", "#A8D8DC", "#9BD2E1", "#8DCBE4", "#81C4E7", "#7BBCE7", "#7EB2E4", "#88A5DD", "#9398D2", "#9B8AC4", "#9D7DB2", "#9A709E", "#906388", "#805770", "#684957", "#46353A")
gradient_occ <- c(
  "#F5F3C1", "#EAF0B5", "#DDECBF", "#D0E7CA",
  "#C2E3D2", "#B5DDD8", "#A8D8DC", "#9BD2E1", "#8DCBE4", "#81C4E7",
  "#7BBCE7", "#7EB2E4", "#88A5DD", "#9398D2", "#9B8AC4", "#9D7DB2",
  "#9A709E", "#906388", "#805770", "#684957", "#46353A"
)

# Internal: shared theme elements used by both themes
.theme_common <- function() {
  theme(
    text = element_text(size = 8, colour = "black"),
    axis.text = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", linewidth = 0.1),
    rect = element_rect(colour = "black", linewidth = 0.1),
    strip.background = element_rect(fill = "white", linewidth = 0.1),
    legend.position = "bottom"
  )
}

# Clean black/white theme with thin lines and small text
theme_nice <- function() {
  theme_bw() +
    .theme_common() +
    theme(
      # Kept explicit to guarantee 0.1 border width regardless of theme defaults
      axis.ticks   = element_line(colour = "black", linewidth = 0.1),
      panel.border = element_rect(colour = "black", linewidth = 0.1)
    )
}

# Classic theme tailored for time/facet plots
theme_time <- function() {
  theme_classic() +
    .theme_common() +
    theme(
      panel.spacing.x = unit(0, "mm"),
      panel.spacing.y = unit(1.5, "mm"),
      legend.title    = element_text(size = 6, colour = "black"),
      axis.ticks      = element_line(colour = "black", linewidth = 0.1),
      legend.margin   = margin(0, 0, 0, 0)
    )
}
