# Set lower and upper scale limits for
# NMDS plots
plot_scale_limits <- function(vec, res, fun) {
  if (fun == "min") {
    floor(min(vec) / res) * res
  } else {
    ceiling(max(vec) / res) * res
  }
}

# Cluster analysis --------------------------------------------------------

plot_cluster <- function(dend_obj = NULL) {
  ggplot(dend_obj, horiz = TRUE, offset_labels = -0.02) +
    scale_x_continuous(expand = c(-1, -1)) +
    scale_y_reverse(expand = expansion(mult = c(0.1, 0.5))) +
    #scale_y_reverse(expand = c(0, 1)) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}


# NMDS plot ---------------------------------------------------------------

plot_nmds <- function(scores = NULL) {
  
  min_y <- plot_scale_limits(scores$NMDS2, 0.2, "min")
  max_y <- plot_scale_limits(scores$NMDS2, 0.2, "max")
  min_x <- plot_scale_limits(scores$NMDS1, 0.2, "min")
  max_x <- plot_scale_limits(scores$NMDS1, 0.2, "max")
  
  
  ggplot(scores) +
  geom_point(aes(
    x = NMDS1, y = NMDS2),
    color = scores$colr,
    size = 3
  ) +
  geom_text(aes(x = NMDS1, 
                y = NMDS2, 
                label = label),
            color = scores$colr,
            size = 5,
            vjust = -1, 
            hjust = .50) + 
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3) +
  scale_colour_manual(values = mycolors, guide = NULL) +
  scale_y_continuous(breaks = seq(min_y, max_y, 0.1),
                     limits = c(min_y, max_y),
                     expand = c(0, 0.3)) +
  scale_x_continuous(breaks = seq(min_x, max_x, 0.1),
                     limits = c(min_x, max_x)) +
  coord_equal() +
  theme_minimal() +
  theme(
    line = element_blank(),
    text = element_text(size = 12),
    axis.text = element_blank()
  )
}
