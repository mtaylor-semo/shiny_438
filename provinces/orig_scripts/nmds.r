###################################################
# Non-metric Dimensional Analysis
###################################################
# Non-metric Dimensional Analysis (NMDS) is a method that allows complex data
# to be more readily interpreted.  Here, similar watersheds tend to cluster
# together in 2-D space, providing a different view of the cluster results.
fish.nmds <- metaMDS(fishFile, k = 2, trymax = 100)

## GGPlot

watershed_scores <- as_tibble(scores(fish.nmds, display = "sites")) %>%
  mutate(
    watershed = rownames(scores(fish.nmds, display = "sites")),
    grp = fish.clust.cut
  )


plot_scale_limits <- function(vec, res, fun){
  if (fun == "min")
    floor(min(vec) / res) * res
  else
    ceiling(max(vec) / res) * res
}

min_y <- plot_scale_limits(watershed_scores$NMDS2, 0.1, "min")
max_y <- plot_scale_limits(watershed_scores$NMDS2, 0.1, "max")
min_x <- plot_scale_limits(watershed_scores$NMDS1, 0.1, "min")
max_x <- plot_scale_limits(watershed_scores$NMDS1, 0.1, "max")

nmdplt <- watershed_scores %>%
  ggplot() +
  geom_point(aes(
    x = NMDS1, y = NMDS2,
    color = as.factor(grp)
  )) +
  geom_text(aes(x = NMDS1, y = NMDS2, label = watershed, color = as.factor(grp)), vjust = -1, hjust = .50) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3) +
  scale_colour_manual(values = mycolors[1:clustCut], guide = NULL) +
  scale_y_continuous(breaks = seq(min_y, max_y, 0.1),
                     limits = c(min_y, max_y)) +
  scale_x_continuous(breaks = seq(min_x, max_x, 0.1),
                     limits = c(min_x, max_x)) +
  coord_equal() +
  theme_minimal() +
  theme(
    line = element_blank(),
    text = element_text(size = 12),
    axis.text = element_blank()
   )

print(nmdplt)
