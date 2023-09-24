# Plot functions used by Provinces app.
library(RColorBrewer)
mycolors <- brewer.pal(8, "Dark2")

# Set lower and upper scale limits for
# NMDS plots
plot_scale_limits <- function(vec, res, fun) {
  if (fun == "min") {
    floor(min(vec) / res) * res
  } else {
    ceiling(max(vec) / res) * res
  }
}

prep_cluster_data <- function(state = "Montana", cutoff = 4) {
  fish.hel <- decostand(state, method = "hellinger")
  fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)
  fish.clust <- hclust(fish.dist, method = "ward.D2")
  fish.clust.cut <- cutree(fish.clust, k = cutoff)
  list(fish.clust, fish.clust.cut)
}
# fishFile <- state_fishes$Montana
#clustCut <- 4

# Transform the data using hellinger, then calculate Bray-Curtis
# distance between watersheds

# Cluster analysis --------------------------------------------------------


plot_cluster <- function(state = NULL, cutoff = 7) {
  mycolors <- brewer.pal(8, "Dark2")
  
  fish.hel <- decostand(state, method = "hellinger")
  fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)
  fish.clust <- hclust(fish.dist, method = "ward.D2")

  dend <- as.dendrogram(fish.clust, rotate = TRUE) %>%
  set("branches_k_color",
    value = mycolors[1:cutoff], k = cutoff
  ) %>%
  set("labels_colors", value = mycolors[1:cutoff], k = cutoff) %>%
  set("branches_lwd", 0.5) %>%
  set("labels_cex", 0.75)
  
  print(cutoff)

  dend1 <- as.ggdend(dend)
    

  ggplot(dend1, horiz = TRUE) +
    scale_x_continuous(expand = c(-1, -1)) +
    scale_y_reverse(expand = c(1, 1)) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}


# NMDS plot ---------------------------------------------------------------

# fish.nmds <- metaMDS(state_fishes$Montana, k = 2, trymax = 100)
# 
# 
# 
# watershed_scores <- as_tibble(scores(fish.nmds, display = "sites")) %>%
#   mutate(
#     watershed = rownames(scores(fish.nmds, display = "sites")),
#     grp = fish.clust.cut
#   )
# 
# min_y <- plot_scale_limits(watershed_scores$NMDS2, 0.1, "min")
# max_y <- plot_scale_limits(watershed_scores$NMDS2, 0.1, "max")
# min_x <- plot_scale_limits(watershed_scores$NMDS1, 0.1, "min")
# max_x <- plot_scale_limits(watershed_scores$NMDS1, 0.1, "max")
# 
# nmdplt <- watershed_scores %>%
#   ggplot() +
#   geom_point(aes(
#     x = NMDS1, y = NMDS2,
#     color = as.factor(grp)
#   )) +
#   geom_text(aes(x = NMDS1, y = NMDS2, label = watershed, color = as.factor(grp)), vjust = -1, hjust = .50) +
#   geom_hline(yintercept = 0, color = "gray70", linewidth = 0.3) +
#   geom_vline(xintercept = 0, color = "gray70", linewidth = 0.3) +
#   scale_colour_manual(values = mycolors[1:clustCut], guide = NULL) +
#   scale_y_continuous(breaks = seq(min_y, max_y, 0.1),
#                      limits = c(min_y, max_y)) +
#   scale_x_continuous(breaks = seq(min_x, max_x, 0.1),
#                      limits = c(min_x, max_x)) +
#   coord_equal() +
#   theme_minimal() +
#   theme(
#     line = element_blank(),
#     text = element_text(size = 12),
#     axis.text = element_blank()
#   )
# 
# print(nmdplt)
