

# Read the data file.
fishFile <- getFishFile()

# fish <- tryCatch({
#   read.csv(fishFile, row.names = 1)
#   },
#   error = function(cond) {
#     message("Here's the original error message:")
#     message(cond)
#     # Choose a return value in case of error
#     return(NULL)
#   },
#   warning=function(cond) {
#     message(cond)
#     message("\nSource 'cluster.r' again and enter the correct file name.")
#     return()
#   },
#   finally = NULL
#   )


clustCut <- readCutoff()


# fish <- read.csv("mississippi_fishes.csv", row.names = 1)
# Transform the data using hellinger. Necessary for data
# with many zeros.
fish.hel <- decostand(fishFile, method = "hellinger")


# Calculate distance between watersheds using Bray-Curtis dissimilarity.
fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)

###################################################
# Cluster Analysis
###################################################

fish.clust <- hclust(fish.dist, method = "ward.D2")
fish.clust.cut <- cutree(fish.clust, k = clustCut)
# plot(fish.clust, labels = paste(rownames(fish), fish.clust.cut), xlab='', ylab=NULL, sub='', axes=FALSE, main='Fishes')

dend <- as.dendrogram(fish.clust, rotate = TRUE)
# dend_data <- dendro_data(dend, type = "rectangle", rotate = TRUE)

dend1 <- as.ggdend(dend %>% set("branches_k_color",
  value = mycolors[1:clustCut], k = clustCut
) %>%
  set("labels_col", value = mycolors[1:clustCut], k = clustCut) %>%
  set("branches_lwd", 0.5) %>%
  set("labels_cex", 0.75)) # %>%
# plot(horiz = TRUE)

denplt <- ggplot(dend1, horiz = TRUE) +
  scale_x_continuous(expand = c(-1, -1)) +
  scale_y_reverse(expand = c(1, 0)) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

print(denplt)
