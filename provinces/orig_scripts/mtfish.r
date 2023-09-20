mycolors <- c(
  "blue3", "darkcyan", "darkgoldenrod", "brown", "purple",
  "deeppink1", "darkorange", "darkolivegreen", "maroon", "darksalmon",
  "darkred", "forestgreen", "darkblue", "darkturquoise", "red", "green3",
  "gray50", "gray33", "darkmagenta", "black"
)

mtfish <- read.csv("provinces/data/montana_fishes.csv", row.names = 1)
clustCut <- 4

fish.hel <- decostand(mtfish, method = "hellinger")


# Calculate distance between watersheds using Bray-Curtis dissimilarity.
fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)

fish.clust <- hclust(fish.dist, method = "ward.D2")
fish.clust.cut <- cutree(fish.clust, k = clustCut)

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

nataxa.pco = cmdscale(fish.dist, k = 3, eig=TRUE)
plot(nataxa.pco$points, main = 'North American Fishes PCO', xlab = 'PCO 1', ylab = 'PCO 2', col=mycolors[1:clustCut], pch=19)

text(nataxa.pco$points, labels = rownames(mtfish), col=mycolors[1:clustCut], adj=1.1, cex=0.8)
abline(h=0, col='grey50')
abline(v=0, col='grey50')

library('rgl')

plot3d(nataxa.pco$points, col = mycolors[1:clustCut], type='s', size=1, xlab='PCO1', ylab='PCO2', zlab='PCO3')

text3d(nataxa.pco$points, text=rownames(mtfish), col = mycolors[1:clustCut], cex=0.7, adj = -0.3)
