library(readr)
library(tidyr)
library(dplyr)
# Script for Distribution of Species Richess
# BI 438 Biogeography

# Read the data set of all U.S. fishes
# Prepare the data for ggplot using function
# defined in setup.r
nagrid <- read_csv("provinces/data/NAfish_grid.csv") %>% select(-1)
grid_long <- prepare_data(nagrid)

# colnames(nagrid) <- as.character(long)
# nagrid <- nagrid %>%
#   mutate(lat = lat)# %>%
# #  select(lat, everything())

# grid_long <- nagrid %>%
#   pivot_longer(
#     cols = c(`-125`:`-65`),
#     names_to = "long",
#     values_to = "N"
#   ) %>%
#   mutate(
# #    lat = as.numeric(lat),
#     long = as.numeric(long),
# #    N = as.numeric(N)
#   )


plot_na_grid()

# ggplot(data = world) +
#   geom_raster(
#     data = grid_long,
#     aes(x = long, y = lat, fill = N),
#     interpolate = TRUE
#   ) +
#   scale_fill_viridis_c(guide = NULL, option = "cividis") +
#   geom_path(data = rivers, aes(x = X1, y = X2), color = "grey70", size = 0.25) +
#   geom_sf(color = "gray80", fill = NA, size = 0.25) +
#   geom_sf(data = states, color = "gray80", fill = NA, size = 0.25) +
#   coord_sf(
#     default_crs = sf::st_crs(4326),
#     xlim = c(-125, -65),
#     ylim = c(24, 49),
#     expand = FALSE
#   ) +
#   annotate(geom = "text", x = -90, y = 26.5, label = "Gulf of Mexico",
#            color = "white", size = 3) +
#   annotate(geom = "text", x = -73, y = 30.5, label = "Atlantic",
#            color = "gray90", size = 3) +
#   annotate(geom = "text", x = -121, y = 30.5, label = "Pacific",
#            color = "gray90", size = 3) +
#   labs(x = "Longtitude", y = "Latitude") +
#   theme_bw()


