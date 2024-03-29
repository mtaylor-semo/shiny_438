# Raster density plots for all U.S. species
# and select families.

plot_na_grid <- function(plot_data = NULL) {
  ggplot(data = world) +
    geom_raster(
      data = plot_data,
      aes(x = long, y = lat, fill = N),
      interpolate = TRUE
    ) +
    scale_fill_viridis_c(guide = NULL, option = "magma") +
    geom_path(
      data = rivers,
      aes(x = X1, y = X2),
      color = "grey70",
      linewidth = 0.25
    ) +
    geom_sf(
      color = "gray80",
      fill = NA,
      linewidth = 0.25
    ) +
    geom_sf(
      data = states, 
      color = "gray80", 
      fill = NA, 
      linewidth = 0.25
    ) +
    coord_sf(
      default_crs = sf::st_crs(4326),
      xlim = c(-125, -65),
      ylim = c(24, 49),
      expand = FALSE
    ) +
    annotate(
      geom = "text", x = -90, y = 26.5, label = "Gulf of Mexico",
      color = "white", size = 4
    ) +
    annotate(
      geom = "text", x = -73, y = 30.5, label = "Atlantic",
      color = "gray90", size = 4
    ) +
    annotate(
      geom = "text", x = -121, y = 30.5, label = "Pacific",
      color = "gray90", size = 4
    ) +
    labs(x = "Longitude", y = "Latitude") +
    # Remove space from bottom for closer legend
    theme(
      plot.margin = margin(0, 0, -1, 0, "cm"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    )
}
