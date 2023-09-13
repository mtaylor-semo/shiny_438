library(tidyr)
library(ggplot2)

# Global functions --------------------------------------------------------
# for the geographic range shiny app.

# Semo colors for plots. Cardiac Red and Riverfront
semo_palette <- c("#9d2235", "#003b5c", "#000000", "#C8102E", "#A2AAAD")
names(semo_palette) <- c("cardiac_red", "riverfront", "rich_black", "semo_red", "pewter")

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
fix_special_chars <- function(str = NULL){
  str_replace_all(str, "([#%$_])", "\\\\\\1")
}

# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96

# Open data
na_grid <- readRDS("data/rapo_data.rds")$nagrid
fish_area <- readRDS("data/rapo_data.rds")$fish_area

gis_data <- readRDS("data/gis_data.rds")


## Prediction check. Move requirement check for predictions here.
## sn = student_name, ps = pred_state, pn = pred_na, pc = pred_ca
pred_check <- function(sn = NULL, pn = NULL, pc = NULL) {
  req(sn, pn, pc)
}

result_check <- function(exp = NULL) {
  req(exp)
}


empty_field <- function(input_field) {
  ifelse (input_field == "", TRUE, FALSE)
}

plot_champagne <- function(dat = NULL, ...) {
  fish_area$dia <- sqrt(fish_area$area / pi)
  plot(
    fish_area$long,
    fish_area$lat,
    las = 1,
    cex = fish_area$dia,
    lwd = 1.2,
    col = semo_palette["riverfront"],
    xlab = expression(paste('Longitude ', degree, 'W')),
    ylab = expression(paste('Latitude ', degree, 'N'))
  )
  
  lines(gis_data$coast_line, lwd = 0.25, col = semo_palette["pewter"])
  
  lines(gis_data$bnd,
        lwd = 0.25,
        lty = 'dashed',
        col = semo_palette["pewter"])
  
  lines(gis_data$rivers, lwd = 0.25, col = semo_palette["pewter"])
}

plot_latitudes <- function(dat = NULL, ...) {
  #mean_spp <- apply(na_grid, 1, mean)
  #max_spp <- apply(na_grid, 1, max)
  #lat <- 24:49
  
  plot_data <- tibble(mean_spp = apply(na_grid, 1, mean),
                      max_spp = apply(na_grid, 1, max),
                      lat = 24:49)
  
  mean_plot <- plot_data %>% 
    ggplot() +
    geom_segment(aes(x = mean_spp,
                     xend = max_spp,
                     y = lat,
                     yend = lat),
                 color = semo_palette["rich_black"],
                 linewidth = 0.25) +
    geom_point(aes(x = mean_spp,
                   y = lat),
               size = 3,
               color = semo_palette["cardiac_red"]) +
    geom_point(aes(x = max_spp,
                   y = lat),
               size = 3,
               color = semo_palette["riverfront"]) +
    scale_y_continuous(breaks = seq(25, 50, 5)) +
    theme_minimal() +
    labs(x = "Mean and maximum species richness",
         y = "Latitude °N") +
    theme(panel.grid.minor = element_blank())
  
  mean_plot
}


plotPC <- function(dat = NULL, ...) {
  numSpecies <- dat %>% 
    select(-1) %>% 
    summarize(across(everything(), sum)) %>% 
    pivot_longer(cols = everything(),
                 names_to = "latitude", values_to = "numSpecies") %>% 
    mutate(latitude = str_replace(latitude, "S", "-"),
           latitude = str_remove(latitude, "N"),
           latitude = as.numeric(latitude))
  
  numSpecies %>% ggplot() +
    geom_vline(xintercept = 34.4,
               color = "gray") +
    geom_point(aes(x = latitude, y = numSpecies,
                   color = cut(latitude, c(-31, 34, 68)))) +
    scale_x_continuous(breaks = seq(-30, 70, 5)) +
    theme_minimal() +
    labs(x = "Latitude (°S - °N)",
         y = "Species richness") +
    scale_color_manual(name = "Latitude",
                       #values = c("(-31,34]" = scale_pal[1],
                      #            "(34,68]" = scale_pal[2]),
                       values = c("(-31,34]" = mycolors[1],
                                  "(34,68]" = mycolors[2]),
                       labels = c("South of P.C.",
                                  "North of P.C."))
  
}
