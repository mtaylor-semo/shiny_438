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
## sn = student_name, ps = pred_state, ra = pred_ra, pc = pred_ca
pred_check <- function(sn = NULL, ra = NULL, pc = NULL) {
  req(sn, ra, pc)
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
    cex = fish_area$dia * 0.6,
    lwd = 1.2,
    col = semo_palette["cardiac_red"],
    xlab = expression(paste('Longitude ', degree, 'W')),
    ylab = expression(paste('Latitude ', degree, 'N'))
  )
  
  lines(gis_data$coast_line, lwd = 0.25, col = semo_palette["rich_black"])
  
  lines(gis_data$bnd,
        lwd = 0.25,
        lty = 'dashed',
        col = semo_palette["rich_black"])
  
  lines(gis_data$rivers, lwd = 0.25, col = semo_palette["rich_black"])
}

plot_latitudes <- function(dat = NULL, ...) {

  plot_data <- tibble(mean_spp = apply(dat, 1, mean),
                      max_spp = apply(dat, 1, max),
                      lat = 24:49)
  
  lat_plot <- plot_data %>% 
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
  
  lat_plot
}

plot_relative_area <- function(dat = NULL, ...) {
  #plot(dat$lat ~ dat$area, main='Relative Area Occupied by U.S Freshwater Fishes', xlab = 'Area', ylab = expression(paste('Latitude ',degree,'N')))
  rel_area_plot <- dat %>% 
    ggplot() +
    geom_point(aes(x = dat$area, y = dat$lat),
               color = semo_palette["cardiac_red"]) +
    theme_minimal() +
    labs(x = "Relative area occupied",
         y = "Degrees latitude °N")
  
  rel_area_plot
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
