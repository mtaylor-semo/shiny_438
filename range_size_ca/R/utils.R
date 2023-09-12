library(tidyr)

# Global functions --------------------------------------------------------
# for the geographic range shiny app.

# Semo colors for plots.
mycolors <- c("#9d2235", "#003b5c")

# Open data
cafish <- readRDS("data/ca_data.rds")[["california_marine_fishes"]]

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
fix_special_chars <- function(str = NULL){
  str_replace_all(str, "([#%$_])", "\\\\\\1")
}

# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96



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

plotHistogram <- function(dat = NULL, x = NULL, closed = "right", breaks = c(y, z), ...) {
  ggplot(data = dat, aes(x = x)) +
    geom_histogram(
      #        binwidth = 5,
      closed = closed,
      breaks = seq(0, breaks[1], breaks[2]),
      color = "white",
      fill = "#9d2235"
    ) +
    xlab("Number of Watersheds") +
    ylab("Number of Species") +
    xlim(0, breaks[1]) +
    theme_minimal()
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
                       values = c("(-31,34]" = mycolors[1],
                                  "(34,68]" = mycolors[2]),
                       labels = c("South of P.C.",
                                  "North of P.C."))
  
}
