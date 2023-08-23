# Global functions --------------------------------------------------------
# for the climate tutorial shiny app.

# Open the data set. Can probably generalize these
# to open csv and tsv files.

res = 96


## Prediction check. Move requirement check for predictions here.
## sn = student_name, pn = pred_na
pred_check <- function(sn = NULL, pn = NULL) {
  req(sn, pn)
}

result_check <- function(exp = NULL) {
  req(exp)
}


empty_field <- function(input_field) {
  ifelse (input_field == "", TRUE, FALSE)
}


plotScatter <- function(dat = NULL, ...) {
  ggplot(data = dat, aes(x = MAP, y = MAT, color = Species, shape = Species)) +
    geom_point(size = 2) +
    labs(x = "Mean Annual Precipitation (mm)",
         y = "Mean Annual Temperature (°C)") +
    theme_minimal() +
    scale_shape_discrete(name = "Ecosystem", labels = c("Western Redcedar", "Grassland", "Subalpine Larch")) +
    scale_color_brewer(palette = "Dark2", 
                       name = "Ecosystem", 
                       labels = c("Western Redcedar", 
                                  "Grassland", 
                                  "Subalpine Larch")) +
    annotate("text", x = 700, y = 4, label = "Grassland", size = 4, hjust = "left") +
    annotate("text", x = 2100, y = 8, label = "Redcedar", size = 4, hjust = "left") +
    annotate("text", x = 1800, y = 0, label = "Larch", size = 4, hjust = "left")
}
