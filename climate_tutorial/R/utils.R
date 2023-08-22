# Global functions --------------------------------------------------------
# for the geographic range shiny app.

# Open the data set. Can probably generalize these
# to open csv and tsv files.

res = 96

open_file <- function(tx, st = NULL) {
  if (is.null(st)) {
    na_data <- readRDS("data/na_data.rds")
    data_file <- paste0("na_", tx)
    return(na_data[[data_file]])
    #file_to_open <- paste0("na_data/na_", tx, ".csv")
  } else {
    switch(st,
      "California" = {
        ca_data <- readRDS("data/ca_data.rds")
        return(ca_data[["california_marine_fishes"]])
        #file_to_open <- "marine/california_marine_fishes.csv"
      },
      {
        the_state <- str_replace_all(st, " ", "_")
        data_file <- paste0(the_state, "_", tx)
        return(state_data[[data_file]])
#        file_to_open <- paste0("state_data/", the_state, "_", tx, ".csv")
      }
    )
  }
#  read.csv(file_to_open, row.names = 1)
}



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
  ggplot(data = dat, aes(x = MAP, y = MAT, color = Ecosys, shape = Species)) +
    geom_point(size = 2) +
    labs(x = "Mean Annual Precipitation (mm)",
         y = "Mean Annual Temperature (°C)") +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2") +
    scale_shape_discrete(name = "Ecosystem", labels = c("Western Redcedar", "Grassland", "Subalpine Larch")) +
    annotate("text", x = 550, y = 4.7, label = "Grassland", size = 5) +
    annotate("text", x = 1900, y = 8.5, label = "Redcedar", size = 5) +
    annotate("text", x = 1450, y = -1, label = "Larch", size = 5) +
    guides(color = "none") +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
}
