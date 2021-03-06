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
## sn = student_name, ps = pred_state, pn = pred_na, pc = pred_ca
pred_check <- function(sn = NULL, ps = NULL, pn = NULL, pc = NULL) {
  req(sn, ps, pn, pc)
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
