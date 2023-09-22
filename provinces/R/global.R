library(tidyr)
library(ggplot2)

# Global vars -------------------------------------------------------------

# Define file name constants
base_rmd <- "provinces.Rmd"
base_pdf <- "provinces.pdf"


world <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))


# Semo colors for plots. Cardiac Red and Riverfront
semo_palette <- c("#9d2235", "#003b5c", "#000000", "#C8102E", "#A2AAAD")
names(semo_palette) <- c("cardiac_red", "riverfront", "rich_black", "semo_red", "pewter")

# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96


# Open data files ---------------------------------------------------------

# nafish_grid contains all U.S. fishes
# state_fishes contains fishes for select U.S. states
# species_groups contains select families of U.S. freshwater fishes
# Rivers is the data set to plot the rivers.
nagrid <- read_csv("data/NAfish_grid.csv", show_col_types = FALSE) %>% select(-1)
state_fishes <- readRDS("data/state_fishes.rds")
species_groups <- readRDS("data/species_groups.rds")
rivers <- read_table("data/rivers.txt", col_names = c("X1", "X2"), show_col_types = FALSE)


# Global functions --------------------------------------------------------

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
fix_special_chars <- function(str = NULL) {
  str_replace_all(str, "([#%$_])", "\\\\\\1")
}

has_empty_input <- function(lst = NULL) {
  if (any(lst == "")) {
    "Please fill in all blanks."
  }
}

# Pivot csv data to long format for ggplot2
prepare_data <- function(.data) {
  # Define the latitude and longitude for the data and plotting.
  lat <- 24:49
  long <- -125:-65

  colnames(.data) <- as.character(long)
  mutate(.data, lat = lat) %>%
    pivot_longer(
      cols = -lat,
      names_to = "long",
      values_to = "N"
    ) %>%
    mutate(
      long = as.integer(long),
    )
}

# Province functions ------------------------------------------------------

# Functions used by the Provinces exercise.

# Will I need these in Shiny?

# Reference list for the optimum cluster cuts for each state.
## Alabama 7, PCO
## Mississippi 7, PCO
## Georgia Fishes 6
## North Carolina 5
## South Carolina 5 NO LONGER USED
## Virginia 6
# 3 Missouri 7

# Number of cuts for the cluster and nmds analysis.
# Cut number provided to students for each state.
readCutoff <- function() {
  cutoff <- readline(prompt = "Enter the number of clusters provided for you: ")
  if (!grepl("^[5-7]$", cutoff)) { # Ensure cuts are between 5 and 7
    cat(file = stderr(), "The cluster number must be a number between 5-7.")
    return(
      readCutoff()
    )
  }
  return(
    as.integer(cutoff)
  )
}


## Prediction check. Move requirement check for predictions here.
## sn = student_name, ps = pred_state, ra = pred_ra, pc = pred_ca
pred_check <- function(sn = NULL, na = NULL, pc = NULL) {
  req(sn, na, pc)
}

result_check <- function(exp = NULL) {
  req(exp)
}


empty_field <- function(input_field) {
  ifelse (input_field == "", TRUE, FALSE)
}

