library(tidyr)
library(ggplot2)
library(readr)
library(tibble) # Remove once MO data set is finalized. Needed for column_to_rownames
library(dplyr)
library(stringr)

library(shinycssloaders)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)
library(maps)

library(vegan)
library(ggdendro)
library(dendextend)
library(RColorBrewer)

# Global vars -------------------------------------------------------------


# Semo colors for plots. Cardiac Red and Riverfront used most often.
semo_palette <- c("#9d2235", 
                  "#003b5c", 
                  "#000000", 
                  "#C8102E", 
                  "#A2AAAD")
names(semo_palette) <- c("cardiac_red", 
                         "riverfront", 
                         "rich_black", 
                         "semo_red", 
                         "pewter")

mycolors <- brewer.pal(8, "Dark2")

# Define file name constants
base_rmd <- "provinces.Rmd"
base_pdf <- "provinces.pdf"

# Reference list for the optimum cluster cuts for each state.
## Alabama 6, PCO
## Georgia Fishes 6
## Mississippi 7, PCO
## Missouri 5
## North Carolina 5
## South Carolina 5
## Virginia 5

# 2 October 2023: Remove Montana from dataset as that is moved to
# spearate exercise. Add South Carolina so complete fall line from
# Virginia to Mississippi

# Vector of cut numbers to group the clusters
state_cuts <- c(6, 6, 7, 5, 5, 5, 5)
names(state_cuts) <- c("Alabama", "Georgia", "Mississippi", "Missouri",  "NorthCarolina", "SouthCarolina", "Virginia")

world <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")
states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96

# Number of rows to use for textInput questions
nrows= 5

# Open data files ---------------------------------------------------------

# nafish_grid contains all U.S. fishes
# state_fishes contains fishes for select U.S. states
# species_groups contains select families of U.S. freshwater fishes
# Rivers is the data set to plot the rivers.
nagrid <- read_csv("data/NAfish_grid.csv", 
                   show_col_types = FALSE) %>% 
  dplyr::select(-1)
rivers <- read_table("data/rivers.txt", 
                     col_names = c("X1", "X2"), 
                     show_col_types = FALSE)


state_fishes <- readRDS("data/state_fishes.rds")
species_groups <- readRDS("data/species_groups.rds")

# Global functions --------------------------------------------------------

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
fix_special_chars <- function(str = NULL) {
  str_replace_all(str, "([#%$_])", "\\\\\\1")
}

has_empty_input <- function(lst = NULL) {
  if (any(lst == "")) {
    "Please answer all questions below."
  }
}

next_btn <- function(id) {
  actionButton(inputId = id, label = "Next")
}

prev_btn <- function(id) {
  actionButton(inputId = id, label = "Prev")
}

next_tab <- function(tab = NULL, target = NULL, test = NULL) {
  if (is.null(test)) {
    appendTab(inputId = "tabs", tab = tab, select = TRUE)
  } else {
    showTab(inputId = "tabs", target = target, select = TRUE)
  }
}

prev_tab <- function(target) {
  showTab(inputId = "tabs", target = target, select = TRUE)
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


