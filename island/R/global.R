library(tidyr)
library(ggplot2)
library(readr)
library(tibble) # Remove once MO data set is finalized. Needed for column_to_rownames
library(dplyr)
library(stringr)
library(DT)

library(shinycssloaders)

library(RColorBrewer)

# Global vars -------------------------------------------------------------

error_check = FALSE # Use this to quickly turn off error checks during dev.

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
base_rmd <- "island_biogeo.Rmd"
base_pdf <- "island_biogeo.pdf"


# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96

# Number of rows to use for textInput questions
nrows = 5

# Open data files ---------------------------------------------------------

# Merge these two once you are sure of format
birds <- read_csv("data/birds.csv") %>% 
  pivot_longer(
    cols = -species,
    names_to = "island",
    values_to = "presence"
  ) 

islands_per_bird <- 
  birds %>% 
  group_by(species) %>% 
  summarize(number_islands = sum(presence))

birds_per_island <- 
  birds %>% 
  group_by(island) %>% 
  summarize(number_species = sum(presence))


# Santiago is correct name for San Salvador
islands <- read_csv("data/islands.csv") %>% 
  rename(island = Island) %>% 
  mutate(
    log_area = log10(area),
    log_elevation = log10(elevation)
  ) %>% 
  mutate(island = replace(island, island == "San_Salvador", "Santiago"))
#mutate(island = ifelse(island == "San_Salvador", "Santiago", island))

islands <- 
  birds %>% 
  group_by(island) %>% 
  summarize(richness = sum(presence)) %>% 
  left_join(x = ., y = islands, by = "island")


herps <- read_csv("island/data/carib_herps.csv") %>% 
  mutate(
    lspecies = log10(species), 
    larea = log10(area)
  )

beetle <- read_csv("island/data/florida_beetles.csv") %>% 
  mutate(
    lspecies = log10(species),
    larea = log10(area),
    ldist = log10(distance)
  )

mtn <- read_csv("island/data/montaine_mammals.csv") %>% 
  mutate(
    lspecies = log10(species),
    larea = log10(area),
    ldist_mtn = log10(dist_mtn),
    ldist_main = log10(dist_mainland)
  )

arthro <- read_csv("island/data/arboreal_arthropods.csv") %>% 
  mutate(
    lspecies = log10(species),
    area = log10(area)
  )


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


# Modify this so that user chooses axes.
plot_galapagos <- function(plot_data = NULL, xaxis = NULL) {
  ggplot(
    req(plot_data, xaxis),
    aes(x = .data[[xaxis]])
  ) +
    geom_point(
      aes(y = richness),
      color = semo_palette["cardiac_red"],
      size = 3
    ) +
    geom_smooth(
      aes(y = richness),
      formula = y ~ x,  # Needed to suppress console msg
      method = "lm",
      color = semo_palette["cardiac_red"],
      fill = semo_palette["pewter"]
    ) +
    scale_x_log10() +
    theme_minimal() +
    labs(
      x = ifelse(
        aes(.data[[xaxis]]) == "area",
        "Island area (sq. km)",
        "Elevation (m)"
      ),
      y = "Species Richness"
    ) +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )
}

plot_birds_per_island <- function() {
  ggplot(birds_per_island) +
    geom_point(
      aes(
        x = number_species, 
        y = reorder(
          island, number_species
        )
      ), 
      color = semo_palette["cardiac_red"],
      size = 3
    ) +
    scale_x_continuous(breaks = seq(6, 18, 2),
                       limits = c(6, 18)) +
    theme_minimal() +
    labs(
      x = "Bird species richness",
      y = "Island"
    ) +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )
}

plot_islands_per_bird <- function() {
  ggplot(islands_per_bird) +
    geom_point(
      aes(
        x = number_islands,
        y = reorder(species, number_islands)
      ), 
      color = semo_palette["cardiac_red"],
      size = 3
    ) +
    theme_minimal() +
    labs(
      x = "Number of islands of occurrence",
      y = "Species"
    ) +
    theme(
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )
}

build_data_table <- function(sort_order = NULL) {
  islands %>% 
    select(-c(5, 6)) %>% 
    mutate(island = str_replace(island, "_", " ")) %>% 
    #arrange(desc(richness)) %>% 
    datatable(
      class = "compact",
      rownames = FALSE,
      colnames = c("Island", "Richness", "Area (sq. km)", "Elevation (m)"),
      options = sort_order
    )
}
