library(purrr)
library(tidyr)

# Script to take data files, turn them into a single list object, 
# and then save them an rds file.

# Uses purrr:: for state data to read all csv files in state_data folder.

# FOR NOW, create three separate rds files, one for each scale.

# North America
na_fishes <- read.csv("range_size/na_data/na_fishes.csv", row.names = 1)
na_mussels <- read.csv("range_size/na_data/na_mussels.csv", row.names = 1)

na_data <- list(na_fishes = na_fishes, na_mussels = na_mussels)

saveRDS(na_data, "range_size/data/na_data.rds")


# California Marine

california_marine_fishes <- read.csv("range_size/marine/california_marine_fishes.csv", row.names = 1)

ca_data <- list(california_marine_fishes = california_marine_fishes)

saveRDS(ca_data, "range_size/data/ca_data.rds")


# State Data.  Will eventually cull files from folder to use only the data needed for the exercise.
path_csv_files <- "range_size/state_data/"

files <- dir(path = path_csv_files, pattern = "*.csv")
names <- tools::file_path_sans_ext(files)

state_data <- files %>% 
  map(~ read.csv(file.path(path_csv_files, .), row.names = 1)) %>% 
  set_names(nm = names)

saveRDS(state_data, "range_size/data/state_data.rds")


# NA Grid Data and fish_area for rapo

nagrid <- read.csv('rapo/data/nagrid.csv', row.names=1)
fish_area <- read.csv('rapo/data/fish_area_new.csv', row.names=1)

rapo_data <- list(nagrid = nagrid, fish_area = fish_area)

saveRDS(rapo_data, "rapo/data/rapo_data.rds")

bnd <- read.table('rapo/data/boundaries.txt')
rivers <- read.table('rapo/data/rivers.txt')
coastLine <- read.table('rapo/data/coastLoRes.txt')	# U.S. outline

gis_data <- list(bnd = bnd, rivers = rivers, coastline = coastLine)
saveRDS(gis_data, "rapo/data/gis_data.rds")

# Provinces data

# State Data.  Will eventually cull files from folder to use only the data needed for the exercise.
path_csv_files <- "provinces/data"

valid_groups <- c(
  "catostomid",
  "cottid",
  "cyprinid1",
  "cyprinid2",
  "cyprinodontid",
  "etheostoma",
  "fundulus",
  "ictalurid",
  "percid",
  "salmonid"
)

valid_files <- c(
  "alabama_fishes",
  "georgia_fishes",
  "mississippi_fishes",
  "missouri_fishes",
  "north_carolina_fishes",
  "virginia_fishes",
  "montana_fishes"
)

files <- dir(path = path_csv_files, pattern = "*.csv")
files <- files[files %in% paste0(valid_groups, ".csv")]
files <- files[files %in% paste0(valid_files, ".csv")]

names <- tools::file_path_sans_ext(files)

species_groups <- files %>% 
  map(~ read.csv(file.path(path_csv_files, .), row.names = 1)) %>% 
  set_names(nm = names)

state_fishes <- files %>% 
  map(~ read.csv(file.path(path_csv_files, .), row.names = 1)) %>% 
  set_names(nm = names)

saveRDS(species_groups, "provinces/data/species_groups.rds")
saveRDS(state_fishes, "provinces/data/state_fishes.rds")

