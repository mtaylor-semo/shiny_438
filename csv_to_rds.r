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
