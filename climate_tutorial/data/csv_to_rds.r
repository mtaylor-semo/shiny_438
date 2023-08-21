library(purrr)
library(tidyr)

# Script to take data files, turn them into a single list object, 
# and then save them an rds file.

# Uses purrr:: for state data to read all csv files in state_data folder.

# FOR NOW, create three separate rds files, one for each scale.

# North America
tutorial_data <- read.csv("tutorial/data/tutorial_climate_data.csv", header = TRUE)

#tutorial_list <- list(tutorial_data = tutorial_data)

saveRDS(tutorial_data, "tutorial/data/tutorial_climate_data.rds")


x <- readRDS("tutorial/data/tutorial_climate_data.rds")

