# Script for Distribution of Species Richess
# BI 438 Biogeography

# Generate a raster map of the U.S.
# The map will show species richness for
# select groups of fishes.

# 'setup.r' must run first to add needed
# functions to the environment.

# Read the csv files provided by the instructor.
group <- getGroupFile()

# Prepare data for plotting. Calls function
# defined in setup.r
grid_long <- prepare_data(group)

# Original calls, in case the above function doesn't work.
# # Convert longitude variable variable to column headers, and
# # convert latitude variable to a column of data.
# colnames(group) <- as.character(long)
# nagrid <- group %>%
#   mutate(lat = lat)
# 
# # Pivot table to long format for use with ggplot.
# # Convert column header names be converted to integers
# grid_long <- nagrid %>%
#   pivot_longer(
#     cols = -lat,
#     names_to = "long",
#     values_to = "N"
#   ) %>%
#   mutate(
#     long = as.integer(long),
#   )

# Call the plot function from setup.r
fish <- plot_na_grid()


ggsave("fundulus.png", fish, width = 12.8, height = 9.6, units = "cm")
