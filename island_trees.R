library(readr)
library(dplyr)
library(tidyr)


tree_dat <- read_tsv("island/data/oo_444807.txt", skip = 25) %>% 
  filter(species_number > 0)

tree_dat %>% 
  ggplot(aes(x = island_perimeter, y = species_number)) +
  geom_smooth(
    formula = "y ~ x",
    method = "lm"
  ) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal()

tree_dat |>
  filter(soil_depth_mean > 0) |> 
  ggplot(aes(x = island_area, y = species_number)) +
  geom_smooth(
    formula = y ~ x,
    method = "lm"
  ) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

summary(lm(species_number ~ distance_Gam, data = tree_dat))
summary(lm(species_number ~ island_area, data = tree_dat))
