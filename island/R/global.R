library(tidyr)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(broom)
library(ggrepel)
library(DT)

library(shinycssloaders)

library(RColorBrewer)

# Global vars -------------------------------------------------------------

# error_check = TRUE # Use this to quickly turn off error checks during dev.

semo_palette <- list(
  cardiac_red = "#9d2235",
  riverfront = "#003b5c",
  rich_black = "#000000",
  semo_red = "#C8102E",
  pewter = "#A2AAAD"
)

# mycolors <- brewer.pal(8, "Dark2")

# Define file name constants
# base_rmd <- "island_biogeo.Rmd"
# base_pdf <- "island_biogeo.pdf"


# Set resolution of plot to 96 dpi. Most users
# are PC.
res <- 96

# Number of rows to use for textInput questions
nrows <- 5

# Number of digits to round for tables and formulas
digits <- 4

# Open data files ---------------------------------------------------------

herps <- read_csv(
  "data/carib_herps.csv",
  show_col_types = FALSE
) %>%
  mutate(
    lspecies = log10(species),
    larea = log10(area)
  )

rajas <- read_csv(
  "data/trees.csv",
  show_col_types = FALSE
) |>
  mutate(
    lrichness = log10(species_number),
    larea = log10(island_area),
    ldistance = log10(distance_Gam)
  ) |>
  rename(
    area = island_area,
    distance = distance_Gam,
    richness = species_number,
    island = island_ID
  ) |>
  select(
    island,
    area,
    distance,
    richness,
    larea,
    ldistance,
    lrichness
  )

aleuts <- read_csv(
  "data/aleutians.csv",
  show_col_types = FALSE
) |> 
  mutate(
    lrichness = log10(richness),
    larea = log10(area),
    ldAlaska = log10(dAlaska),
    ldKamchatka = log10(dKamchatka)
  )

beetles <- read_csv(
  "data/florida_beetles.csv",
  show_col_types = FALSE
) %>%
  mutate(
    lspecies = log10(species),
    larea = log10(area),
    ldist = log10(distance)
  )

mtn <- read_csv(
  "data/montaine_mammals.csv",
  show_col_types = FALSE
) %>%
  mutate(
    lspecies = log10(species),
    larea = log10(area),
    ldist_mtn = log10(dist_mtn),
    ldist_main = log10(dist_mainland)
  )

arthro <- read_csv(
  "data/arboreal_arthropods.csv",
  show_col_types = FALSE
) %>%
  subset(island != "IN1") %>%
  mutate(
    lspecies = log10(species),
    area = log10(area)
  )

birds <- read_csv(
  "data/birds.csv",
  show_col_types = FALSE
) %>%
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
islands <- read_csv("data/islands.csv", show_col_types = FALSE) %>%
  rename(island = Island) %>%
  mutate(
    log_area = log10(area),
    log_elevation = log10(elevation)
  ) %>%
  mutate(island = replace(island, island == "San_Salvador", "Santiago"))

islands <-
  birds %>%
  group_by(island) %>%
  summarize(richness = sum(presence)) %>%
  left_join(x = ., y = islands, by = "island")

rm(birds)

# Global functions --------------------------------------------------------

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
# fix_special_chars <- function(str = NULL) {
#   str_replace_all(str, "([#%$_])", "\\\\\\1")
# }

# has_empty_input <- function(lst = NULL) {
#   if (any(lst == "")) {
#     "Please answer all questions below."
#   }
# }

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


lm_regress <- function(x = NULL, y = NULL, term = "x") {
  # col_names <- c(
  #   Term = "term",
  #   Estimate = "estimate",
  #   `Std Err` = "std.error",
  #   `t value` = "statistic",
  #   `p value` = "p.value"
  # )
  res_lm <- lm(y ~ x)
  sum_lm <- summary(res_lm)

  result <- list()
  # result$lm_tidy <- broom::tidy(res_lm) %>% rename(all_of(col_names))# %>%
  #   mutate(Term = ifelse(row_number() == 2, term, Term))
  result$adjr <- round(sum_lm$adj.r.squared, digits)
  result$intercept <- round(sum_lm$coefficients[1, 1], digits)
  result$beta <- round(sum_lm$coefficients[2, 1], digits)

  result$p <- case_when(
    sum_lm$coefficients[2, 4] < 0.0001 ~ "< 0.0001",
    sum_lm$coefficients[2, 4] < 0.001 ~ "< 0.001",
    sum_lm$coefficients[2, 4] < 0.01 ~ "< 0.01",
    sum_lm$coefficients[2, 4] <= 0.05 ~ "< 0.05",
    sum_lm$coefficients[2, 4] > 0.05 ~ "> 0.05 (not significant)"
  )

  return(result)
}

build_stats <- function(x = NULL) {
  if (!is.list(x)) {
    return("x must be a list")
  } else {
    intercept <- x$intercept
    beta <- x$beta
    if (intercept >= 0) {
      sign <- " + "
    } else {
      sign <- " - "
      intercept <- abs(intercept)
    }

    regreq <- paste0("Regression equation: Y = ", beta, "X", sign, intercept)
    adjr <- paste0("Adjusted R²: ", x$adjr)
    prob <- paste0("p ", x$p)

    # Return tags for renderUI.
    stats <- tags$div(
      regreq, tags$br(), adjr, tags$br(), prob
    )

    return(stats)
  }
}


# Plots -------------------------------------------------------------------

# Modify this so that user chooses axes.
plot_galapagos <- function(plot_data = NULL, xaxis = NULL) {
  ggplot(
    req(plot_data, xaxis),
    aes(x = .data[[xaxis]])
  ) +
    geom_point(
      aes(y = richness),
      color = semo_palette$cardiac_red,
      size = 3
    ) +
    geom_smooth(
      aes(y = richness),
      formula = y ~ x, # Needed to suppress console msg
      method = "lm",
      color = semo_palette$cardiac_red,
      fill = semo_palette$pewter
    ) +
    scale_x_log10() +
    theme_minimal() +
    labs(
      x = if_else(
        xaxis == "area", # aes(.data[[xaxis]]) == "area",
        "Area (km²)",
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
      color = semo_palette$cardiac_red,
      size = 3
    ) +
    scale_x_continuous(
      breaks = seq(6, 18, 2),
      limits = c(6, 18)
    ) +
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
      color = semo_palette$cardiac_red,
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

build_ib_plot <- function(df, x = NULL, y = NULL) {
  if (deparse(substitute(df)) == "herps") {
    lab <- c("10", "100", "1000", "10,000", "100,000")
    brks <- c(10, 100, 1000, 10000, 100000)
  } else {
    lab <- waiver()
    brks <- waiver()
  }

  ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_smooth(
      formula = "y ~ x",
      method = "lm",
      se = FALSE,
      color = semo_palette$pewter,
      linewidth = 0.75
    ) +
    geom_point(
      color = semo_palette$cardiac_red,
      size = 3,
    ) +
    theme_minimal() +
    scale_x_log10(
      labels = lab,
      breaks = brks
    ) +
    scale_y_log10() +
    labs(
      x = "Area (km²)",
      y = "Species Richness"
    ) +
    geom_text_repel(
      aes(label = island),
      size = 5
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14)
    )

  # return(ggObj)
}

# Separate function to easily update plot with proper x-axis label
update_xlabel <- function(ggObj = NULL, label = "Area (km²)") {
  ggObj <- ggObj + labs(x = label)
  return(ggObj)
}

# ib_plot <- function(df, x = NULL, y = NULL) {
#
#   if (deparse(substitute(df)) == "herps") {
#     lab = c("10", "100", "1000", "10,000", "100,000")
#     brks = c(10, 100, 1000, 10000, 100000)
#   } else {
#     lab = waiver()
#     brks = waiver()
#   }
#
#   ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
#     geom_smooth(
#       formula = "y ~ x",
#       method = "lm",
#       se = FALSE,
#       color = semo_palette$pewter,
#       linewidth = 0.75
#     ) +
#     geom_point(
#       color = semo_palette$cardiac_red,
#       size = 3,
#     ) +
#     theme_minimal() +
#     scale_x_log10(
#       labels = lab,
#       breaks = brks
#     ) +
#     scale_y_log10() +
#     labs(
#       x = "Island Area (km²)",
#       y = "Species Richness"
#     ) +
#     geom_text_repel(
#       aes(label = island),
#       size = 5) +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_text(size = 13),
#       axis.title = element_text(size = 14))
# }

plot_arthro_by_island <- function() {
  ggplot(arthro, aes(x = area, y = species)) +
    geom_smooth(
      formula = "y ~ x",
      method = "lm",
      se = FALSE,
      color = semo_palette$pewter,
      linewidth = 0.75
    ) +
    geom_point(
      size = 2,
      color = semo_palette$cardiac_red
    ) +
    facet_wrap(facets = vars(island)) +
    theme_bw() +
    scale_x_log10() +
    scale_y_log10() +
    geom_text_repel(
      aes(label = year),
      size = 5
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 13),
      axis.title = element_text(size = 14),
      strip.text = element_text(size = 12, color = semo_palette$semo_red),
      strip.background = element_rect(fill = "white")
    ) +
    labs(
      x = "Area (m²)",
      y = "Species Richness"
    )
}
# Data tables -------------------------------------------------------------

build_data_table <- function(sort_order = NULL) {
  islands %>%
    select(-c(5, 6)) %>%
    mutate(island = str_replace(island, "_", " ")) %>%
    # arrange(desc(richness)) %>%
    datatable(
      class = "compact",
      rownames = FALSE,
      colnames = c("Island", "Richness", "Area (sq. km)", "Elevation (m)"),
      options = sort_order
    )
}
