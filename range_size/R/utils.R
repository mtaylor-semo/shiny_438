# Rapo vars -------------------------------------------------------------
# Assemble the Rapoport data. Some mussel and crayfish data are not
# available to me as presence/absence data.
five_state_names <- c("Alabama",
                      "Tennessee",
                      "Kentucky",
                      "Illinois",
                      "Wisconsin")

# Number of species for each of the five states above.
rapo_fishes <- c(288, 292, 210, 196, 132)
rapo_mussels <- c(179, 128, 98, 82, 42)
rapo_crayfishes <- c(99, 90, 53, 21, 8)

rapo_data <- tibble(
  fishes = rapo_fishes,
  mussels = rapo_mussels,
  crayfishes = rapo_crayfishes,
  state = five_state_names
) %>%
  pivot_longer(cols = -state,
               names_to = "taxon",
               values_to = "richness") %>%
  mutate(state = factor(state,
                        levels = five_state_names,
                        ordered = TRUE))

# Global functions --------------------------------------------------------

# Called from Rmd file to replace LaTeX special
# characters with escaped version.
fix_special_chars <- function(str = NULL) {
  str_replace_all(str, "([#%$_])", "\\\\\\1")
}

res <- 96

open_file <- function(tx, st = NULL) {
  if (is.null(st)) {
    na_data <- readRDS("data/na_data.rds")
    data_file <- paste0("na_", tx)
    return(na_data[[data_file]])
  } else {
    switch(st,
      "California" = {
        ca_data <- readRDS("data/ca_data.rds")
        return(ca_data[["california_marine_fishes"]])
      },
      {
        the_state <- str_replace_all(st, " ", "_")
        data_file <- paste0(the_state, "_", tx)
        return(state_data[[data_file]])
      }
    )
  }
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
  ifelse(input_field == "", TRUE, FALSE)
}

plotHistogram <- function(dat = NULL,
                          x = NULL,
                          closed = "right",
                          breaks = c(y, z),
                          ...) {
  ggplot(data = dat, aes(x = x)) +
    geom_histogram(
      closed = closed,
      breaks = seq(0, breaks[1], breaks[2]),
      color = "white",
      fill = "#9d2235"
    ) +
    xlab("Number of Watersheds") +
    ylab("Number of Species") +
    theme_minimal()
}

plot_rapo <- function(plot_data = NULL) {
  rapo_plot <- ggplot(plot_data) +
    geom_col(aes(x = state, y = richness),
             color = "white",
             fill = "#9d2235") +
    coord_flip() +
    labs(y = "Species Richness",
         x = NULL) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.ticks.y = element_blank())
  
  y_intercept <- ggplot_build(rapo_plot)$layout$panel_params[[1]]$x.sec$breaks
  
  rapo_plot <- rapo_plot + geom_hline(yintercept = na.omit(y_intercept), color="white", linewidth =  0.25)
  
  return(rapo_plot)
}
