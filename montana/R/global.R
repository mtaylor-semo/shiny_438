library(tidyr)
library(ggplot2)
library(dplyr)
library(stringr)

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
base_rmd <- "montana.Rmd"
base_pdf <- "montana.pdf"

# Set resolution of plot to 96 dpi. Most users
# are PC.
res = 96

# Number of rows to use for textInput questions
nrows= 5

mt <- read.table("data/montana.csv", row.names = 1, header = TRUE, sep = ",")

similarity_table <- data.frame(
  Species = paste("Species", seq(1:6)),
  Watershed.A = c(1, 1, 1, 0, 0, 0),
  Watershed.B = c(1, 0, 1, 0, 0, 0),
  Watershed.C = c(0, 0, 1, 1, 1, 1),
  Watershed.D = c(0, 0, 0, 1, 1, 1)
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

