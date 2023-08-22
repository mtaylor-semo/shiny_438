#library(dplyr)
#library(stringr)

#file_list <- list.files("state_data/")
#file_list_no_ext <- tools::file_path_sans_ext(file_list)

#tutorial_data <- readRDS("data/tutorial_climate_data.rds")
#state_name <- names(state_data)

# states <- state_name %>%
#   word(start = 1, end = -2, sep = "_") %>%
#   str_replace("_", " ") %>%
#   str_to_title()
# 
# taxa <-
#   word(state_name,
#     start = -1,
#     sep = "_"
#   ) %>%
#   str_to_title()
# 
# state_taxa <- tibble(states, taxa)
# state_choices <- unique(states)


# Define North America Tab ------------------------------------------------

na_tab <- tabPanel(
  "Climate Plot",
  fluidRow(
    # column(#style="padding-left:3%", # Same for paddingg-top, etc.
    #   3,
    #   wellPanel(
    #     p("Ecosystems and Plot"),
    #     radioButtons("na_taxon",
    #       label = "Choose taxon:",
    #       choices = c("Fishes", "Mussels"),
    #       selected = "Fishes"
    #     )
    #   ),
    #   hr(),
    # ),
    column(9, plotOutput("na_histogram"),
           hr()),
           #uiOutput("na_numbers")),
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_na"),
      br(),
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "na_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_na", label = "Next", width = "35%"),
      span(textOutput("na_result_error"), style = "color:#9D2235")
    )
  )
)


# Summary tab -------------------------------------------------------------

summary_tab <- tabPanel(
  "Summary",
  fluidRow(
    column(
      3,
      wellPanel(
        p(em("Write a summary of what you learned from this lesson."),
          "Your summary must address the relationship between the two
          specific climate variables and the three ecosystems."),
        br(),
        p("After you have entered your summary, press the 'Download' button
        to download your final report.", 
          em("Upload your report to the dropbox for this exercise."))
      )
    ),
    column(
      6,
      textAreaInput(
        inputId = "summary",
        label = NULL,
        placeholder = "Enter your summary here.",
        width = "100%",
        rows = 10)
    ),
    fluidRow(
      column(
        2,
        downloadButton("downloadReport")
      )
    )
  )
)
