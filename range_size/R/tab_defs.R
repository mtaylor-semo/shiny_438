library(dplyr)
library(stringr)

#file_list <- list.files("state_data/")
#file_list_no_ext <- tools::file_path_sans_ext(file_list)

state_data <- readRDS("data/state_data.rds")
state_name <- names(state_data)

states <- state_name %>%
  word(start = 1, end = -2, sep = "_") %>%
  str_replace("_", " ") %>%
  str_to_title()

taxa <-
  word(state_name,
    start = -1,
    sep = "_"
  ) %>%
  str_to_title()

state_taxa <- tibble(states, taxa)
state_choices <- unique(states)


# Define North America Tab ------------------------------------------------

na_tab <- tabPanel(
  "North America",
  fluidRow(
    column(#style="padding-left:3%", # Same for paddingg-top, etc.
      3,
      wellPanel(
        p("Range size for North America."),
        radioButtons("na_taxon",
          label = "Choose taxon:",
          choices = c("Fishes", "Mussels"),
          selected = "Fishes"
        )
      ),
      hr(),
    ),
    column(6, plotOutput("na_histogram"),
           hr(),
           uiOutput("na_numbers")),
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_na"),
      br(),
      p(strong("Describe the pattern that you see in the histogram."), 
      "Do most
      species occur in most watersheds (large range size) or do most
      species occur in just a few watersheds (small range size)? Or,
      is it some combination of the two? Explain below how the observed
      result supports or falsifies your prediction,
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


# Define States tab -------------------------------------------------------

states_tab <- tabPanel(
  "State",
  fluidRow(
    column(
      3,
      wellPanel(
        p("Choose your state and then taxon
                 to see the histogram."),
        selectInput(
          inputId = "state",
          label = "Choose a state",
          choices = state_choices,
          selected = "Georgia",
          multiple = FALSE
        ),
        uiOutput("dynamic_radio_buttons")
      )
    ),
    column(6, plotOutput("state_histogram"),
           hr(),
           uiOutput("state_numbers")),
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_state"),
      br(),
      p(strong("Describe and interpret and the histograms."), "\n Do most
species occur in most watersheds (large range size) or do most species
occur in just a few watersheds (small range size)? Or, is it some
combination of the two? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "state_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_state", label = "Next", width = "35%"),
      span(textOutput("state_result_error"), style = "color:#9D2235")
    )
  )
)



# Define Rapoport's Rule Tab --------------------------------------------


rapo_five_tab <- tabPanel(
  "Rapoport's Rule",
  fluidRow(
    column(
      3,
      wellPanel(radioButtons(
        inputId = "rapo_taxon",
        label = "Choose taxon to plot",
        choices = c("Fishes", "Mussels", "Crayfishes")
      )),
      hr()#,
      #p(strong("You predicted:")),
     # uiOutput("prediction_rapo_five")
    ),
    column(6, plotOutput("rapo_five_plot"),
           hr(),
           p("Species richness for five states from south to north.")),
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_rapo_five"),
      hr(),
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "rapo_five_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_rapo_five", label = "Next", width = "35%"),
      span(textOutput("rapo_five_result_error"), style = "color:#9D2235")
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
          "Your summary must address geographic range size at both large
          geographic scales (North America), small scale (one stage), and
          regional scales (the south to north trend)."),
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
