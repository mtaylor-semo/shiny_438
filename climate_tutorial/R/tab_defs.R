#library(dplyr)
library(stringr)


# Define North America Tab ------------------------------------------------

na_tab <- tabPanel(
  "Climate Plot",
  fluidRow(
    column(9, plotOutput("na_histogram"),
           hr()),
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
