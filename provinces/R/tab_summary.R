library(dplyr)
library(stringr)

# Summary tab -------------------------------------------------------------

summary_tab <- tabPanel(
  "Summary",
  fluidRow(
    column(
      3,
      wellPanel(
        p(
          em("Write a summary of what you learned from this lesson."),
          "Your summary must address geographic range size at both large
          geographic scales (North America), small scale (one stage), and
          regional scales (the south to north trend)."
        ),
        br(),
        p(
          "After you have entered your summary, press the 'Download' button
        to download your final report.",
          em("Upload your report to the dropbox for this exercise.")
        )
      )
    ),
    column(
      6,
      textAreaInput(
        inputId = "summary",
        label = NULL,
        placeholder = "Enter your summary here.",
        width = "100%",
        rows = 10
      )
    ),
    fluidRow(
      column(
        2,
        downloadButton("downloadReport")
      )
    )
  )
)
