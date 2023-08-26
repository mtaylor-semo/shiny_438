# Define North America Tab ------------------------------------------------

plot_tab <- tabPanel(
  "Climate Plot",
  fluidRow(
    column(8, plotOutput("na_scatter"),
           hr()),
    column(
      4,
      p(strong("You predicted:")),
      uiOutput("prediction_na"),
      br(),
      p(strong("Do the results agree with your predictions?"), "Did you
      successfully predict which species requires warmer or cooler 
      temperatures, or less precipitation? Describe the pattern you see
        in the scatterplot."),
      p("Press the Next button after you finish your answer."),
      textAreaInput(inputId = "interpret_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_interpret", label = "Next", width = "35%"),
      span(textOutput("interpret_error"), style = "color:#9D2235")
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
          strong("Upload your report to the dropbox for this exercise."))
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
