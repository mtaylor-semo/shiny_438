library(dplyr)
library(stringr)



# Define Point Conception tab ---------------------------------------------

pc_tab <- tabPanel(
  "Point Conception",
  fluidRow(
    column(9, plotOutput("pc_plot",
                         width = "97%",
                         height = "600px"),
           hr()),
           #uiOutput("na_numbers")),
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_pc"),
      br(),
      p("Do the results agree with your prediction? Explain below,
      then press the Next button."),
      textAreaInput(inputId = "pc_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_pc", label = "Next", width = "35%"),
      span(textOutput("pc_result_error"), style = "color:#9D2235")
    )
  )
)


# Define Richness and Area Tab --------------------------------------------


ca_tab <- tabPanel(
  "Richness and area",
  fluidRow(
    column(
      3,
      wellPanel(radioButtons(
        inputId = "richness_area",
        label = "Choose plot type",
        choices = c("Species richness", "Area occupied")
      )),
      hr(),
      p(strong("You predicted:")),
      uiOutput("prediction_ca")
    ),
    column(6, plotOutput("richness_area_plot"),
           hr(),
           p("This data set has 516 species.")),
    column(
      3,
      uiOutput("ca_info"),
      hr(),
      p("Do the results agree with your prediction? Explain below, 
      then press the Next button."),
      textAreaInput(inputId = "ca_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_ca", label = "Next", width = "35%"),
      span(textOutput("ca_result_error"), style = "color:#9D2235")
      
      # img(src = "california.png", width = "320px")
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
