library(dplyr)
library(stringr)


#ca_data <- readRDS("data/ca_data.rds")



# Define Point Conception tab ---------------------------------------------

pc_tab <- tabPanel(
  "Point Conception",
  fluidRow(
    column(
      8, plotOutput("pc_plot"),
      hr(),
      p(),
      img(src = "california_pc.png", width = "50%")
    ),
    # uiOutput("na_numbers")),
    column(
      4,
      p(strong("You predicted:")),
      uiOutput("prediction_pc"),
      br(),
      p("The region where species richness is greatest is near", strong("Point 
        Conception"), "(34.4°N, below left), a biogeographic barrier to dispersal. The barrier 
        is created by a strong difference in average water surface temperature north 
        and south of Point Conception."),
      p("This data set contains only species that occur in California. What if the data 
        set contained", em("all"), "species between 30°S and 68°N, regardless of whether 
        they occured in California. Do you still think species richness would still be highest
        around 35°N or somewhere else along the west coast of South and North America? 
        Consider what you know about the latitudinal diversity gradient."),
      p("Tell in your answer whether the results agreed with your prediction but
        also tell whether you think richness will still be highest around Point Conception
        or elsewhere along the coast. Explain your reasoning, then press the Next button."),
      textAreaInput(
        inputId = "pc_result",
        label = NULL,
        rows = 5
      ),
      hr(),
      actionButton(inputId = "btn_next_pc", label = "Next", width = "35%"),
      span(textOutput("pc_result_error"), style = "color:#9D2235")
    )
  )
)


# Define California Marine Tab --------------------------------------------


ca_tab <- tabPanel(
  "California Marine Fishes",
  fluidRow(
    column(
      3,
      wellPanel(radioButtons(
        inputId = "ca_marine",
        label = "Choose plot type",
        choices = c("Range size", "Range extent")
      )),
      hr(),
      p(strong("You predicted:")),
      uiOutput("prediction_ca")
    ),
    column(6, plotOutput("ca_marine_plot"),
           hr(),
           uiOutput("pc_split")
    ),
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
