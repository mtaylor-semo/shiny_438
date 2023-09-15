library(dplyr)
library(stringr)

# Define Richness and Area Tab --------------------------------------------

richness_area_tab <- tabPanel(
  "Richness and Area",
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
      uiOutput("prediction_ca"),
      hr(),
      actionButton(inputId = "btn_next_ra", label = "Next", width = "35%"),
      span(textOutput("richness_area_result_error"), style = "color:#9D2235")
    ),
    column(6, plotOutput("richness_area_plot"),
           hr(),
           p("At what latitudes, approximately, are mean and maximum species
        richness the highest? Does this agree with area occupied? Explain."),
           textAreaInput(inputId = "richness_area_q1",
                         label = NULL,
                         rows = 5,
                         width = "97%"),
           hr(),
           p("Does species richness and area for North American freshwater
        fishes follow Rapoport's Rule? Explain."),
           textAreaInput(inputId = "richness_area_q2",
                         label = NULL,
                         rows = 5,
                         width = "97%"),
           hr(),
           p("Why do you think species richness is relatively low between 
              24-28°N compared to between 30-38°N? The map below might 
              help you think about this."),
           textAreaInput(inputId = "richness_area_q3",
                         label = NULL,
                         rows = 5,
                         width = "97%"),
           hr(),
           img(src = "us_lat_lon.png", width = "97%")
          ),
    column(
      3,
      uiOutput("richness_area_info")
      #uiOutput("richness_area_info"),
      
      # img(src = "california.png", width = "320px")
    )
  )
)

# Define Champgagne plot tab ---------------------------------------------

pc_tab <- tabPanel(
  "U.S Range Size",
  fluidRow(
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_pc"),
      br(),
      p("This figure shows the relative range size for 529 species of
        primary freshwater fishes that occur in the U.S. Circle diamter
        is proportional to the relative area occupied by each species."),
      textAreaInput(inputId = "pc_result",
                    label = NULL,
                    rows = 5),
      hr(),
      actionButton(inputId = "btn_next_pc", label = "Next", width = "35%"),
      span(textOutput("pc_result_error"), style = "color:#9D2235")
    ),
    column(
      9, 
      plotOutput("pc_plot",
                 width = "97%",
                 height = "600px"),
        hr())
    ),
  fluidRow(
    column(width = 6,
           offset = 3,
      p("Based on this figure, combined with the previous graphs, 
        do U.S. freshwater fishes follow Rapoport’s Rule for geographic
        range size? Explain."),
      textAreaInput(inputId = "pc_q4",
                    label = NULL,
                    rows = 5,
                    width = "97%"),
      hr(),
      p("Considering the geographic ranges east of the Rocky Mountains,
       what region of the U.S. seems to have most of the smaller bubbles?
       Why do you think this is?"),
      textAreaInput(inputId = "pc_q5",
                    label = NULL,
                    rows = 5,
                    width = "97%"),
      hr(),
      p("What geographic region west of the Rocky Mountains seems
        to have the fewest bubbles? (Or, most of the smallest bubbles?)
        Why do you think this is?"),
      textAreaInput(inputId = "pc_q6",
                    label = NULL,
                    rows = 5,
                    width = "80%"),
      ),
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
