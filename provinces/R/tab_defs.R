library(dplyr)
library(stringr)

# Define Richness and Area Tab --------------------------------------------

na_richess_tab <- tabPanel(
  "North America",
  fluidRow(
    column(
      12,
      p("Scroll down for questions."),
      plotOutput("na_richness_plot",
                 #width = "100%",
                 height = "600px"),
      br(),
      p("Density plot of species richness for U.S. Freshwater fishes. Brighter colors
        indicate greater species richness. Look carefully at the western U.S. Notice
        that richness increases somewhat along the coast. Notice too that richness
        tends to follow the rivers."),
      hr()
    )
  ),
  fluidRow(
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_na_richness")
    ),
    column(6, 
           p(strong("How does the map compare to your prediction?"), "Where is overall
            species richness the greatest? In what watershed(s) does richness appear to
             be highest? Is it in just one location? More than one location?"),
           textAreaInput(inputId = "question1",
                         label = NULL,
                         rows = 5,
                         width = "97%"),
           hr(),
           p("Locations where species richness is very high are often called",
           strong("biodiversity hotspots."), "Such hotspots are often areas of conservation
           concern. Preserving biodiversity hotspots protects the greatest number
           of species, with efficient use of conservation dollars."),
           hr(),
           p("Press the Next button to make relief maps of different species groups.")
           #img(src = "us_lat_lon.png", width = "97%")
          ),
    column(
      3,
      #uiOutput("richness_area_info"),
      br(),
      actionButton(inputId = "btn_next_na", label = "Next", width = "35%"),
      span(textOutput("na_richness_result_error"), style = "color:#9D2235")
    )
  )
)

# Define Group Richness tab ---------------------------------------------

species_tab <- tabPanel(
  "Species Richness",
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          inputId = "spp_menu",
          label = "Choose a species group",
          choices = valid_groups,
          multiple = FALSE
        ),
        hr(),
        p(strong("Be sure to examine all assigned taxa."))
      ),
      actionButton(inputId = "btn_next_spp", label = "Next", width = "35%"),
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
                    width = "97%"),
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
