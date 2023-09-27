# Define North America Tab --------------------------------------------

na_richess_tab <- tabPanel(
  "North America",
  fluidRow(
    column(
      12,
      p("Scroll down for questions."),
      plotOutput("na_richness_plot",
                 height = "600px"
      ) %>% 
        withSpinner(type = 4,
                    color = semo_palette["cardiac_red"]),
      br(),
      p("Density plot of species richness for U.S. Freshwater fishes. Brighter
        colors indicate greater species richness. Look carefully at the western
        U.S. Notice that richness increases somewhat along the coast. Notice
        too that richness tends to follow the rivers."),
      hr()
    )
  ),
  fluidRow(
    column(
      3,
      p(strong("You predicted:")),
      uiOutput("prediction_na_richness")
    ),
    column(
      6,
      p(strong("Question:"), question1_text),
      textAreaInput(
        inputId = "question1",
        label = NULL,
        rows = 5,
        width = "97%"
      ),
      hr(),
      p(
        "Locations where species richness is very high are often called",
        strong("biodiversity hotspots."), "Such hotspots are often areas of
        conservation concern. Preserving biodiversity hotspots protects the
        greatest number of species, with efficient use of conservation
        dollars."
      ),
      hr(),
      p("Press the Next button to make relief maps of different species
        groups.")
    ),
    column(
      3,
      br(),
      actionButton(inputId = "btn_next_na", label = "Next", width = "35%"),
      span(textOutput("na_richness_result_error"), style = "color:#9D2235")
    )
  )
)
