# Define North America Tab --------------------------------------------

na_richess_tab <- tabPanel(
  "North America",
  fluidRow(
    column(
      1,
      prev_btn("btn_prev_na"),
    ),
    column(
      2,
      p("Scroll down for questions.")
    ),
    column(
      3,
      align = "right",
      offset = 5,
      span(
        textOutput("na_richness_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      next_btn("btn_next_na"),
    )
  ),
  hr(),
  fluidRow(
    column(
      12,
      plotOutput(
        "na_richness_plot",
        height = "600px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        ),
      br(),
      p("Density plot of species richness for U.S. Freshwater fishes. Brighter
        colors indicate greater species richness. Look carefully at the western
        half of the country. Notice that richness increases somewhat along the
        coast. Notice too that richness tends to follow the rivers.")
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      p(strong("You predicted:")),
      uiOutput("prediction_na_richness"),
      hr(),
      img(src = "relief_map.jpg", width = "97%")
    ),
    column(
      6,
      p(strong("Question:"), na_question1_text),
      textAreaInput(
        inputId = "na_question1",
        label = NULL,
        rows = nrows,
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
      p("Press the Next button above to make relief maps of different species
        groups.")
    )
  )
)
