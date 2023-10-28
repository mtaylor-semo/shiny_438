# Define North America Tab --------------------------------------------

na_richess_tab <- tabPanel(
  "North America",
  fluidRow(
    # column(
    #   1,
    #   prev_btn("btn_prev_na"),
    # ),
    # column(
    #   2,
    #   p("Scroll down for questions.")
    # ),
    # column(
    #   3,
    #   align = "right",
    #   offset = 7,
    #   span(
    #     textOutput("na_richness_result_error"),
    #     style = "color:#9D2235"
    #   )
    # ),
    column(
      1,
      offset = 10,
      prev_btn("btn_prev_na"),
    ),
    column(
      1,
      next_btn("btn_next_na"),
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      img(src = "relief_map.png", width = "97%"),
      hr(),
      p(
        "Locations where species richness is very high are often called",
        strong("biodiversity hotspots."), "Such hotspots are often areas of
        conservation concern. Preserving biodiversity hotspots protects the
        greatest number of species, with efficient use of conservation
        dollars."
      )
    ),
    column(
      9,
      plotOutput(
        "na_richness_plot",
        height = "500px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        ),
      br(),
      p("This plot shows species richness for 529 U.S. freshwater fishes.
        Brighter colors indicate greater species richness. Look carefully at
        the western half of the country. Notice that richness increases
        somewhat along the coast. Notice too that richness tends to follow
        the rivers."),
      p("Reflect back on the “champaigne bubble plot” you made in the
        Rapoport's Rule exercise. That exercise helped you understand the
        distribution of species richness of freshwater fishes. Think about how
        that plot compares to this map as you answer the questions on the
        handout."),
      p(strong("Important:"), "The range of colors that indicate species
        richness varies based on the number of species in the data set.
        In the plot above, the brightest colors may represent richness of
        well over 100 species. On the next tab, the brightest colors
        may represent 10 or 20 species. Your goal is simply to determine
        which areas have the highest richness", em("for that data set."))
    )
  )
  # hr(),
  # fluidRow(
  #   column(
  #     6,
  #     p(strong("You predicted:")),
  #     uiOutput("prediction_na_richness")
  #   ),
  #   column(
  #     6,
  #     p(strong("Question:"), na_question1_text),
  #     textAreaInput(
  #       inputId = "na_question1",
  #       label = NULL,
  #       rows = nrows,
  #       width = "97%"
  #     ),
  #     p("Press the Next button above to make relief maps of different species
  #       groups.")
  #   )
  # )
)

