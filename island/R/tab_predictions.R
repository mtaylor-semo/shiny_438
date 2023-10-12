# Predictions tab ---------------------------------------------------------
predictions_tab <- tabPanel(
  "Predictions",
  fluidRow(
    column(
      3,
      align = "right",
      offset = 7,
      span(textOutput("prediction_error"),
           style = "color:#9D2235"
      )
    ),
    column(
      1,
      prev_btn("btn_prev_pred"),
    ),
    column(
      1,
      next_btn("btn_next_pred"),
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "galapagos_map.png", width = "97%"),
      br(),
      p("Map of the Galapagos Islands archipelago"),
      hr(),
      p("Image credit:"),
      tags$a(href = "https://commons.wikimedia.org/wiki/File:Galapagos_Islands_topographic_map-en.svg", "Matthew Stevens, Wikimedia Commons, CC BY-SA 3.0")
    ),
    column(
      6,
      p(strong("Think carefully about your prediction below."), "Is the United
        States uniform from south to north? What about east to west? Think
        about where the U.S. tends  to be wetter (more precipitation) or more
        arid (drier). Do some regions have more rivers than other regions?
        Will the U.S. have any regional climate effects due to rain shadows
        or deserts? How might this influence the distribution of species
        richness of freshwater fishes?"),
      p(strong("What do you predict for species richness for U.S. freshwater
               fishes?")),
      p("Where in the U.S. do you think species richness will be the highest?
        Northeastern U.S.? Southeastern U.S.? Northwest? Southwest? Midwest?
        You may already an idea from the Rapoport's Rule exercises.
        Take a look at the map of U.S. rivers that was given to you?  Below,
        name the region and 3--4 rivers in that region where you think
        diversity may be the highest. Explain your reasoning."),
      hr(),
      textAreaInput(
        inputId = "predict_na_richness",
        label = NULL, # "Enter your prediction:",
        rows = nrows,
        placeholder = "U.S. species richness prediction…",
        width = "90%"
      )
    )
  )
)
