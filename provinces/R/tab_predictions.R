# Predictions tab ---------------------------------------------------------
predictions_tab <- tabPanel(
  "Predictions",
  fluidRow(
    column(
      width = 3,
      textInput("student_name",
                "Enter your name:",
                placeholder = "First Last"
      ),
      hr(),
      p(),
      p("Enter your predictions at right, then press the 'Next' button."),
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
        Take a look at the map of U.S.~rivers that was given to you?  Below,
        name the region and 3--4 rivers in that region where you think
        diversity may be the highest. Explain your reasoning."),
      hr(),
      textAreaInput(
        inputId = "predict_na_richness",
        label = NULL, # "Enter your prediction:",
        rows = 6,
        placeholder = "U.S. species richness prediction…",
        width = "90%"
      ),
      br()
    ),
    column(
      3,
      br(),
      actionButton(
        inputId = "btn_next_pred",
        label = "Next",
        width = "35%"
      ),
      span(textOutput("prediction_error"),
           style = "color:#9D2235"
      ),
    )
  )
)
