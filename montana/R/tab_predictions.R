# Predictions tab ---------------------------------------------------------
predictions_tab <- tabPanel(
  "Predictions",
  fluidRow(
    column(
      width = 6,
      img(src = "montana_watersheds.png", width = "97%"),
    ),
    column(
      6,
      p("The Kootenai and Clark Fork watersheds flow west towards the
        Pacific Ocean. The remaining watersheds all flow east to the
        Atlantic Ocean via the Mississippi River. The continental divide
        is a potent barrier to dispersal of freshwater fishes because 
        fishes cannot move from one side of the divide to the other side."
      ),
      p(strong("Question:"), "Based on what you know so far, do you think
        that Where in the U.S. do you think species richness will be the highest?
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
