# Predictions tab ---------------------------------------------------------
predictions_tab <- tabPanel(
  "Predictions",
  fluidRow(
    column(
      1,
      prev_btn("btn_prev_pred"),
    ),
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
      next_btn("btn_next_pred"),
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "montana_watersheds.png", width = "97%"),
      hr(),
    ),
    column(
      6,
      p("The Kootenai and Clark Fork watersheds flow west towards the
        Pacific Ocean (western slope). The remaining watersheds all
        flow east to the Atlantic Ocean via the Mississippi River
        (eastern slope). The continental divide
        is a potent barrier to dispersal of freshwater fishes because
        fishes cannot move from one side of the divide to the other side."),
      p(strong("Question:"), "Based on what you know so far, do you think
        that the fish fauna of the western slope
        will be similar to or different from watersheds of the eastern slope?
        Why?"),
      textAreaInput(
        inputId = "predictions_question1",
        label = NULL, # "Enter your prediction:",
        rows = nrows,
        placeholder = "First prediction…",
        width = "90%"
      ),
      hr(),
      p("The Kootenai and Clark Fork watersheds
        are in the Rocky Mountains, west of the divide. The Upper 
        Missouri and Saskatchewan (Sask.) watersheds are also in the
        Rocky Mountains, but east of the divide."),
      
      p(strong("Question:"), "Based on what you know so far, do you think that 
        the fishes of the Upper Missouri and Saskatchewan watersheds will be
        more similar to the Kootenai and Clark Fork (both in the mountains)
        or more similar to the other eastern watersheds, like the Lower 
        Missouri, in the plains of Montana?"),
      textAreaInput(
        inputId = "predictions_question2",
        label = NULL, # "Enter your prediction:",
        rows = nrows,
        placeholder = "Second prediction…",
        width = "90%"
      )
    )
  )
)
