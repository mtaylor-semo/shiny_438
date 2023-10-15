# Define basic I.B. Tab --------------------------------------------

ib_tab <- tabPanel(
  "Islands and Animals",
  fluidRow(
    column(
      3,
      selectInput(
        inputId = "ib_group",
        label = "Choose an animal group",
        choices = c(
          "Caribbean Herps", 
          "Florida Beetles", 
          "Montaine Mammals", 
          "Arboreal Arthropods"),
        selected = "Caribbean Herps"
      )
    ),
    column(
      3,
      align = "right",
      offset = 4,
      span(
        textOutput("ib_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      prev_btn("btn_prev_ib"),
    ),
    column(
      1,
      next_btn("btn_next_ib"),
    )
  ),
  hr(),
  fluidRow(
    # column(
    #   6,
    #   tableOutput(
    #     "ib_table"
    #   )
    # ),
    # column(
    #   6,
    #   # plotOutput(
    #   #   "ib_plot",
    #   # ),
    #   br(),
    #   #p("Words")
      uiOutput("ib_ui")
#    ),
  ),
  hr(),
  fluidRow(
    column(
      6,
      p(strong("You predicted:")),
      uiOutput("prediction_ib_richness")
    ),
    column(
      6,
      p(strong("Question:"), ib_question1_text),
      textAreaInput(
        inputId = "ib_question1",
        label = NULL,
        rows = nrows,
        width = "97%"
      ),
      p("Press the Next button above to make relief maps of different species
        groups.")
    )
  )
)

