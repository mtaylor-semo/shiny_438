# Define North America Tab --------------------------------------------

galapagos_tab <- tabPanel(
  "Galapagos",
  fluidRow(
    column(
      3,
      selectInput(
        inputId = "choose_galapagos_data_set",
        label = "Choose data set",
        choices = c("Birds", "Islands"),
        selected = "Birds"
      )
    ),
    column(
      3,
      align = "right",
      offset = 4,
      span(
        textOutput("galapagos_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      prev_btn("btn_prev_galapagos"),
    ),
    column(
      1,
      next_btn("btn_next_galapagos"),
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      DTOutput("island_summary", width = "95%"),    ),
    column(
      6,
      selectInput(
        inputId = "galapagos_plot_xaxis",
        label = "Choose X-axis variable",
        choices = c("area", "elevation"),
        selected = "area"
      ),
      plotOutput(
        "galapagos_plot",
        # height = "500px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        ),
      br(),
      p("Plot of something vs another thing")
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      p(strong("You predicted:")),
      #uiOutput("prediction_na_richness")
    ),
    column(
      6,
      p(strong("Question:"), galapagos_question1_text),
      textAreaInput(
        inputId = "galapagos_question1",
        label = NULL,
        rows = nrows,
        width = "97%"
      )
    )
  )
)

