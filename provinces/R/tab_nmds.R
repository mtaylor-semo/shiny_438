# Define Family Richness tab ---------------------------------------------

nmds_tab <- tabPanel(
  "NMDS",
  fluidRow(
    column(
      1,
      prev_btn("btn_prev_nmds"),
    ),
    column(
      3,
      align = "right",
      offset = 7,
      span(
        textOutput("nmds_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      next_btn("btn_next_nmds"),
    )
  ),
  hr(),
  fluidRow(
    column(
      8,
      plotOutput(
        "nmds_plot",
        width = "97%",
        height = "500px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        )
    ),
    column(
      4,
      uiOutput("state_menu_nmds"),
      plotOutput(
        "cluster_plot_rep",
        width = "97%",
        height = "400px"
      ),
      p("Choose the Cluster tab (or press the Prev button) to view a larger
        version of the cluster plot.")
    ),
    hr(),
    fluidRow(
      column(
        6,
        uiOutput("watershed_map_nmds")
      ),
      column(
        width = 6,
        p("Based on this figure, combined with the previous graphs,
        do U.S. freshwater fishes follow Rapoport’s Rule for geographic
        range size? Explain."),
        textAreaInput(
          inputId = "nmds_question1",
          label = NULL,
          rows = nrows,
          width = "97%"
        ),
        hr(),
        p("Considering the geographic ranges east of the Rocky Mountains,
       what region of the U.S. seems to have most of the smaller bubbles?
       Why do you think this is?"),
        textAreaInput(
          inputId = "nmds_question2",
          label = NULL,
          rows = nrows,
          width = "97%"
        ),
        hr(),
        p("What geographic region west of the Rocky Mountains seems
        to have the fewest bubbles? (Or, most of the smallest bubbles?)
        Why do you think this is?"),
        textAreaInput(
          inputId = "nmds_question3",
          label = NULL,
          rows = nrows,
          width = "97%"
        ),
      )
    )
  )
)
