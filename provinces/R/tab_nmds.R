
# Define Family Richness tab ---------------------------------------------

nmds_tab <- tabPanel(
  "NMDS",
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          inputId = "state_menu",
          label = "Choose a state",
          choices = names(state_fishes),
          selected = "Montana",
          multiple = FALSE
        ),
      ),
      actionButton(
        inputId = "btn_next_cn",
        label = "Next",
        width = "35%"
      ),
      span(textOutput("pc_result_error"),
           style = "color:#9D2235"
      )
    ),
    column(
      9,
      plotOutput("nmds_plot",
                 width = "97%",
                 height = "500px")
    ),
    fluidRow(
      column(
        6,
        plotOutput("cluster_plot",
                   width = "97%",
                   height = "400px"
        ) %>%
          withSpinner(type = 4,
                      color = semo_palette["cardiac_red"])#,
        # hr(),
      ), # next column here
      column(
        width = 5,
        p("Based on this figure, combined with the previous graphs,
        do U.S. freshwater fishes follow Rapoport’s Rule for geographic
        range size? Explain."),
        textAreaInput(
          inputId = "cluster_q4",
          label = NULL,
          rows = 5,
          width = "97%"
        ),
        hr(),
        p("Considering the geographic ranges east of the Rocky Mountains,
       what region of the U.S. seems to have most of the smaller bubbles?
       Why do you think this is?"),
        textAreaInput(
          inputId = "cluster_q5",
          label = NULL,
          rows = 5,
          width = "97%"
        ),
        hr(),
        p("What geographic region west of the Rocky Mountains seems
        to have the fewest bubbles? (Or, most of the smallest bubbles?)
        Why do you think this is?"),
        textAreaInput(
          inputId = "cluster_q6",
          label = NULL,
          rows = 5,
          width = "97%"
        ),
      )
    )
  )
)
