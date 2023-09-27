
# Define Family Richness tab ---------------------------------------------

cluster_tab <- tabPanel(
  "Cluster",
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          inputId = "state_menu_cluster",
          label = "Choose a state",
          choices = names(state_fishes),
          selected = "Montana",
          multiple = FALSE
        ),
      ),
      actionButton(
        inputId = "btn_next_cluster",
        label = "Next",
        width = "35%"
      ),
      span(textOutput("cluster_result_error"),
          style = "color:#9D2235"
      )
    ),
    column(
      9,
      plotOutput("cluster_plot",
                 width = "97%"
      ) %>% 
        withSpinner(type = 4,
                    color = semo_palette["cardiac_red"]),
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      p("Based on this figure, combined with the previous graphs,
        do U.S. freshwater fishes follow Rapoport’s Rule for geographic
        range size? Explain."),
      textAreaInput(
        inputId = "question4",
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
    ),
  )
)
