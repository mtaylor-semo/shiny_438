
# Define Family Richness tab ---------------------------------------------

cluster_tab <- tabPanel(
  "Cluster",
  fluidRow(
    column(
      3,
      align = "right",
      offset = 7,
      span(
        textOutput("cluster_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      prev_btn("btn_prev_cluster"),
    ),
    column(
      1,
      next_btn("btn_next_cluster"),
    )
  ),

  hr(),

  fluidRow(
    column(
      6,
      selectInput(
        inputId = "state_menu_cluster",
        label = "Choose a state",
        choices = names(state_fishes),
        selected = "Virginia",
        multiple = FALSE,
        width = "50%"
      ),
      plotOutput(
        "cluster_plot",
        width = "100%"
      ) %>%
        withSpinner(type = 4,
                    color = semo_palette["cardiac_red"]),
    ),
    column(
      6,
      uiOutput("watershed_map_cluster")
      #uiOutput("watershed_info_cluster_wide")
    )
    # column(
    #   width = 6,
    #   p("Based on this figure, combined with the previous graphs,
    #     do U.S. freshwater fishes follow Rapoport’s Rule for geographic
    #     range size? Explain."),
    #   textAreaInput(
    #     inputId = "cluster_question1",
    #     label = NULL,
    #     rows = 5,
    #     width = "97%"
    #   ),
    #   hr(),
    #   p("Considering the geographic ranges east of the Rocky Mountains,
    #    what region of the U.S. seems to have most of the smaller bubbles?
    #    Why do you think this is?"),
    #   textAreaInput(
    #     inputId = "cluster_question2",
    #     label = NULL,
    #     rows = 5,
    #     width = "97%"
    #   ),
    #   hr(),
    #   p("What geographic region west of the Rocky Mountains seems
    #     to have the fewest bubbles? (Or, most of the smallest bubbles?)
    #     Why do you think this is?"),
    #   textAreaInput(
    #     inputId = "cluster_question3",
    #     label = NULL,
    #     rows = 5,
    #     width = "97%"
    #   )
    # )
  )
)
