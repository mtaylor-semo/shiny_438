# Define Family Richness tab ---------------------------------------------

family_richness_tab <- tabPanel(
  "Family Richness",
  fluidRow(
    column(
      7,
      align = "left",
      span(textOutput("family_richness_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      offset = 3,
      prev_btn("btn_prev_family_richness"),
    ),
    column(
      1,
      next_btn("btn_next_family_richness"),
    )
  ),
  hr(),
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          inputId = "family_menu",
          label = "Choose a family:",
          choices = names(species_groups),
          multiple = FALSE
        ),
        uiOutput("spp_info")
      ),
      p(strong("Be sure to view all families listed.")),
    ),
    column(
      9,
      plotOutput("family_plot",
        width = "97%",
        height = "450px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        )
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 9,
      offset = 3,
      p("Carefully compare all of the families available in the drop-down menu.
        Which families have the highest richness in the interior 
        highlands (generally the eastern)? Which families have the
        greatest richness outside of the highlands? Which family has the 
        greatest richness on the coastal plain? Which families have the
        greatest richness in the Great Lakes or northwest? Read the descriptions
        for these two families. What do they have in common? How does that fit
        with the richness shown on their maps?")
    )
  #   column(
  #     width = 6,
  #     p(strong("Question:"), family_richness_question1_text),
  #     textAreaInput(
  #       inputId = "family_richness_question1",
  #       label = NULL,
  #       rows = nrows,
  #       width = "97%"
  #     ),
  #   ),
  #   column(
  #     6,
  #     p(strong("Question:"), family_richness_question2_text),
  #     textAreaInput(
  #       inputId = "family_richness_question2",
  #       label = NULL,
  #       rows = nrows,
  #       width = "97%"
  #     )
  #   )
  )
)
