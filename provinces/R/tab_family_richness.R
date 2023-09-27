
# Define Family Richness tab ---------------------------------------------

species_tab <- tabPanel(
  "Family Richness",
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
      actionButton(
        inputId = "btn_next_family",
        label = "Next",
        width = "35%"
      ),
      span(textOutput("family_result_error"),
           style = "color:#9D2235"
      )
    ),
    column(
      9,
      plotOutput("family_plot",
                 width = "97%",
                 height = "600px"
      ) %>% 
        withSpinner(type = 4,
                    color = semo_palette["cardiac_red"]),
      hr()
    )
  ),
  fluidRow(
    column(
      width = 6,
      offset = 3,
      p(strong("Question:"), question2_text),
      textAreaInput(
        inputId = "question2",
        label = NULL,
        rows = 5,
        width = "97%"
      ),
      hr(),
      p(strong("Question:"), question3_text),
      textAreaInput(
        inputId = "question3",
        label = NULL,
        rows = 5,
        width = "97%"
      )
    )
  )
)
