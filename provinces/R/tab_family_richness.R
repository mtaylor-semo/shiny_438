
# Define Family Richness tab ---------------------------------------------

species_tab <- tabPanel(
  "Family Richness",
  fluidRow(
    column(
      3,
      wellPanel(
        selectInput(
          inputId = "family_menu",
          label = "Choose a species group",
          choices = names(species_groups),
          multiple = FALSE
        ),
        #p(strong("Be sure to examine all assigned taxa."))
        #textOutput("species_info"),
        #img(get_species_image(input$spp_menu)),
        uiOutput("spp_info")
      ),
      actionButton(
        inputId = "btn_next_spp",
        label = "Next",
        width = "35%"
      ),
      span(textOutput("pc_result_error"),
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
      p("Based on this figure, combined with the previous graphs,
        do U.S. freshwater fishes follow Rapoport’s Rule for geographic
        range size? Explain."),
      textAreaInput(
        inputId = "pc_q4",
        label = NULL,
        rows = 5,
        width = "97%"
      ),
      hr(),
      p("Considering the geographic ranges east of the Rocky Mountains,
       what region of the U.S. seems to have most of the smaller bubbles?
       Why do you think this is?"),
      textAreaInput(
        inputId = "pc_q5",
        label = NULL,
        rows = 5,
        width = "97%"
      ),
      hr(),
      p("What geographic region west of the Rocky Mountains seems
        to have the fewest bubbles? (Or, most of the smallest bubbles?)
        Why do you think this is?"),
      textAreaInput(
        inputId = "pc_q6",
        label = NULL,
        rows = 5,
        width = "97%"
      ),
    ),
  )
)
