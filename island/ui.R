# UI functions for Island Biogeography

# Libraries ---------------------------------------------------------------

library(shiny)
#library(reactlog)

#reactlog_enable()

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Island Biogeography",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Island Biogeography"
    ),

    # Instructions tab -------------------------------------------------------
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          1,
          p(strong("Name:"))
        ),
        column(
          2,
          textInput(
            "student_name",
            label = NULL,
            placeholder = "First Last",
            width = "100%"
          )
        ),
        column(
          3,
          align = "left",
          span(textOutput("stu_name_error"), style = "color:#9D2235")
        ),
        column(
          3,
          align = "right",
          offset = 2,
          span(textOutput("next_ready"), style = "color:#9D2235")
        ),
        column(
          1,
          next_btn("btn_next_intro")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          img(src = "galapagos_map.png", width = "97%"),
          br(),
          p("Map of the Galapagos Islands archipelago."),
          hr(),
          p("Image credit:"),
          tags$a(href = "https://commons.wikimedia.org/wiki/File:Galapagos_Islands_topographic_map-en.svg", "Matthew Stevens, Wikimedia Commons, CC BY-SA 3.0")
        ),
        column(
          width = 6,
          p("Words"),
          p("More Words"),
          p("LAst words"),
        )
      )
    )
  )
) # end UI
