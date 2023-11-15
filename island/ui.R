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
        # column(
        #   1,
        #   p(strong("Name:"))
        # ),
        # column(
        #   2,
        #   textInput(
        #     "student_name",
        #     label = NULL,
        #     placeholder = "First Last",
        #     width = "100%"
        #   )
        # ),
        # column(
        #   3,
        #   align = "left",
        #   span(textOutput("stu_name_error"), style = "color:#9D2235")
        # ),
        # column(
        #   3,
        #   align = "right",
        #   offset = 8,
        #   span(textOutput("next_ready"), style = "color:#9D2235")
        # ),
        column(
          width = 1,
          offset = 11,
          next_btn("btn_next_intro")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          img(src = "raja_ampat.jpeg", width = "97%"),
          br(),
          p("A few islands from Raja Ampat archipelago."),
          br(),
          p("Image credit:"),
          a(href = "https://indonesiad.com/raja-ampat-papua-the-amazon-of-the-oceans/", "Indonesia'd")
        ),
        # column(
        #   width = 6,
        #   img(src = "galapagos_map.png", width = "97%"),
        #   br(),
        #   p("Map of the Galapagos Islands archipelago."),
        #   hr(),
        #   p("Image credit:"),
        #   tags$a(href = "https://commons.wikimedia.org/wiki/File:Galapagos_Islands_topographic_map-en.svg", "Matthew Stevens, Wikimedia Commons, CC BY-SA 3.0")
        # ),
        column(
          width = 6,
          p("Islands, broadly speaking, are patches of suitable habitat
            surrounded by unsuitable habitat. For classic ocean islands,
            the land is the suitable habitat for terrestrial organisms
            surrounded by the unsuitable ocean water. Other habitats also
            have suitable habitat patches surrounded by unsuitable habitat,
            such as freshwater ponds surrounded by land, or patches of woods
            surrounded by agricultural fields."),
          p("The ability of organisms to disperse among suitable habitats
            depends on the biology of the organism and the type of unsuitable
            habitat. Amphibians cannot easily disperse across ocean waters to
            nearby islands but many species palm trees produce seeds (e.g.,
            coconuts) that can tolerate salt water for many weeks or months.
            Some birds can easily fly among nearby islands but small rodents
            usually cannot swim for long distances in the oceans."),
          p("The goal of this exercise is for you to explore the relationship
            of island isolation and area with species richness for both plants
            and animals. Along the way, you will look at other island-based
            relationships as well."),
        )
      )
    )
  )
) # end UI
