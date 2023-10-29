# UI functions for Provinces app.

# Libraries ---------------------------------------------------------------

library(shiny)
#library(reactlog)

#reactlog_enable()

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Fall Line",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Fall Line"
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
          1,
          offset = 11,
          next_btn("btn_next_intro")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          img(src = "relief_map.png", width = "97%"),
          br(),
          p("Shaded relief map of the U.S. showing approximate locations of
            Pleistocene glacial maxima (black line) and fall line (yellow line).
            Other colors indicate elevation.
            Darker green is low elevation close to sea level. Light colors
            indicate high elevation in the mountains."),
          hr(),
          p("Image credit:"),
          tags$a(href = "https://www.jpl.nasa.gov/images/pia03377-shaded-relief-with-height-as-color-north-america", "NASA Jet Propulsion Lab, California Institute of Technology.")
        ),
        column(
          width = 6,
          p("This exercise has two parts. In Part 1, you will explore the 
            distribution of species richness relative to the Pleistocene maximum
            and the fall line (see image at left) that separates the",
            strong("coastal plain"), "from uplifted", strong("interior 
            highlands"), "(central and eastern highlands). In Part 2, you will
            explore river watersheds that cross the fall line to learn whether
            the distribution of freshwater fishes is affected by this important
            biogeographic feature.")
        )
      )
    )
  )
) # end UI
