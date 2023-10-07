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
    windowTitle = "Biogeograpy: Provinces",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Provinces"
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
          img(src = "relief_map.jpg", width = "97%"),
          br(),
          p("Shaded relief map of the U.S. Color indicates elevation.
            Darker green is low elevation close to sea level. Light colors
            indicate high elevation in the mountains."),
          hr(),
          p("Image credit:"),
          tags$a(href = "https://www.jpl.nasa.gov/images/pia03377-shaded-relief-with-height-as-color-north-america", "NASA Jet Propulsion Lab, California Institute of Technology.")
        ),
        column(
          width = 6,
          p("You are going to make a “shaded relief map” for species richness
            of U.S.  fishes. The data were obtained by creating a presence / 
            absence matrix for each species of native fish. Presence or absence
            was based on 1° x 1° longitude /  latitude grids. If a fish species
            was present in a grid cell then 1 was entered for that grid cell. 
            If the species was absent, 0 was entered. Finally, all matrices
            were summed together to create the final data matrix; that is, the
            data set contains the number of species in each 1° x 1° cell for 
            the entire U.S.  The data set  represents a total of 529 species"),
          p("In this expercise, the relief map uses colors to show relative 
            species richness. Dark colors indicate low species richness (few
            species). Bright colors indicate higher species richness."),
          p("After you explore species richness for North America, you will
            explore the distribution of species richness for specific groups 
            of fishes."),
        )
      )
    )
  )
) # end UI
