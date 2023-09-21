# UI functions for Provinces app.

# Libraries ---------------------------------------------------------------

library(shiny)
library(stringr)
library(ggplot2)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Rapoport's Rule",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Rapoport's Rule"
    ),
    # Instructions tab ------------------------------------------------------------
    
    tabPanel(
      "Instructions",
      fluidRow(
        column(
          width = 3,
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
          width = 7,
          p("You are going to make a “shaded relief map” for species richness of U.S.~fishes. 
            The data were obtained by creating a presence / absence matrix for each species 
            of native fish. Presence or absence was based on 1° x 1° longitude /  latitude
            grids. If a fish species was present in a grid cell then 1 was entered for that
            grid cell. If the species was absent, 0 was entered. Finally, all matrices were
            summed together to create the final data matrix; that is, the data set contains
            the number of species in each 1° x 1° cell for the entire U.S.  The data set
            represents a total of 529 species"),
          p("In this expercise, the relief map uses colors to show relative species 
            richness. Dark colors indicate low species richness (few species). Bright 
            colors indicate higher species richness."),
          p("After you explore species richness for North America, you will explore the 
            distribution of species richness for specific groups of fishes."),
          hr(),
          p("Choose the Predictions tab above to begin."),
          hr()
        )
      )
    ),
    
    
    # Predictions tab ---------------------------------------------------------
    tabPanel(
      "Predictions",
      fluidRow(
        # column(1),
        column(
          width = 3,
          textInput("student_name",
                    "Enter your name:",
                    placeholder = "First Last"
          ),
          hr(),
          p(),
          p("Enter your predictions at right, then press the
               'Next' button."),
        ),
        column(
          6,
          p(strong("Think carefully about your prediction below."), "Is the U.S. uniform from
            south to north? What about east to west? Think about where the U.S. tends
            to be wetter (more precipitation) or more arid (drier). Do some regions
            have more rivers than other regions? Will the U.S. have any regional climate
            effects due to rain shadows or deserts? How might this influence the distribution
            of species richness of freshwater fishes?"),
          p(strong("What do you predict for species richness for U.S. freshwater fishes?")),
          p("Where in the U.S. do you think species richness will be the highest? 
            Northeastern U.S.? Southeastern U.S.? Northwest? Southwest? Midwest? 
            Take a look at the map of U.S.~rivers that was given to you?  Below, 
            name the region and 3--4 rivers in that region where you think diversity 
            may be the highest. Explain your reasoning."),
          hr(),
          textAreaInput(
            inputId = "predict_na_richness",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "U.S. species richness prediction…",
            width = "90%"
          ),
          br()
        ),
        
        column(
          3,
          br(),
          actionButton(inputId = "btn_next_pred", label = "Next", width = "35%"),
          span(textOutput("prediction_error"), style = "color:#9D2235"),
        )
      )
    )
  )
) # end UI
