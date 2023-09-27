# UI functions for Provinces app.

# Libraries ---------------------------------------------------------------

library(shiny)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Cluster and NMDS Analysis",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Cluster and NMDS"
    ),

    # Instructions tab -------------------------------------------------------
    tabPanel(
      "Introduction",
      fluidRow(
        column(
          width = 6,
          img(src = "montana_watersheds.png", width = "97%"),
          br(),
          img(src = "montana_rivers_overlay.png", width = "97%"),
        ),
        column(
          width = 6,
          p("In an upcoming exercise, you will be introduced to output
            from cluster analysis and non-metric multidimensional 
            scaling (NMDS) analysis. These analyses group 'objects' 
            based on similarity."
          ),
          p("The 'objects' in this exercise are the watersheds of
            Montana. A watershed is a segment of a larger river plus
            all of the smaller streams and rivers (tributaries) that
            flow into the larger river." 
          ),
          p("The watersheds of Montana are shown at left (top). The
            watershed boundaries are overlaid on the rivers of 
            Montana (bottom) to help you visualize the relationship."
          ),
          p("The thick black line (top) is the continental divide. The
            continental divide is the division between rivers that flow
            east to the Atlantic Ocean or west to the Pacific Ocean."
          )
        ),
        column(
          3,
          br(),
          actionButton(
            inputId = "btn_next_intro",
            label = "Next",
            width = "35%"
          ),
          p("Start here!", style = "color:#9D2235")
        )
      )
    )
  )
) # end UI
