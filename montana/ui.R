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
          2,
          align = "right",
          offset = 9,
          p("Start here!", style = "color:#9D2235")
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
          img(src = "montana_watersheds.png", width = "97%"),
          br(),
          img(src = "montana_rivers_overlay.png", width = "97%"),
        ),
        column(
          width = 6,
          p("In an upcoming exercise, you will interpret output
            from cluster and non-metric multidimensional 
            scaling (NMDS) analysess. These analyses group 'objects' 
            based on similarity."
          ),
          p("The 'objects' in this exercise are the watersheds of
            Montana. A watershed is a segment of a larger river plus
            all of the smaller streams and rivers (tributaries) that
            flow into the larger river." 
          ),
          
          p("The watersheds of Montana are shown at left (top). The
            watershed boundaries are overlaid on the rivers of 
            Montana (bottom) to help you visualize the relationship.
            The thick black line (top) is the continental divide. The
            continental divide is the division between rivers that flow
            east to the Atlantic Ocean or west to the Pacific Ocean."
          ),
          
          p(strong("Similarity")),
          
          p("Species similarity is a measure of how many species are shared
            among watersheds. Consider this simple presence/absence matrix
            for four watersheds and six species."),
          
          tableOutput("similarity_table"),
          
          p("Watersheds A and B have a high similarity because they share
            Species 1 and Species 3. Further, Watersheds A and B have a low
            similarity with Watershed C because they also share Species 3.
            Watersheds A and B have zero similarity with Watersheds D
            because they share zero species.", strong("Which two watersheds
            have the highest similarity and why?"), "(This is a thought 
            question.)")
        ),
        column(
          3,
          br()
        )
      )
    )
  )
) # end UI
