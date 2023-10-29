
# Define Family Richness tab ---------------------------------------------

cluster_tab <- tabPanel(
  "Cluster",
  fluidRow(
    column(
      width = 3,
      align = "right",
      span(
        textOutput("cluster_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      width = 1,
      offset = 7,
      prev_btn("btn_prev_cluster"),
    ),
    column(
      width = 1,
      next_btn("btn_next_cluster"),
    )
  ),

  hr(),

  fluidRow(
    column(
      6,
      selectInput(
        inputId = "state_menu_cluster",
        label = "Choose a state",
        choices = names(state_fishes),
        selected = "Virginia",
        multiple = FALSE,
        width = "50%"
      ),
      plotOutput(
        "cluster_plot",
        width = "100%"
      ) %>%
        withSpinner(type = 4,
                    color = semo_palette["cardiac_red"]),
    ),
    column(
      6,
      uiOutput("watershed_map_cluster")
      #uiOutput("watershed_info_cluster_wide")
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "watershed_map.png", width = "97%")
    ),
    column(
      width = 6,
      p("River names in the plots that have some form of “upper,” “lower,”
        “above,” “below,” “_up”, “_low,” etc. indicate portions of the river
        above and below the fall line respectively. The black line in the map
        above shows the location of the fall line."),
      p("The state map shows each river and lists the major watershed that
        the river flows to. For this exercise, rivers flow to the Alantic Ocean
        (Atlantic Slope), the Gulf of Mexico, the Tennessee River, the Ohio
        River, the Mississippi River, the Missouri River, the Arkansas River,
        or the White River. Reference the map above to the map at left to help
        keep you oriented as you compare the watersheds of each
        state and among states.")
    )
  )
)
