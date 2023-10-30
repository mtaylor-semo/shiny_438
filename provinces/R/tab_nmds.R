# Define Family Richness tab ---------------------------------------------

nmds_tab <- tabPanel(
  "NMDS",
  fluidRow(
    column(
      width = 3,
      align = "right",
      offset = 7,
      span(
        textOutput("nmds_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      width = 1,
      prev_btn("btn_prev_nmds"),
    ),
    column(
      width = 1,
      p("Last page.")
#      next_btn("btn_next_nmds"),
    )
  ),
  hr(),
  fluidRow(
    column(
      7,
      uiOutput("state_menu_nmds"),
      plotOutput(
        "nmds_plot",
        width = "97%",
        height = "500px"
      ) %>%
        withSpinner(
          type = 4,
          color = semo_palette["cardiac_red"]
        ),
      hr(),
      plotOutput(
        "cluster_plot_rep",
        width = "97%",
        height = "400px"
      )
    ),
    column(
      5,
      uiOutput("watershed_map_nmds"),
      #uiOutput("watershed_info_nmds")
      hr(),
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
        state and among states."),
      img(src = "watershed_map.png", width = "97%")
    )
  )
)
