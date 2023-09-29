# Define Family Richness tab ---------------------------------------------

nmds_tab <- tabPanel(
  "NMDS",
  fluidRow(
    column(
      1,
      prev_btn("btn_prev_nmds")
    ),
    column(
      3,
      offset = 7,
      align = "right",
      span(textOutput("nmds_result_error"),
           style = "color:#9D2235"
      )
    ),
    column(
      1,
      next_btn("btn_next_nmds")
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      plotOutput(
        "nmds_plot",
        width = "97%",
        height = "500px"
      ),
      plotOutput("cluster_plot_rep",
        width = "97%",
        height = "400px"
      )
    ),
    column(
      6,
      p("This scatterplot is the result of a non-metric multidimensional scaling
        analysis (NMDS). We don't need to cover the details of the analysis but
        it captures more of the similarity and differences among the watersheds.
        Watersheds that are closer to each other on the plot have greater
        similarity. Less similar watersheds are farther apart. (The 
        left-to-rightor lower-to-upper directions on the x- and y-axes are 
        meaningless. The plot could be flipped in both directions but the
        interpretation is the same."),
      
      p("You can see the high similarity of the Milk, Lower Yellowstone, and
        Lower Missouri. In fact, they have the same species so they occupy
        the same location on the scatterplot. Same for the Kootenai and Clark
        Fork watersheds. (It's not common to have watersheds with exactly the
        same species but Montana does not have very many species of fishes.)"),
      
      p("This plot shows the clear differences of the Saskatchewan and Upper
        Missouri watersheds from each other and from the other watersheds. The
        Upper Missouri is about as different from the Upper Yellowstone and
        Bighorn as these two are from the Lower Yellowstone, Lower Missouri, 
        and Milk."),
      
      p("Both cluster and NMDS analyses are useful tools to help identify
        biogeographic provinces and ecoregions at larger and smaller
        geographic scales. You will use these tools, and others, in the next
        exercise.")
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      p("For overall similarity, you predicted:"),
      textOutput("question1n_prediction"),
      br(),
      p("For specific similarity between the western slope
        and eastern slope watersheds, you predicted:"),
      textOutput("question2n_prediction")
    ),
    column(
      width = 6,
      p(
        strong("Question:"), "Do the results of NMDS change how you compare
          the overall results (NMDS and cluster analysis) to both of your
          predictions? Summarize the results in comparison to your predictions",
        strong("with an emphasis on the NMDS plot.")
      ),
      textAreaInput(
        inputId = "nmds_question1",
        label = NULL,
        rows = nrows,
        width = "97%"
      )
    )
  )
)
