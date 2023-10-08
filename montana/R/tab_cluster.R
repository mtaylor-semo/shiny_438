# Define Family Richness tab ---------------------------------------------

cluster_tab <- tabPanel(
  "Cluster",
  fluidRow(
    column(
      3,
      offset = 7,
      align = "right",
      span(
        textOutput("cluster_result_error"),
        style = "color:#9D2235"
      )
    ),
    column(
      1,
      prev_btn("btn_prev_cluster")
    ),
    column(
      1,
      next_btn("btn_next_cluster")
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      align = "left",
      plotOutput(
        "cluster_plot",
        width = "100%"
      ),
      img(src = "montana_watersheds.png", width = "100%")
    ),
    column(
      6,
      p("This cluster plot groups watersheds based on shared fish fauna. 
        Compare the clusters to the watershed map. Notice the upper
        most cluster contains the Milk, the Lower Yellowstone, and the Lower
        Missouri watersheds. These three watersheds share many of the same
        species."),
      p("The next cluster down has the Musselshell, Upper Yellowstone, and
        Bighorn watersheds, again based on similarity of the fish fauna. This 
        cluster is in a larger cluster with the Milk, Lower Yellowstone, and
        Lower Missouri. This makes biogeographic sense because these watersheds
        are all in the plains of Montana."),
      p("As you might have predicted, the Kootenai and Clark Fork watersheds
        cluster together because they are Pacific slope watersheds and thus have
        a high faunal similarity.", strong("But what is up with the Saskatchewan
        and Upper Missouri watersheds?"), "Did you predict these Atlantic slope
        watersheds to be similar to the western slope watersheds?"),
      p(
        "The Saskatchewan and Upper Missouri watersheds have lower similarity to
        all other watersheds, including each other. These differences are hard
        to show in the linear arrangement of a cluster plot. Cluster analysis
        forces these watersheds into a cluster because they have to go",
        em("somewhere."), "(By the way, the vertical arrangement of the labels
        is arbitrary. For example, the cluster with the Kootenai and Clark Fork
        watersheds could be the uppermost cluster, followed by the cluster with
        the Saskatchewan and Upper Missouri. Similarity is conveyed",
        em("only"), "by the branching pattern.)"
      ),
      p("Cluster plots are good for initial inspection of the data. They give
        an initial idea of watershed similarity but it cannot show how similar
        or differt the watersheds are. We can get better results. First, compare
        the results of the cluster analysis to your predictions below, then 
        click the Next button to learn about another helpful analysis."),
      hr(),
      column(
        6,
        p(
          "Pacific slope watersheds:",
          tags$ul(
            tags$li("Clark Fork"),
            tags$li("Kootenai")
          )
        )
      ),
      column(
        6,
        p(
          "Atlantic slope watersheds:",
          tags$ul(
            tags$li("Bighorn"),
            tags$li("Milk"),
            tags$li("Missouri (upper and lower)"),
            tags$li("Musselshell"),
            tags$li("Saskatchewan"),
            tags$li("Yellowstone (upper and lower)")
          )
        )
      )
    )
  ),
  hr(),
  fluidRow(
    column(
      6,
      p("For overall similarity, you predicted:"),
      textOutput("question1_prediction"),
      br(),
      p("For specific similarity between the Pacific slope
        and Atlantic slope watersheds, you predicted:"),
      textOutput("question2_prediction")
    ),
    column(
      width = 6,
      p(strong("Question:"), cluster_question1),
      textAreaInput(
        inputId = "cluster_question1",
        label = NULL,
        placeholder = "Summarize the results…",
        rows = nrows,
        width = "97%"
      )
    )
  )
)
