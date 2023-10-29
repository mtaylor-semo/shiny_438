# Predictions tab ---------------------------------------------------------
fall_line_tab <- tabPanel(
  "Part 2: Fall Line",
  fluidRow(
    column(
      6,
      align = "left",
      span(textOutput("fall_result_error"),
           style = "color:#9D2235"
      )
    ),
    column(
      1,
      offset = 4,
      prev_btn("btn_prev_fall"),
    ),
    column(
      1,
      next_btn("btn_next_fall"),
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "fall_line.png", width = "97%"),
      br(),
      p(
        "Coastal Plain", tags$b("fall line"), "(red line) along the eastern and
        southeastern states. Major cities located along the fall line are also
        shown illegibly and unimportantly."),
      hr(),
      p("Image credit:"),
      tags$a(href = "https://www.researchgate.net/figure/Trace-of-the-Coastal-Plain-unconformity-Fall-Line-across-the-US-Eastern-Seaboard-and_fig1_292993968", 
             "Clinton I. Barineau")
    ),
    column(
      6,
      h4("Part 2"),
      p("The", strong("fall line"), "is the boundary between the coastal plain
        and the uplifted interior (eastern and central) highlands."),
      p("The coastal plain is very flat so the streams and rivers tend to be
        sluggish, warm, and more turbid (more sediment in the water). The 
        stream bottoms tend to be sandy or muddy. The interior highlands are 
        mountainous regions so the streams and rivers tend to have faster 
        moving, cooler water, with gravel bottoms in the faster stretches."),
      p("Many southeastern rivers start in the interior highlands above the
        fall line but flow across the fall line to the coastal plain before
        reaching the ocean."),
      p(strong("What do you predict?"), "Do you think the fishes found in the
        watersheds above the fall line will be more similar to the fishes 
        in the", em("same river below"), "the fall line? Or, will they be
        more similar to the fishes in", em("nearby rivers above"), "the fall
        line?"),
      p("On the next two tabs, you will use cluster and NMDS analyses to
        text your predictions, similar to what you did in the Montana exercise.
        You will explore the results across several states that include the
        fall line, beginning with Virginia and ending with Missouri."),
      p("Press the Next button above to begin.")
      # hr(),
      # textAreaInput(
      #   inputId = "predict_na_richness",
      #   label = NULL, # "Enter your prediction:",
      #   rows = nrows,
      #   placeholder = "U.S. species richness prediction…",
      #   width = "90%"
      # )
    )
  )
)
