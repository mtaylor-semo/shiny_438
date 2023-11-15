# Predictions tab ---------------------------------------------------------
predictions_tab <- tabPanel(
  "Size and Distance",
  fluidRow(
    # column(
    #   3,
    #   align = "right",
    #   offset = 7,
    #   span(textOutput("prediction_error"),
    #        style = "color:#9D2235"
    #   )
    # ),
    column(
      width = 1,
      offset = 10,
      prev_btn("btn_prev_pred"),
    ),
    column(
      1,
      next_btn("btn_next_pred"),
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "species_island_area.png", width = "97%"),
      br(),
      p("Species-area relationship predicted by island biogeography."),
      hr(),
      p("Image credit:"),
      p("Fig. 13.5, Lomolino et al. 2017.", em("Biogeography"), "5th ed.")
    ),
    column(
      6,
      p("MacArthur and Wilson's model of island biogeography (right) makes two
        specific predictions. First, species richness should increase as 
        island area increases. Second, species richness should decrease as 
        islands become more isolated from source populations."),
      p("This relationship is easiest to visualize with log-transformed data.
        Consider this graph below. Species richness increases with island size
        but it is not easy to see the linear relationship because island area
        increases exponentially while richness increases linearly."),
      plotOutput("curvilinear_example"),
      p("Log-transforming the data makes the linear relationship more explicit.
        Here are the same data, but island area and species richness are log",
        tags$sub("10"), "transformed."),
      plotOutput("linear_example"),
      p("Log transformation allows for easier analysis by linear regression.
        You may recall the equation, Y = mX + B from algebra to represent the
        slope of a line. In this case, Y is species richness and X is island
        area. The larger the value of m the more quickly richness is increasing
        for a given increase of island area. If m is negative, then richness is
        decreasing. For the graph above, the equation is", em("Y = 0.3137X +
        0.3425.")),
      p("The next tabs will present the results of linear regression for each
        data set. I am not going to ask you to interpret the linear regression
        equations but I do want you to understand two other values you will see.
        R² indicates the amount of variation in species
        richness that is explained by island area (or other variable such as
        distance. The higher the value of R² the more island
        area (or other variable) accounts for richness. In the example above,
        R² is 0.997, so 99.7% of the variation in species 
        richness is explained by island size."),
      p("The last important value for you to understand is p. For our purposes, 
        p is the statistical probability that the sampled data are likely 
        random. Values of p that are 0.05 or less are considered to be 
        statistically significant; that is the data are probably not random.
        Values of p greater than 0.05 are not significant. The data are
        considered to be random.  In the example above, p < 0.0001 so island
        area is a significant predictor of species richness."),
      p("Be sure you understand R² and p. You will be asked
        about them on the questions. Press the Next button to explore some basic
        data sets.")
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
