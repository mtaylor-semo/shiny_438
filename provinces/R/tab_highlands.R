# Predictions tab ---------------------------------------------------------
interior_highlands_tab <- tabPanel(
  "Part 1: Interior Highlands",
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
      1,
      offset = 10,
      prev_btn("btn_prev_highland"),
    ),
    column(
      1,
      next_btn("btn_next_highland"),
    )
  ),
  hr(),
  fluidRow(
    column(
      width = 6,
      img(src = "relief_map.png", width = "97%"),
      br(),
      p("Shaded relief map of the U.S. showing approximate locations of
            Pleistocene glacial maxima (black line) and fall line (yellow line).
            Other colors indicate elevation.
            Darker green is low elevation close to sea level. Light colors
            indicate high elevation in the mountains."),
      hr(),
      p("Image credit:"),
      tags$a(href = "https://www.jpl.nasa.gov/images/pia03377-shaded-relief-with-height-as-color-north-america", "NASA Jet Propulsion Lab, California Institute of Technology.")
    ),
    column(
      6,
      h4("Part 1"),
      
       
      p("In the Rapoport's Rule exercise, you gained a sense of how species
        richness of freshwater fishes is distributed across the U.S. For this
        exercise, you will explore the same data relative to the Pleistocene
        glacial maximum and the fall line (see image at left). The area between
        the glacial maximum and the fall line is called the", 
        strong("interior highlands"), "consisting of the eastern highlands
        (formally, the", tags$a(href = "https://en.wikipedia.org/wiki/Appalachian_Highlands", "Appalachian Highlands"), ") and the central highlands
      (formally. the ", tags$a(href = "https://en.wikipedia.org/wiki/Ozarks", "Ozark Highlands"), ")."),
      p("You will generate a “shaded relief map”, that uses colors to show
        relative species richness, similar to the map at left. Your maps will
        use dark colors to indicate low species richness 
        (few species) and bright colors to indicate higher species richness.
        After you explore overall species richness for the U.S. you will
        explore the distribution of species richness for selected families 
        of fishes."),
      p(strong("Think carefully about your prediction."), "Where will species
        richness be highest? Will it be north of the glacial maximum? Will
        richness be highest in the interior highlands between the glacial 
        maximum and coastal plain? Will richness be highest on the coastal 
        plain? Why do you think so?"),
      p("The data were obtained by creating a presence / absence matrix for each
        species of native fish. Presence or absence was based on 1° x 1° 
        longitude / latitude grids. All matrices were summed together to
        calculate species richness for each 1° x 1° cell across the entire U.S.
        The data set represents a total of 529 species")
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
