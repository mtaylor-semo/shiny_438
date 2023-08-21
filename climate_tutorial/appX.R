##
## Show range size histograms for fishes, crayfishes, or mussels
## at state and North American levels.


# Libraries ---------------------------------------------------------------

library(shiny)
#library(dplyr)
#library(stringr)
library(ggplot2)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Climate and Ecosystems",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Climate and Ecosystems"
    ),  # Overview tab ------------------------------------------------------------
    
    tabPanel(
      "Instructions",
      mainPanel(
        p("This page allows you to determine whether three different
        tree-based ecosystems show a relationship with
          average annual precipitation and average annual temperature."),
        p("Choose the Predictions tab to begin. Enter your first and last
          name, then a prediction."),
        p("NOTE TO MST: Rework the assignment to have students
        explore latitudal gradient, compare taxa within state, etc...")
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
            "Enter your first and last name:",
            placeholder = "First Last"
          ),
          hr(),
          p(),
          p("Enter your prediction at right, then press the
               'Next' button."),
        ),
        column(
          6,
          p(strong("What do you predict? Will the tree-based ecosystems show a 
                   relationship between mean annual temperature and mean 
                   annual precipation?"),
          textAreaInput(
            inputId = "predict_tutorial",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "Enter your prediction…"
            ),
          br(),
          hr(),
          actionButton(inputId = "btn_next_pred", label = "Next", width = "35%"),
          span(textOutput("prediction_error"), style = "color:#9D2235")
        )
      )
    )
  )
)
)# end UI


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  output$prediction_error <- renderText({
    if (input$student_name == "" |
      input$predict_tutorial == "") {
      "Please fill in all blanks."
    }
  })

  output$tutorial_result_error <- renderText({
    if (input$tutorial_result == "") {
      "Please interpret scatterplot."
    }
  })
  
  output$state_result_error <- renderText({
    if (input$state_result == "") {
      "Please interpret the histogram."
    }
  })

 
  ## Reactive values ---------------------------------------------------------

  state <- reactive({
    filter(
      state_taxa,
      states == input$state
    )
  })

  spp <- reactive({
    open_file(tx = str_to_lower(input$taxon), st = str_to_lower(input$state))
  })

  spp_na <- reactive({
    open_file(tx = str_to_lower(input$na_taxon))
  })

  plots <- reactiveValues(na = NULL, state = NULL, ca = NULL)
  
  results <- reactiveValues(na = NULL, state = NULL, ca = NULL)


  # Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$na_taxon)) {
    # Comment out for development.
     pred_check(sn = input$student_name,
                ps = input$predict_tutorial)

    removeTab(inputId = "tabs", target = "Predictions")
    appendTab(inputId = "tabs", tab = na_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "North America", select = TRUE)
    }
  })

  observeEvent(input$btn_next_na, {
    if (is.null(input$state)) {
      result_check(exp = input$tutorial_result)
      appendTab(inputId = "tabs", tab = states_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "State", select = TRUE)
    }
  })


  observeEvent(input$btn_next_ca, {
    if (is.null(input$summary)) {
      result_check(exp = input$ca_result)
      appendTab(inputId = "tabs", tab = summary_tab, select = TRUE)  
    } else {
      showTab(inputId = "tabs", target = "Summary", select = TRUE) 
    }
  })
  

  ## Outputs -------------------------------------------------------------

  output$dynamic_radio_buttons <- renderUI({
    choices <- unique(state()$taxa)
    freezeReactiveValue(input, "taxon")
    radioButtons(
      inputId = "taxon",
      "Choose a taxon",
      choices = choices
    )
  })

  output$state_numbers <- renderUI({
    dims <- dim(spp())
    sprintf("%s has %d watersheds and %d species of %s.", input$state, dims[1], dims[2], str_to_lower(input$taxon))
  })

  output$na_numbers <- renderUI({
    dims <- dim(spp_na())
    sprintf("North America has %d watersheds and %d species of %s.", dims[1], dims[2], str_to_lower(input$na_taxon))
  })
  
  output$prediction_na <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na)
  })
  
  output$prediction_state <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_state)
  })
  
  output$prediction_ca <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_ca)
  })
  
  output$ca_info <- renderUI({
    if (input$ca_marine == "Range extent") {
      p("Range extent for California coastal marine fishes. Each
        vertical bar shows the minimum to maximum latitude for
        one species of fish. Species with a
        median latitude above Point Conception are shown in
        red. Species with an average latitude below Point Conception
        are shown in blue. The horizontal black line is the latitude of
        Point Conception. Fishes are sorted (left to right on
        x-axis) in order of minimum latitude. ")
    } else {
      img(src = "california.png", width = "97%")
    }
  })



  ## State histograms ------------------------------------------------------

  output$state_histogram <- renderPlot({
    numWatersheds <- colSums(spp())
    numSpecies <- rowSums(spp())

    nws <- nrow(spp())

    bins <- input$bins

    plots$state <- plotHistogram(dat = tibble(numWatersheds), x = numWatersheds, breaks = c(nws, 1))

    plots$state
  }, res = res)

  ## North America histogram -------------------------------------------------

  output$na_histogram <- renderPlot({
    numWatersheds <- colSums(spp_na())
    numSpecies <- rowSums(spp_na())

    dat <- tibble(numWatersheds)

    nws <- nrow(spp_na()) # Number of watersheds for x-axis

    plots$na <- plotHistogram(dat = tibble(numWatersheds), x = numWatersheds, breaks = c(nws, 5))

    plots$na
  }, res = res)


  ## California Marine plots -------------------------------------------------

  output$ca_marine_plot <- renderPlot({
    cafish <- open_file(st = "California")

    if (input$ca_marine == "Range size") {
      rangeSize <- rowSums(cafish)
      numSpecies <- colSums(cafish)

      plots$ca <- plotHistogram(dat = tibble(rangeSize), x = rangeSize, breaks = c(100, 5)) +
        scale_x_continuous(breaks = seq(0, 100, 20)) +
        xlab("Range size (degrees of latitude occupied)")

      plots$ca
    } else { # plot 2.  Need better checks for the if/else

      ## Convert much of this manipulation to dplyr / tidyverse
      mycolors <- c("#9d2235", "#003b5c")
      numRows <- nrow(cafish) ## number of species
      numCols <- ncol(cafish) ## Number of 1° latitude cells

      meanCut <- 34.4481 ## Point Conception latitude as cutoff for northern and southern species.

      medianLat <- rep(NA, numRows) ## Create a vector same length as number of species.

      minLat <- vector("numeric")
      maxLat <- vector("numeric")

      for (i in 1:numRows) {
        x <- data.frame(cafish)[i, ]
        y <- colnames(x)[x == 1]

        colNames <- gsub("N", "", y)
        colNames <- gsub("S", "-", colNames)

        minLat[i] <- as.numeric(colNames[1])
        maxLat[i] <- as.numeric(colNames[length(colNames)])
        medianLat[i] <- median(as.numeric(colNames))
      }

      cafish$minLat <- minLat
      cafish$maxLat <- maxLat
      cafish$medianLat <- medianLat

      latCol <- vector("character")
      for (i in 1:numRows) {
        if (cafish$medianLat[i] > meanCut) {
          latCol[i] <- mycolors[1]
        } else {
          latCol[i] <- mycolors[2]
        }
      }

      cafish$latCol <- latCol
      cafish$xrow <- seq(1:516)

      cafish <- cafish[order(-cafish$minLat, -cafish$medianLat), ]

      ggplot(cafish) +
        geom_segment(aes(x = xrow, y = minLat, xend = xrow, yend = maxLat),
          color = latCol, size = 1.2
        ) +
        theme_minimal() +
        ylab("Latitude (°S — °N)") +
        xlab(NULL) +
        geom_hline(yintercept = c(36, 32), col = "gray") +
        geom_hline(yintercept = meanCut, col = "gray10") +
        scale_y_continuous(breaks = seq(-40, 70, 10)) +
        theme(axis.text.x = element_blank())
    }
  }, res = res)
  
  
  # Report Download ---------------------------------------------------------
  
  # Report output idea from Shiny Gallery
  output$downloadReport <- downloadHandler(
    filename = function() {
      stu_name <- str_to_lower(str_split(input$student_name, " ", simplify = TRUE))
      # For the student (or famous soocer player) with only one name.
      # What happens for students with three or more names?
      paste(
        paste0(
          rev(stu_name), 
          collapse = "_"), 
        "geographic_range.pdf", 
        sep = "_"
      )
      # if (!is.na(stu_name[2])) {
      #   paste(stu_name[2], stu_name[1], "geographic_range.pdf", sep = "_")  
      # } else {
      #   paste(stu_name[1], "geographic_range.pdf", sep = "_")
      # }
      
    },
    content = function(file) {
      notification_id <- showNotification(
        "Generating report for download.",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      src <- normalizePath("tutorial.Rmd")
      src_tex <- normalizePath("tex/tex_header.tex")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "tutorial.Rmd", overwrite = TRUE)
      file.copy(src_tex, "tex_header.tex", overwrite = TRUE)
      
      library(rmarkdown)
      
      out <- render(
        "range_report.Rmd",
        pdf_document(latex_engine = "lualatex",
                     keep_tex = TRUE,
                     includes = includes(in_header = "tex_header.tex"))
      )
      file.rename(out, file)
      on.exit(removeNotification(notification_id), add = TRUE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
