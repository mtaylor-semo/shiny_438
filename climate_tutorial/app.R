##
## Simple exercise to plot relationship of three ecosystems
# with two climate variables (Mean annual temp & precip.)


# Libraries ---------------------------------------------------------------

library(shiny)
#library(dplyr)
#library(stringr)
library(ggplot2)
library(readr)
library(RColorBrewer)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: ecosystems and climate",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Ecosystems and climate"
    ),
    # Instructions tab ------------------------------------------------------------
    
    tabPanel(
      "Instructions",
      fluidRow(
        column(
          width = 6,
          p(
            "Temperature and precipitation are the two climate variables 
            that most influence the distribution of ecosystems. This is 
            a fundamental biogeographic concept which you will explore 
            using a climate data set taken from Alberta, Canada. The data 
            set contains mean annual temperature (°C) and mean 
            annual precipitation (mm) from 80 weather stations 
            spread across eight different ecosystems (Ecosys), was recorded 
            (Cw = Western Redcedar, Gr = mixed grasses, La = Subalpine
            Larch). The measurements are in five-year intervals from 
            1965-2010."
          ),
          hr(),
          p("Choose the Predictions tab above to begin.")
        ),
        column(width = 3,
               tags$figure(
                 img(
                   src = "larch.jpg",
                   align = "middle",
                   width = "240"
                 ),
                 tags$figcaption("Subalpine Larch"),
               )),
        column(width = 3,
               tags$figure(
                 img(
                   src = "western_redcedar.jpg",
                   align = "middle",
                   width = "240"
                 ),
                 tags$figcaption("Western Redcedar")
               )),
      ),
      fluidRow(
        column(width = 6,
               br(),
               p(strong("Photo credits")),
               p(
                 tags$a(href = "https://www.facebook.com/GlacierNPS/photos/a.360427434911/10158084082764912/",
                        "Subalpine Larch: Glacier National Park, Public Domain")
               ),
               p(
                 tags$a(href = "https://commons.wikimedia.org/wiki/File:Western_redcedar_grove,_Moose_Creek,_Selway-Bitteroot_Wilderness,_Idaho,_USA.jpg",
                        "Western Redcedar: Answer to the Rock, Wikimedia Commons, CC BY-SA 4.0")
               ),
               p(
                 tags$a(href = "https://commons.wikimedia.org/wiki/File:James_Woodworth_Prairie.jpg",
                        "Grassland: Peter W Chen, Wikimedia, CC BY-SA 4.0")
               )),
        column(width = 4,
               #                 offset = 6,
               tags$figure(
                 img(src = "grassland.jpg", align = "middle", width = "240"),
                 tags$figcaption("Grassland")
               ))
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
                    "Enter your name:",
                    placeholder = "First Last"),
          hr(),
          p(),
          p("Enter your predictions at right, then press the
               'Next' button."),
        ),
        column(
          6,
          p(
            strong("What do you predict?"),
            "Your predictions
                   should address at least these questions with simple
                   direct sentences."
          ),
          p("Which ecosystem requires the warmest mean annual
                   temperatures?"),
          p("Which ecosystem requires the coolest mean annual temperature?"),
          p("Which ecosystem requires the least amount of precipitation?"),
          p("Which ecosystem requires the most precipitation?"),
          p(
            "Will any ecosystems co-occur? That is, will they require
                   the same range of temperature",
            em("and"),
            "precipitation?"
          ),
          textAreaInput(
            inputId = "predict_tutorial",
            label = NULL,
            #"Enter your prediction:",
            rows = 6,
            width = "90%",
            placeholder = "Enter your prediction…"
          ),
          br(),
          hr(),
          #   actionButton(
          #     inputId = "btn_next_pred",
          #     label = "Next",
          #     width = "35%"
          #   ),
          #   span(textOutput("prediction_error"), style = "color:#9D2235")
        ),
        column(
          width = 3,
          actionButton(
            inputId = "btn_next_pred",
            label = "Next",
            width = "35%"
          ),
          span(textOutput("prediction_error"), style = "color:#9D2235")
          
        )
      ),
      # End first fluid row
      fluidRow(
        column(
          width = 3,
          offset = 3,
          tags$figure(
            img(
              src = "larch.jpg",
              align = "middle",
              width = "240"
            ),
            tags$figcaption("Larch tree",)
          )
        ),
        column(width = 3,
               tags$figure(
                 img(
                   src = "western_redcedar.jpg",
                   align = "middle",
                   width = "240"
                 ),
                 tags$figcaption("Western Redcedar")
               )),
        column(width = 3,
               tags$figure(
                 img(src = "grassland.jpg", align = "middle", width = "240"),
                 tags$figcaption("Grassland")
               ))
      ),
    )
  )
) # end UI


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  output$prediction_error <- renderText({
    if (input$student_name == "" |
        input$predict_tutorial == "") {
      "Please fill in all blanks."
    }
  })
  
  output$na_result_error <- renderText({
    if (input$na_result == "") {
      "Please interpret the scatterplot."
    }
  })
  
  # output$state_result_error <- renderText({
  #   if (input$state_result == "") {
  #     "Please interpret the histogram."
  #   }
  # })
  #
  # output$ca_result_error <- renderText({
  #   if (input$ca_result == "") {
  #     "Please interpret the histogram."
  #   }
  # })
  
  ## Reactive values ---------------------------------------------------------
  
  # state <- reactive({
  #   filter(state_taxa,
  #          states == input$state)
  # })
  #
  # spp <- reactive({
  #   open_file(tx = str_to_lower(input$taxon), st = str_to_lower(input$state))
  # })
  #
  # spp_na <- reactive({
  #   open_file(tx = str_to_lower(input$na_taxon))
  # })
  #
  #  plots <- reactiveValues(na = NULL, state = NULL, ca = NULL)
  #    results <- reactiveValues(na = NULL, state = NULL, ca = NULL)
  
  plots <- reactiveValues(na = NULL)
  
  results <- reactiveValues(na = NULL)
  
  
  # Button observers --------------------------------------------------------
  
  observeEvent(input$btn_next_pred, {
    if (is.null(input$na_taxon)) {
      # Comment out for development.
      pred_check(sn = input$student_name,
                 pn = input$predict_tutorial)
      
      removeTab(inputId = "tabs", target = "Predictions")
      appendTab(inputId = "tabs",
                tab = na_tab,
                select = TRUE)
    } else {
      showTab(inputId = "tabs",
              target = "North America",
              select = TRUE)
    }
  })
  
  # observeEvent(input$btn_next_na, {
  #   if (is.null(input$state)) {
  #     result_check(exp = input$na_result)
  #     appendTab(inputId = "tabs", tab = states_tab, select = TRUE)
  #   } else {
  #     showTab(inputId = "tabs", target = "State", select = TRUE)
  #   }
  # })
  #
  # observeEvent(input$btn_next_state, {
  #   if (is.null(input$ca_marine)) {
  #     result_check(exp = input$state_result)
  #     appendTab(inputId = "tabs", tab = ca_tab, select = TRUE)
  #   } else {
  #     showTab(inputId = "tabs", target = "California Marine Fishes", select = TRUE)
  #   }
  # })
  
  observeEvent(input$btn_next_na, {
    if (is.null(input$summary)) {
      result_check(exp = input$na_result)
      appendTab(inputId = "tabs",
                tab = summary_tab,
                select = TRUE)
    } else {
      showTab(inputId = "tabs",
              target = "Summary",
              select = TRUE)
    }
  })
  
  
  ## Outputs -------------------------------------------------------------
  
  # output$dynamic_radio_buttons <- renderUI({
  #   choices <- unique(state()$taxa)
  #   freezeReactiveValue(input, "taxon")
  #   radioButtons(inputId = "taxon",
  #                "Choose a taxon",
  #                choices = choices)
  # })
  
  # output$state_numbers <- renderUI({
  #   dims <- dim(spp())
  #   sprintf(
  #     "%s has %d watersheds and %d species of %s.",
  #     input$state,
  #     dims[1],
  #     dims[2],
  #     str_to_lower(input$taxon)
  #   )
  # })
  
  # output$na_numbers <- renderUI({
  #   dims <- dim(spp_na())
  #   sprintf(
  #     "North America has %d watersheds and %d species of %s.",
  #     dims[1],
  #     dims[2],
  #     str_to_lower(input$na_taxon)
  #   )
  # })
  
  output$prediction_na <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_tutorial)
  })
  
  # output$prediction_state <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_state)
  # })
  #
  # output$prediction_ca <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_ca)
  # })
  #
  # output$ca_info <- renderUI({
  #   if (input$ca_marine == "Range extent") {
  #     p(
  #       "Range extent for California coastal marine fishes. Each
  #       vertical bar shows the minimum to maximum latitude for
  #       one species of fish. Species with a
  #       median latitude above Point Conception are shown in
  #       red. Species with an average latitude below Point Conception
  #       are shown in blue. The horizontal black line is the latitude of
  #       Point Conception. Fishes are sorted (left to right on
  #       x-axis) in order of minimum latitude. "
  #     )
  #   } else {
  #     img(src = "california.png", width = "97%")
  #   }
  # })
  #
  #
  
  ## State histograms ------------------------------------------------------
  
  # output$state_histogram <- renderPlot({
  #   numWatersheds <- colSums(spp())
  #   numSpecies <- rowSums(spp())
  #
  #   nws <- nrow(spp())
  #
  #   bins <- input$bins
  #
  #   plots$state <-
  #     plotHistogram(
  #       dat = tibble(numWatersheds),
  #       x = numWatersheds,
  #       breaks = c(nws, 1)
  #     )
  #
  #   plots$state
  # }, res = res)
  #
  ## North America histogram -------------------------------------------------
  
  #dat <-
  #  read_csv("data/tutorial_climate_data.csv", show_col_types = FALSE)
  
  dat <- readRDS("data/tutorial_climate_data.rds")
  
  output$na_histogram <- renderPlot({
    #numWatersheds <- colSums(spp_na())
    #numSpecies <- rowSums(spp_na())
    
    #dat <- tibble(numWatersheds)
    
    #nws <- nrow(spp_na()) # Number of watersheds for x-axis
    #plots$na <- plotHistogram(dat = tibble(numWatersheds), x = numWatersheds, breaks = c(nws, 5))
    plots$na <- plotScatter(dat = dat)
    #plots$na <- ggplot(dat) +
    #  geom_point(aes(x = MAT, y = MAP))
    
    plots$na
  }, res = res)
  
  
  # ## California Marine plots -------------------------------------------------
  #
  # output$ca_marine_plot <- renderPlot({
  #   cafish <- open_file(st = "California")
  #
  #   if (input$ca_marine == "Range size") {
  #     rangeSize <- rowSums(cafish)
  #     numSpecies <- colSums(cafish)
  #
  #     plots$ca <- plotHistogram(dat = tibble(rangeSize), x = rangeSize, breaks = c(100, 5)) +
  #       scale_x_continuous(breaks = seq(0, 100, 20)) +
  #       xlab("Range size (degrees of latitude occupied)")
  #
  #     plots$ca
  #   } else { # plot 2.  Need better checks for the if/else
  #
  #     ## Convert much of this manipulation to dplyr / tidyverse
  #     mycolors <- c("#9d2235", "#003b5c")
  #     numRows <- nrow(cafish) ## number of species
  #     numCols <- ncol(cafish) ## Number of 1° latitude cells
  #
  #     meanCut <- 34.4481 ## Point Conception latitude as cutoff for northern and southern species.
  #
  #     medianLat <- rep(NA, numRows) ## Create a vector same length as number of species.
  #
  #     minLat <- vector("numeric")
  #     maxLat <- vector("numeric")
  #
  #     for (i in 1:numRows) {
  #       x <- data.frame(cafish)[i, ]
  #       y <- colnames(x)[x == 1]
  #
  #       colNames <- gsub("N", "", y)
  #       colNames <- gsub("S", "-", colNames)
  #
  #       minLat[i] <- as.numeric(colNames[1])
  #       maxLat[i] <- as.numeric(colNames[length(colNames)])
  #       medianLat[i] <- median(as.numeric(colNames))
  #     }
  #
  #     cafish$minLat <- minLat
  #     cafish$maxLat <- maxLat
  #     cafish$medianLat <- medianLat
  #
  #     latCol <- vector("character")
  #     for (i in 1:numRows) {
  #       if (cafish$medianLat[i] > meanCut) {
  #         latCol[i] <- mycolors[1]
  #       } else {
  #         latCol[i] <- mycolors[2]
  #       }
  #     }
  #
  #     cafish$latCol <- latCol
  #     cafish$xrow <- seq(1:516)
  #
  #     cafish <- cafish[order(-cafish$minLat, -cafish$medianLat), ]
  #
  #     ggplot(cafish) +
  #       geom_segment(aes(x = xrow, y = minLat, xend = xrow, yend = maxLat),
  #         color = latCol, size = 1.2
  #       ) +
  #       theme_minimal() +
  #       ylab("Latitude (°S — °N)") +
  #       xlab(NULL) +
  #       geom_hline(yintercept = c(36, 32), col = "gray") +
  #       geom_hline(yintercept = meanCut, col = "gray10") +
  #       scale_y_continuous(breaks = seq(-40, 70, 10)) +
  #       theme(axis.text.x = element_blank())
  #   }
  # }, res = res)
  #
  #
  # Report Download ---------------------------------------------------------
  
  # Report output idea from Shiny Gallery
  output$downloadReport <- downloadHandler(
    filename = function() {
      stu_name <-
        str_to_lower(str_split(input$student_name, " ", simplify = TRUE))
      # For the student (or famous soocer player) with only one name.
      # What happens for students with three or more names?
      paste(paste0(rev(stu_name),
                   collapse = "_"),
            "geographic_range.pdf",
            sep = "_")
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
        "tutorial.Rmd",
        pdf_document(
          latex_engine = "lualatex",
          keep_tex = TRUE,
          includes = includes(in_header = "tex_header.tex")
        )
      )
      file.rename(out, file)
      on.exit(removeNotification(notification_id), add = TRUE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
