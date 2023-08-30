## California coastal marine fishes.
## Histogram shows distribution of range size (degrees of latitude covered)
## Range extent shows vertical line for each species showing minimum and
## maximum points of reange.


# Libraries ---------------------------------------------------------------

library(shiny)
#library(dplyr)
library(stringr)
library(ggplot2)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Geographic Range Size: California Fishes",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "California Marine Fishes"
    ),
    # Instructions tab ------------------------------------------------------------

    tabPanel(
      "Instructions",
      mainPanel(
        p("Fishes, mussels, and some crayfishes can't cross land so it 
          seems reasonable that they may be restricted to just a few 
          watersheds. However, coastal marine fishes do not have obvious 
          limitations to their distribution. Adults of many coastal marine 
          fishes are able to swim very long distances. Thus, at least 
          potentially, more marine fishes may have relatively large range 
          sizes compared to those with small ranges."),
        p("Your goal for this exercise is to determine whether California
          coastal marine fishes have large range sizes. All species occur 
          in California but some species have ranges that extend as far 
          south as 30°S (central Chile, South America) or as far north as 
          68°N (north of the Arctic Circle, Alaska, North America). The only 
          requirement was that some part of the species' range had to occur 
          within the coastal waters of California. For each degree of 
          latitude, a species was assigned 1 if present, and 0 if absent."),
        p("This app helps you calculate the range size (number of degrees of
          latitude occupied) for each species. You will also calculate the
          number of species present in each degree of latitude."),
        p("Choose the Predictions tab to begin. Choose the state and the
        taxon that was assigned to you."),
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
            placeholder = "First Last"
          ),
          hr(),
          p(),
          p("Enter your predictions at right, then press the
               'Next' button."),
        ),
        column(
          3,
          p(strong("What is your hypothesis?")),
          p("Given that the total latitudinal range in the data set 
            covers 99° of latitude, from 30°S to 68°N, does the mean
            number of degrees latitude occupied suggest that most 
            coastal marine fishes likely have large or small range 
            size? Explain."),
          textAreaInput(
            inputId = "predict_ca_range",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "California range size prediction…"
            ),
          br(),
          hr()
        ),
        
        column(
          3,
          p(strong("What do you predict for the state level?")),
          p("Will
               most species have small, moderate, or large range
               sizes?"),
          textAreaInput(
            inputId = "predict_state",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "DO I NEED?"
          ),
          br(),
          hr()
        ),
        
        column(
          3,
          p(strong("What do you predict for California?")),
          p("Will most species have small, moderate,
               or large range sizes?"),
          textAreaInput(
            inputId = "predict_ca",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "California prediction…"
          ),
          hr(),
          p(),
          actionButton(inputId = "btn_next_pred", label = "Next", width = "35%"),
          span(textOutput("prediction_error"), style = "color:#9D2235")
        )
      )
    )
  )
) # end UI


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  output$prediction_error <- renderText({
    if (input$student_name == "" |
      input$predict_ca_range == "" |
      input$predict_ca == "") {
      "Please fill in all blanks."
    }
  })

  output$na_result_error <- renderText({
    if (input$na_result == "") {
      "Please interpret the histogram."
    }
  })
  
  output$ca_result_error <- renderText({
    if (input$ca_result == "") {
      "Please interpret the histogram."
    }
  })
  
  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(ca = NULL)
  
  results <- reactiveValues(ca = NULL)


  # Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$na_taxon)) {
    # Comment out for development.
     pred_check(sn = input$student_name,
                pn = input$predict_ca_range,
                pc = input$predict_ca)

    removeTab(inputId = "tabs", target = "Predictions")
    appendTab(inputId = "tabs", tab = ca_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "North America", select = TRUE)
    }
  })

  observeEvent(input$btn_next_na, {
    if (is.null(input$ca_marine)) {
      result_check(exp = input$ca_result)
      appendTab(inputId = "tabs", tab = ca_tab, select = TRUE)  
    } else {
      showTab(inputId = "tabs", target = "California Marine Fishes", select = TRUE) 
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

  output$prediction_na <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_ca_range)
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



  ## California Marine plots -------------------------------------------------

  output$ca_marine_plot <- renderPlot({
    cafish <- readRDS("data/ca_data.rds")[["california_marine_fishes"]]

    if (input$ca_marine == "Range size") {
      rangeSize <- rowSums(cafish)
      numSpecies <- colSums(cafish)

      plots$ca <- plotHistogram(dat = tibble(rangeSize), x = rangeSize, breaks = c(100, 5)) +
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
          color = latCol, linewidth = 1.2
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
      src <- normalizePath("range_report.Rmd")
      src_tex <- normalizePath("tex/tex_header.tex")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "range_report.Rmd", overwrite = TRUE)
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
