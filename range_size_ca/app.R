## California coastal marine fishes.
## Histogram shows distribution of range size (degrees of latitude covered)
## Range extent shows vertical line for each species showing minimum and
## maximum points of range.


# Libraries ---------------------------------------------------------------

library(shiny)
library(stringr)
library(ggplot2)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Range Size: California Marine Fishes",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "California Marine Fishes"
    ),
    # Instructions tab -------------------------------------------------------

    tabPanel(
      "Instructions",
      fluidRow(
        column(
          width = 2,
          img(src = "ca_fishes.jpg", width = "97%"),
          br(),
          br(),
          p(strong("Photo credits")),
          p(
            tags$a(
              href = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/ce/Grahambones_-_3626550703.jpg/640px-Grahambones_-_3626550703.jpg",
              "Top: Garibaldi (juvenile), Graham Hellewell, CC BY 2.0"
            )
          ),
          p(
            tags$a(
              href = "https://commons.wikimedia.org/wiki/File:Gopher_rockfish.jpg",
              "Bottom: Gopher Rockfish, Tom Murphy VII, CC BY-SA 3.0"
            )
          )
        ),
        column(
          width = 6,
          p("Freshwater fishes, mussels, and some crayfishes can't cross
          land so it seems reasonable that their ranges are often restricted
          to just a few watersheds. However, coastal marine fishes do not
          have obvious  limitations to their distribution. Adults of many
          coastal marine fishes are able to swim very long distances. Thus,
          more marine fishes may have relatively large range
          sizes compared to those with small ranges."),
          p("Your goal for this exercise is to determine whether California
          coastal marine fishes have large range sizes. All 516 species in
          the data set occur
          in California but some species have ranges that extend as far
          south as 30°S (central Chile, South America) or as far north as
          68°N (north of the Arctic Circle, Alaska, North America). The only
          requirement was that some part of the species' range had to occur
          within the coastal waters of California. For each degree of
          latitude, a species was assigned 1 if present, and 0 if absent."),
          p("This app helps you calculate the range size (number of degrees
          of latitude occupied) for each species. You will also calculate
          the number of species present in each degree of latitude."),
          hr(),
          p("Choose the Predictions tab above to begin.")
        ),
        column(
          width = 4,
          img(src = "west_coast.png", width = "97%")
        )
      ),
    ),


    # Predictions tab --------------------------------------------------------
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
          6,
          p(strong("What is your prediction?")),
          p("Given that the total latitudinal range in the data set
            covers 99° of latitude, from 30°S to 68°N, does the mean
            number of degrees latitude occupied suggest that most
            coastal marine fishes likely have large (many degrees of latitude)
            or small range size (a few degrees of latitude)? Explain."),
          p("For comparison, Missouri from south (bottom of the bootheel) to north (Iowa
            border) spans about 4.5° of latitude."),
          textAreaInput(
            inputId = "predict_ca_range",
            label = NULL, # "Enter your prediction:",
            rows = 6,
            placeholder = "Range size prediction…",
            width = "90%"
          ),
          br(),
          hr(),
          p(strong("Where will species richness be highest?")),
          p("The data set spans from tropical regions south of the equator to north
            of the Arctic Circle. In general terms, where do you think
            species richness (number of species) will be highest? You
            do not have to give an exact latitude, but you can enter things
            like 'close to the equator', or 'at the higher latitudes.' Remember that
            all species in the data set occur at least partially in California."),
          textAreaInput(
            inputId = "predict_pc",
            label = NULL, # "Enter your prediction:",
            rows = 6,
            placeholder = "Species richness prediction…",
            width = "90%"
          )
        ),
        column(
          3,
          img(src = "west_coast.png", width = "97%"),
          hr(),
          br(),
          actionButton(inputId = "btn_next_pred", label = "Next", width = "35%"),
          span(textOutput("prediction_error"), style = "color:#9D2235"),
        )
      )
    )
  )
) # end UI


# Server ---------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)


  ## Error message outputs ---------------------------------------------------

  output$prediction_error <- renderText({
    if (input$student_name == "" |
      input$predict_ca_range == "" |
      input$predict_pc == "") {
      "Please fill in all blanks."
    }
  })

  output$pc_result_error <- renderText({
    if (input$pc_result == "") {
      "Please interpret the plot"
    }
  })

  output$ca_result_error <- renderText({
    if (input$ca_result == "") {
      "Please interpret the histogram."
    }
  })

  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(ca = NULL, pc = NULL)


  # results <- reactiveValues(ca = NULL)


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    # if (is.null(input$na_taxon)) {
    if (is.null(input$ca_marine)) {
      # Comment out for development.
      pred_check(
        sn = input$student_name,
        pn = input$predict_ca_range,
        pc = input$predict_pc
      )

      removeTab(inputId = "tabs", target = "Predictions")
      appendTab(inputId = "tabs", tab = ca_tab, select = TRUE)
      # appendTab(inputId = "tabs", tab = pc_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "California Marine Fishes", select = TRUE)
    }
  })

  observeEvent(input$btn_next_ca, {
    if (is.null(input$pc_tab)) {
      result_check(exp = input$ca_result)
      appendTab(inputId = "tabs", tab = pc_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Point Conception", select = TRUE)
    }
  })

  observeEvent(input$btn_next_pc, {
    if (is.null(input$summary)) {
      result_check(exp = input$pc_result)
      appendTab(inputId = "tabs", tab = summary_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Summary", select = TRUE)
    }
  })



  ## Outputs -------------------------------------------------------------

  output$prediction_pc <- renderUI({
    p("You predicted:")
    p(input$predict_pc)
    # sprintf("%s", input$predict_ca_range)
  })

  output$prediction_ca <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_ca_range)
  })

  output$ca_info <- renderUI({
    if (input$ca_marine == "Range extent") {
      p("Range extent for California coastal marine fishes. Each
        vertical bar shows the minimum to maximum latitude for
        one species of fish. Species with a
        median latitude above Point Conception are shown in
        blue. Species with an average latitude below Point Conception
        are shown in red. The horizontal black line is the latitude of
        Point Conception (more on next tab). Fishes are sorted (left to right on
        x-axis) in order of minimum latitude. ")
    } else {
      img(src = "california.png", width = "97%")
    }
  })



  ## California Marine plots -------------------------------------------------

  output$ca_marine_plot <- renderPlot(
    {
      if (input$ca_marine == "Range size") {
        
        output$pc_split <- renderUI({
          sprintf("This data set has 516 species.")
        })
        
        rangeSize <- rowSums(cafish)
        numSpecies <- colSums(cafish)

        plots$ca <- plotHistogram(dat = tibble(rangeSize), x = rangeSize, breaks = c(100, 5)) +
          xlab("Range size (degrees of latitude occupied)")

        plots$ca
        
      } else { # plot 2.  Need better checks for the if/else

        ## Convert much of this manipulation to dplyr / tidyverse

        numRows <- nrow(cafish) ## number of species
        numCols <- ncol(cafish) ## Number of 1° latitude cells

        meanCut <- 34.4481 ## Point Conception latitude as cutoff for N and S species.

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
            latCol[i] <- mycolors[2]
          } else {
            latCol[i] <- mycolors[1]
          }
        }
        
        
        output$pc_split <- renderUI({
          sprintf(
            "%d species have a median range north of Point Conception.\n
            %d species have a median range south of Point Conception.\n\n
            The median is the midpoint of the range.",
            nrow(filter(cafish, medianLat > 34.4)), nrow(filter(cafish, medianLat < 34.4)))
        })
        

        cafish$latCol <- latCol
        cafish$xrow <- seq(1:516)

        cafish <- cafish[order(-cafish$minLat, -cafish$medianLat), ]

        ggplot(cafish) +
          geom_segment(aes(x = xrow, y = minLat, xend = xrow, yend = maxLat),
            color = latCol, linewidth = 0.5
          ) +
          theme_minimal() +
          ylab("Latitude (°S — °N)") +
          xlab(NULL) +
          # geom_hline(yintercept = c(36, 32), col = "gray") +
          geom_hline(yintercept = meanCut, col = "gray10") +
          scale_y_continuous(breaks = seq(-40, 70, 10)) +
          theme(axis.text.x = element_blank())
      }
    },
    res = res
  )

  output$pc_plot <- renderPlot(
    {
      plots$pc <- plotPC(cafish)

      plots$pc
    },
    res = res
  )
  # Report Download ---------------------------------------------------------

  # Report output idea from Shiny Gallery
  output$downloadReport <- downloadHandler(
    filename = function() {
      stu_name <- str_to_lower(str_split(input$student_name, " ", simplify = TRUE))

      paste(
        paste0(
          rev(stu_name),
          collapse = "_"
        ),
        "california_coastal.pdf",
        sep = "_"
      )
    },
    content = function(file) {
      notification_id <- showNotification(
        "Generating report for download. Please wait.",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      src <- normalizePath("range_size_ca.Rmd")
      src_tex <- normalizePath("tex/tex_header.tex")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "range_size_ca.Rmd", overwrite = TRUE)
      file.copy(src_tex, "tex_header.tex", overwrite = TRUE)

      library(rmarkdown)

      out <- render(
        "range_size_ca.Rmd",
        pdf_document(
          latex_engine = "lualatex",
          keep_tex = TRUE,
          includes = includes(in_header = "tex_header.tex")
        )
      )
      file.rename(out, file)
      on.exit(showNotification(
        "Download complete. You may close your browser.",
        duration = NULL,
        closeButton = FALSE,
        type = "message", id = notification_id
      ), add = FALSE, after = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
