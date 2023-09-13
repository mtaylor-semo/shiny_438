## Rapoport's Rule
## Richness and area show plots to identify latitudes in the U.S. with
## the greatest species richness and largest geographic area.

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
    windowTitle = "Biogeograpy: Rapoport's Rule",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Rapoport's Rule"
    ),
    # Instructions tab ------------------------------------------------------------

    tabPanel(
      "Instructions",
      fluidRow(
        column(
          width = 3,
          img(src = "birds.png", width = "97%"),
          br(),
          img(src = "mammals.png", width = "97%")
        ),

        column(
          width = 6,
          p("Rapo instructions go here."),
          p("More instructions."),
          p("Yet more if necessary."),
          hr(),
          p("Choose the Predictions tab above to begin.")
        )
      ),
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
          6,
          p(strong("What is your hypothesis?")),
          p("Given that the total latitudinal range in the data set 
            covers 99° of latitude, from 30°S to 68°N, does the mean
            number of degrees latitude occupied suggest that most 
            coastal marine fishes likely have large or small range 
            size? Explain."),
          textAreaInput(
            inputId = "predict_richness_area",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "Range size prediction…",
            width = "90%"
            ),
          br(),
          hr(),
          p(strong("Where will species richness be highest?")),
          p("The data set spans across tropical regions north
            past the Arctic Circle. In general terms, where do you think
            species richness (number of species) will be highest? You
            do not have to give an exact latitude, but you can enter things
            like 'close to the equator', or 'at the higher latitudes.'"),
          textAreaInput(
            inputId = "predict_pc",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "Species richness prediction…",
            width = "90%"
          )
        ),
        
        column(
          3,
          #img(src = "west_coast.png", width = "97%"),
          hr(),
          br(),
          actionButton(inputId = "btn_next_pred", label = "Next", width = "35%"),
          span(textOutput("prediction_error"), style = "color:#9D2235"),
        )
      )
    )
  )
) # end UI


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)


  ## Error message outputs ---------------------------------------------------

  output$prediction_error <- renderText({
    if (input$student_name == "" |
      input$predict_richness_area == "" |
      input$predict_pc == "") {
      "Please fill in all blanks."
    }
  })

  output$pc_result_error <- renderText({
    if (input$pc_result == "") {
      "Please interpret the plot"
    }
  })
  
  output$richness_area_result_error <- renderText({
    if (input$richness_area_result == "") {
      "Please interpret the histogram."
    }
  })
  
  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(ra = NULL, pc = NULL)

  
  # results <- reactiveValues(ca = NULL)


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$richness_area)) {
    # Comment out for development.
     pred_check(sn = input$student_name,
                ra = input$predict_richness_area,
                pc = input$predict_pc)

    removeTab(inputId = "tabs", target = "Predictions")
    appendTab(inputId = "tabs", tab = richness_area_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Richness and Area", select = TRUE)
    }
  })

  observeEvent(input$btn_next_ra, {
    if (is.null(input$pc_tab)) {
      result_check(exp = input$richness_area_result)
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
    #sprintf("%s", input$predict_richness_area)
  })
  
  output$prediction_ca <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_richness_area)
  })
  
  output$richness_area_info <- renderUI({
    if (input$richness_area == "Species richness") {
      p("Mean (red) and maximum (blue) species richness for freshwater
        fishes in the U.S. The solid black line connects mean and
        maximum values for the same degree of latitude.")
    } else {
      p("Relative area occupied by freshwater
        fishes in the U.S. Area is the total number of 1 x 1° cells
        occupied by a species. A species that occupies 5° of longitude
        and 2° of latitude has a relative area of 10. A species
        that occupies 4° of longitude and 5° of latitude has a relative
        area of 20. Species with large relative geographic areas cover
        a broader part of the U.S.")
    }
  })



  ## Richness and area -------------------------------------------------

  output$richness_area_plot <- renderPlot({
    
    if (input$richness_area == "Species richness") {
      plots$ca <- plot_latitudes(dat = na_grid)
    } else {
      plots$ca <- plot_relative_area(dat = fish_area)
    }

    plots$ca
 }, res = res)
  
  output$pc_plot <- renderPlot({
    plots$pc <- plot_champagne(fish_area)
    
    plots$pc
    
  }, res = res)
  # Report Download ---------------------------------------------------------
  
  # Report output idea from Shiny Gallery
  base_rmd <- "rapoports_rule.Rmd"
  base_pdf <- "rapoports_rule.pdf"

  output$downloadReport <- downloadHandler(
    base_rmd,
    filename = function() {
      stu_name <- str_to_lower(str_split(input$student_name, " ", simplify = TRUE))

      paste(
        paste0(
          rev(stu_name), 
          collapse = "_"), 
        #"rapoports_rule.pdf", 
        base_pdf,
        sep = "_"
      )
    },
    content = function(file) {
      notification_id <- showNotification(
        "Generating report for download.",
        duration = NULL,
        closeButton = FALSE,
        type = "message"
      )
      #src <- normalizePath("rapoports_rule.Rmd")
      src <- normalizePath(base_rmd)
      src_tex <- normalizePath("tex/tex_header.tex")
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      #file.copy(src, "rapoports_rule.Rmd", overwrite = TRUE)
      file.copy(src, base_rmd, overwrite = TRUE)
      file.copy(src_tex, "tex_header.tex", overwrite = TRUE)
      
      library(rmarkdown)
      
      out <- render(
        #"rapoports_rule.Rmd",
        base_rmd,
        pdf_document(latex_engine = "lualatex",
                     keep_tex = TRUE,
                     includes = includes(in_header = "tex_header.tex"))
      )
      file.rename(out, file)
      on.exit(showNotification(
        "Download complete. You may close your browser.",
        duration = NULL,
        closeButton = FALSE,
        type = "message", id = notification_id), add = FALSE, after = FALSE
      )
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)
