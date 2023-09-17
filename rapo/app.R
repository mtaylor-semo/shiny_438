## Rapoport's Rule
## Richness and area show plots to identify latitudes in the U.S. with
## the greatest species richness and largest geographic area.

# Libraries ---------------------------------------------------------------

library(shiny)
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
          img(src = "animals.jpg", width = "97%")
        ),
        column(
          width = 7,
          p("Rapoport's Rules were based originally on land birds and mammals. You have
            already explored how the rules fit coastal marine fishes and, to a lesser extent,
            freshwater fishes."),
          p("For this exercise, you will explore whether the rules apply to primary freshwater
            fishes in the U.S., plus parts of northern Mexico and southern Canada. First, you
            will explore mean and maximum richness as a function of latitude. Second, you will
            explore whether range size increases with latitude."),
          p("The geographic area was divided into a grid of 1° latitude x 1° longitude cell
            (see figure below). If a species was collected in one of the 1° cells, then
            presence was recorded with a 1. Absence was recorded with a 0. The 
            presence/absence data for each cell was recorded for 529 species of North 
            American freshwater fishes."),
          hr(),
          p("Choose the Predictions tab above to begin."),
          hr(),
          img(src = "na_grid.png", width = "100%")
        )
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
          6,
          p(strong("Think carefully about your predictions below."), "Is the U.S. uniform from
            south to north? What about east to west? Think about where the U.S. tends
            to be wetter (more precipitation) or more arid (drier). Do some regions
            have more rivers than other regions? Will the U.S. have any regional climate
            effects due to rain shadows or deserts? How might this influence freshwater
            fishes and Rapoport's Rule?"),
          p(strong("What do you predict for species richness?")),
          p("Based on what you have learned so far from lecture and
            other exercises, do you think freshwater fishes of the U.S.
            will follow Rapoport's Rule for species richness? Do you think species richness
            could be higher at mid-U.S. latitudes compared to lower or higher latitudes in
            the U.S.? Explain."),
          hr(),
          textAreaInput(
            inputId = "predict_richness_area",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "Species richness prediction…",
            width = "90%"
            ),
          br(),
          hr(),
          p(strong("What do you predict for range size?")),
          p("Based on what you have learned so far from lecture and
            other exercises, do you think range size of freshwater fishes will strictly
            follow Rapoport's Rule for range size as you go from south to north? Why or
            why not? Do you think there might be differences in the south to north trend
            if you compare the eastern U.S. to the western U.S.? Why or why not?"),
          textAreaInput(
            inputId = "predict_pc",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "Range size prediction…",
            width = "90%"
          )
        ),
        
        column(
          3,
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
    if (input$pc_q4 == "" |
        input$pc_q4 == "" |
        input$pc_q4 == "" ) {
      "Please answer all three questions below the figure."
    }
  })
  
  output$richness_area_result_error <- renderText({
    if (input$richness_area_q1 == "" |
        input$richness_area_q2 == "" |
        input$richness_area_q3 == "" ) {
      "Please answer all three questions below the figure."
    }
  })
  
  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(rapo = NULL, pc = NULL)

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
      req(input$richness_area_q1, 
          input$richness_area_q2, 
          input$richness_area_q3)
      appendTab(inputId = "tabs", tab = pc_tab, select = TRUE)  
    } else {
      showTab(inputId = "tabs", target = "U.S. Range Size", select = TRUE) 
    }
  })

  observeEvent(input$btn_next_pc, {
    if (is.null(input$summary)) {
      #result_check(exp = input$pc_result)
      req(input$pc_q5,
          input$pc_q4,
          input$pc_q6)
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
      plots$rapo <- plot_latitudes(dat = na_grid)
    } else {
      plots$rapo <- plot_relative_area(dat = fish_area)
    }

    plots$rapo
 }, res = res)
  
  output$pc_plot <- renderPlot({
    plots$pc <- plot_champagne(fish_area)
    
    plots$pc
    
  }, res = res)
  # Report Download ---------------------------------------------------------
  # Report output idea from Shiny Gallery
  
  # Define file name constants
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
