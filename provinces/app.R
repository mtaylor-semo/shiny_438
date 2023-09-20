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
          img(src = "relief_map.jpg", width = "97%"),
          br(),
          p("Shaded relief map of the U.S. Color indicates elevation.
            Darker green is low elevation close to sea level. Light colors 
            indicate high elevation in the mountains."),
          hr(),
          p("Image credit:"),
          tags$a(href = "https://www.jpl.nasa.gov/images/pia03377-shaded-relief-with-height-as-color-north-america", "NASA Jet Propulsion Lab, California Institute of Technology.")
        ),
        column(
          width = 7,
          p("You are going to make a “shaded relief map” for species richness of U.S.~fishes. 
            The data were obtained by creating a presence / absence matrix for each species 
            of native fish. Presence or absence was based on 1° x 1° longitude /  latitude
            grids. If a fish species was present in a grid cell then 1 was entered for that
            grid cell. If the species was absent, 0 was entered. Finally, all matrices were
            summed together to create the final data matrix; that is, the data set contains
            the number of species in each 1° x 1° cell for the entire U.S.  The data set
            represents a total of 529 species"),
          p("In this expercise, the relief map uses colors to show relative species 
            richness. Dark colors indicate low species richness (few species). Bright 
            colors indicate higher species richness."),
          p("After you explore species richness for North America, you will explore the 
            distribution of species richness for specific groups of fishes."),
          hr(),
          p("Choose the Predictions tab above to begin."),
          hr()
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
          p(strong("Think carefully about your prediction below."), "Is the U.S. uniform from
            south to north? What about east to west? Think about where the U.S. tends
            to be wetter (more precipitation) or more arid (drier). Do some regions
            have more rivers than other regions? Will the U.S. have any regional climate
            effects due to rain shadows or deserts? How might this influence the distribution
            of species richness of freshwater fishes?"),
          p(strong("What do you predict for species richness for U.S. freshwater fishes?")),
          p("Where in the U.S. do you think species richness will be the highest? 
            Northeastern U.S.? Southeastern U.S.? Northwest? Southwest? Midwest? 
            Take a look at the map of U.S.~rivers that was given to you?  Below, 
            name the region and 3--4 rivers in that region where you think diversity 
            may be the highest. Explain your reasoning."),
          hr(),
          textAreaInput(
            inputId = "predict_na_richness",
            label = NULL, #"Enter your prediction:",
            rows = 6,
            placeholder = "U.S. species richness prediction…",
            width = "90%"
            ),
          br()
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
      input$predict_na_richness == "") {
      "Please fill in all blanks."
    }
  })

  output$pc_result_error <- renderText({
    if (input$pc_q4 == "" |
        input$pc_q5 == "" |
        input$pc_q6 == "" ) {
      "Please answer all three questions below the figure."
    }
  })
  
  output$na_richness_result_error <- renderText({
    if (input$question1 == "") {
      "Please answer the question to continue."
    }
  })
  
  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(na_richness = NULL, pc = NULL)

  # results <- reactiveValues(ca = NULL)


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$richness_area_q1)) {
    # Comment out for development.
     # pred_check(sn = input$student_name,
     #            ra = input$predict_na_richness)

    removeTab(inputId = "tabs", target = "Predictions")
    appendTab(inputId = "tabs", tab = na_richess_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "North America", select = TRUE)
    }
  })

  observeEvent(input$btn_next_na, {
    if (is.null(input$pc_q4)) {
      req(input$question1)
      appendTab(inputId = "tabs", tab = species_tab, select = TRUE)  
    } else {
      showTab(inputId = "tabs", target = "Species Richness", select = TRUE) 
    }
  })

  observeEvent(input$btn_next_spp, {
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

  # output$prediction_pc <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_na_richness)
  # })
  
  output$prediction_na_richness <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na_richness)
  })


  ## Richness and area -------------------------------------------------

  output$na_richness_plot <- renderPlot({
    
    plots$na_richness <- plot_na_grid()
    
    # plots$na_richness
    
 }, res = res, width = "100%") %>% 
    bindCache()
  
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
                     keep_tex = FALSE,
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
