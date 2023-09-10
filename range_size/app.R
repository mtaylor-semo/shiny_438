##
## Show range size histograms for fishes, crayfishes, or mussels
## at state and North American levels.


# Libraries ---------------------------------------------------------------

library(shiny)
library(tibble)
library(tidyr)
#library(dplyr)
library(stringr)
library(ggplot2)

## UI ----------------------------------------------------------------------

ui <- tagList(
  includeCSS("www/semo_mods.css"),
  navbarPage(
    id = "tabs",
    windowTitle = "Biogeograpy: Geographic Range Size",
    title = div(
      img(src = "semo_logo.png", height = "70px"),
      "Geographic range size"
    ),
    # Instructions tab ------------------------------------------------------------

    tabPanel(
      "Instructions",
      mainPanel(
        p("This app allows you to explore range sizes for three
        aquatic groups (crayfishes, fishes, and mussels) for
        several states and for North America (primarily U.S.)."),
        p("Choose the Predictions tab to begin. Choose the state and the
        taxon that was assigned to you."),
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
          p(strong("What do you predict for North America?")),
          p("Will
               most species have small, moderate, or large
               range sizes?"),
          textAreaInput(
            inputId = "predict_na",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "North America prediction…"
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
            placeholder = "State prediction…"
          ),
          br(),
          hr()
        ),
        
        column(
          3,
          p(strong("Consider these five states:"), "(from south to
            north) Alabama, Tennessee, Kentucky, Illinois, Wisconsin."),
          p("Do you think the range size of fishes will follow Rapoport's
            Rule for range size from Alabama north to Wisconsin?"),
          textAreaInput(
            inputId = "predict_rapo_five",
            label = NULL, #"Enter your prediction:",
            rows = 4,
            placeholder = "Rapoport's Rule prediction…"
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
      input$predict_state == "" |
      input$predict_na == "" |
      input$predict_rapo_five == "") {
      "Please fill in all blanks."
    }
  })

  output$na_result_error <- renderText({
    if (input$na_result == "") {
      "Please interpret the histogram."
    }
  })
  
  output$state_result_error <- renderText({
    if (input$state_result == "") {
      "Please interpret the histogram."
    }
  })

  output$rapo_five_result_error <- renderText({
    if (input$rapo_five_result == "") {
      "Please interpret the bar charts."
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
  
  rapo <- reactive({
    filter(rapo_data,
    str_to_lower(input$rapo_taxon))
  })

  plots <- reactiveValues(na = NULL, state = NULL, rapo = NULL)
  
  results <- reactiveValues(na = NULL, state = NULL, ca = NULL)


  # Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$na_taxon)) {
    # Comment out for development.
     pred_check(sn = input$student_name,
                ps = input$predict_state,
                pn = input$predict_na,
                pc = input$predict_rapo_five)

    removeTab(inputId = "tabs", target = "Predictions")
    appendTab(inputId = "tabs", tab = na_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "North America", select = TRUE)
    }
  })

  observeEvent(input$btn_next_na, {
    if (is.null(input$state)) {
      result_check(exp = input$na_result)
      appendTab(inputId = "tabs", tab = states_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "State", select = TRUE)
    }
  })

  observeEvent(input$btn_next_state, {
    if (is.null(input$ca_marine)) {
      result_check(exp = input$state_result)
      appendTab(inputId = "tabs", tab = rapo_five_tab, select = TRUE)  
    } else {
      showTab(inputId = "tabs", target = "Rapoport's Rule", select = TRUE) 
    }
  })

  observeEvent(input$btn_next_rapo_five, {
    if (is.null(input$summary)) {
      result_check(exp = input$rapo_five_result)
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
  
  output$prediction_rapo_five <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_rapo_five)
  })
  
  output$rapo_five_info <- renderUI({
    p("Placeholder. How will I use this")
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


  ## Rapoport plots -------------------------------------------------

  output$rapo_five_plot <- renderPlot({
    data_to_plot <- filter(rapo_data,
                           taxon == str_to_lower(input$rapo_taxon))
    plots$rapo <- plot_rapo(plot_data = data_to_plot)
    
    plots$rapo
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
