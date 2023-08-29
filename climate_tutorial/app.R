##
## Simple exercise to plot relationship of three ecosystems
# with two climate variables (Mean annual temp & precip.)


# Libraries ---------------------------------------------------------------

library(shiny)
library(stringr)
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
            spread across southern Alberta. The dominant plant species of
            each ecosystem was recorded 
            (Cw = Western Redcedar, Gr = mixed grassland, La = Subalpine
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
          p(strong("What do you predict?"), "Your predictions should address
            at least these questions with short direct sentences."),
          p("Which ecosystem requires the warmest mean annual
            temperatures?"),
          p("Which ecosystem requires the coolest mean annual temperature?"),
          p("Which ecosystem requires the least amount of precipitation?"),
          p("Which ecosystem requires the most precipitation?"),
          p("Will any ecosystems co-occur? That is, will they require the 
            same range of temperature", em("and"), "precipitation?"),
          br(),
          p("You may use the internet to look up each species to help
            you answer the questions above."),
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
  
  output$interpret_error <- renderText({
    if (input$interpret_result == "") {
      "Please interpret the scatterplot."
    }
  })

    
  ## Reactive values ---------------------------------------------------------
  
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
                tab = plot_tab,
                select = TRUE)
    } else {
      showTab(inputId = "tabs",
              target = "North America",
              select = TRUE)
    }
  })
  

  observeEvent(input$btn_next_interpret, {
    if (is.null(input$summary)) {
      result_check(exp = input$interpret_result)
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
  

  output$prediction_na <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_tutorial)
  })

  dat <- readRDS("data/tutorial_climate_data.rds")
  
  output$na_scatter <- renderPlot({
    plots$na <- plotScatter(dat = dat)
    plots$na
  }, res = res)


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
            "climate_tutorial.pdf",
            sep = "_")
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
