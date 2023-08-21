##
## This is a skeleton shiny app with navBar that I can use
## for multiple exercises and courses.

## I took the skeleton from the hwe_practice app for 163.
## May have to reference it to fill in some knowledge gaps.

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Global variables --------------------------------------------------------

clim_dat <- read_csv('data/tutorial_climate_data.csv')



# Global functions --------------------------------------------------------

# Define global functions that can be used for multiple 
# reactives, etc.

# UI ----------------------------------------------------------------------


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
  
  # Simple problems tab -----------------------------------------------------
  
  tabPanel("Tab 1",
           sidebarLayout(
             sidebarPanel(
               withMathJax(),
               p("Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes
                 for details of the steps."
               ),
               actionButton("button_one",
                            "Button 1"),
               actionButton("button_two",
                            "Button 2")
               ),
             
             mainPanel(
               p("The frequency of one allele or one homozygous genotype is provided.
                 Calculate the four remaining frequencies using the two Hardy-Weinberg
                 equations, shown below left. The population is in Hardy-Weinberg equilibrium.
                 Each gene has only two alleles."
               ),
               p(), # A global variable was called here.
               hr(),
               htmlOutput("question"),
               hr(),
               htmlOutput("button_two_result")
               )
             )),
  
  # Counting problems -------------------------------------------------------
  
  tabPanel("Tab 2",
           sidebarLayout(
             sidebarPanel(
               p("Press \"New problem\" for a new problem to solve. Press
                 \"Show answer\" to see the solution. Refer to your notes
                 for details of the steps."
               ),
               actionButton("button_three",
                            "Button 3"),
               actionButton("button_four",
                            "Button 4")
               ),
             mainPanel(
               p("Use the methods learned in class to calculate whether
                 this population is in Hardy-Weinberg equilibrium.
                 Use the observed number of each genotype to calculate
                 genotype frequencies and observed allele frequencies."
               ),
               p(strong(
                 "Round each step to 3 digits after the decimal point."
               )),
               p(), # Global variable was called here.
               tags$hr(),
               textOutput("intro_counting"),
               htmlOutput("question_counting"),
               tags$hr(),
               htmlOutput("answer_counting"),
               uiOutput("check")
               )
)),
  # Chi-square problems  ---------------------------------------------------
tabPanel("Tab 3",
         sidebarLayout(
           sidebarPanel(
             p(
               "A third tab if you need it."
             ),
             actionButton("new_chi_problem",
                          "New problem"),
             actionButton("show_chi_answer",
                          "Show answer")
             ),
           mainPanel(
             "Text for Tab 3 Main Panel"
           )
)) # End chi tabPanel
)) # end UI

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  # Overview output ---------------------------------------------------------
  
  output$equations <- renderUI({
    #withMathJax( # Use if equations are needed.
      HTML(
        "Dynamic text"
      )
      # ) # End withMathJax
  })
  
  
  # Reactive values ---------------------------------------------------------
  
  show_simple_ans <- reactiveVal(FALSE)
  show_ans <- reactiveVal(FALSE)
  
  simple <- reactiveValues()#
  counting <- reactiveValues()
  chi <- reactiveValues()
  
  # Simple output -----------------------------------------------------------
  
  observeEvent(input$button_one, {
    show_simple_ans(FALSE)
    
    # Stuff goes here on observe event
    
  }, ignoreNULL = FALSE) #end observeEvent
  
  # output$EVNT goes here if FALSE?
  
  observeEvent(input$button_two, {
    show_simple_ans(TRUE)
  })
  
  # output$EVNT goes here if TRUE?
  output$button_two_result <- renderText({
    if (show_simple_ans()) {
      sprintf("You just pressed Button 2.")
    }
  })
  
  # Counting output ---------------------------------------------------------
  
  observeEvent(input$button_three, {
    show_ans(FALSE)
    
    # Some global functions were called here

  }, ignoreNULL = FALSE)
  

  observeEvent(input$button_four, {
    show_ans(TRUE)
  })
  
 
  
  }

# Run the application
shinyApp(ui = ui, server = server)
