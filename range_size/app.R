##
## Show range size histograms for fishes, crayfishes, or mussels
## at state and North American levels.


# Libraries ---------------------------------------------------------------

library(shiny)
library(tibble)
library(tidyr)
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

    # Instructions tab --------------------------------------------------
    tabPanel(
      "Instructions",
      fluidRow(
        column(
          width = 3,
          img(src = "species.jpg", width = "97%"),
          hr(),
          br(),
          p(strong("Photo credits")),
          p("Top: Rainbow Darter, Uland Thomas, NANFA"),
          p("Middle: Spot-handed Crayfish, Jim Rathert, MDC"),
          p("Bottom: Rabbitsfoot Mussel, USFWS")
        ),
        column(
          8,
          p("This app allows you to explore range sizes for three
        aquatic groups (fishes, crayfishes, and mussels) for
        North America (primarily U.S.) and for several states. Range size
        is defined here as the number of watersheds occupied by a species.
        A watershed is a large river and the smaller rivers that flow in to
        it. The number of watersheds varies with scale. Watersheds of
        North America are larger rivers; states use smaller 
        rivers. The image below shows one representation of the major
        watersheds of North America. "),
          p("You will also begin to explore Rapoport's rule for five
        selected states."),
          p("Choose the Predictions tab to begin. Follow the accompanying
        handout carefully for additional instructions."),
          hr(),
          img(src = "watershed_map.png", width = "97%"),
          p("Watersheds of North America. Each color represents a
            different watershed.")
        )
      ),
    ),


    # Predictions tab ----------------------------------------------------
    tabPanel(
      "Predictions",
      fluidRow(
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
          hr(),
          p(),
          img(src = "mo_watersheds.png", width = "97%")
        ),
        column(
          6,
          p(strong("What do you predict for North America?")),
          p("Will most species have small, moderate, or large range sizes?
            The North America data set has 126 watersheds. Consider 
            'small range size' to be fewer than 10-15 watersheds, 'large
            range size' to be more than 75, and 'moderate range size' 
            to be somewhere between. These are arbitrary cutoffs to get 
            you started. You may also predict some combination of the three
            options." ),
          textAreaInput(
            inputId = "predict_na",
            label = NULL,
            rows = 4,
            placeholder = "North America prediction…",
            width = "90%"
          ),
          hr(),
          p(strong("What do you predict for the state level?")),
          p("Will most species have small, moderate, or large range
            sizes? Missouri has 44 watersheds (left) but most states that
            you will explore vary greatly in their number of watersheds."),
          textAreaInput(
            inputId = "predict_state",
            label = NULL,
            rows = 4,
            placeholder = "State level prediction…",
            width = "90%"
          ),
          hr(),
          p(strong("Consider these five states:"), "(from south to
            north) Alabama, Tennessee, Kentucky, Illinois, and Wisconsin."),
          p("Do you think fishes, mussels, or crayfishes will follow
            Rapoport's Rule for range size and species richness from
            Alabama north to Wisconsin?"),
          p("It is okay for you to say that all, some, or none of these
            taxonomic groups will agree.  If you think only some taxa will
            follow Rapoport's Rule, be sure to identify those taxa in your
            prediction."),
          textAreaInput(
            inputId = "predict_rapo_five",
            label = NULL,
            rows = 4,
            placeholder = "Rapoport's Rule prediction…",
            width = "90%"
          )
        ),
        column(
          3,
          actionButton(
            inputId = "btn_next_pred",
            label = "Next",
            width = "35%"
          ),
          span(textOutput("prediction_error"),
            style = "color:#9D2235"
          )
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

  # rapo <- reactive({
  #   filter(rapo_data,
  #   str_to_lower(input$rapo_taxon))
  # })

  plots <- reactiveValues(na = NULL, state = NULL, rapo = NULL)

  results <- reactiveValues(na = NULL, state = NULL, ca = NULL)


  # Button observers --------------------------------------------------------

  observeEvent(input$btn_next_pred, {
    if (is.null(input$na_taxon)) {
      pred_check(
        sn = input$student_name,
        ps = input$predict_state,
        pn = input$predict_na,
        rf = input$predict_rapo_five
      )

      removeTab(inputId = "tabs", target = "Predictions")
      appendTab(
        inputId = "tabs",
        tab = na_tab,
        select = TRUE
      )
    } else {
      showTab(
        inputId = "tabs",
        target = "North America",
        select = TRUE
      )
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
    if (is.null(input$rapo_five_result)) {
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
    sprintf(
      "%s has %d watersheds and %d species of %s.",
      input$state, dims[1], dims[2],
      str_to_lower(input$taxon)
    )
  })

  output$na_numbers <- renderUI({
    dims <- dim(spp_na())
    sprintf(
      "North America has %d watersheds and %d species of %s.",
      dims[1], dims[2],
      str_to_lower(input$na_taxon)
    )
  })

  output$prediction_na <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na)
  })
  
  prediction_na_escaped <- reactive({
    fix_special_chars(input$predict_na)
  })

  output$prediction_state <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_state)
  })
  
  prediction_state_escaped <- reactive({
    fix_special_chars(input$predict_state)
  })

  output$prediction_rapo_five <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_rapo_five)
  })

  output$rapo_five_info <- renderUI({
    p("Placeholder. How will I use this")
  })



  ## State histograms ------------------------------------------------------

  output$state_histogram <- renderPlot(
    {
      num_watersheds <- colSums(spp())
      num_species <- rowSums(spp())

      nws <- nrow(spp())

      bins <- input$bins

      plots$state <- plot_histogram(
        dat = tibble(num_watersheds),
        x = num_watersheds,
        breaks = c(nws, 1)
      )

      plots$state
    },
    res = res
  )

  ## North America histogram ------------------------------------------

  output$na_histogram <- renderPlot(
    {
      num_watersheds <- colSums(spp_na())
      num_species <- rowSums(spp_na())

      nws <- nrow(spp_na()) # Number of watersheds for x-axis

      plots$na <- plot_histogram(
        dat = tibble(num_watersheds),
        x = num_watersheds,
        breaks = c(nws, 5)
      )

      plots$na
    },
    res = res
  )


  ## Rapoport plots -------------------------------------------------

  output$rapo_five_plot <- renderPlot(
    {
      data_to_plot <- filter(
        rapo_data,
        taxon == str_to_lower(input$rapo_taxon)
      )
      plots$rapo <- plot_rapo(plot_data = data_to_plot)

      plots$rapo
    },
    res = res
  )


  # Report Download ---------------------------------------------------------

  # Report output idea from Shiny Gallery
  output$downloadReport <- downloadHandler(
    filename = function() {
      stu_name <- str_to_lower(str_split(input$student_name,
        " ",
        simplify = TRUE
      ))
      paste(
        paste0(rev(stu_name),
          collapse = "_"
        ),
        "geographic_range.pdf",
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
        type = "message", id = notification_id), add = FALSE, after = FALSE
        )
    }
  )
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
