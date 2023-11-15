# Server functions for Island Biogeograph app

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  ## Error message outputs ---------------------------------------------------

  # output$stu_name_error <- renderText({
  #   if (input$student_name == "") {
  #     msg <- "Please enter your name"
  #   } else {
  #     msg <- ""
  #   }
  # })
  # 
  # output$next_ready <- renderText({
  #   if (input$student_name == "") {
  #     msg <- ""
  #   } else {
  #     msg <- "Press Next after reading."
  #   }
  # })
  # 
  # output$prediction_error <- renderText({
  #   has_empty_input(list(input$student_name, input$predict_na_richness))
  # })
  # 
  # output$galapagos_result_error <- renderText({
  #   has_empty_input(list(input$galapagos_question1))
  # })
  # 
  # output$ib_result_error <- renderText({
  #   has_empty_input(list(input$ib_question1))
  # })
  # 
  # output$cluster_result_error <- renderText({
  #   has_empty_input(list(input$cluster_question1))
  # })
  # 
  # output$nmds_result_error <- renderText({
  #   has_empty_input(list(input$nmds_question1))
  # })

  ## Reactive values ---------------------------------------------------------

  values <- reactiveValues(
    options = list(
      dom = "tip",
      order = list()
    ),
    state =
      list(
        birds = "Birds per Island",
        islands = "Area"
      ),
    beetles_xaxis = "area",
    rajas_xaxis = "area",
    aleuts_xaxis = "area",
    mtn_xaxis = "area"
  )

  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_intro, {
    # if (error_check) req(input$student_name)
    next_tab(
      tab = predictions_tab,
      target = "Size and Distance",
      test = input$btn_prev_pred
    )
  })

  observeEvent(input$btn_prev_pred, {
    prev_tab("Introduction")
  })

  observeEvent(input$btn_next_pred, {
    # if (error_check) req(input$predict_na_richness)
    next_tab(
      tab = ib_tab,
      target = "Species Richness",
      test = input$ib_group
    )
    # hideTab(
    #   inputId = "tabs",
    #   target = "Predictions")
  })

  observeEvent(input$btn_prev_ib, {
    prev_tab("Size and Distance")
  })

  observeEvent(input$btn_next_ib, {
    # if (error_check) req(input$ib_question1)
    next_tab(
      tab = galapagos_tab,
      target = "Galapagos",
      test = input$btn_prev_galapagos
    )
  })

  observeEvent(input$btn_prev_galapagos, {
    prev_tab("Size and Distance")
  })

  # observeEvent(input$btn_next_galapagos, {
  #   if (error_check) req(input$galapagos_question1)
  #   next_tab(
  #     tab = summary_tab,
  #     target = "Summary",
  #     test = input$summary
  #   )
  # })


  ## Outputs -------------------------------------------------------------

  # output$spp_info <- renderUI({
  #   p(
  #     get_species_info(input$family_menu),
  #     img(src = get_species_image(input$family_menu), width = "97%")
  #   )
  # })
  # 
  # 
  # output$prediction_na_richness <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_na_richness)
  # })

  output$curvilinear_example <- renderPlot(curvilinear_example)

  output$linear_example <- renderPlot(linear_example)
  
  output$plot_menu <- renderUI({
    if (input$choose_galapagos_data_set == "Birds") {
      selectInput(
        "galapagos_bird_plot",
        label = "Choose a plot type",
        choices = c("Birds per Island", "Islands per Bird"),
        selected = values$state$birds
      )
    } else if (input$choose_galapagos_data_set == "Islands") {
      selectInput(
        inputId = "galapagos_plot_xaxis",
        label = "Choose X-axis variable",
        choices = c("Area", "Elevation"),
        selected = values$state$islands
      )
    } else {
      renderText("somthing has gone horribly wrong")
    }
  })


  # IB ----------------------------------------------------------------------
  
  observe({
    req(input$ib_group)
    switch(input$ib_group,
      "Caribbean Herps" = output$ib_ui <- renderUI(build_herp_ui()),
      "Raja Ampat Trees" = output$ib_ui <- renderUI(build_raja_ui()),
      "Aleutian Plants" = output$ib_ui <- renderUI(build_aleut_ui()),
      "Florida Beetles" = output$ib_ui <- renderUI(build_beetle_ui()),
      "Montaine Mammals" = output$ib_ui <- renderUI(build_mammal_ui()),
      "Arboreal Arthropods" = output$ib_ui <- renderUI(build_arthro_ui())
    )
  })


  # Herps UI ----------------------------------------------------------------

  build_herp_ui <- function() {
    herps_regression <- lm_regress(
      x = log10(herps$area),
      y = log10(herps$species),
      term = "Area"
    )

    herp_gg_obj <- build_ib_plot(
      herps,
      x = "area",
      y = "species"
    )

    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(herps_regression),
        hr(),
        p(tags$b("About the data")),
        p(herps_info)
      ),
      column(
        6,
        renderPlot(herp_gg_obj)
      )
    )
  }



  # Raja Ampat Trees --------------------------------------------------------

  observe({
    req(input$raja_xaxis)
    values$rajas_xaxis <- if_else(
      input$raja_xaxis == "Area",
      "area", # if
      "distance" # else
    )
  })
  
  build_raja_ui <- function() {
    # xvar = if_else(
    #   values$rajas_xaxis == "area",
    #   "larea",
    #   "ldistance"
    # )
    rajas_regression <- lm_regress(
      x = log10(rajas[[values$rajas_xaxis]]),
      y = log10(rajas$richness),
      term = "Area"
    )
    
    rajas_gg_obj <- build_ib_plot(
      rajas,
      x = values$rajas_xaxis,
      y = "richness"
    )
    
    rajas_gg_obj <- update_xlabel(
      rajas_gg_obj,
      label = if_else(
        input$raja_xaxis == "Area",
        "Area (km²)",
        "Distance (km)"
      )
    )
    
    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(rajas_regression),
        hr(),
        p(tags$b("About the data")),
        p(rajas_info)
      ),
      column(
        6,
        selectInput(
          inputId = "raja_xaxis",
          label = "Choose x-axis",
          choices = c("Area", "Distance"),
          selected = input$raja_xaxis
        ),
        br(),
        renderPlot(rajas_gg_obj)
      )
    )
  }
  

  # Aleutian Plants ---------------------------------------------------------

  observe({
    req(input$aleut_xaxis)
    values$aleuts_xaxis <- case_when(
      input$aleut_xaxis == "Area" ~ "area",
      input$aleut_xaxis == "Distance from Alaska (km)" ~ "dAlaska",
      input$aleut_xaxis == "Distance from Kamchatka (km)" ~ "dKamchatka"
    )
  })
  
  build_aleut_ui <- function() {
    # xvar = if_else(
    #   values$aleuts_xaxis == "area",
    #   "area",
    #   "ldistance"
    # )
    # xvar = case_when(
    #   values$aleuts_xaxis == "area" ~ "area",
    #   values$aleuts_xaxis == "dAlaska" ~ "dAlaska",
    #   values$aleuts_xaxis == "dKamchatka" ~ "dKamchatka"
    # )

    aleuts_regression <- lm_regress(
      x = log10(aleuts[[values$aleuts_xaxis]]),
      y = log10(aleuts$richness),
      term = "Area"
    )
    
    aleuts_gg_obj <- build_ib_plot(
      aleuts,
      x = values$aleuts_xaxis,
      y = "richness"
    )
    
    aleuts_gg_obj <- update_xlabel(
      aleuts_gg_obj,
      label = if_else(
        input$aleut_xaxis == "Area",
        "Area (km²)",
        "Distance from Alaska (km)",
        "Distance from Kamchatka (km)"
      )
    )
    
    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(aleuts_regression),
        hr(),
        p(tags$b("About the data")),
        p(aleuts_info)
      ),
      column(
        6,
        selectInput(
          inputId = "aleut_xaxis",
          label = "Choose x-axis",
          choices = c(
            "Area",
            "Distance from Alaska (km)",
            "Distance from Kamchatka (km)"
          ),
          selected = input$aleut_xaxis
        ),
        br(),
        renderPlot(aleuts_gg_obj)
      )
    )
  }
  

  # Beetles UI --------------------------------------------------------------

  observe({
    req(input$beetle_xaxis)
    values$beetles_xaxis <- if_else(
      input$beetle_xaxis == "Area",
      "area", # if
      "distance" # else
    )
  })


  build_beetle_ui <- function() {
    beetles_regression <- lm_regress(
      x = log10(beetles[[values$beetles_xaxis]]),
      y = log10(beetles$species),
      term = input$beetle_xaxis
    )

    beetle_gg_obj <- build_ib_plot(
      beetles,
      x = values$beetles_xaxis,
      y = "species"
    )

    beetle_gg_obj <- update_xlabel(
      beetle_gg_obj,
      label = if_else(
        input$beetle_xaxis == "Area",
        "Area (km²)",
        "Distance (km)"
      )
    )

    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(beetles_regression),
        hr(),
        p(tags$b("About the data")),
        p(beetles_info)
      ),
      column(
        6,
        selectInput(
          inputId = "beetle_xaxis",
          label = "Choose x-axis",
          choices = c("Area", "Distance"),
          selected = input$beetle_xaxis
        ),
        br(),
        renderPlot(beetle_gg_obj)
      )
    )
  }

  observe({
    req(input$mtn_xaxis)
    values$mtn_xaxis <- switch(input$mtn_xaxis,
      "Area" = "area",
      "Distance Between Mountains" = "dist_mtn",
      "Distance From Mainland" = "dist_mainland"
    )
  })


  # Montaine Mammals UI -----------------------------------------------------

  build_mammal_ui <- function() {
    mtn_regression <- lm_regress(
      x = log10(mtn[[values$mtn_xaxis]]),
      y = log10(mtn$species),
      term = input$mtn_xaxis
    )

    mtn_gg_obj <- build_ib_plot(
      mtn,
      x = values$mtn_xaxis,
      y = "species"
    )

    mtn_gg_obj <- update_xlabel(
      mtn_gg_obj,
      label = if_else(
        input$mtn_xaxis == "Area",
        "Area (km²)",
        if_else(
          input$mtn_xaxis == "Distance Between Mountains",
          "Distance Between Mountains (km)",
          "Distance From Mainland (km)"
        )
      )
    )

    plot <- renderPlot(gg_obj)

    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(mtn_regression),
        hr(),
        p(tags$b("About the data")),
        p(mtn_info)
      ),
      column(
        6,
        selectInput(
          inputId = "mtn_xaxis",
          label = "Choose x-axis",
          choices = c(
            "Area",
            "Distance Between Mountains",
            "Distance From Mainland"
          ),
          selected = input$mtn_xaxis
        ),
        br(),
        # plot
        renderPlot(mtn_gg_obj)
      )
    )
  }


  # Arboreal Arthropods UI --------------------------------------------------

  # Toggle button idea from
  # https://stackoverflow.com/a/74475204/3832941
  observeEvent(input$arthro_by_island, {
    if (input$arthro_by_island %% 2 != 0) {
      updateActionButton(session, "arthro_by_island", label = "Plot All Data")
    } else {
      updateActionButton(session, "arthro_by_island", NULL, label = "Plot By Island")
    }
  })

  # Toggling between plots from
  # https://stackoverflow.com/a/63044795/3832941
  arthro_plot <- reactiveVal(TRUE)

  observeEvent(input$arthro_by_island, {
    arthro_plot(!arthro_plot())
  })

  arthro_plot1 <-
    build_ib_plot(
      arthro,
      x = "area",
      y = "species"
    )

  arthro_plot1 <- update_xlabel(
    arthro_plot1,
    label = "Area (m²)"
  )

  arthro_plot2 <- plot_arthro_by_island()

  which_arthro <- reactive({
    if (arthro_plot()) {
      arthro_plot1
    } else {
      arthro_plot2
    }
  })
  # End toggle plots.


  build_arthro_ui <- function() {
    arthro_regression <- lm_regress(
      x = log10(arthro$area),
      y = log10(arthro$species),
      term = "Area"
    )

    # plot <- renderPlot(
    #   which_arthro()
    # )

    tagList(
      column(
        6,
        p(tags$b("Statistics")),
        build_stats(arthro_regression),
        hr(),
        p(tags$b("About the data")),
        p(arthro_info)
      ),
      column(
        6,
        actionButton(
          "arthro_by_island",
          label = "Plot by Island",
          width = "35%"
        ),
        br(),
        br(),
        renderPlot(which_arthro())
      )
    )
  }

  # Galapagos ---------------------------------------------------------------

  observe({
    if (!is.null(input$galapagos_bird_plot)) {
      if (input$galapagos_bird_plot == "Birds per Island") {
        output$galapagos_plot <- renderPlot({
          plot_birds_per_island()
        })
        values$state$birds <- "Birds per Island"
      } else {
        output$galapagos_plot <- renderPlot({
          plot_islands_per_bird()
        })
        values$state$birds <- "Islands per Bird"
      }
    }
  })

  observe({
    if (!is.null(input$galapagos_plot_xaxis)) {
      if (input$galapagos_plot_xaxis == "Area") {
        output$galapagos_plot <- renderPlot(
          plot_galapagos(
            plot_data = islands,
            xaxis = str_to_lower(input$galapagos_plot_xaxis)
          )
        )
        values$state$islands <- "Area"
      } else {
        output$galapagos_plot <- renderPlot(
          plot_galapagos(
            plot_data = islands,
            xaxis = str_to_lower(input$galapagos_plot_xaxis)
          )
        )
        values$state$islands <- "Elevation"
      }
    }
  })

  ## Move this up later.
  update_selector <- function(id = NULL, value = NULL) {
    reactive({
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = id,
        selected = value
      )
    })
  }

  observe({
    req(input$choose_galapagos_data_set)
    if (input$choose_galapagos_data_set == "Birds") {
      if (values$state$birds == "Birds per Island") {
        output$galapagos_plot <- renderPlot({
          plot_birds_per_island()
        })

        update_selector(id = "galapagos_bird_plot", value = values$state$birds)
        values$state$birds <- "Birds per Island"
      } else {
        output$galapagos_plot <- renderPlot({
          plot_islands_per_bird()
        })

        update_selector("galapagos_bird_plot", values$state$birds)
        values$state$birds <- "Islands per Bird"
      }
    } else {
      if (values$state$islands == "Area") {
        output$galapagos_plot <- renderPlot({
          plot_galapagos(
            plot_data = islands,
            xaxis = str_to_lower(values$state$islands)
          )
        })

        update_selector("galapagos_plot_xaxis", values$state$islands)
        values$state$islands <- "Area"
      } else {
        output$galapagos_plot <- renderPlot({
          plot_galapagos(
            plot_data = islands,
            xaxis = str_to_lower(values$state$islands)
          )
        })

        update_selector("galapagos_plot_xaxis", values$state$islands)
        values$state$islands <- "Elevation"
      }
    }
  })

  observe({
    if (!is.null(input$choose_galapagos_data_set)) {
      if (input$choose_galapagos_data_set == "Birds") {
        values$options$order <- list(1, "desc")
      } else if (input$choose_galapagos_data_set == "Islands") {
        if (!is.null(input$galapagos_plot_xaxis)) {
          if (input$galapagos_plot_xaxis == "Area") {
            values$options$order <- list(2, "asc")
          } else {
            values$options$order <- list(3, "asc")
          }
        }
      }
    }
    output$island_summary <- renderDT({
      build_data_table(sort_order = values$options)
    })
  })



  observe({
    if (!is.null(input$choose_galapagos_data_set)) {
      if (input$choose_galapagos_data_set == "Birds") {
        output$gala_regression <- renderUI(
          tagList(tags$hr())
        )
      } else {
        if (!is.null(input$galapagos_plot_xaxis)) {
          xvar <- if_else(
            input$galapagos_plot_xaxis == "Area",
            "area",
            "elevation"
          )
          gala_regression <- lm_regress(
            x = islands[[xvar]],
            y = islands$richness
          )
          output$gala_regression <- renderUI(
            tagList(
              tags$hr(),
              p(tags$b("Statistics:")),
              build_stats(gala_regression)
            )
          )
        }
      }
    }
  })

  # Report Download ---------------------------------------------------------
  # Report output idea from Shiny Gallery

  output$downloadReport <- downloadHandler(
    base_rmd,
    filename = function() {
      stu_name <- str_to_lower(str_split(input$student_name, " ", simplify = TRUE))

      paste(
        paste0(
          rev(stu_name),
          collapse = "_"
        ),
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
      # file.copy(src, "rapoports_rule.Rmd", overwrite = TRUE)
      file.copy(src, base_rmd, overwrite = TRUE)
      file.copy(src_tex, "tex_header.tex", overwrite = TRUE)

      library(rmarkdown)

      out <- render(
        base_rmd,
        pdf_document(
          latex_engine = "lualatex",
          keep_tex = FALSE,
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
