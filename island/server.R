# Server functions for Island Biogeograph app

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  ## Error message outputs ---------------------------------------------------

  output$stu_name_error <- renderText({
    if (input$student_name == "") {
      msg <- "Please enter your name"
    } else {
      msg <- ""
    }
  })
  
  output$next_ready <- renderText({
    if (input$student_name == "") {
      msg <- ""
    } else {
      msg = "Press Next after reading."
    }
  })
  
  output$prediction_error <- renderText({
      has_empty_input(list(input$student_name, input$predict_na_richness))
  })

  output$galapagos_result_error <- renderText({
      has_empty_input(list(input$galapagos_question1))
  })
  
  output$family_richness_result_error <- renderText({
    has_empty_input(
      list(
        input$family_richness_question1, 
        input$family_richness_question2
      )
    )
  })
  
  output$cluster_result_error <- renderText({
    has_empty_input(list(input$cluster_question1))
  })

  output$nmds_result_error <- renderText({
    has_empty_input(list(input$nmds_question1))
  })
  
  ## Reactive values ---------------------------------------------------------

  values <- reactiveValues(
   options = list(
     dom = "tip",
     order = list()
   ) 
  )
  

  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_intro, {
    if (error_check) req(input$student_name)
    next_tab(
      tab = predictions_tab, 
      target = "Predictions", 
      test = input$predict_na_richness)
  })
  
  observeEvent(input$btn_prev_pred, {
    prev_tab("Introduction")
  })
  
  observeEvent(input$btn_next_pred, {
    if (error_check) req(input$predict_na_richness)
    next_tab(
      tab = galapagos_tab,
      target = "Galapagps",
      test = NULL)
    hideTab(
      inputId = "tabs",
      target = "Predictions")
  })
  # 
  # observeEvent(input$btn_prev_na, {
  #   prev_tab("Predictions")
  # })
  # 
  # observeEvent(input$btn_next_na, {
  #   req(input$na_question1)
  #   next_tab(
  #     tab = family_richness_tab,
  #     target = "Family Richness",
  #     test = input$family_richness_question1
  #   )
  # })
  # 
  # observeEvent(input$btn_prev_family_richness, {
  #   prev_tab("North America")
  # })
  # 
  # observeEvent(input$btn_next_family_richness, {
  #   req(
  #     input$family_richness_question1,
  #     input$family_richness_question2
  #   )
  #   next_tab(
  #     tab = cluster_tab,
  #     target = "Cluster",
  #     test = input$cluster_question1
  #   )
  # })
  # 
  # observeEvent(input$btn_prev_cluster, {
  #   prev_tab("Family Richness")
  # })
  # 
  # observeEvent(input$btn_next_cluster, {
  #   req(
  #     input$cluster_question1,
  #     input$cluster_question2,
  #     input$cluster_question3
  #   )
  #   next_tab(
  #     tab = nmds_tab,
  #     target = "NMDS",
  #     test = input$nmds_question1
  #   )
  # })
  # 
  # observeEvent(input$btn_prev_nmds,{
  #   prev_tab("Cluster")
  # })
  # 
  # observeEvent(input$btn_next_nmds, {
  #   req(input$nmds_question1)
  #   next_tab(
  #     tab = summary_tab,
  #     target = "Summary",
  #     test = input$summary
  #   )
  # })
  # 
  # observeEvent(input$btn_prev_summary, {
  #   prev_tab("NMDS")
  # })
  # 
  # observeEvent(input$family_menu, {
  #   output$species_info <- renderText(get_species_info(input$family_menu))
  # })

  ## Outputs -------------------------------------------------------------

  output$spp_info <- renderUI({
    p(get_species_info(input$family_menu),
    img(src = get_species_image(input$family_menu), width = "97%"))
  })


  output$prediction_na_richness <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na_richness)
  })
  
  output$plot_menu <- renderUI({
    if (input$choose_galapagos_data_set == "Birds") {
        selectInput(
        "galapagos_bird_plot",
        label = "Choose a plot type",
        choices = c("Birds per Island", "Islands per Bird"),
        selected = "Birds per Island"
      )
    } else if (input$choose_galapagos_data_set == "Islands") {
      selectInput(
        inputId = "galapagos_plot_xaxis",
        label = "Choose X-axis variable",
        choices = c("Area", "Elevation"),
        selected = "Area"
      )
    } else {
      renderText("somthing has gone horribly wrong")
    }
  })



# Galapagos ---------------------------------------------------------------

  observeEvent(input$choose_galapagos_data_set, {
    if (input$choose_galapagos_data_set == "Birds") {
      output$galapagos_plot <- renderPlot(
        plot_birds_per_island()  
      )
    } else {
      output$galapagos_plot <- renderPlot(
        plot_galapagos(
          plot_data = islands,
          xaxis = str_to_lower(input$galapagos_plot_xaxis))
      )
    }
  })

  observeEvent(input$galapagos_bird_plot, {
    if (!is.null(input$galapagos_bird_plot)) {
      if (input$galapagos_bird_plot == "Birds per Island") {
        output$galapagos_plot <- renderPlot({
          plot_birds_per_island()
        })
      } else {
        output$galapagos_plot <- renderPlot({
          plot_islands_per_bird()
        })
      }
    }
  })

  # observe(
  #   if (!is.null(input$galapagos_plot_axis)) {
  #     if (input$galapagos_plot_xaxis == "Area") {
  #       values$options$order <- list(2, "asc")
  #     } else {
  #       values$options$order <- list(3, "asc")
  #     }
  #     output$island_summary <- renderDT({
  #       build_data_table(sort_order = values$options)
  #     })
  #   }
  # )

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
  
  # observeEvent(input$choose_galapagos_data_set, {
  #   if (input$choose_galapagos_data_set == "Birds") {
  #     values$options$order <- list(1, "desc")
  #   } else {
  #     if (input$galapagos_plot_xaxis == "Area") {
  #       values$options$order <- list(2, "asc")
  #     } else {
  #       values$options$order <- list(3, "asc")
  #     }
  #   }
  #   output$island_summary <- renderDT({
  #     build_data_table(sort_order = values$options)
  #   })
  # })

  # output$island_summary <- renderDT({
  #   islands %>% 
  #     select(-c(5, 6)) %>% 
  #     #arrange(desc(richness)) %>% 
  #     datatable(
  #     class = "compact",
  #     rownames = FALSE,
  #     colnames = c("Island", "Richness", "Area (sq. km)", "Elevation (m)"),
  #     options = list(
  #       dom = 'tip',
  #       order = list(list(1, "desc"))
  #     )
  #   ) #%>% 
  #     #formatRound(columns = 2:4, digits = 0)
  # })

  # observeEvent(input$state_menu_cluster, {
  #   #print("update cluster")
  #   state_choice(input$state_menu_cluster)
  # })
  
  # output$state_menu_nmds <- renderUI({
  #   selectInput(
  #     inputId = "state_menu_nmds",
  #     label = "Choose a state",
  #     choices = names(state_fishes),
  #     selected = state_choice(),
  #     multiple = FALSE
  #   )
  # })
  
  # observeEvent(input$state_menu_nmds, {
  #   state_choice(input$state_menu_nmds)
  #   updateSelectInput(
  #     session = getDefaultReactiveDomain(),
  #     inputId = "state_menu_cluster",
  #     selected = state_choice()
  #   )
  # })
  # 



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
