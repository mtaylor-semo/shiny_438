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
  
  output$ib_result_error <- renderText({
    has_empty_input(list(input$ib_question1))
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
   ),
   state = 
     list(
       birds = "Birds per Island",
       islands = "Area"
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
      tab = ib_tab,
      target = "Islands and Animals",
      test = NULL)
    hideTab(
      inputId = "tabs",
      target = "Predictions")
  })

  observeEvent(input$btn_prev_ib, {
    prev_tab("Predictions")
  })

  observeEvent(input$btn_next_ib, {
    if (error_check) req(input$ib_question1)
    next_tab(
      tab = galapagos_tab,
      target = "Galapagos",
      test = NULL)
  })

  observeEvent(input$btn_prev_galapagos, {
    prev_tab("Islands and Animals")
  })

  observeEvent(input$btn_next_galapagos, {
    if (error_check) req(input$galapagos_question1)
    next_tab(
      tab = summary_tab,
      target = "Summary",
      test = input$summary
    )
  })

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
      "Caribbean Herps" = {
        output$ib_table <-
          renderTable(
            lm_summary(
              x = herps$area,
              y = herps$species
            )
          )
        output$ib_plot <-
          renderPlot(
            ib_plot(
              herps,
              x = "area",
              y = "species"
            )
          )
      },
      "Florida Beetles" = {
        output$ib_table <-
          renderTable(
            lm_summary(
              x = beetles$area,
              y = beetles$species
            )
          )
        output$ib_plot <-
          renderPlot(
            ib_plot(
              beetles,
              x = "area",
              y = "species"
            )
          )
      },
      "Montaine Mammals" = {
        output$ib_table <-
          renderTable(
            lm_summary(
              x = mtn$area,
              y = mtn$species
            )
          )
        output$ib_plot <-
          renderPlot(
            ib_plot(
              mtn,
              x = "area",
              y = "species"
            )
          )
      },
      "Arboreal Arthropods" = {
        output$ib_table <-
          renderTable(
            lm_summary(
              x = arthro$area,
              y = arthro$species
            )
          )
        output$ib_plot <-
          renderPlot(
            ib_plot(
              arthro,
              x = "area",
              y = "species"
            )
          )
      }
    )
  })


# Galapagos ---------------------------------------------------------------

####### Next two blocks work fine but always revert menu. 
  ##### Goin to try to fix. These are in the last working commit. f276c46c
  # observeEvent(input$choose_galapagos_data_set, {
  #   if (input$choose_galapagos_data_set == "Birds") {
  #     output$galapagos_plot <- renderPlot(
  #       plot_birds_per_island()  
  #     )
  #   } else {
  #     output$galapagos_plot <- renderPlot(
  #       plot_galapagos(
  #         plot_data = islands,
  #         xaxis = str_to_lower(input$galapagos_plot_xaxis))
  #     )
  #   }
  # })

  # observeEvent(input$galapagos_bird_plot, {
  #   if (!is.null(input$galapagos_bird_plot)) {
  #     if (input$galapagos_bird_plot == "Birds per Island") {
  #       output$galapagos_plot <- renderPlot({
  #         plot_birds_per_island()
  #       })
  #     } else {
  #       output$galapagos_plot <- renderPlot({
  #         plot_islands_per_bird()
  #       })
  #     }
  #   }
  # })

  ###### End the two working blocks.
  
  ###### Here is where the edits starts.
  
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

# NO EDITS BELOW THIS LINE ------------------------------------------------

  
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
