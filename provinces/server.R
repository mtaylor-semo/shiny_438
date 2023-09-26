# Server functions for Provinces app

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  ## Error message outputs ---------------------------------------------------

  output$prediction_error <- renderText({
    has_empty_input(list(input$student_name, input$predict_na_richness))
  })

  # output$pc_result_error <- renderText({
  #   has_empty_input(list(pc_q4, pc_q5, pc_q6))
  # })

  output$na_richness_result_error <- renderText({
    has_empty_input(list(input$question1))
  })

  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(na_richness = NULL, pc = NULL)

  # results <- reactiveValues(ca = NULL)
  
  cluster <- reactiveValues(
    hel = NULL,
    dist = NULL,
    clust = NULL,
    num_groups_to_cut = NULL,
    colors = NULL,
    dend = NULL
  )
  
  nmds <- reactiveValues(
    mds = NULL,
    watershed_scores = NULL,
    tree_cut = NULL
  )

  #state_choice <- reactiveVal()



  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_inst, {
    if (is.null(input$student_name)) {
      appendTab(inputId = "tabs", tab = predictions_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Predictions", select = TRUE)
    }
  })

  observeEvent(input$btn_next_pred, {
    if (is.null(input$richness_area_q1)) {
      req(input$student_name, input$predict_na_richness)
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
      showTab(inputId = "tabs", target = "Family Richness", select = TRUE)
    }
  })

  observeEvent(input$btn_next_spp, {
    if (is.null(input$cluster_q4)) {
      req(
        input$pc_q5,
        input$pc_q4,
        input$pc_q6
      )
      appendTab(inputId = "tabs", tab = cluster_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Cluster", select = TRUE)
    }
  })
  
  observeEvent(input$btn_next_cluster, {
    if (is.null(input$nmds_q4)) {
      appendTab(inputId = "tabs", tab = nmds_tab, select = TRUE)
      #showTab(inputId = "tabs", target = "NMDS", select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "NMDS", select = TRUE)
    }
    # if (is.null(NULL)) {
    #   # req(
    #   #   input$pc_q5,
    #   #   input$pc_q4,
    #   #   input$pc_q6
    #   # )
    #   appendTab(inputId = "tabs", tab = nmds_tab, select = TRUE)
    # } else {
    #   showTab(inputId = "tabs", target = "NMDS", select = TRUE)
    # }
  })

  observeEvent(input$btn_next_nmds, {
    if (is.null(input$summary)) {
      # req(
      #   input$pc_q5,
      #   input$pc_q4,
      #   input$pc_q6
      # )
      appendTab(inputId = "tabs", tab = summary_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Summary", select = TRUE)
    }
  })
  
  observeEvent(input$spp_menu, {
    output$species_info <- renderText(get_species_info(input$spp_menu))
  })
  
  ## Outputs -------------------------------------------------------------

  # output$prediction_pc <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_na_richness)
  # })
  
  output$spp_info <- renderUI({
    p(get_species_info(input$spp_menu),
    img(src = get_species_image(input$spp_menu), width = "97%"))
    
    #img(src = get_species_image(input$spp_menu), width = "97%")
  })

  output$prediction_na_richness <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na_richness)
  })


  ## Richness and area -------------------------------------------------

  output$na_richness_plot <- renderPlot(
    {
      plot_na_grid(
        species_data = prepare_data(nagrid)
      )
    },
    res = res,
    width = "100%"
  ) %>%
    bindCache(nagrid)

  output$pc_plot <- renderPlot(
    {
      plot_na_grid(
        species_data = prepare_data(species_groups[[input$spp_menu]])
      )
    },
    res = res,
    width = "100%"
  ) %>%
    bindCache(input$spp_menu)
  
  state_choice <- reactiveVal()
  
  observeEvent(input$state_menu_cluster, {
    state_choice(input$state_menu_cluster)
    print("Inside cluster")
  })
  
  observeEvent(input$state_menu_nmds, {
    state_choice(input$state_menu_nmds)
    print("Inside nmds")
  })
  
  output$cluster_plot <- output$cluster_plot_rep <- renderPlot(
    {
      fish.hel <- decostand(state_fishes[[state_choice()]], method = "hellinger")
      fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)
      cluster$clust <- hclust(fish.dist, method = "ward.D2")
      
      cluster$num_groups_to_cut <- state_cuts[state_choice()]
      
      cluster$dend <- as.dendrogram(cluster$clust) %>%
        set("branches_k_color",
            value = mycolors, k = cluster$num_groups_to_cut
        ) %>%
        set("labels_colors", 
            value = mycolors, 
            k = cluster$num_groups_to_cut) %>%
        set("branches_lwd", 1.0) %>%
        set("labels_cex", 1)
      
      plot_cluster(cluster$dend)
    },
    res = res,
    width = "100%"
  ) %>%
    bindCache(input$state_menu_cluster, input$state_menu_nmds)
  

  output$nmds_plot <- renderPlot(
    {
      nmds$mds <- metaMDS(
        state_fishes[[state_choice()]],
        k = 2,
        trymax = 100,
        trace = 0
      )
      
      nmds$tree_cut <- dendextend::cutree(
        cluster$clust,
        k = cluster$num_groups_to_cut,
        order_clusters_as_data = FALSE
      )

      tmp.df <-
        data.frame(
          label = names(nmds$tree_cut), 
          colr = get_leaves_branches_col(cluster$dend)
        )
      
      nmds$watershed_scores <-
        scores(
          nmds$mds,
          display = "sites",
          tidy = TRUE
        ) %>% 
        left_join(x = ., y = tmp.df, by = "label")

      plot_nmds(nmds$watershed_scores)
    },
    res = res,
    width = "100%"
  ) %>%
    bindCache(input$state_menu_cluster, input$state_menu_nmds)

  
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
