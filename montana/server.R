# Server functions for Provinces app

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
  
  ## Error message outputs ---------------------------------------------------

  output$prediction_error <- renderText({
    has_empty_input(list(input$predictions_question1, input$predictions_question2))
  })
  # 
  # output$na_richness_result_error <- renderText({
  #   has_empty_input(list(input$question1))
  # })
  # 
  # output$family_result_error <- renderText({
  #   has_empty_input(list(input$question2, input$question3))
  # })
  # 
  output$cluster_result_error <- renderText({
    has_empty_input(list(input$cluster_question1))
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


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_intro, {
    #shinyjs::runjs("window.scrollTo(0, 0)")
    next_tab(
      tab = predictions_tab, 
      target = "Predictions", 
      test = input$predictions_question1)
  })
  
  observeEvent(input$btn_prev_pred, {
    prev_tab("Introduction")
  })

  observeEvent(input$btn_next_pred, {
    req(input$predictions_question1, input$predictions_question2)
    next_tab(
      tab = cluster_tab, 
      target = "Cluster",
      test = input$cluster_question1)
  })
  
  observeEvent(input$btn_prev_cluster, {
    prev_tab(target = "Predictions")
  })
  
  observeEvent(input$btn_next_cluster, {
    req(input$cluster_question1)
    next_tab(
      tab = nmds_tab,
      target = "NMDS",
      test = input$nmds_question1
    )
  })

  observeEvent(input$btn_prev_nmds, {
    showTab(inputId = "tabs", target = "Cluster", select = TRUE)
  })
  

  observeEvent(input$btn_next_nmds, {
    if (is.null(input$summary)) {
      appendTab(inputId = "tabs", tab = summary_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Summary", select = TRUE)
    }
  })
  
  ## Outputs -------------------------------------------------------------

  output$similarity_table <- renderTable(
    similarity_table,
    digits = 0)

  output$prediction_na_richness <- renderUI({
    p("You predicted:")
    sprintf("%s", input$predict_na_richness)
  })


  output$question1n_prediction <- 
    output$question1_prediction <- 
    renderText(input$predictions_question1)

  output$question2n_prediction <- 
    output$question2_prediction <- 
    renderText(input$predictions_question2)
  
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
  output$cluster_plot <- output$cluster_plot_rep <- renderPlot(
    {
      fish.hel <- decostand(mt, method = "hellinger")
      fish.dist <- vegdist(fish.hel, method = "bray", binary = TRUE)
      cluster$clust <- hclust(fish.dist, method = "ward.D2")
      
      cluster$num_groups_to_cut <- 4
      
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
        mt,
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
