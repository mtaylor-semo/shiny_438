# Server functions for Provinces app

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
  
  # output$next_ready <- renderText({
  #   if (input$student_name == "") {
  #     msg <- ""
  #   } else {
  #     msg = "Press Next after reading."
  #   }
  # })
  
  # output$prediction_error <- renderText({
  #   has_empty_input(list(input$student_name, input$predict_na_richness))
  # })

  # output$na_richness_result_error <- renderText({
  #   has_empty_input(list(input$na_question1))
  # })
  
  output$family_richness_result_error <- renderText({
    "After you finish with this tab, press Next for Part 2. This tab and 
    the previous two will be hidden. You 
    can view them again by pressing the Prev buttons or by choosing
    the Introduction tab to start again."
  })

  output$fall_result_error <- renderText({
    "Press the Prev buttons to view the hidden tabs."
  })
  
  # output$family_richness_result_error <- renderText({
  #   has_empty_input(
  #     list(
  #       input$family_richness_question1, 
  #       input$family_richness_question2
  #     )
  #   )
  # })
  # 
  # output$cluster_result_error <- renderText({
  #   has_empty_input(list(input$cluster_question1))
  # })
  output$cluster_result_error <- renderText({
    "Scroll down for important information."
  })
  # 
  # output$nmds_result_error <- renderText({
  #   has_empty_input(list(input$nmds_question1))
  # })
  
  ## Reactive values ---------------------------------------------------------

  plots <- reactiveValues(na_richness = NULL, pc = NULL)

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

  state_choice <- reactiveVal()


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_intro, {
    # if (error_check) req(input$student_name)
    next_tab(
      tab = interior_highlands_tab, 
      target = "Part 1: Interior Highlands", 
      test = input$btn_next_highland
      )
  })
  
  observeEvent(input$btn_prev_highland, {
    prev_tab("Introduction")
  })
  
  observeEvent(input$btn_next_highland, {
    # if (error_check) req(input$predict_na_richness)
    next_tab(
      tab = na_richess_tab, 
      target = "North America",
      test = input$btn_next_na
      )
    # hideTab(
    #   inputId = "tabs",
    #   target = "Predictions"
    #)
  })
  
  observeEvent(input$btn_prev_na, {
    prev_tab("Part 1: Interior Highlands")
  })
  
  observeEvent(input$btn_next_na, {
    #if (error_check) req(input$na_question1)
    next_tab(
      tab = family_richness_tab,
      target = "Family Richness",
      test = input$btn_next_family_richness
    )
  })

  observeEvent(input$btn_prev_family_richness, {
    prev_tab("North America")
  })
  
  observeEvent(input$btn_next_family_richness, {
    # if (error_check) req(
    #   input$family_richness_question1,
    #   input$family_richness_question2
    # )
    next_tab(
      tab = fall_line_tab,
      target = "Part 2: Fall Line",
      test = input$btn_next_fall
    )
    hideTab(
      inputId = "tabs",
      target = "Part 1: Interior Highlands"
    )
    hideTab(
      inputId = "tabs",
      target = "North America"
    )
    hideTab(
      inputId = "tabs",
      target = "Family Richness"
    )
  })

  observeEvent(input$btn_prev_fall, {
    prev_tab("Family Richness")
    hideTab(
      inputId = "tabs",
      target = "Part 2: Fall Line"
    )
    hideTab(
      inputId = "tabs",
      target = "Cluster"
    )
    hideTab(
      inputId = "tabs",
      target = "NMDS"
    )
    
  })
  
  observeEvent(input$btn_next_fall, {
    next_tab(
      tab = cluster_tab,
      target = "Cluster",
      test = input$btn_next_cluster
    )
    
  })
  
  observeEvent(input$btn_prev_cluster, {
    prev_tab("Part 2: Fall Line")
  })
  
  observeEvent(input$btn_next_cluster, {
    # if (error_check) req(
    #   input$cluster_question1,
    #   input$cluster_question2,
    #   input$cluster_question3
    # )
    next_tab(
      tab = nmds_tab,
      target = "NMDS",
      test = input$btn_prev_nmds
    )
  })

  observeEvent(input$btn_prev_nmds,{
    prev_tab("Cluster")
  })
  
  # observeEvent(input$btn_next_nmds, {
  #   # if (error_check) req(input$nmds_question1)
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
  
  observeEvent(input$family_menu, {
    output$species_info <- renderText(get_species_info(input$family_menu))
  })
  
  ## Outputs -------------------------------------------------------------

  output$spp_info <- renderUI({
    p(get_species_info(input$family_menu),
    img(src = get_species_image(input$family_menu), width = "97%"))
  })

  output$watershed_map_cluster <-
    output$watershed_map_nmds <- renderUI({
      watershed_map <- str_to_lower(
        paste0(
          state_choice(), "_watersheds.png"
        )
      )
      if (file.exists(paste0("www/", watershed_map))) {
        img(src = watershed_map, width = "97%")
      } else {
        p("Watershed map not available.")
      }
    })

  # output$prediction_na_richness <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_na_richness)
  # })


  ## Richness and area -------------------------------------------------

  output$na_richness_plot <- renderPlot(
    {
      plot_na_grid(
        plot_data = prepare_data(nagrid)
      )
    },
    res = res
  ) %>%
    bindCache(input$btn_next_pred, input$btn_prev_family_richness)

  output$family_plot <- renderPlot(
    {
      plot_na_grid(
        plot_data = prepare_data(species_groups[[input$family_menu]])
      )
    },
    res = res
  ) %>%
    bindCache(input$family_menu, input$btn_next_na)
  
  observeEvent(input$state_menu_cluster, {
    state_choice(input$state_menu_cluster)
  })
  
  output$state_menu_nmds <- renderUI({
    selectInput(
      inputId = "state_menu_nmds",
      label = "Choose a state",
      choices = names(state_fishes),
      selected = state_choice(),
      multiple = FALSE
    )
  })
  
  observeEvent(input$state_menu_nmds, {
    state_choice(input$state_menu_nmds)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      inputId = "state_menu_cluster",
      selected = state_choice()
    )
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
    res = res
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
    res = res
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
