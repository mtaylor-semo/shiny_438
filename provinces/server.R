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


  ## Button observers --------------------------------------------------------

  observeEvent(input$btn_next_inst, {
    if (is.null(input$student_name)) {
      #    removeTab(inputId = "tabs", target = "Instructions")
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
      showTab(inputId = "tabs", target = "Species Richness", select = TRUE)
    }
  })

  observeEvent(input$btn_next_spp, {
    if (is.null(input$summary)) {
      # result_check(exp = input$pc_result)
      req(
        input$pc_q5,
        input$pc_q4,
        input$pc_q6
      )
      appendTab(inputId = "tabs", tab = summary_tab, select = TRUE)
    } else {
      showTab(inputId = "tabs", target = "Summary", select = TRUE)
    }
  })


  ## Outputs -------------------------------------------------------------

  # output$prediction_pc <- renderUI({
  #   p("You predicted:")
  #   sprintf("%s", input$predict_na_richness)
  # })

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
      #plots$na_richness
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
