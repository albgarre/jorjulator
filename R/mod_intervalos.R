#' intervalos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_column_wrap card card_header card_footer card_body layout_sidebar sidebar
#' @importFrom readxl read_excel
#' @importFrom rlang set_names
#' @importFrom tidyr drop_na pivot_longer pivot_wider separate
#' @importFrom dplyr mutate select if_else filter left_join select row_number
#' @importFrom purrr pmap_dfr map imap imap_dfr map2
#' @importFrom tibble tibble tribble as_tibble
#' @importFrom ggplot2 ggplot geom_col aes geom_point facet_wrap geom_errorbar theme scale_color_manual scale_fill_manual scale_fill_discrete labs scale_y_log10 coord_flip theme element_text
#' @importFrom formula.tools lhs
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
mod_intervalos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_column_wrap(
      width = 1,
      lorem::ipsum(paragraphs = 2)
    ),

    layout_column_wrap(
      width = 1,
      card(
        class = "bg-secondary",
        card_header(
          h4("Upload your data"),
          class = "bg-primary"
        ),
        "You can use this Excel template to upload your data:",
        downloadLink(NS(id, "downloadData"), "Download template"
                     ),
        hr(),
        "Once you filled it in, upload it using this button",
        fileInput(NS(id, "excel_file"), ""
                  ),
        actionButton(NS(id, "load_data"), "Calculate Ecosystem Services",
                     class = "btn-primary"
        )
      )
    ),

    layout_column_wrap(
      width = 1,
      card(
        class = "bg-warning",
        card_header(h4("Predicted ES"),
                    class = "bg-primary"
                    ),
        # tableOutput(ns("test")),



        layout_sidebar(
          sidebar = sidebar(
            checkboxInput(ns("fix_facet_prediction"), "Use same axes for all plots?", TRUE),
            checkboxInput(ns("errorbars_prediction"), "Include uncertainty estimates?", FALSE),
            checkboxInput(ns("logy_prediction"), "Plot in logscale?", FALSE),
          ),
          plotlyOutput(ns("plot_prediction"))
        ),

        card_footer(
          downloadButton(
            ns("down_prediction")
          )
        )
      ),
      card(
        class = "bg-warning",
        card_header(h4("Predictions by category"),
                    class = "bg-primary"),
        # tableOutput(ns("grouped_table")),

        layout_sidebar(
          sidebar = sidebar(
            checkboxInput(ns("fix_facet_grouped"), "Use same axes for all plots?", TRUE),
            checkboxInput(ns("errorbars_grouped"), "Include uncertainty estimates?", FALSE),
            checkboxInput(ns("logy_grouped"), "Plot in logscale?", FALSE)
          ),
          plotlyOutput(ns("grouped_plot"))
        ),
        card_footer(
          downloadButton(
            ns("down_grouped")
          )
        )
      )
      # card(
      #   card_header("Comparison grouped"),
      #   selectInput(ns("ref_soil"),
      #               "Baseline",
      #               choices = c()),
      #   plotOutput(ns("plot_comparison")),
      #   checkboxInput(ns("fix_facet"), "Same axes?", TRUE)
      # ),
      # card(
      #   card_header("Comparison individual"),
      #   selectInput(ns("ref_soil_indiv"),
      #               "Baseline",
      #               choices = c()),
      #   plotOutput(ns("plot_comparison_indiv")),
      #   checkboxInput(ns("fix_facet_indiv"), "Same axes?", TRUE)
      # ),
      # card(
      #   card_header("Comparison with project"),
      #   tableOutput(ns("table_project"))
      # )
    )

  )
}

#' intervalos Server Functions
#'
#' @noRd
mod_intervalos_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## Downloading the template

    output$downloadData <- downloadHandler(
      filename = function() {
        "simulation_template.xlsx"
      },
      content = function(file) {
        my_file <- system.file("extdata", "data_template.xlsx", package = "jorjulator")
        file.copy(my_file, file)
      }
    )

    ## Variables for the Excel file

    excelFile <- reactive({
      input$excel_file
    })

    ## Reactive values

    d_new <- reactiveVal()
    indicators_new <- reactiveVal()
    indicators_unscaled <- reactiveVal()
    intervals_new <- reactiveVal()


    ## Do stuff when loading the data

    observeEvent(input$load_data, {

      ## Reset the reactives

      d_new(NULL)
      indicators_new(NULL)
      indicators_unscaled(NULL)
      intervals_new(NULL)

      ## Names for the columns

      default_names <- c("sample", "soil_type", "temp", "rain",
                         "biomass", "CO2", "nitrates", "retention",
                         "erosion", "bare",

                         "water_bodies", "nat_vegetation", "protected_spaces",
                         "geological_heritage", "recreation", "motor_vehicles",
                         "accessibility",

                         "employment")

      ## Load the data

      d <- read_excel(excelFile()$datapath,
                      # sheet = input$excel_sheet,
                      skip = 2,
                      sheet = "Template",
                      col_types = c(rep("text", 2),
                                    rep("numeric", 16)
                                    )
                      )

      ## Do a few checks

      # browser()

      #- Number of columns

      if (length(colnames(d)) != length(default_names)) {

        showModal(modalDialog(
          title = "Error loading the template",
          "The number of columns does not match. Did you delete/add any?",
          easyClose = TRUE,
          footer = NULL
        ))

        safeError("The number of columns is not right")
        return(NULL)

      }

      d <- set_names(d, default_names)

      #- Fill in empty values

      d <- d |>
        mutate(i = row_number()) |>
        mutate(sample = if_else(is.na(sample),
                                paste0("Case #, ", i),
                                sample
                                )) |>
        select(-i) |>
        mutate(across(default_names[-(1:2)], as.numeric)) |>
        mutate(across(default_names[-(1:2)], ~ if_else(is.na(.), 0, .)))

      #- Format of the columns

      old_nrow <- nrow(d)

      d <- drop_na(d)

      if (old_nrow != nrow(d)) {

        showModal(modalDialog(
          title = "Error loading the template",
          "There was a problem loading some rows. Did you use the wrong data format? Did you leave some cell empty?",
          easyClose = TRUE,
          footer = NULL
        ))

        safeError("The number of rows is not right")
        return(NULL)

      }

      ## Update the data reactive

      d_new(d)

      ## Get the models

      data("models_SECOs")

      # browser()

      ## Make the prediction

      d <- d |>  # codify the soil types
        left_join(
          tribble(
            ~soil_type, ~suelo,
            "Agricultural", 1,
            "Forest", 2,
            "Urban", 6
          )
        ) |>
        mutate(suelo = factor(suelo, levels = c(6, 2, 1)),
               aesthetics = (water_bodies + nat_vegetation + protected_spaces + geological_heritage + recreation + motor_vehicles + accessibility)*10/7
               ) |>
        mutate(precip = rain)

      my_cols <- c("CO2", "biomass", "nitrates", "retention",  # columns for each model
                   "erosion", "bare", "aesthetics", "employment")

      x_vects <- map2(models_SECOs, my_cols,  # get the x vector for each prediction
           ~ switch(.x$input,
                  log = log(d[[.y]]),
                  d[[.y]]
                  )
           )

      predictions <- models_SECOs |>  # calculate the predictions
        map2(x_vects,
             ~ predict(.x$model,
                       interval = "confidence",
                       newdata = d |> mutate(x =.y,
                                             logx = log(x),
                                             x2 = x^2,
                                             logx2 = logx^2
                                             )
                       )
        )

      ## Convert the scale when needed

      all_lhs <- models_SECOs |> map(~ .$output)
        # map(formula) |>
        # map(lhs)

      predictions_unscaled <- lapply(names(predictions), function(each_name) {

        if (all_lhs[each_name] == "log") {

          exp(predictions[[each_name]])

        } else {

          predictions[[each_name]]

        }

      })

      names(predictions_unscaled) <- names(predictions)

      ## Save the results

      # browser()

      indicators_new(predictions)
      indicators_unscaled(predictions_unscaled)

      ## Calculate the combinations

      grouped_ses <- predictions_unscaled |>
        map(as_tibble) |>
        map(~ mutate(., sample = d_new()$sample)) |>
        map(
          ~ mutate(., se = (upr-lwr)/2/1.96)
        ) |>
        imap_dfr(~ mutate(.x, index = .y)) |>
        select(index, sample, se) |>
        pivot_wider(names_from = index, values_from = se) |>
        mutate(
          se_Provision = plant_biomass,
          se_Regulating = sqrt(CO2_seq^2 + pollution^2 + water_retention^2 + soil_erosion_rate^2 + vegetation_cover^2),
          se_Cultural = sqrt(aesthetics^2 + employment^2),
          se_Total = sqrt(plant_biomass^2 + CO2_seq^2 + pollution^2 + water_retention^2 + soil_erosion_rate^2 + vegetation_cover^2 + aesthetics^2 + employment^2),
        ) |>
        select(sample, se_Provision, se_Regulating, se_Cultural, se_Total)

      grouped_mus <- predictions_unscaled |>
        map(as_tibble) |>
        map(~ mutate(., sample = d_new()$sample)) |>
        imap_dfr(~ mutate(.x, index = .y)) |>
        select(index, sample, fit) |>
        pivot_wider(names_from = index, values_from = fit) |>
        mutate(
          mu_Provision = plant_biomass,
          mu_Regulating = CO2_seq + pollution + water_retention + soil_erosion_rate + vegetation_cover,
          mu_Cultural = aesthetics + employment,
          mu_Total = mu_Provision + mu_Regulating + mu_Cultural,
        ) |>
        select(sample, mu_Provision, mu_Regulating, mu_Cultural, mu_Total)

      gruoped_intervals <- left_join(grouped_ses, grouped_mus) |>
        pivot_longer(-sample) |>
        separate(name, into = c("what", "index"), sep = "_") |>
        pivot_wider(names_from = "what", values_from = "value") |>
        mutate(lwr = mu - 1.96*se,
               upr = mu + 1.96*se,
               )

      ## Update the reactive

      intervals_new(gruoped_intervals)

      ## Update the selectInput

      updateSelectInput(
        session = session,
        "ref_soil",
        choices = d$sample
      )

      updateSelectInput(
        session = session,
        "ref_soil_indiv",
        choices = d$sample
      )

    })

    ## Plot of the indices

    output$plot_prediction <- renderPlotly({

      # validate(need(indicators_new(), message = "Load the data"))
      validate(need(indicators_unscaled(), message = "Load the data"))
      validate(need(d_new(), message = "Load the data"))

      # browser()

      p <- indicators_unscaled() |>
        map(as_tibble) |>
        map(~ mutate(., sample = d_new()$sample)) |>
        imap_dfr(~ mutate(.x, index = .y)) |>
        ggplot(aes(x = sample, y = fit)) +
        geom_point()

      if (input$errorbars_prediction) {
        p <- p + geom_errorbar(aes(ymin = lwr, ymax = upr))
      }

      if (input$fix_facet_prediction) {

        p <- p + facet_wrap("index", nrow = 1)

      } else {

        p <- p + facet_wrap("index", scales = "free", nrow = 2)

      }

      if (input$logy_prediction) {
        p <- p + scale_y_log10()
      }

      p <- p + labs(x = "", y = "€/Ha") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)

    })

    # ## Indices grouped by category
    #
    # output$grouped_table <- renderTable({
    #
    #   validate(need(indicators_new(), message = "Load the data"))
    #
    #   indicators_new() |>
    #     mutate(
    #       provision = biomass,
    #       regulating = CO2 + pollution + retention + erosion + vegetation,
    #       cultural = aesthetics + employment
    #     ) |>
    #     select(sample, provision, regulating, cultural, total)
    #
    # })

    ## Plot of the indices grouped by category

    output$grouped_plot <- renderPlotly({

      validate(need(intervals_new(), message = "Load the data"))

      p <- intervals_new() |>
        ggplot(aes(x = sample, y = mu)) +
        geom_point()

      if (input$errorbars_grouped) {
        p <- p + geom_errorbar(aes(ymin = lwr, ymax = upr))
      }

      if (input$fix_facet_grouped) {

        p <- p + facet_wrap("index", nrow = 1)

      } else {

        p <- p + facet_wrap("index", scales = "free", nrow = 1)

      }

      if (input$logy_grouped) {
        p <- p + scale_y_log10()
      }

      p <- p + labs(x = "", y = "€/Ha") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotly(p)

    })

    # ## Plot with respect to baseline
    #
    # output$plot_comparison <- renderPlot({
    #
    #   validate(need(intervals_new(), message = "Load the data"))
    #   validate(need(input$ref_soil, message = ""))
    #
    #   baseline_cond <- input$ref_soil
    #
    #   facet_scale <- if_else(input$fix_facet, "fixed", "free")
    #
    #   intervals_new() |>
    #     select(sample, index, mu) |>
    #     mutate(ref = mean(if_else(sample == baseline_cond,
    #                               mu,
    #                               NA),
    #                       na.rm = TRUE),
    #            .by = c("index")
    #     ) |>
    #     mutate(dif = mu - ref) |>
    #     filter(sample != baseline_cond) |>
    #     mutate(great = dif > 0) |>
    #     ggplot(aes(x = sample, y = dif)) +
    #     geom_col(aes(fill = dif)) +
    #     facet_wrap("index", scales = facet_scale) +
    #     theme(legend.position = "none")
    #
    # })
    #
    # ## Plot with respect to baseline for individual indices
    #
    # output$plot_comparison_indiv <- renderPlot({
    #
    #   validate(need(indicators_unscaled(), message = "Load the data"))
    #   validate(need(input$ref_soil_indiv, message = ""))
    #
    #   baseline_cond <- input$ref_soil_indiv
    #
    #   facet_scale <- if_else(input$fix_facet_indiv, "fixed", "free")
    #
    #   indicators_unscaled() |>
    #     map(as_tibble) |>
    #     map(~ mutate(., sample = d_new()$sample)) |>
    #     imap_dfr(~ mutate(.x, index = .y)) |>
    #     select(sample, index, mu = fit) |>
    #     mutate(ref = mean(if_else(sample == baseline_cond,
    #                               mu,
    #                               NA),
    #                       na.rm = TRUE),
    #            .by = c("index")
    #     ) |>
    #     mutate(dif = mu - ref) |>
    #     filter(sample != baseline_cond) |>
    #     mutate(great = dif > 0) |>
    #     ggplot(aes(x = sample, y = dif)) +
    #     geom_col(aes(fill = dif)) +
    #     facet_wrap("index", scales = facet_scale) +
    #     theme(legend.position = "none")
    #
    # })

    # ## Comparison with the data from the project
    #
    # data_project <- tribble(
    #
    #   ~index, ~ecosystem, ~n, ~sd, ~min_eco, ~max_eco,
    #   "plant_biomass", 0.174, 11, 0.504, 0, 0.513,
    #   "CO2_seq",  88.205, 54, 216.651, 29.071, 147.339,
    #   "pollution", 5.861, 6, 11.123, -5.811, 17.533,
    #   "water_retention", 1.049, 7, 2.658, -5.811, 17.733,
    #   "vegetation_cover", 0.145, 5, 0.069, 0.06, 0.231,
    #   "aesthetics", 1534.36, 23, 1993.733, 572.205, 2396.515,
    #   "employment", 6136.24, 5, 821.38, 5116.468, 7156.212
    #
    # )
    #
    # output$table_project <- renderTable({
    #
    #   validate(need(indicators_unscaled(), message = "Load the data"))
    #
    #   indicators_unscaled() |>
    #     map(as_tibble) |>
    #     map(~ mutate(., sample = d_new()$sample)) |>
    #     imap_dfr(~ mutate(.x, index = .y)) |>
    #     select(sample, index, mu = fit) |>
    #     left_join(data_project) |>
    #     mutate(percent = pnorm(mu, mean = ecosystem, sd = sd)*100) |>
    #     select(sample, index, percent) |>
    #     pivot_wider(names_from = index, values_from = percent)
    #
    # })

    ## Downloads

    output$down_prediction <- downloadHandler(
      filename = function() {
        paste0("predicted_ES", ".csv")
      },
      content = function(file) {

        validate(need(indicators_unscaled(), message = "Load the data"))

        indicators_unscaled() |>
          map(as_tibble) |>
          map(~ mutate(., sample = d_new()$sample)) |>
          imap_dfr(~ mutate(.x, index = .y)) |>
          write.csv(file, row.names = FALSE)

      }
    )

    output$down_grouped <- downloadHandler(
      filename = function() {
        paste0("predicted_ES_categories", ".csv")
      },
      content = function(file) {

        validate(need(intervals_new(), message = "Load the data"))

        intervals_new() |>
          write.csv(file, row.names = FALSE)

      }
    )

  })
}

## To be copied in the UI
# mod_intervalos_ui("intervalos_1")

## To be copied in the server
# mod_intervalos_server("intervalos_1")
