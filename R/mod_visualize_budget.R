#' visualize_budget UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visualize_budget_ui <- function(id){
  ns <- NS(id)

  granularity_choices <- list(
    "Monthly" = "month",
    "Weekly" = "week",
    "Daily" = "day"
  )

  tagList(
    fluidRow(
      column(
        12,
        align = "center",
        dateRangeInput(ns("query_range_selection"), "Select Query Range")
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        selectInput(ns("granularity_selection"), "Summarize By", granularity_choices)
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(ns("send_query_button"), "Produce Report"),
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        DT::DTOutput(ns("report_table"))
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        plotOutput(ns("report_graph"))
      )
    )
  )
}

#' visualize_budget Server Functions
#'
#' @noRd
mod_visualize_budget_server <- function(id, budget_table = "revenue"){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    report_data <- reactiveVal()

    observeEvent(input$send_query_button, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      set_SQLite_database()
      granularity_choice_proxy <- input$granularity_selection
      report_data(
        DBI::dbGetQuery(
          con,
          paste0("
              SELECT *
              FROM ", budget_table,"
              WHERE date BETWEEN '", input$query_range_selection[1],"' AND '", input$query_range_selection[2],"'
          ")
        ) |>
          dplyr::mutate(
            date = lubridate::floor_date(lubridate::as_date(date), granularity_choice_proxy)
          ) |>
          dplyr::group_by(
            date,
            classification
          ) |>
          dplyr::summarize(
            total = sum(total)
          ) |>
          dplyr::ungroup()
      )
    })

    output$report_table <- DT::renderDT(
      report_data()
    )

    output$report_graph <- renderPlot({
      req(input$send_query_button)
      report_data() |>
        ggplot2::ggplot(ggplot2::aes(x = classification, y = total, fill = classification)) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::theme_classic() +
        ggplot2::scale_fill_brewer("Classification", palette = "Dark2")
    })

  })
}

## To be copied in the UI
# mod_visualize_budget_ui("visualize_budget_1")

## To be copied in the server
# mod_visualize_budget_server("visualize_budget_1")
