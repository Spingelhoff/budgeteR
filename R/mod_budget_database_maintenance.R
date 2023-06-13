#' budget_database_maintenance UI Function
#'
#' @description A shiny Module for simple read and write processes for a SQLite database.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_budget_database_maintenance_ui <- function(id){
  ns <- NS(id)

  table_list <- list(
    "Revenue" = "revenue",
    "Cost" = "cost"
  )

  tagList(
    fluidRow(
      column(
        12,
        align = "center",
        titlePanel(h1("Database Interface"))
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        selectInput(ns("table_selection"), "Select Table", choices = table_list)
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        h2("Add Records to Table")
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        dateInput(ns("transaction_date"), "Date")
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        selectInput(ns("transaction_classification"), "Classification", choices = NULL)
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        numericInput(ns("transaction_total"), "Total", 0)
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(ns("send_transaction_button"), "Save Record")
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        h2("View Table")
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        dateRangeInput(ns("query_range"), "Preview Range")
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(ns("preview_database_button"), "Preview/Refresh Data"),
        actionButton(ns("delete_record_button"), "Delete Record")
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        DT::DTOutput(ns("queried_table"))
      )
    )
  )
}

#' budget_database_maintenance Server Functions
#'
#' @noRd
mod_budget_database_maintenance_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    revenue_classification_list <- list(
      "Job",
      "Gig",
      "Sale",
      "Government",
      "Help"
    )

    cost_classification_list <- list(
      "Groceries",
      "Restaurant",
      "Fuel",
      "Entertainment",
      "Subscription",
      "Debt",
      "Miscellaneous"
    )

    observe({
      table_selection <- input$table_selection
      if (table_selection == "revenue") {
        updateSelectInput(
          session,
          "transaction_classification",
          choices = revenue_classification_list
        )
      } else if (table_selection == "cost") {
        updateSelectInput(
          session,
          "transaction_classification",
          choices = cost_classification_list
        )
      }
    })

    observeEvent(input$send_transaction_button, {
      showModal(modalDialog(
        title = "Send Record",
        "Are you sure you want to send?",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("confirm_transaction_button"), "Confirm"),
          modalButton("Cancel")
        )
      ))
    })

    observeEvent(input$confirm_transaction_button, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      DBI::dbSendStatement(
        con,
        paste0("
        INSERT INTO ", input$table_selection, " (date, classification, total)
        VALUES ('", as.character(input$transaction_date), "', ",
               "'", input$transaction_classification, "'", ", ",
               input$transaction_total, ");"
        )
      )
      removeModal()
    })

    updated_query_data <- eventReactive(input$preview_database_button, {
      set_SQLite_database()
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      dbGetQuery(
        con,
        paste0("
          SELECT *
          FROM ", input$table_selection, "
          WHERE date BETWEEN '", as.character(input$query_range[1]), "' AND '", as.character(input$query_range[2]), "'"
        )
      )
    })

    observeEvent(input$delete_record_button, {
      showModal(modalDialog(
        title = "Delete Record(s)",
        "Are you sure you want to delete?",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("confirm_delete"), "Confirm"),
          modalButton("Cancel")
        )
      ))
    })

    observeEvent(input$confirm_delete, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      DBI::dbSendStatement(
        con,
        paste0("
        DELETE FROM ", input$table_selection, "
        WHERE id IN (", paste(updated_query_data()$id[input$queried_table_rows_selected], collapse = ", "), ");"
        )
      )
      removeModal()
    })

    output$queried_table <- DT::renderDT(
      updated_query_data(),
      rownames = FALSE,
      filter = list(position = "top")
    )
  })
}

## To be copied in the UI
# mod_budget_database_maintenance_ui("budget_database_maintenance_1")

## To be copied in the server
# mod_budget_database_maintenance_server("budget_database_maintenance_1")
