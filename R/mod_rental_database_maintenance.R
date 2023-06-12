#' rental_database_maintenance UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_rental_database_maintenance_ui <- function(id){
  ns <- NS(id)

  month_list <- list(
    "JAN" = "01",
    "FEB" = "02",
    "MAR" = "03",
    "APR" = "04",
    "MAY" = "05",
    "JUN" = "06",
    "JUL" = "07",
    "AUG" = "08",
    "SEP" = "09",
    "OCT" = "10",
    "NOV" = "11",
    "DEC" = "12"
  )

  table_list <- list(
    "Rent Settings" = "rent_defaults",
    "Rental Recievables" = "rent"
  )

  tagList(
    fluidRow(
      column(
        12,
        align = "center",
        titlePanel(h1("Rent Database Interface")),
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        selectInput(ns("table_selection"), "Select Table", choices = table_list),
      )
    ),
    div(),
    fluidRow(
      column(
        12,
        align = "center",
        selectInput(ns("month_selection"), "Select Month", choices = month_list),
        numericInput(ns("year_selection"), "Select Year", 2023)
      )
    ),
    fluidRow(
      column(
        12,
        align = "center",
        actionButton(ns("record_retrieval_button"), "Load"),
        actionButton(ns("reset_table_button"), "Reset"),
        actionButton(ns("update_records_button"), "Update"),
        actionButton(ns("delete_record_button"), "Delete"),
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

#' rental_database_maintenance Server Functions
#'
#' @noRd
mod_rental_database_maintenance_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    queried_data <- reactiveVal()

    observeEvent(input$record_retrieval_button, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      set_SQLite_database()
      if (input$table_selection == "rent_defaults") {
        con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
        on.exit(DBI::dbDisconnect(con))
        queried_data(
          DBI::dbGetQuery(
            con,
            "SELECT * FROM rent_defaults"
          )
        )
      } else if (input$table_selection == "rent") {
        con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
        on.exit(DBI::dbDisconnect(con))
        queried_data(
          DBI::dbGetQuery(
            con,
            paste0("
            SELECT *
            FROM rent
            WHERE month = ", input$month_selection, " AND year = ", input$year_selection
            )
          )
        )
      }
    })

    observeEvent(input$reset_table_button, {
      if (input$table_selection == "rent_defaults") {
        queried_data(
          data.frame(
            "room" = c("11", "21", "22", "23", "24", "B1", "B2", "B3"),
            "name" = c("Name", "Name", "Name", "Name", "Name", "Name", "Name", "Name"),
            "expected" = c(0, 0, 0, 0, 0, 0, 0, 0)
          )
        )
      } else if (input$table_selection == "rent") {
        con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
        on.exit(DBI::dbDisconnect(con))
        queried_data(
          DBI::dbGetQuery(
            con,
            paste0("
            SELECT *
            FROM rent_defaults"
            )
          ) |>
            cbind(
              "recieved" = c(0, 0, 0, 0, 0, 0, 0, 0),
              "month" = rep(input$month_selection, 8),
              "year" = rep(input$year_selection, 8)
            ) |>
            subset(
              select = -name
            )
        )
      }
    })

    output$queried_table <- DT::renderDT(
      queried_data(),
      editable = 'cell',
      rownames = FALSE
    )

    observeEvent(input$queried_table_cell_edit, {
      queried_data(
        DT::editData(queried_data(), input$queried_table_cell_edit, rownames = FALSE)
      )
    })

    observeEvent(input$update_records_button, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      set_SQLite_database()
      if (input$table_selection == "rent_defaults") {
        DBI::dbWriteTable(
          con,
          "rent_defaults",
          queried_data(),
          overwrite = TRUE
        )
      } else if (input$table_selection == "rent") {
        DBI::dbSendStatement(
          con,
          paste0("
            DELETE FROM rent
            WHERE month = ", input$month_selection, " AND year = ", input$year_selection ,"
          ")
        )

        DBI::dbSendStatement(
          con,
          paste0("
            DELETE FROM revenue
            WHERE date = '", input$year_selection, "-", input$month_selection, "-", "01'
          ")
        )

        DBI::dbWriteTable(
          con,
          "rent",
          queried_data(),
          append = TRUE
        )

        rent_earnings <- sum(queried_data()$recieved)

        DBI::dbSendStatement(
          con,
          paste0("
            INSERT INTO revenue (date, classification, total)
            VALUES ('", input$year_selection, "-", input$month_selection, "-", "01", "', 'Rent', ", rent_earnings, ")
          ")
        )
      }
    })

    observeEvent(input$delete_record_button, {
      con <- DBI::dbConnect(RSQLite::SQLite(), "database.sqlite")
      on.exit(DBI::dbDisconnect(con))
      set_SQLite_database()
      if (input$table_selection == "rent_defaults") {
        DBI::dbSendStatement(
          con,
          paste0("
            DROP TABLE IF EXISTS rent_defaults
          ")
        )
      } else if (input$table_selection == "rent") {
        DBI::dbSendStatement(
          con,
          paste0("
            DELETE FROM rent
            WHERE month = ", input$month_selection, " AND year = ", input$year_selection ,"
          ")
        )

        DBI::dbSendStatement(
          con,
          paste0("
            DELETE FROM revenue
            WHERE date = '", input$year_selection, "-", input$month_selection, "-", "01'
          ")
        )
      }
    })
  })
}

## To be copied in the UI
# mod_rental_database_maintenance_ui("rental_database_maintenance_1")

## To be copied in the server
# mod_rental_database_maintenance_server("rental_database_maintenance_1")
