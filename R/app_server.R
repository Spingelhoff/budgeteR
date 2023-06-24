#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_budget_database_maintenance_server("budget_database_maintenance_1")
  mod_rental_database_maintenance_server("rental_database_maintenance_1")
  mod_visualize_budget_server("visualize_budget_1")
  mod_plan_database_maintenance_server("plan_database_maintenance_1")
}
