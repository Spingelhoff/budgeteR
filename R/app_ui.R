#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      navbarPage(
        "BudgeteR",
        tabPanel(
          "Budget",
          mod_budget_database_maintenance_ui("budget_database_maintenance_1")
        ),
        tabPanel(
          "Rent",
          mod_rental_database_maintenance_ui("rental_database_maintenance_1")
        ),
        tabPanel(
          "Visualize",
          mod_visualize_budget_ui("visualize_budget_1")
        ),
        tabPanel(
          "Plan",
          mod_plan_database_maintenance_ui("plan_database_maintenance_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "budgeteR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
