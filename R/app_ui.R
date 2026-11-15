#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(),

    bslib::page_navbar(
      title = "Trading Dashboard",
      theme = bslib::bs_theme(bootswatch = "superhero"),
      navbar_options = bslib::navbar_options(
        class = "bg-primary",
        theme = "dark"
      ),

      bslib::nav_spacer(),
      bslib::nav_item(
        bslib::input_dark_mode(id = "dark_mode", mode = "dark")
      ),

      # Live panel
      bslib::nav_panel("Chart", mod_chart_ui("chart")),

      # Placeholder panels — real modules added one at a time
      bslib::nav_panel("Journal Review", placeholder_panel("Journal Review")),
      bslib::nav_panel("Performance", placeholder_panel("Performance")),
      bslib::nav_panel("Signals", placeholder_panel("Signals")),
      bslib::nav_panel("Report", placeholder_panel("Report")),
      bslib::nav_panel("Paper Trading", placeholder_panel("Paper Trading")),
      bslib::nav_panel("Reversal Watch", placeholder_panel("Reversal Watch")),
      bslib::nav_panel("COT Report", placeholder_panel("COT Report"))
    )
  )
}

#' Simple placeholder for modules not yet ported
#' @noRd
placeholder_panel <- function(name) {
  shiny::div(
    style = "padding: 40px; text-align: center;",
    shiny::h3(name),
    shiny::p(
      style = "color: #888;",
      "This module will be added in a later iteration."
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
      app_title = "forexviz"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
