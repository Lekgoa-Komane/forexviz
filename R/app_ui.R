#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(), # CRITICAL: For showing/hiding screens

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # 1. THE WELCOME PAGE SCREEN (Shown first)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    div(
      id = "welcome_page",
      class = "welcome-screen", # Defined in custom CSS

      div(
        class = "welcome-content-panel",
        # Custom "FX" Logo (from image_1.png/image_2.png)
        tags$img(
          src = "www/fxviz_logo.jpeg",
          class = "welcome-logo"
        ),

        # Main Welcome text
        tags$h1("Welcome to ForexViz."),

        # Subtext
        tags$p(
          "Your professional toolkit for automated FX analysis",
          "and trading system dashboards."
        ),

        # THE ENTER BUTTON (Must have an Input ID)
        actionButton(
          inputId = "enter_dashboard_btn",
          label = "Enter Dashboard",
          class = "btn-welcome-enter" # Custom CSS style
        )
      )
    ),

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # 2. THE DASHBOARD SCREEN (Hidden on start)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    shinyjs::hidden(
      div(
        id = "dashboard_page",
        bslib::page_navbar(
          title = tags$span(
            style = "display: flex; align-items: center;",
            tags$img(
              src = "www/fxviz_logo.jpeg", # Matches the file in inst/app/www/
              height = "40px", # Keeps it compact inside the navbar
              style = "margin-right: 10px;"
            ),
            "Trading System Dashboard"
          ),
          theme = bslib::bs_theme(bootswatch = "superhero"),
          navbar_options = bslib::navbar_options(
            class = "bg-primary",
            theme = "dark"
          ),

          bslib::nav_spacer(),
          bslib::nav_item(
            bslib::input_dark_mode(id = "dark_mode", mode = "light")
          ),

          bslib::nav_menu(
            title = "Analytics & Reports",
            # Live panel
            bslib::nav_panel("Chart", mod_chart_ui("chart")),

            # Placeholder panels — real modules added one at a time
            bslib::nav_panel(
              "Journal Review",
              placeholder_panel("Journal Review")
            ),
            bslib::nav_panel("Performance", placeholder_panel("Performance")),
            bslib::nav_panel("Signals", placeholder_panel("Signals")),
            bslib::nav_panel("Report", placeholder_panel("Report")),
            bslib::nav_panel(
              "Paper Trading",
              placeholder_panel("Paper Trading")
            ),
            bslib::nav_panel(
              "Reversal Watch",
              placeholder_panel("Reversal Watch")
            ),
            bslib::nav_panel("COT Report", placeholder_panel("COT Report"))
          )
        )
      )
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
  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"), app_title = "forexviz"),

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # CUSTOM CSS: Styles the Welcome Screen (matching image_2.png aesthetic)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    tags$style(HTML(
      "
      /* Full screen welcome container with background image */
      .welcome-screen {
        position: fixed;
        top: 0; left: 0;
        width: 100vw; height: 100vh;
        background-color: #1a1e26; /* Dark slate (matching superhero) */
        /* background-image: url('www/welcome_bg.png'); */ 
        /* Use background from image_2.png */
        background-size: cover;
        background-position: center;
        display: flex;
        justify-content: center;
        align-items: center;
        z-index: 9999; /* Ensure it stays on top */
        color: #FFFFFF;
        font-family: 'Segoe UI', Roboto, Helvetica, sans-serif;
      }
      
      /* The central floating panel */
      .welcome-content-panel {
        /* Darker panel (matching image_2.png) */
        background: rgba(43, 49, 61, 0.95);
        padding: 50px;
        border-radius: 12px;
        text-align: center;
        border: 1px solid rgba(255, 255, 255, 0.1);
        box-shadow: 0 10px 40px rgba(0, 0, 0, 0.5);
      }
      
      .welcome-logo {
        height: 300px; /* Larger logo size */
        margin-bottom: 30px;
        /* Glow effect (image_1.png/image_2.png) */
        filter: drop-shadow(0 0 10px rgba(0, 255, 255, 0.4)); 
      }
      
      .welcome-content-panel h1 {
        font-weight: 300;
        font-size: 32px;
        margin-bottom: 10px;
      }
      
      .welcome-content-panel p {
        font-weight: 300;
        opacity: 0.7;
        margin-bottom: 40px;
        line-height: 1.6;
      }
      
      /* Standard button override to match visual look of button from 
      image_2.png */
      .btn-welcome-enter {
        background-color: transparent !important;
        color: #FFFFFF !important;
        /* Radiant cyan border (from image_2.png/image_1.png) */
        border: 2px solid #00FFFF !important; 
        border-radius: 4px;
        font-weight: bold;
        text-transform: uppercase;
        padding: 12px 30px;
        font-size: 14px;
        transition: 0.3s ease;
      }
      
      .btn-welcome-enter:hover {
        background-color: rgba(0, 255, 255, 0.1) !important;
        box-shadow: 0 0 15px rgba(0, 255, 255, 0.3);
      }
      
      /* Force the navbar to a clean, slim height */
      .navbar {
        min-height: 50px !important;
        max-height: 56px !important;
        padding-top: 0px !important;
        padding-bottom: 0px !important;
      }
      
      /* Ensure the brand area vertically centers the logo and text without 
      stretching */
      .navbar .navbar-brand {
        display: flex !important;
        align-items: center !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;
        line-height: 1 !important;
      }
    "
    ))
  )
}
