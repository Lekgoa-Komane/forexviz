#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Config: loaded once for each user session -------------------------
  config <- forexdatasets::load_config()

  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # WELCOME PAGE LOGIC: Handle switching the screens
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  observeEvent(
    input$enter_dashboard_btn, # Listen for the specific input ID from app_ui
    {
      logger::log_info("Welcome page 'Enter Dashboard' clicked.")

      # 1. Hide the welcome page division
      shinyjs::hide(id = "welcome_page")

      # 2. Show the dashboard page division (which contains page_navbar)
      shinyjs::show(id = "dashboard_page")
    }
  )

  # ---- Chart module (iteration 2) -------------------------------
  mod_chart_server("chart", config = config)

  # Other modules re-added in later iterations:
  # mod_journal_review_server("journal", journal_con)
  # mod_performance_server("performance", journal_con)
  # ...
}
