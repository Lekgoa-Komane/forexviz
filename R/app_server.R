#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # ---- Config: loaded once for each user session -------------------------
  config <- forexdatasets::load_config()

  # ---- Chart module (iteration 2) -------------------------------
  mod_chart_server("chart", config = config)

  # Other modules re-added in later iterations:
  # mod_journal_review_server("journal", journal_con)
  # mod_performance_server("performance", journal_con)
  # ...
}
