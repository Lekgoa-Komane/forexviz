#' price_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList fluidRow column selectInput dateRangeInput
#' @importFrom shiny tags div br
#' @importFrom shinyWidgets switchInput
#' @importFrom echarts4r echarts4rOutput
# mod_chart_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # --- Top controls: ticker, timeframe, date range -----------------------
#     fluidRow(
#       column(3, selectInput(ns("asset_class"), "Asset Class", choices = NULL)),
#       column(2, selectInput(ns("ticker"), "Ticker", choices = NULL)),
#       column(
#         2,
#         selectInput(
#           ns("timeframe"),
#           "Timeframe",
#           choices = all_timeframes,
#           selected = "weekly"
#         )
#       ),
#       column(
#         3,
#         dateRangeInput(
#           ns("date_range"),
#           "Date Range",
#           start = Sys.Date() - 90,
#           end = Sys.Date()
#         )
#       )
#     ),
#     br(),
#     # --- Row 1: standard SMC overlays (Swing/FVG/OB/BOS-CHoCH/Kill) --------
#     fluidRow(
#       column(
#         1,
#         switchInput(
#           ns("show_swing"),
#           "Swing Points",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("swing_child_ui"))
#       ),
#       column(
#         1,
#         switchInput(
#           ns("show_fvg"),
#           "FVG",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("fvg_child_ui"))
#       ),
#       column(
#         1,
#         switchInput(
#           ns("show_ob"),
#           "OB",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("ob_child_ui"))
#       ),
#       column(
#         1,
#         switchInput(
#           ns("show_bos_choch"),
#           "BOS CHoCH",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("bos_choch_child_ui"))
#       ),
#       column(
#         1,
#         div(
#           id = ns("kill_col"),
#           switchInput(
#             ns("show_kill"),
#             "Kill Zone",
#             value = FALSE,
#             width = "100%",
#             labelWidth = 60
#           ),
#           uiOutput(ns("kill_child_ui"))
#         )
#       ),
#       column(
#         1,
#         switchInput(
#           ns("show_rw_weekly"),
#           "RW Weekly",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("rw_weekly_child_ui"))
#       ),
#       column(
#         1,
#         switchInput(
#           ns("show_rw_daily"),
#           "RW Daily",
#           value = FALSE,
#           width = "100%",
#           labelWidth = 60
#         ),
#         uiOutput(ns("rw_daily_child_ui"))
#       )
#     ),
#     br(),
#     # --- Chart output ------------------------------------------------------
#     fluidRow(column(12, echarts4rOutput(ns("chart"), height = "600px")))
#   )
# }

mod_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Custom CSS to stop switch text from wrapping ---
    tags$style(HTML(
      "
      .bootstrap-switch-label {
        white-space: nowrap !important;
        text-overflow: ellipsis;
        overflow: hidden;
      }
    "
    )),

    # --- Top controls: ticker, timeframe, date range -----------------------
    fluidRow(
      column(3, selectInput(ns("asset_class"), "Asset Class", choices = NULL)),
      column(2, selectInput(ns("ticker"), "Ticker", choices = NULL)),
      column(
        2,
        selectInput(
          ns("timeframe"),
          "Timeframe",
          choices = all_timeframes,
          selected = "weekly"
        )
      ),
      column(
        3,
        dateRangeInput(
          ns("date_range"),
          "Date Range",
          start = Sys.Date() - 90,
          end = Sys.Date()
        )
      )
    ),
    br(),

    # --- Row 1: standard SMC overlays (Flexbox layout) --------------------
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 15px; align-items: flex-start; margin-bottom: 20px;",

      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_swing"),
          "Swing Points",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("swing_child_ui"))
      ),
      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_fvg"),
          "FVG",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("fvg_child_ui"))
      ),
      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_ob"),
          "OB",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("ob_child_ui"))
      ),
      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_bos_choch"),
          "BOS CHoCH",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("bos_choch_child_ui"))
      ),
      tags$div(
        style = "min-width: 120px;",
        div(
          id = ns("kill_col"),
          switchInput(
            ns("show_kill"),
            "Kill Zone",
            value = FALSE,
            width = "100%",
            labelWidth = 60
          ),
          uiOutput(ns("kill_child_ui"))
        )
      ),
      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_rw_weekly"),
          "RW Weekly",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("rw_weekly_child_ui"))
      ),
      tags$div(
        style = "min-width: 120px;",
        switchInput(
          ns("show_rw_daily"),
          "RW Daily",
          value = FALSE,
          width = "100%",
          labelWidth = 60
        ),
        uiOutput(ns("rw_daily_child_ui"))
      )
    ),
    br(),

    # --- Chart output ------------------------------------------------------
    fluidRow(column(12, echarts4rOutput(ns("chart"), height = "600px")))
  )
}


#' price_chart Server Functions
#' Chart Module Server
#'
#' Loads candles for the selected ticker/timeframe and renders an
#' interactive candlestick chart. Iteration 2: candles only — overlays
#' are added later.
#'
#' @param id Character string. Module namespace ID.
#' @param config A validated configuration list.
#'
#' @return None. Called for side effects.
#' @importFrom shiny moduleServer observeEvent reactive renderUI
#' @importFrom shiny showNotification updateSelectInput updateDateRangeInput
#' @importFrom shinyWidgets updateSwitchInput
#' @importFrom data.table as.data.table setDT setorderv rleid fifelse rbindlist
#' @importFrom DBI dbConnect dbGetQuery dbDisconnect dbIsValid
#' @importFrom RSQLite SQLite
#' @importFrom echarts4r e_charts e_candle e_scatter e_mark_area e_mark_line
#' @importFrom echarts4r e_title e_tooltip e_x_axis e_y_axis e_datazoom
#' @importFrom echarts4r renderEcharts4r e_grid e_legend
#' @importFrom htmlwidgets JS
#' @importFrom lubridate weeks
#' @importFrom shinyjs toggle show hide
#'
#' @noRd
mod_chart_server <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- The doorman: only continue when ticker belongs to the asset class ---
    selection <- shiny::reactive({
      ac <- input$asset_class
      tk <- input$ticker
      tf <- input$timeframe
      shiny::req(ac, tk, tf)
      shiny::req(tk %in% config$asset_classes[[ac]]$tickers)
      shiny::req(tf %in% timeframes_for(ac, config))
      list(asset_class = ac, ticker = tk, timeframe = tf)
    })

    # --- The notebook: last ticker used in each asset class ---
    last_ticker <- shiny::reactiveValues()

    # --- Candles from forexdatasets ---
    candles_raw <- shiny::reactive({
      sel <- selection()
      # shiny::req(input$timeframe)

      tryCatch(
        forexdatasets::get_dataset(
          timeframe = sel$timeframe,
          ticker = sel$ticker,
          asset_class = sel$asset_class,
          config = config
        ),
        error = function(e) {
          logger::log_error(
            "get_dataset failed: {conditionMessage(e)}"
          )
          shiny::showNotification(
            "Could not load candles for this selection",
            type = "error"
          )
          data.table::data.table()
        }
      )
    })

    # --- Build time_chr (echarts category axis label) ---
    chart_data_full <- shiny::reactive({
      dt <- data.table::copy(candles_raw())
      if (nrow(dt) == 0L) {
        return(dt)
      }
      data.table::set(
        dt,
        j = "time_chr",
        value = format(dt$date_time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
      )
      dt
    })

    # ---- Populate asset class dropdown from config ---
    shiny::observe({
      ac <- config$asset_classes
      shiny::req(ac)

      class_names <- setdiff(names(ac), "names")
      shiny::updateSelectInput(
        session,
        "asset_class",
        choices = sort(class_names),
        selected = if (length(class_names) > 0L) sort(class_names)[1] else NULL
      )
    })

    # ---- When asset class changes: pick the remembered ticker, or the first ---
    shiny::observeEvent(
      input$asset_class,
      {
        ac <- input$asset_class
        tickers <- sort(config$asset_classes[[ac]]$tickers)

        tfs <- timeframes_for(ac, config)
        current <- input$timeframe
        shiny::updateSelectInput(
          session,
          "timeframe",
          choices = tfs,
          selected = if (!is.null(current) && current %in% tfs) {
            current
          } else {
            "weekly"
          }
        )

        if (length(tickers) == 0L) {
          shiny::updateSelectInput(
            session,
            "ticker",
            choices = character(0),
            selected = NULL
          )
          return()
        }

        remembered <- last_ticker[[ac]]
        pick <- if (!is.null(remembered) && remembered %in% tickers) {
          remembered
        } else {
          tickers[1]
        }

        shiny::updateSelectInput(
          session,
          "ticker",
          choices = tickers,
          selected = pick
        )
      },
      ignoreInit = FALSE
    )

    # ---- Write in the notebook (only if the ticker belongs to this class) ---
    shiny::observeEvent(input$ticker, {
      ac <- input$asset_class
      if (
        !is.null(ac) && input$ticker %in% config$asset_classes[[ac]]$tickers
      ) {
        last_ticker[[ac]] <- input$ticker
      }
    })

    # --- One place that decides the default window ---
    default_window <- shiny::reactive({
      sel <- selection()
      dt <- chart_data_full()
      shiny::req(nrow(dt) > 0L)
      max_date <- as.Date(max(dt$date_time, na.rm = TRUE))
      list(
        start = max_date - default_days_back(sel$timeframe),
        end = max_date
      )
    })

    # --- Keep the date boxes in step with the chart's default window --
    shiny::observeEvent(default_window(), {
      dw <- default_window()
      shiny::updateDateRangeInput(
        session,
        "date_range",
        start = dw$start,
        end = dw$end
      )
    })

    # --- User picks a date range: only slide the zoom, no repaint ---
    shiny::observeEvent(
      input$date_range,
      {
        cd <- chart_data_full()
        shiny::req(nrow(cd) > 0L)

        rng <- as.Date(input$date_range)
        dw <- default_window()
        if (identical(rng, c(dw$start, dw$end))) {
          return() # same as what the chart already shows
        }

        zr <- zoom_pct(cd$date_time, rng[1], rng[2])

        # NEW: log line, to see if this observer runs
        logger::log_info(
          "date range zoom: candles {zr$start_idx} to {zr$end_idx}"
        )

        # CHANGED: ns("chart") instead of "chart"
        echarts4r::echarts4rProxy(ns("chart")) |>
          echarts4r::e_dispatch_action_p(
            "dataZoom",
            startValue = zr$start_idx,
            endValue = zr$end_idx
          )
      },
      ignoreInit = TRUE
    )

    y_decimals <- shiny::reactive({
      sel <- selection()
      price_decimals(sel$ticker, config)
    })

    # --- Render Chart ---
    output$chart <- renderEcharts4r({
      # shiny::req(input$timeframe)

      # Get everything that can "wait" BEFORE the tryCatch (the Fix 2 rule)
      sel <- selection()
      cd <- chart_data_full()
      if (nrow(cd) == 0) {
        showNotification("No data for selection", type = "warning")
        return(NULL)
      }
      dw <- default_window()
      zr <- zoom_pct(cd$date_time, dw$start, dw$end)
      dec <- y_decimals()

      tryCatch(
        {
          opts <- build_candle_option(
            dt = cd,
            title = paste0(toupper(sel$ticker), " _ ", toupper(sel$timeframe)),
            decimals = dec,
            zoom = zr,
            show_time = sel$timeframe %in% c("h4", "h1")
          )
          echarts4r::e_charts() |> echarts4r::e_list(opts)
        },
        error = function(e) {
          logger::log_error("Chart render failed: {conditionMessage(e)}")
          showNotification("Error rendering chart", type = "error")
          NULL
        }
      )
    })
  })
}

## To be copied in the UI
# mod_chart_ui("chart_1")

## To be copied in the server
# mod_chart_server("chart_1")
