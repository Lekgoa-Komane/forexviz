#' Width (pixels) of the label part of every overlay switch.
#' @noRd
overlay_label_width <- 60

#' One overlay switch, with its sub-options underneath
#'
#' @param ns The module's namespace function.
#' @param id Input id of the switch, such as "show_swing".
#' @param label Text on the switch.
#' @param child_ui_id Id of the output that holds the sub-options.
#' @param wrapper_id Optional id for the wrapper (shinyjs can hide it).
#' @noRd
overlay_switch <- function(ns, id, label, child_ui_id, wrapper_id = NULL) {
  div(
    id = wrapper_id,
    class = "overlay-item",
    switchInput(
      inputId = ns(id),
      label = label,
      value = FALSE,
      width = "100%",
      labelWidth = overlay_label_width
    ),
    div(class = "overlay-children", uiOutput(ns(child_ui_id))),
    div(class = "overlay-note", uiOutput(ns("overlay_note")))
  )
}

#' The Bull and Bear child switches of an overlay (both on by default)
#'
#' @param ns The module's namespace function.
#' @param prefix "fvg" or "ob".
#' @noRd
bull_bear_switches <- function(ns, prefix) {
  tagList(
    switchInput(
      ns(paste0(prefix, "_bull")),
      "Bull",
      value = TRUE,
      inline = TRUE
    ),
    switchInput(
      ns(paste0(prefix, "_bear")),
      "Bear",
      value = TRUE,
      inline = TRUE
    )
  )
}

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
mod_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Style for the overlay switches (only touches this row)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    tags$style(HTML(
      "
      .overlay-switches {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        align-items: flex-start;
        margin-bottom: 10px;
      }
      .overlay-item { min-width: 120px; }
      .overlay-item .bootstrap-switch-label {
        white-space: nowrap !important;
        text-overflow: ellipsis;
        overflow: hidden;
      }
      /* Children sit under their switch and never make it wider */
      .overlay-children {
        width: 0;
        min-width: 100%;
        margin-top: 4px;
      }
      /* One reserved line for the note, so the page does not jump */
      .overlay-note {
        min-height: 22px;
        font-size: 12px;
        opacity: 0.7;
        margin-bottom: 4px;
      }
    "
    )),

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Top controls: ticker, timeframe, date range
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Overlay switches: one row, same size, wraps when narrow
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    div(
      class = "overlay-switches",
      overlay_switch(ns, "show_swing", "Swing Points", "swing_child_ui"),
      overlay_switch(ns, "show_fvg", "FVG", "fvg_child_ui"),
      overlay_switch(ns, "show_ob", "OB", "ob_child_ui"),
      overlay_switch(ns, "show_bos_choch", "BOS CHoCH", "bos_choch_child_ui"),
      overlay_switch(
        ns,
        "show_kill",
        "Kill Zone",
        "kill_child_ui",
        wrapper_id = ns("kill_col")
      ),
      overlay_switch(ns, "show_rw_weekly", "RW Weekly", "rw_weekly_child_ui"),
      overlay_switch(ns, "show_rw_daily", "RW Daily", "rw_daily_child_ui")
    ),
    br(),

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Chart output
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # The doorman: only continue when ticker belongs to the asset class
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    selection <- shiny::reactive({
      ac <- input$asset_class
      tk <- input$ticker
      tf <- input$timeframe
      shiny::req(ac, tk, tf)
      shiny::req(tk %in% config$asset_classes[[ac]]$tickers)
      shiny::req(tf %in% timeframes_for(ac, config))
      list(asset_class = ac, ticker = tk, timeframe = tf)
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # The notebook: last ticker used in each asset class
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    last_ticker <- shiny::reactiveValues()

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Candles from forexdatasets
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    candles_raw <- shiny::reactive({
      sel <- selection()

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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Build time_chr (echarts category axis label)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Populate asset class dropdown from config
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # When asset class changes: pick the remembered ticker, or the first
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Write in the notebook (only if the ticker belongs to this class)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    shiny::observeEvent(input$ticker, {
      ac <- input$asset_class
      if (
        !is.null(ac) && input$ticker %in% config$asset_classes[[ac]]$tickers
      ) {
        last_ticker[[ac]] <- input$ticker
      }
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # One place that decides the default window
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Keep the date boxes in step with the chart's default window
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    shiny::observeEvent(default_window(), {
      dw <- default_window()
      shiny::updateDateRangeInput(
        session,
        "date_range",
        start = dw$start,
        end = dw$end
      )
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # User picks a date range: only slide the zoom, no repaint
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Registry rows for the chosen ticker and timeframe
    # Only read when an overlay needs it (a reactive that nobody asks for
    # is never run).
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    registry_selected <- shiny::reactive({
      sel <- selection() # can "wait", so it stays outside the tryCatch
      tryCatch(
        forexengine::get_result(
          table = "registry",
          timeframe = sel$timeframe,
          ticker = sel$ticker,
          asset_class = sel$asset_class,
          config = config
        ),
        error = function(e) {
          logger::log_error("Failed to load registry: {conditionMessage(e)}")
          shiny::showNotification("Error loading registry data", type = "error")
          data.table::data.table()
        }
      )
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Swing point markers (NULL when swing points are off)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    swing_series <- shiny::reactive({
      show_high <- isTRUE(input$show_swing) && isTRUE(input$swing_high)
      show_low <- isTRUE(input$show_swing) && isTRUE(input$swing_low)
      sel <- selection()
      cd <- chart_data_full()
      shiny::req(nrow(cd) > 0L)

      # Read the registry only when it is needed, and only where it exists
      wanted <- (show_high || show_low) &&
        sel$asset_class %in% overlay_asset_classes
      registry <- if (wanted) registry_selected() else NULL

      build_swing_series(
        registry = registry,
        last_time = max(cd$date_time, na.rm = TRUE),
        decimals = y_decimals(),
        show_high = show_high,
        show_low = show_low
      )
    })

    # shiny::observeEvent(
    #   swing_series(),
    #   {
    #     session$sendCustomMessage(
    #       "forexviz_set_overlay",
    #       list(id = ns("chart"), series = unname(swing_series()))
    #     )
    #   },
    #   ignoreInit = TRUE
    # )

    shiny::observeEvent(
      overlay_series(),
      {
        session$sendCustomMessage(
          "forexviz_set_overlay",
          list(id = ns("chart"), series = unname(overlay_series()))
        )
      },
      ignoreInit = TRUE
    )

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Child switches for swing points
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    output$swing_child_ui <- shiny::renderUI({
      shiny::req(isTRUE(input$show_swing))
      tagList(
        shinyWidgets::switchInput(
          ns("swing_high"),
          "High",
          value = TRUE,
          inline = TRUE
        ),
        shinyWidgets::switchInput(
          ns("swing_low"),
          "Low",
          value = TRUE,
          inline = TRUE
        )
      )
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Grey out the overlay switches where there is no overlay data
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    shiny::observeEvent(input$asset_class, {
      available <- input$asset_class %in% overlay_asset_classes
      for (id in overlay_master_ids) {
        if (!available) {
          shinyWidgets::updateSwitchInput(session, id, value = FALSE)
        }
        shinyWidgets::updateSwitchInput(session, id, disabled = !available)
      }
    })

    output$overlay_note <- shiny::renderUI({
      shiny::req(input$asset_class)
      if (!input$asset_class %in% overlay_asset_classes) {
        "No overlay data for this asset class"
      }
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Child switches for FVG & OB
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    output$fvg_child_ui <- shiny::renderUI({
      shiny::req(isTRUE(input$show_fvg))
      bull_bear_switches(ns, "fvg")
    })

    output$ob_child_ui <- shiny::renderUI({
      shiny::req(isTRUE(input$show_ob))
      bull_bear_switches(ns, "ob")
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Exclusive pairs: switching one on switches the other off
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    make_exclusive <- function(on, off) {
      shiny::observeEvent(
        input[[on]],
        {
          if (isTRUE(input[[on]])) {
            shinyWidgets::updateSwitchInput(session, off, value = FALSE)
          }
        },
        ignoreInit = TRUE
      )
    }
    make_exclusive("show_fvg", "show_ob")
    make_exclusive("show_ob", "show_fvg")

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>-
    # FVG and OB zone rectangles (always four series, empty when off)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    zone_series <- shiny::reactive({
      sel <- selection()
      cd <- chart_data_full()
      shiny::req(nrow(cd) > 0L)

      fvg_on <- isTRUE(input$show_fvg)
      ob_on <- isTRUE(input$show_ob)
      show_fvg_bull <- fvg_on && isTRUE(input$fvg_bull)
      show_fvg_bear <- fvg_on && isTRUE(input$fvg_bear)
      show_ob_bull <- ob_on && isTRUE(input$ob_bull)
      show_ob_bear <- ob_on && isTRUE(input$ob_bear)

      # Read the registry only when needed, and only where it exists
      wanted <- (show_fvg_bull ||
        show_fvg_bear ||
        show_ob_bull ||
        show_ob_bear) &&
        sel$asset_class %in% overlay_asset_classes
      registry <- if (wanted) registry_selected() else NULL

      c(
        build_zone_series(
          registry,
          cd$date_time,
          "fvg",
          show_bull = show_fvg_bull,
          show_bear = show_fvg_bear
        ),
        build_zone_series(
          registry,
          cd$date_time,
          "ob",
          show_bull = show_ob_bull,
          show_bear = show_ob_bear
        )
      )
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Everything drawn on top of the candles, in one list
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    overlay_series <- shiny::reactive({
      c(swing_series(), zone_series())
    })

    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # Render Chart
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    output$chart <- renderEcharts4r({
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
      # swings <- shiny::isolate(swing_series())
      overlays <- shiny::isolate(overlay_series())

      tryCatch(
        {
          opts <- build_candle_option(
            dt = cd,
            title = paste0(toupper(sel$ticker), " _ ", toupper(sel$timeframe)),
            decimals = dec,
            zoom = zr,
            show_time = sel$timeframe %in% c("h4", "h1"),
            # extra_series = swings
            extra_series = overlays
          )
          echarts4r::e_charts() |>
            echarts4r::e_list(opts) |>
            htmlwidgets::onRender(label_toggle_js())
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
