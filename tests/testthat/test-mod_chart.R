# testServer(
#   mod_price_chart_server,
#   # Add here your module params
#   args = list(),
#   {
#     ns <- session$ns
#     expect_true(
#       inherits(ns, "function")
#     )
#     expect_true(
#       grepl(id, ns(""))
#     )
#     expect_true(
#       grepl("test", ns("test"))
#     )
#     # Here are some examples of tests you can
#     # run on your module
#     # - Testing the setting of inputs
#     # session$setInputs(x = 1)
#     # expect_true(input$x == 1)
#     # - If ever your input updates a reactiveValues
#     # - Note that this reactiveValues must be passed
#     # - to the testServer function via args = list()
#     # expect_true(r$x == 1)
#     # - Testing output
#     # expect_true(inherits(output$tbl$html, "html"))
#   }
# )
#
# test_that("module ui works", {
#   ui <- mod_price_chart_ui(id = "test")
#   golem::expect_shinytaglist(ui)
#   # Check that formals have not been removed
#   fmls <- formals(mod_price_chart_ui)
#   for (i in c("id")) {
#     expect_true(i %in% names(fmls))
#   }
# })

# Smoke test: the module starts with a valid config and namespaces work
testServer(
  mod_chart_server,
  args = list(config = make_valid_config()),
  {
    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))
  }
)

test_that("module ui works", {
  ui <- mod_chart_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_chart_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

test_that("mod_chart_ui builds the controls and the chart", {
  html <- as.character(mod_chart_ui("test"))
  expect_match(html, "test-asset_class")
  expect_match(html, "test-ticker")
  expect_match(html, "test-timeframe")
  expect_match(html, "test-chart")
})

# ---- The doorman: selection() ---------------------------------------------

test_that("selection lets a matching asset class, ticker and timeframe through", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "eurusd",
      timeframe = "h1"
    )
    expect_equal(
      selection(),
      list(asset_class = "Currency_Pairs", ticker = "eurusd", timeframe = "h1")
    )
  })
})

test_that("selection waits quietly when the ticker is from another asset class", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "GC.F",
      timeframe = "daily"
    )
    expect_error(selection(), class = "shiny.silent.error")
  })
})

test_that("selection waits quietly when the timeframe is not offered", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    # Commodity_Futures has daily data only, so h1 must wait
    session$setInputs(
      asset_class = "Commodity_Futures",
      ticker = "GC.F",
      timeframe = "h1"
    )
    expect_error(selection(), class = "shiny.silent.error")

    # the same pair with a valid timeframe goes through
    session$setInputs(timeframe = "weekly")
    expect_equal(selection()$timeframe, "weekly")
  })
})

test_that("selection waits quietly while nothing is chosen yet", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    expect_error(selection(), class = "shiny.silent.error")
  })
})

# ---- The notebook: last ticker used in each asset class --------------------

test_that("the last ticker used in an asset class is remembered", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(asset_class = "Currency_Pairs", ticker = "gbpjpy")
    expect_equal(last_ticker[["Currency_Pairs"]], "gbpjpy")
  })
})

test_that("a ticker from another asset class is not written in the notebook", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    # "gbpjpy" is not a Commodity_Futures ticker, so it must be ignored
    session$setInputs(asset_class = "Commodity_Futures", ticker = "gbpjpy")
    expect_null(last_ticker[["Commodity_Futures"]])
  })
})

test_that("overlay_switch keeps the ids the server and shinyjs rely on", {
  ns <- shiny::NS("t")
  html <- as.character(
    overlay_switch(
      ns,
      "show_kill",
      "Kill Zone",
      "kill_child_ui",
      wrapper_id = ns("kill_col")
    )
  )
  expect_match(html, "t-show_kill")
  expect_match(html, "t-kill_child_ui")
  expect_match(html, "t-kill_col")
  expect_match(html, "overlay-item")
})

test_that("mod_chart_ui has all seven overlay switches in one row", {
  html <- as.character(mod_chart_ui("t"))
  expect_match(html, "overlay-switches")
  for (id in c(
    "show_swing",
    "show_fvg",
    "show_ob",
    "show_bos_choch",
    "show_kill",
    "show_rw_weekly",
    "show_rw_daily"
  )) {
    expect_match(html, paste0("t-", id))
  }
})

test_that("child switches are wrapped so they cannot widen their master", {
  ns <- shiny::NS("t")
  html <- as.character(overlay_switch(
    ns,
    "show_swing",
    "Swing Points",
    "swing_child_ui"
  ))
  expect_match(html, "overlay-children")
  expect_match(
    as.character(mod_chart_ui("t")),
    ".overlay-children",
    fixed = TRUE
  )
})

fake_candles <- function() {
  data.table::data.table(
    asset_class = "Currency_Pairs",
    ticker = "eurusd",
    date_time = as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9,
    open = 1.1,
    high = 1.3,
    low = 1.0,
    close = 1.2
  )
}

fake_module_registry <- function() {
  data.table::data.table(
    type = c("swing_high", "swing_low", "swing_high"),
    formed_at = as.POSIXct(
      c("2026-01-02", "2026-01-03", "2026-01-04"),
      tz = "UTC"
    ),
    critical_value = c(1.3, 1.0, 1.3),
    invalidated_at = as.POSIXct(
      c("2026-01-06", "2027-02-05 23:00:00", "2027-02-05 23:00:00"),
      tz = "UTC"
    )
  )
}

total_points <- function(series) {
  sum(vapply(series, function(x) length(x$data), integer(1)))
}

test_that("swing_series follows the switches", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = function(...) fake_module_registry(),
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "eurusd",
      timeframe = "weekly"
    )

    session$setInputs(show_swing = FALSE)
    expect_length(swing_series(), 4)
    expect_equal(total_points(swing_series()), 0)

    session$setInputs(show_swing = TRUE, swing_high = TRUE, swing_low = FALSE)
    s <- swing_series()
    expect_equal(
      length(s$swing_high$data) + length(s$swing_high_broken$data),
      2
    )
    expect_equal(total_points(s), 2) # the low is switched off

    session$setInputs(swing_low = TRUE)
    expect_equal(total_points(swing_series()), 3)
  })
})

test_that("an asset class without overlay data never reads the registry", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = function(...) stop("registry must not be read"),
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Commodity_Futures",
      ticker = "GC.F",
      timeframe = "weekly",
      show_swing = TRUE,
      swing_high = TRUE,
      swing_low = TRUE
    )
    expect_equal(total_points(swing_series()), 0)
  })
})
