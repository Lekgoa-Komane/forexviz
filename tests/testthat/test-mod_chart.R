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
