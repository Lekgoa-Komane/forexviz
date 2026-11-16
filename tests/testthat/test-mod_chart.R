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

test_that("the seven overlay master switches and the note are in the UI", {
  html <- as.character(mod_chart_ui("t"))
  expect_length(unique(overlay_master_ids), 7)
  for (id in overlay_master_ids) {
    expect_match(html, paste0("t-", id))
  }
  expect_match(html, "overlay-note")
  expect_match(html, "t-overlay_note")
})

test_that("the note appears only for asset classes without overlay data", {
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(asset_class = "Commodity_Futures")
    expect_match(
      paste(output$overlay_note$html, collapse = ""),
      "No overlay data"
    )

    session$setInputs(asset_class = "Currency_Pairs")
    expect_false(grepl(
      "No overlay data",
      paste(output$overlay_note$html, collapse = "")
    ))
  })
})

fake_full_registry <- function() {
  placeholder <- as.POSIXct("2027-02-05 23:00:00", tz = "UTC")
  data.table::data.table(
    type = c("swing_high", "bull_fvg", "bear_fvg", "bull_ob"),
    formed_at = as.POSIXct(
      c("2026-01-02", "2026-01-03", "2026-01-04", "2026-01-05"),
      tz = "UTC"
    ),
    critical_value = c(1.3, NA, NA, NA),
    invalidated_at = rep(placeholder, 4),
    type_zone_bot_value = c(NA, 1.10, 1.30, 1.00),
    type_zone_top_value = c(NA, 1.15, 1.35, 1.05),
    mit_zone_dist_at = rep(placeholder, 4)
  )
}

total_rectangles <- function(series) {
  sum(vapply(series, function(x) length(x$markArea$data), integer(1)))
}

test_that("bull_bear_switches builds a Bull and a Bear switch", {
  html <- as.character(bull_bear_switches(shiny::NS("t"), "fvg"))
  expect_match(html, "t-fvg_bull")
  expect_match(html, "t-fvg_bear")
})

test_that("zone_series follows the FVG and OB switches", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = function(...) fake_full_registry(),
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "eurusd",
      timeframe = "weekly"
    )

    session$setInputs(show_fvg = FALSE, show_ob = FALSE)
    expect_length(zone_series(), 4)
    expect_equal(total_rectangles(zone_series()), 0)

    session$setInputs(show_fvg = TRUE, fvg_bull = TRUE, fvg_bear = FALSE)
    s <- zone_series()
    expect_length(s$zones_bull_fvg$markArea$data, 1)
    expect_length(s$zones_bear_fvg$markArea$data, 0)
    expect_equal(total_rectangles(s), 1)

    session$setInputs(fvg_bear = TRUE)
    expect_equal(total_rectangles(zone_series()), 2)

    session$setInputs(
      show_fvg = FALSE,
      show_ob = TRUE,
      ob_bull = TRUE,
      ob_bear = TRUE
    )
    s <- zone_series()
    expect_length(s$zones_bull_ob$markArea$data, 1)
    expect_equal(total_rectangles(s), 1) # no bear OB in the registry
  })
})

test_that("overlay_series holds the four swing and the four zone series", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = function(...) fake_full_registry(),
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "eurusd",
      timeframe = "weekly"
    )
    expect_named(
      overlay_series(),
      c(
        "swing_high",
        "swing_high_broken",
        "swing_low",
        "swing_low_broken",
        "zones_bull_fvg",
        "zones_bear_fvg",
        "zones_bull_ob",
        "zones_bear_ob",
        "lines_bos_bull",
        "lines_bos_bear",
        "lines_choch_bull",
        "lines_choch_bear"
      )
    )
  })
})

test_that("no zones, and no registry read, where there is no overlay data", {
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
      show_fvg = TRUE,
      fvg_bull = TRUE,
      fvg_bear = TRUE,
      show_ob = FALSE
    )
    expect_equal(total_rectangles(zone_series()), 0)
  })
})

# BOS/CHoCH
fake_module_bos_choch <- function() {
  data.table::data.table(
    date_time = fake_candles()$date_time,
    weekly_bos_type = c(NA, NA, NA, "bull_bos", rep(NA, 6)),
    weekly_choch_type = c(rep(NA, 6), "bear_choch", rep(NA, 3)),
    weekly_choch_level = c(rep(NA_real_, 6), 1.0, rep(NA_real_, 3)),
    weekly_prev_sh = c(NA, NA, NA, 1.3, rep(NA_real_, 6)),
    weekly_prev_sl = rep(NA_real_, 10)
  )
}

fake_lines_registry <- function() {
  data.table::data.table(
    type = c("swing_high", "swing_low"),
    formed_at = as.POSIXct(c("2026-01-02", "2026-01-03"), tz = "UTC"),
    critical_value = c(1.3, 1.0)
  )
}

total_marklines <- function(series) {
  sum(vapply(series, function(x) length(x$markLine$data), integer(1)))
}

# a stand-in for forexengine::get_result() that knows both tables
fake_get_result <- function(table, ...) {
  if (table == "bos_choch") fake_module_bos_choch() else fake_lines_registry()
}

test_that("bos_choch_switches builds a BOS and a CHoCH switch", {
  html <- as.character(bos_choch_switches(shiny::NS("t")))
  expect_match(html, "t-bos_lines")
  expect_match(html, "t-choch_lines")
})

test_that("bos_choch_series follows the BOS and CHoCH switches", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = fake_get_result,
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Currency_Pairs",
      ticker = "eurusd",
      timeframe = "weekly"
    )

    session$setInputs(show_bos_choch = FALSE)
    expect_length(bos_choch_series(), 4)
    expect_equal(total_marklines(bos_choch_series()), 0)

    session$setInputs(
      show_bos_choch = TRUE,
      bos_lines = TRUE,
      choch_lines = FALSE
    )
    s <- bos_choch_series()
    expect_length(s$lines_bos_bull$markLine$data, 1)
    expect_equal(total_marklines(s), 1)

    session$setInputs(bos_lines = FALSE, choch_lines = TRUE)
    s <- bos_choch_series()
    expect_length(s$lines_choch_bear$markLine$data, 1)
    expect_equal(total_marklines(s), 1)
  })
})

test_that("no lines, and no table read, where there is no overlay data", {
  testthat::local_mocked_bindings(
    get_dataset = function(...) fake_candles(),
    .package = "forexdatasets"
  )
  testthat::local_mocked_bindings(
    get_result = function(...) stop("tables must not be read"),
    .package = "forexengine"
  )
  testServer(mod_chart_server, args = list(config = make_valid_config()), {
    session$setInputs(
      asset_class = "Commodity_Futures",
      ticker = "GC.F",
      timeframe = "weekly",
      show_bos_choch = TRUE,
      bos_lines = TRUE,
      choch_lines = TRUE
    )
    expect_equal(total_marklines(bos_choch_series()), 0)
  })
})
