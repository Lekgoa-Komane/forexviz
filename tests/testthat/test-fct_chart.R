test_that("default_days_back gives the right window per timeframe", {
  expect_equal(default_days_back("weekly"), 365 * 3)
  expect_equal(default_days_back("daily"), 34 * 7)
  expect_equal(default_days_back("h4"), 4 * 7)
  expect_equal(default_days_back("h1"), 7)
  expect_equal(default_days_back("something_else"), 7)
})

test_that("zoom_pct turns a date window into percentages and positions", {
  dt <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9 # 10 daily candles

  z <- zoom_pct(dt, as.Date("2026-01-04"), as.Date("2026-01-06"))
  expect_equal(z$start_idx, 3L)
  expect_equal(z$end_idx, 5L)
  expect_equal(z$start_pct, 30)
  expect_equal(z$end_pct, 60)

  # the last day is included in full
  z_end <- zoom_pct(dt, as.Date("2026-01-08"), as.Date("2026-01-10"))
  expect_equal(z_end$end_idx, 9L)
  expect_equal(z_end$end_pct, 100)
})

test_that("zoom_pct shows everything when the window has no candles", {
  dt <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9
  z <- zoom_pct(dt, as.Date("2030-01-01"), as.Date("2030-02-01"))
  expect_equal(z$start_pct, 0)
  expect_equal(z$end_pct, 100)
  expect_equal(z$start_idx, 0L)
  expect_equal(z$end_idx, 9L)
})

test_that("timeframes_for reads the hourly rule from the config", {
  cfg <- make_valid_config()

  # classes that come with the helper config
  expect_equal(timeframes_for("Currency_Pairs", cfg), all_timeframes)
  expect_equal(timeframes_for("Commodity_Futures", cfg), c("weekly", "daily"))
  expect_equal(timeframes_for("Not_An_Asset_Class", cfg), c("weekly", "daily"))

  # add the other real asset classes on top of the helper config
  cfg$asset_classes$Metals <- list(
    frequencies = c("daily", "hourly"),
    tickers = "xauusd"
  )
  cfg$asset_classes$Currency_Indices <- list(
    frequencies = c("daily", "hourly"),
    tickers = "usd_i"
  )
  cfg$asset_classes$Yields <- list(
    frequencies = "daily",
    tickers = "10yusy.b"
  )
  expect_equal(timeframes_for("Metals", cfg), all_timeframes)
  expect_equal(timeframes_for("Currency_Indices", cfg), all_timeframes)
  expect_equal(timeframes_for("Yields", cfg), c("weekly", "daily"))
})

test_that("price_decimals follows the pip_factor rule in the config", {
  cfg <- make_valid_config()
  expect_equal(price_decimals("eurusd", cfg), 5L)
  expect_equal(price_decimals("usdjpy", cfg), 3L)
  expect_equal(price_decimals("USDJPY", cfg), 3L) # upper case works too
  expect_equal(price_decimals("xauusd", cfg), 3L)
  expect_equal(price_decimals("usd_i", cfg), 3L)
  expect_equal(price_decimals("10yusy.b", cfg), 3L)
  # commodity futures, lower case as in config.yml and upper case as in helper
  expect_equal(price_decimals("gc.f", cfg), 3L)
  expect_equal(price_decimals("cb.f", cfg), 3L)
  expect_equal(price_decimals("GC.F", cfg), 3L)
})

test_that("build_candle_option packs the candles as one block of numbers", {
  dt <- data.table::data.table(
    time_chr = c("2026-01-01 00:00:00", "2026-01-02 00:00:00"),
    open = c(1.1, 1.2),
    close = c(1.2, 1.1),
    low = c(1.0, 1.0),
    high = c(1.3, 1.3)
  )
  zoom <- list(start_pct = 20, end_pct = 80)
  opt <- build_candle_option(dt, "EURUSD _ DAILY", 5L, zoom)

  expect_equal(opt$title$text, "EURUSD _ DAILY")
  expect_equal(opt$xAxis$data, dt$time_chr)

  s <- opt$series[[1]]
  expect_equal(s$type, "candlestick")
  # each row is: open, close, low, high
  expect_equal(
    s$data,
    rbind(c(1.1, 1.2, 1.0, 1.3), c(1.2, 1.1, 1.0, 1.3))
  )
})

test_that("build_candle_option uses green for up and red for down", {
  dt <- data.table::data.table(
    time_chr = "2026-01-01 00:00:00",
    open = 1.1,
    close = 1.2,
    low = 1.0,
    high = 1.3
  )
  opt <- build_candle_option(dt, "T", 5L, list(start_pct = 0, end_pct = 100))
  style <- opt$series[[1]]$itemStyle
  expect_equal(style$color, chart_colors$up)
  expect_equal(style$color0, chart_colors$down)
  expect_false(identical(chart_colors$up, chart_colors$down))
})

test_that("build_candle_option passes the zoom and the decimals through", {
  dt <- data.table::data.table(
    time_chr = "2026-01-01 00:00:00",
    open = 1.1,
    close = 1.2,
    low = 1.0,
    high = 1.3
  )
  opt <- build_candle_option(dt, "T", 3L, list(start_pct = 20, end_pct = 80))
  expect_equal(opt$dataZoom[[1]]$start, 20)
  expect_equal(opt$dataZoom[[1]]$end, 80)
  expect_equal(opt$dataZoom[[2]]$start, 20)
  expect_match(as.character(opt$yAxis$axisLabel$formatter), "toFixed\\(3\\)")
})

# --- horizontal data for point ---
one_candle <- function() {
  data.table::data.table(
    time_chr = "2026-01-01 00:00:00",
    open = 1.1,
    close = 1.2,
    low = 1.0,
    high = 1.3
  )
}
full_zoom <- list(start_pct = 0, end_pct = 100)

test_that("the hover readout is pinned in its place, see-through and coloured", {
  tip <- build_candle_option(one_candle(), "T", 5L, full_zoom)$tooltip
  expect_equal(
    tip$position,
    c(chart_layout$readout_x, chart_layout$readout_y)
  )
  expect_equal(tip$backgroundColor, "transparent")
  expect_equal(tip$borderWidth, 0)
  expect_gt(tip$textStyle$fontSize, 12) # bigger than the axis numbers

  js <- as.character(tip$formatter)
  for (colour in c(
    chart_colors$up,
    chart_colors$down,
    chart_colors$date,
    chart_colors$ohlc
  )) {
    expect_match(js, colour, fixed = TRUE)
  }
  expect_match(js, "toFixed(5)", fixed = TRUE)
  expect_false(grepl("__", js, fixed = TRUE)) # no placeholder left over
})

test_that("date and OHLC get colours that differ from each other", {
  expect_false(identical(chart_colors$date, chart_colors$ohlc))
  expect_false(identical(chart_colors$ohlc, chart_colors$up))
  expect_false(identical(chart_colors$ohlc, chart_colors$down))
})

test_that("the readout shows the time only when asked", {
  js_date <- as.character(
    build_candle_option(one_candle(), "T", 5L, full_zoom)$tooltip$formatter
  )
  js_time <- as.character(
    build_candle_option(
      one_candle(),
      "T",
      5L,
      full_zoom,
      show_time = TRUE
    )$tooltip$formatter
  )
  expect_match(js_date, "substring(0, 10)", fixed = TRUE)
  expect_match(js_time, "substring(0, 16)", fixed = TRUE)
})

test_that("title is centred and sits above the readout, above the chart", {
  opt <- build_candle_option(one_candle(), "T", 5L, full_zoom)
  expect_equal(opt$title$left, "center")
  expect_true(opt$title$top < opt$tooltip$position[2])
  expect_true(opt$tooltip$position[2] < opt$grid$top)
  expect_equal(opt$grid$left, chart_layout$grid_left)
})

test_that("the hover readout hides when the mouse leaves the chart", {
  tip <- build_candle_option(one_candle(), "T", 5L, full_zoom)$tooltip
  expect_false(isTRUE(tip$alwaysShowContent))
})

test_that("extra series are added after the candles", {
  extra <- list(
    a = list(name = "a", type = "scatter"),
    b = list(name = "b", type = "scatter")
  )
  opt <- build_candle_option(
    one_candle(),
    "T",
    5L,
    full_zoom,
    extra_series = extra
  )
  expect_length(opt$series, 3)
  expect_equal(opt$series[[1]]$type, "candlestick")
  expect_equal(opt$series[[2]]$name, "a")
  expect_equal(opt$series[[3]]$name, "b")

  # without extras the chart is as before
  expect_length(
    build_candle_option(one_candle(), "T", 5L, full_zoom)$series,
    1
  )
})

# test_that("shaded zones never stretch the price axis", {
#   opt <- build_candle_option(one_candle(), "T", 5L, full_zoom)
#   expect_false(opt$yAxis$includeMarkerInExtent)
# })
