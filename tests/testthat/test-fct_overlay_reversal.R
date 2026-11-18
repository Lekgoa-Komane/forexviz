hours <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC") + 3600 * 0:9 # 00:00 .. 09:00
ts <- function(x) as.POSIXct(x, tz = "UTC")

# six setups: 1 low zone, 2 high zone, 3 low naked, 4 off-chart, 5 low zone
# with no aggressive close, 6 unknown swing type
fake_rw <- function() {
  data.table::data.table(
    swing_type = c("low", "high", "low", "high", "low", NA),
    setup_type = c(
      "zone_swing",
      "zone_swing",
      "naked_swing",
      "zone_swing",
      "zone_swing",
      "zone_swing"
    ),
    swing_time = ts(c(
      "2026-01-01 00:00:00",
      "2026-01-01 02:00:00",
      "2026-01-01 05:00:00",
      "2026-02-01 00:00:00",
      "2026-01-01 07:00:00",
      "2026-01-01 01:00:00"
    )),
    swing_price = c(1.00, 1.30, 0.95, 1.31, 0.90, 1.10),
    aggressive_close_time = ts(c(
      "2026-01-01 01:00:00",
      "2026-01-01 04:00:00",
      "2026-01-01 06:00:00",
      "2026-02-01 01:00:00",
      NA,
      "2026-01-01 02:00:00"
    )),
    aggressive_close_price = c(1.02, 1.27, 0.97, 1.25, NA, 1.10),
    h1_choch_time = ts(c(
      "2026-01-01 03:00:00",
      "2026-01-01 06:00:00",
      "2026-01-01 08:00:00",
      "2026-02-01 02:00:00",
      "2026-01-01 09:00:00",
      "2026-01-01 03:00:00"
    )),
    h1_choch_price = c(1.05, 1.22, 0.99, 1.20, 0.93, 1.10)
  )
}

n_points <- function(s, name) length(s[[name]]$data)

test_that("six named scatter series come back per source", {
  s <- build_reversal_series(fake_rw(), hours, "weekly")
  expect_named(
    s,
    c(
      "rw_weekly_swing_bull",
      "rw_weekly_swing_bear",
      "rw_weekly_aggr_bull",
      "rw_weekly_aggr_bear",
      "rw_weekly_choch_bull",
      "rw_weekly_choch_bear"
    )
  )
  for (one in s) {
    expect_equal(one$type, "scatter")
  }
  d <- build_reversal_series(fake_rw(), hours, "daily")
  expect_true(all(startsWith(names(d), "rw_daily_")))
})

test_that("shapes, sizes and rotation follow the old design", {
  s <- build_reversal_series(fake_rw(), hours, "weekly")
  expect_equal(s$rw_weekly_swing_bull$symbol, "arrow")
  expect_equal(s$rw_weekly_swing_bull$symbolSize, chart_layout$rw_swing_size)
  expect_equal(s$rw_weekly_swing_bull$symbolRotate, 0)
  expect_equal(s$rw_weekly_swing_bear$symbolRotate, 180)
  expect_equal(s$rw_weekly_aggr_bull$symbol, "circle")
  expect_equal(s$rw_weekly_aggr_bull$symbolSize, chart_layout$rw_aggr_size)
  expect_equal(s$rw_weekly_choch_bear$symbol, "diamond")
  expect_equal(s$rw_weekly_choch_bear$symbolSize, chart_layout$rw_choch_size)
})

test_that("bullish is green, bearish is red, weekly hollow, daily filled", {
  w <- build_reversal_series(fake_rw(), hours, "weekly")
  expect_equal(w$rw_weekly_swing_bull$itemStyle$borderColor, "#00CC00")
  expect_equal(w$rw_weekly_swing_bear$itemStyle$borderColor, "#FF3333")
  expect_equal(w$rw_weekly_aggr_bull$itemStyle$color, "rgba(0,0,0,0)") # hollow
  expect_equal(w$rw_weekly_aggr_bull$itemStyle$borderWidth, 2)

  d <- build_reversal_series(fake_rw(), hours, "daily")
  expect_equal(d$rw_daily_aggr_bull$itemStyle$color, "#00CC00") # filled
  expect_equal(d$rw_daily_choch_bear$itemStyle$color, "#FF3333")
  expect_equal(d$rw_daily_aggr_bull$itemStyle$borderWidth, 1)
})

test_that("a low swing is bullish and a high swing bearish, with its markers", {
  s <- build_reversal_series(fake_rw(), hours, "weekly")
  # bull: rows 1, 3, 5 (swing) ; bear: row 2 (row 4 is off the chart)
  expect_equal(n_points(s, "rw_weekly_swing_bull"), 3)
  expect_equal(n_points(s, "rw_weekly_swing_bear"), 1)
  # aggressive close: row 5 has none; row 4 off the chart
  expect_equal(n_points(s, "rw_weekly_aggr_bull"), 2)
  expect_equal(n_points(s, "rw_weekly_aggr_bear"), 1)
  expect_equal(n_points(s, "rw_weekly_choch_bull"), 3)
  expect_equal(n_points(s, "rw_weekly_choch_bear"), 1)
})

test_that("a point carries the H1 candle time and the price", {
  s <- build_reversal_series(fake_rw(), hours, "weekly")
  expect_equal(
    s$rw_weekly_swing_bull$data[[1]],
    list("2026-01-01 00:00:00", 1.00)
  )
  expect_equal(
    s$rw_weekly_swing_bear$data[[1]],
    list("2026-01-01 02:00:00", 1.30)
  )
  expect_equal(
    s$rw_weekly_choch_bear$data[[1]],
    list("2026-01-01 06:00:00", 1.22)
  )
})

test_that("a row with an unknown swing type draws nothing", {
  s <- build_reversal_series(fake_rw(), hours, "weekly")
  total <- sum(vapply(s, function(x) length(x$data), integer(1)))
  expect_equal(total, 3 + 1 + 2 + 1 + 3 + 1)
})

test_that("show_zone and show_naked pick the setup types", {
  zone <- build_reversal_series(fake_rw(), hours, "weekly", show_naked = FALSE)
  expect_equal(n_points(zone, "rw_weekly_swing_bull"), 2) # rows 1 and 5
  expect_equal(n_points(zone, "rw_weekly_swing_bear"), 1)

  naked <- build_reversal_series(fake_rw(), hours, "weekly", show_zone = FALSE)
  expect_equal(n_points(naked, "rw_weekly_swing_bull"), 1) # row 3
  expect_equal(n_points(naked, "rw_weekly_swing_bear"), 0)

  none <- build_reversal_series(
    fake_rw(),
    hours,
    "weekly",
    show_zone = FALSE,
    show_naked = FALSE
  )
  expect_equal(sum(vapply(none, function(x) length(x$data), integer(1))), 0)
})

test_that("missing data or columns give six empty series, not an error", {
  for (args in list(
    list(NULL, hours, "weekly"),
    list(data.table::data.table(), hours, "weekly"),
    list(fake_rw(), hours[0], "weekly"),
    list(fake_rw()[, c("swing_time", "swing_type")], hours, "weekly")
  )) {
    s <- do.call(build_reversal_series, args)
    expect_length(s, 6)
    expect_equal(sum(vapply(s, function(x) length(x$data), integer(1))), 0)
  }
})

test_that("an unknown source timeframe is an error", {
  expect_error(build_reversal_series(fake_rw(), hours, "h4"))
})

test_that("the bull and bear marker colours exist and differ", {
  expect_false(identical(chart_colors$rw_bull, chart_colors$rw_bear))
})
