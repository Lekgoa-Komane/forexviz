candle_times <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9 # 01-01 .. 01-10

fake_bos_choch <- function() {
  data.table::data.table(
    date_time = candle_times,
    daily_bos_type = c(
      NA,
      NA,
      NA,
      "bull_bos",
      NA,
      "bear_bos",
      NA,
      NA,
      NA,
      "no_bos"
    ),
    daily_choch_type = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      "bull_choch",
      NA,
      "bear_choch",
      "no_choch"
    ),
    daily_choch_level = c(NA, NA, NA, NA, NA, NA, 1.35, NA, 0.98, NA),
    daily_prev_sh = c(NA, NA, NA, 1.30, NA, 1.40, NA, NA, NA, NA),
    daily_prev_sl = c(NA, NA, NA, 1.00, NA, 0.95, NA, NA, NA, NA)
  )
}

fake_swings <- function() {
  data.table::data.table(
    type = c(
      "swing_high",
      "swing_low",
      "swing_high",
      "swing_low",
      "swing_high",
      "swing_low",
      "bull_fvg"
    ),
    formed_at = as.POSIXct(
      c(
        "2026-01-02",
        "2026-01-03",
        "2026-01-05",
        "2026-01-08",
        "2025-12-20",
        "2026-01-01",
        "2026-01-02"
      ),
      tz = "UTC"
    ),
    critical_value = c(1.30, 0.95, 1.35, 0.98, 1.30, 1.30, 1.30)
  )
}

total_lines <- function(series) {
  sum(vapply(series, function(x) length(x$markLine$data), integer(1)))
}

test_that("four named carrier series come back, styled per kind", {
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  expect_named(
    s,
    c(
      "lines_bos_bull",
      "lines_bos_bear",
      "lines_choch_bull",
      "lines_choch_bear"
    )
  )
  for (one in s) {
    expect_equal(one$type, "line")
    expect_length(one$data, 0)
  }
  expect_equal(s$lines_bos_bull$markLine$lineStyle$type, "dashed")
  expect_equal(s$lines_bos_bear$markLine$lineStyle$type, "dashed")
  expect_equal(s$lines_choch_bull$markLine$lineStyle$type, "dotted")
  expect_equal(s$lines_choch_bear$markLine$lineStyle$type, "dotted")
  expect_equal(
    s$lines_bos_bull$markLine$lineStyle$color,
    chart_colors$line_bull
  )
  expect_equal(
    s$lines_bos_bear$markLine$lineStyle$color,
    chart_colors$line_bear
  )
  expect_equal(
    s$lines_choch_bull$markLine$lineStyle$color,
    chart_colors$line_bull
  )
  expect_equal(
    s$lines_choch_bear$markLine$lineStyle$color,
    chart_colors$line_bear
  )
  expect_equal(s$lines_bos_bull$markLine$label$formatter, "BOS")
  expect_equal(s$lines_choch_bear$markLine$label$formatter, "CHoCH")
})

test_that("a bull BOS runs from its swing high to the BOS candle", {
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  expect_length(s$lines_bos_bull$markLine$data, 1)
  ln <- s$lines_bos_bull$markLine$data[[1]]
  expect_equal(ln[[1]]$xAxis, "2026-01-02 00:00:00") # swing high formed
  expect_equal(ln[[2]]$xAxis, "2026-01-04 00:00:00") # BOS candle
  expect_equal(ln[[1]]$yAxis, 1.30)
  expect_equal(ln[[2]]$yAxis, 1.30)
})

test_that("the latest same-price swing of the right type is used", {
  # 1.30 also exists as an older swing high (2025-12-20) and as a swing low
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  ln <- s$lines_bos_bull$markLine$data[[1]]
  expect_equal(ln[[1]]$xAxis, "2026-01-02 00:00:00")
})

test_that("a bear BOS runs from its swing low to the BOS candle", {
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  ln <- s$lines_bos_bear$markLine$data[[1]]
  expect_equal(ln[[1]]$xAxis, "2026-01-03 00:00:00")
  expect_equal(ln[[2]]$xAxis, "2026-01-06 00:00:00")
  expect_equal(ln[[1]]$yAxis, 0.95)
})

test_that("CHoCH lines use the choch level and the right swing type", {
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  bull <- s$lines_choch_bull$markLine$data[[1]]
  expect_equal(bull[[1]]$xAxis, "2026-01-05 00:00:00")
  expect_equal(bull[[2]]$xAxis, "2026-01-07 00:00:00")
  expect_equal(bull[[1]]$yAxis, 1.35)

  bear <- s$lines_choch_bear$markLine$data[[1]]
  expect_equal(bear[[1]]$xAxis, "2026-01-08 00:00:00")
  expect_equal(bear[[2]]$xAxis, "2026-01-09 00:00:00")
  expect_equal(bear[[1]]$yAxis, 0.98)
})

test_that("no_bos, no_choch and empty types make no lines", {
  s <- build_line_series(fake_bos_choch(), fake_swings(), candle_times, "daily")
  expect_equal(total_lines(s), 4)
})

test_that("show_bos and show_choch empty their own series only", {
  no_bos <- build_line_series(
    fake_bos_choch(),
    fake_swings(),
    candle_times,
    "daily",
    show_bos = FALSE
  )
  expect_equal(total_lines(no_bos[c("lines_bos_bull", "lines_bos_bear")]), 0)
  expect_equal(
    total_lines(no_bos[c("lines_choch_bull", "lines_choch_bear")]),
    2
  )

  no_choch <- build_line_series(
    fake_bos_choch(),
    fake_swings(),
    candle_times,
    "daily",
    show_choch = FALSE
  )
  expect_equal(
    total_lines(no_choch[c("lines_choch_bull", "lines_choch_bear")]),
    0
  )
  expect_equal(total_lines(no_choch[c("lines_bos_bull", "lines_bos_bear")]), 2)
})

test_that("an event without a matching swing is skipped", {
  swings <- fake_swings()[-3, ] # remove the swing high at 1.35
  s <- build_line_series(fake_bos_choch(), swings, candle_times, "daily")
  expect_length(s$lines_choch_bull$markLine$data, 0)
  expect_length(s$lines_choch_bear$markLine$data, 1)
})

test_that("an event whose candle is not on the chart is skipped", {
  s <- build_line_series(
    fake_bos_choch(),
    fake_swings(),
    candle_times[-6],
    "daily"
  ) # no candle on 01-06
  expect_length(s$lines_bos_bear$markLine$data, 0)
  expect_length(s$lines_bos_bull$markLine$data, 1)
})

test_that("an empty level is skipped", {
  bc <- fake_bos_choch()
  bc$daily_prev_sh[4] <- NA
  s <- build_line_series(bc, fake_swings(), candle_times, "daily")
  expect_length(s$lines_bos_bull$markLine$data, 0)
})

test_that("missing data or columns give four empty series, not an error", {
  for (args in list(
    list(NULL, fake_swings(), candle_times, "daily"),
    list(fake_bos_choch(), NULL, candle_times, "daily"),
    list(data.table::data.table(), fake_swings(), candle_times, "daily"),
    list(fake_bos_choch(), fake_swings(), candle_times[0], "daily"),
    list(fake_bos_choch(), fake_swings(), candle_times, "h1") # no h1_ columns
  )) {
    s <- do.call(build_line_series, args)
    expect_length(s, 4)
    expect_equal(total_lines(s), 0)
  }
})

test_that("the bull and bear line colours exist and differ", {
  expect_false(identical(chart_colors$line_bull, chart_colors$line_bear))
})
