hours <- as.POSIXct("2026-01-01 06:00:00", tz = "UTC") + 3600 * 0:9 # 06:00 .. 15:00

# position:   0     1     2     3     4     5     6     7     8     9
# hour:       06    07    08    09    10    11    12    13    14    15
fake_sessions <- function() {
  data.table::data.table(
    date_time = hours,
    london = c(NA, NA, rep("London", 8)),
    new_york = c(rep(NA, 7), rep("NY", 3)),
    kill_zone = c(
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      TRUE,
      FALSE,
      FALSE,
      TRUE,
      TRUE,
      FALSE
    )
  )
}

bar_positions <- function(series) {
  vapply(series$data, function(p) as.numeric(p[1]), numeric(1))
}

test_that("two named bar series come back, on the hidden axis, coloured", {
  s <- build_session_series(fake_sessions(), hours)
  expect_named(s, c("kz_london", "kz_ny"))
  for (one in s) {
    expect_equal(one$type, "bar")
    expect_equal(one$yAxisIndex, 1)
    expect_equal(one$barWidth, "100%")
  }
  expect_equal(s$kz_london$itemStyle$color, chart_colors$kz_london)
  expect_equal(s$kz_ny$itemStyle$color, chart_colors$kz_ny)
})

test_that("London is the London-only kill zone, NY the NY kill zone", {
  s <- build_session_series(fake_sessions(), hours)
  expect_equal(bar_positions(s$kz_london), c(2, 3, 4)) # 08:00 .. 10:00
  expect_equal(bar_positions(s$kz_ny), c(7, 8)) # 13:00, 14:00
  expect_true(all(vapply(s$kz_london$data, function(p) p[2] == 1, logical(1))))
})

test_that("kill zone candles in the NY session are not London, and non-kill-zone candles are not drawn", {
  s <- build_session_series(fake_sessions(), hours)
  expect_false(7 %in% bar_positions(s$kz_london)) # London hours, but NY too
  expect_false(9 %in% bar_positions(s$kz_ny)) # NY candle, kill zone FALSE
})

test_that("show_london and show_ny empty their own series only", {
  no_london <- build_session_series(fake_sessions(), hours, show_london = FALSE)
  expect_length(no_london$kz_london$data, 0)
  expect_length(no_london$kz_ny$data, 2)

  no_ny <- build_session_series(fake_sessions(), hours, show_ny = FALSE)
  expect_length(no_ny$kz_ny$data, 0)
  expect_length(no_ny$kz_london$data, 3)
})

test_that("positions follow the chart's candles, and missing candles are dropped", {
  # the chart has no 09:00 candle
  s <- build_session_series(fake_sessions(), hours[-4])
  expect_equal(bar_positions(s$kz_london), c(2, 3)) # 08:00 and 10:00
  expect_equal(bar_positions(s$kz_ny), c(6, 7)) # 13:00 and 14:00
})

test_that("an empty kill_zone counts as not in a kill zone", {
  bc <- fake_sessions()
  bc$kill_zone[4] <- NA # the 09:00 candle
  s <- build_session_series(bc, hours)
  expect_equal(bar_positions(s$kz_london), c(2, 4))
})

test_that("missing data or columns give two empty series, not an error", {
  for (args in list(
    list(NULL, hours),
    list(data.table::data.table(), hours),
    list(fake_sessions(), hours[0]),
    list(fake_sessions()[, c("date_time", "london")], hours) # columns missing
  )) {
    s <- do.call(build_session_series, args)
    expect_length(s, 2)
    for (one in s) {
      expect_length(one$data, 0)
    }
  }
})

test_that("the two kill zone colours exist and differ", {
  expect_false(identical(chart_colors$kz_london, chart_colors$kz_ny))
})
