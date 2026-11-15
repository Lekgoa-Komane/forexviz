candle_times <- as.POSIXct("2026-01-01", tz = "UTC") + 86400 * 0:9 # 01-01 .. 01-10
placeholder <- as.POSIXct("2027-02-05 23:00:00", tz = "UTC")

fake_zones <- function() {
  data.table::data.table(
    type = c(
      "bull_fvg",
      "bull_fvg",
      "bear_fvg",
      "bear_ob",
      "bull_ob",
      "swing_high",
      "bull_fvg"
    ),
    formed_at = as.POSIXct(
      c(
        "2026-01-03 00:00:00",
        "2026-01-02 00:00:00",
        "2026-01-04 15:00:00",
        "2026-01-06 00:00:00",
        "2025-12-20 00:00:00",
        "2026-01-05 00:00:00",
        "2026-02-01 00:00:00"
      ),
      tz = "UTC"
    ),
    type_zone_bot_value = c(1.10, 1.00, 1.30, 1.25, 0.90, NA, 1.40),
    type_zone_top_value = c(1.15, 1.05, 1.20, 1.28, 0.95, NA, 1.45),
    mit_zone_dist_at = c(
      placeholder, # active
      as.POSIXct("2026-01-05", tz = "UTC"), # filled: not drawn
      placeholder,
      placeholder,
      placeholder,
      placeholder,
      placeholder # formed after the chart
    )
  )
}

test_that("two named carrier series come back, coloured per type", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  expect_named(s, c("zones_bull_fvg", "zones_bear_fvg"))
  for (one in s) {
    expect_equal(one$type, "line")
    expect_length(one$data, 0) # the line itself is empty
  }
  expect_equal(
    s$zones_bull_fvg$markArea$itemStyle$color,
    chart_colors$zone_bull_fvg
  )
  expect_equal(
    s$zones_bear_fvg$markArea$itemStyle$color,
    chart_colors$zone_bear_fvg
  )
})

test_that("only active zones that start on the chart are drawn", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  # bull: one active; one filled (not drawn); one formed after the last candle
  expect_length(s$zones_bull_fvg$markArea$data, 1)
  expect_length(s$zones_bear_fvg$markArea$data, 1)
})

test_that("a zone runs from its start candle to the last candle", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  z <- s$zones_bull_fvg$markArea$data[[1]]
  expect_equal(z[[1]]$xAxis, "2026-01-03 00:00:00")
  expect_equal(z[[2]]$xAxis, "2026-01-10 00:00:00") # the last candle
  expect_equal(z[[1]]$yAxis, 1.10)
  expect_equal(z[[2]]$yAxis, 1.15)
})

test_that("a zone formed between candles starts at the next candle", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  z <- s$zones_bear_fvg$markArea$data[[1]]
  expect_equal(z[[1]]$xAxis, "2026-01-05 00:00:00")
})

test_that("bottom and top are sorted, whatever their order", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  z <- s$zones_bear_fvg$markArea$data[[1]] # bot 1.30 and top 1.20 in the data
  expect_equal(z[[1]]$yAxis, 1.20)
  expect_equal(z[[2]]$yAxis, 1.30)
})

test_that("a zone formed before the first candle starts at the first candle", {
  s <- build_zone_series(fake_zones(), candle_times, "ob")
  z <- s$zones_bull_ob$markArea$data[[1]]
  expect_equal(z[[1]]$xAxis, "2026-01-01 00:00:00")
})

test_that("OB zones use the OB types and colours", {
  s <- build_zone_series(fake_zones(), candle_times, "ob")
  expect_named(s, c("zones_bull_ob", "zones_bear_ob"))
  expect_length(s$zones_bull_ob$markArea$data, 1)
  expect_length(s$zones_bear_ob$markArea$data, 1)
  expect_equal(
    s$zones_bear_ob$markArea$itemStyle$color,
    chart_colors$zone_bear_ob
  )
})

test_that("swing rows and the other kind are ignored", {
  s <- build_zone_series(fake_zones(), candle_times, "fvg")
  total <- sum(vapply(s, function(x) length(x$markArea$data), integer(1)))
  expect_equal(total, 2)
})

test_that("an empty far-edge time counts as active", {
  reg <- fake_zones()
  reg$mit_zone_dist_at[2] <- NA
  s <- build_zone_series(reg, candle_times, "fvg")
  expect_length(s$zones_bull_fvg$markArea$data, 2)
})

test_that("show_bull and show_bear empty their own series only", {
  no_bull <- build_zone_series(
    fake_zones(),
    candle_times,
    "fvg",
    show_bull = FALSE
  )
  expect_length(no_bull$zones_bull_fvg$markArea$data, 0)
  expect_length(no_bull$zones_bear_fvg$markArea$data, 1)

  no_bear <- build_zone_series(
    fake_zones(),
    candle_times,
    "fvg",
    show_bear = FALSE
  )
  expect_length(no_bear$zones_bear_fvg$markArea$data, 0)
  expect_length(no_bear$zones_bull_fvg$markArea$data, 1)
})

test_that("no registry or no candles gives two empty series, not an error", {
  for (reg in list(NULL, data.table::data.table())) {
    s <- build_zone_series(reg, candle_times, "fvg")
    expect_length(s, 2)
    for (one in s) {
      expect_length(one$markArea$data, 0)
    }
  }
  s <- build_zone_series(fake_zones(), candle_times[0], "fvg")
  for (one in s) {
    expect_length(one$markArea$data, 0)
  }
})

test_that("an unknown kind is an error", {
  expect_error(build_zone_series(fake_zones(), candle_times, "xyz"))
})

test_that("the four zone colours exist and differ", {
  cols <- unlist(chart_colors[c(
    "zone_bull_fvg",
    "zone_bear_fvg",
    "zone_bull_ob",
    "zone_bear_ob"
  )])
  expect_length(cols, 4)
  expect_equal(anyDuplicated(cols), 0L)
})
