last_time <- as.POSIXct("2026-01-10", tz = "UTC")

fake_registry <- function() {
  data.table::data.table(
    type = c("swing_high", "swing_low", "swing_high", "bull_fvg"),
    formed_at = as.POSIXct(
      c("2026-01-01", "2026-01-02", "2026-01-03", "2026-01-04"),
      tz = "UTC"
    ),
    critical_value = c(1.2, 1.0, 1.3, 1.1),
    invalidated_at = as.POSIXct(
      c(
        "2026-01-05 23:59:00",
        "2027-02-05 23:00:00",
        "2027-02-05 23:00:00",
        "2027-02-05 23:00:00"
      ),
      tz = "UTC"
    )
  )
}

test_that("four named scatter series of triangles come back", {
  s <- build_swing_series(fake_registry(), last_time, 5L)
  expect_named(
    s,
    c("swing_high", "swing_high_broken", "swing_low", "swing_low_broken")
  )
  for (one in s) {
    expect_equal(one$type, "scatter")
    expect_equal(one$symbol, "triangle")
  }
})

test_that("swings are split into bright and broken", {
  s <- build_swing_series(fake_registry(), last_time, 5L)
  expect_length(s$swing_high$data, 1) # formed 01-03, never broken
  expect_length(s$swing_high_broken$data, 1) # formed 01-01, broken 01-05
  expect_length(s$swing_low$data, 1) # placeholder = not broken
  expect_length(s$swing_low_broken$data, 0)
})

test_that("only swing rows are drawn", {
  s <- build_swing_series(fake_registry(), last_time, 5L)
  total <- sum(vapply(s, function(x) length(x$data), integer(1)))
  expect_equal(total, 3) # the bull_fvg row is ignored
})

test_that("a point carries the candle time, the price and the label", {
  s <- build_swing_series(fake_registry(), last_time, 5L)
  pt <- s$swing_high$data[[1]]
  expect_equal(pt$value, list("2026-01-03 00:00:00", 1.3))
  expect_equal(pt$name, "1.30000")

  s3 <- build_swing_series(fake_registry(), last_time, 3L)
  expect_equal(s3$swing_high$data[[1]]$name, "1.300")
})

test_that("highs are orange and point down, lows are blue and point up", {
  s <- build_swing_series(fake_registry(), last_time, 5L)

  expect_equal(s$swing_high$itemStyle$color, "#FFA500")
  expect_equal(s$swing_high$symbolRotate, 180)
  expect_equal(s$swing_high$label$position, "top")
  expect_equal(s$swing_high$label$color, "#FFFFFF")
  expect_lt(s$swing_high$symbolOffset[2], 0) # above the candle

  expect_equal(s$swing_low$itemStyle$color, "#0000FF")
  expect_equal(s$swing_low$symbolRotate, 0)
  expect_equal(s$swing_low$label$position, "bottom")
  expect_equal(s$swing_low$label$color, "#FFFFFF")
  expect_gt(s$swing_low$symbolOffset[2], 0) # below the candle
})

test_that("broken swings are dimmer than bright ones", {
  s <- build_swing_series(fake_registry(), last_time, 5L)
  expect_equal(s$swing_high$itemStyle$opacity, 1)
  expect_equal(s$swing_high_broken$itemStyle$opacity, chart_layout$swing_dim)
  expect_equal(s$swing_low_broken$itemStyle$opacity, chart_layout$swing_dim)
  expect_match(s$swing_high_broken$label$color, "^rgba\\(255,255,255,")
})

test_that("show_high and show_low empty their own series only", {
  no_high <- build_swing_series(
    fake_registry(),
    last_time,
    5L,
    show_high = FALSE
  )
  expect_length(no_high$swing_high$data, 0)
  expect_length(no_high$swing_high_broken$data, 0)
  expect_length(no_high$swing_low$data, 1)

  no_low <- build_swing_series(fake_registry(), last_time, 5L, show_low = FALSE)
  expect_length(no_low$swing_low$data, 0)
  expect_length(no_low$swing_high$data, 1)
})

test_that("no registry gives four empty series, not an error", {
  for (reg in list(NULL, data.table::data.table())) {
    s <- build_swing_series(reg, last_time, 5L)
    expect_length(s, 4)
    for (one in s) {
      expect_length(one$data, 0)
    }
  }
})

test_that("a missing invalidation time counts as not broken", {
  reg <- fake_registry()
  reg$invalidated_at[1] <- NA
  s <- build_swing_series(reg, last_time, 5L)
  expect_length(s$swing_high$data, 2)
  expect_length(s$swing_high_broken$data, 0)
})

test_that("label_toggle_js carries the limit and no leftover placeholder", {
  js <- label_toggle_js(250)
  expect_match(js, "var limit = 250;", fixed = TRUE)
  expect_match(js, "datazoom", fixed = TRUE)
  expect_false(grepl("__", js, fixed = TRUE))
  expect_match(label_toggle_js(), as.character(chart_layout$label_max_candles))
})

test_that("only Currency_Pairs and Metals have overlay data", {
  expect_true(all(c("Currency_Pairs", "Metals") %in% overlay_asset_classes))
  expect_false(any(
    c(
      "Yields",
      "Commodity_Futures",
      "Currency_Futures",
      "Currency_Indices"
    ) %in%
      overlay_asset_classes
  ))
})

test_that("the handler file exists and the label script can be refreshed", {
  js_file <- app_sys("app/www/overlay.js")
  expect_true(file.exists(js_file))
  expect_match(
    paste(readLines(js_file), collapse = "\n"),
    "forexviz_set_overlay",
    fixed = TRUE
  )
  expect_match(label_toggle_js(), "forexvizUpdateLabels", fixed = TRUE)
})
