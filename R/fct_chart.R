#' Default window (in days) for each timeframe
#' @noRd
default_days_back <- function(timeframe) {
  switch(
    timeframe,
    weekly = 365 * 3,
    daily = 34 * 7,
    h4 = 4 * 7,
    h1 = 7,
    7
  )
}

#' Turn a date window into zoom percentages for the chart
#' @noRd
zoom_pct <- function(date_time, start, end) {
  n <- length(date_time)
  start <- as.POSIXct(start, tz = "UTC")
  end <- as.POSIXct(end, tz = "UTC") + 86400 - 1
  first <- which(date_time >= start)[1]
  last <- which(date_time <= end)
  if (is.na(first) || length(last) == 0L) {
    return(list(start_pct = 0, end_pct = 100, start_idx = 0L, end_idx = n - 1L))
  }
  last <- last[length(last)]
  list(
    start_pct = max(0, (first - 1) / n * 100),
    end_pct = min(100, last / n * 100),
    start_idx = first - 1L,
    end_idx = last - 1L
  )
}

#' Chart colours, kept in one place
#' @noRd
chart_colors <- list(
  up = "#2ecc71", # soft green: price went up
  down = "#e74c3c", # soft red: price went down
  text = "#e6e6e6" # light text for the dark page
)

#' Build the full echarts option for the candlestick chart
#'
#' Packs the candles as one block of numbers (fast), instead of
#' row by row.
#'
#' @param dt data.table with open, close, low, high and time_chr.
#' @param title Chart title.
#' @param decimals Number of decimals on the price axis.
#' @param zoom List from zoom_pct().
#' @param show_time TRUE to show the time in the readout (H4, H1).
#' @return A list that echarts understands.
#' @noRd
build_candle_option <- function(dt, title, decimals, zoom, show_time = FALSE) {
  cols <- chart_colors

  mat <- as.matrix(dt[, c("open", "close", "low", "high"), with = FALSE])
  mat <- round(mat, 6)
  dimnames(mat) <- NULL

  axis_name_style <- list(
    color = cols$text,
    fontSize = 16,
    fontWeight = "bold"
  )
  axis_line <- list(lineStyle = list(color = cols$text))

  list(
    title = list(
      text = title,
      left = "center",
      top = chart_layout$title_top,
      textStyle = list(color = cols$text, fontSize = 16, fontWeight = "bold")
    ),
    tooltip = list(
      trigger = "axis",
      axisPointer = list(type = "cross"),
      position = c(chart_layout$readout_x, chart_layout$readout_y),
      backgroundColor = "transparent",
      borderWidth = 0,
      padding = 0,
      extraCssText = "box-shadow: none;",
      alwaysShowContent = FALSE,
      transitionDuration = 0,
      textStyle = list(color = cols$text, fontSize = 14),
      formatter = candle_tooltip_js(decimals, show_time)
    ),
    legend = list(show = FALSE),
    grid = list(
      left = chart_layout$grid_left,
      top = chart_layout$grid_top,
      bottom = 100
    ),
    xAxis = list(
      type = "category",
      data = dt$time_chr,
      name = "Date_Time",
      nameLocation = "middle",
      nameGap = 50,
      axisLabel = list(color = cols$text),
      axisLine = axis_line,
      nameTextStyle = axis_name_style
    ),
    yAxis = list(
      scale = TRUE,
      name = "Price",
      nameLocation = "middle",
      nameGap = 40,
      axisLabel = list(
        color = cols$text,
        formatter = htmlwidgets::JS(sprintf(
          "function(v){ return v.toFixed(%d); }",
          decimals
        ))
      ),
      axisLine = axis_line,
      nameTextStyle = axis_name_style
    ),
    dataZoom = list(
      list(
        type = "slider",
        xAxisIndex = 0,
        orient = "vertical",
        right = 10,
        top = "middle",
        height = "80%",
        start = zoom$start_pct,
        end = zoom$end_pct
      ),
      list(
        type = "inside",
        xAxisIndex = 0,
        start = zoom$start_pct,
        end = zoom$end_pct
      )
    ),
    series = list(list(
      type = "candlestick",
      name = "Price",
      data = mat,
      itemStyle = list(
        color = cols$up,
        color0 = cols$down,
        borderColor = cols$up,
        borderColor0 = cols$down
      )
    ))
  )
}

#' Number of price decimals for a ticker, from the config pip_factor rule
#' @param ticker Character ticker, such as "usdjpy".
#' @param config The loaded config list.
#' @return Integer, such as 3L or 5L.
#' @noRd
price_decimals <- function(ticker, config) {
  pf <- config$transformation$pip_factor
  matched <- grepl(pf$regex_pattern, tolower(ticker))
  value <- if (matched) pf$value_for_match else pf$default_value
  as.integer(round(-log10(value)))
}

#' All timeframes the chart can show
#' @noRd
all_timeframes <- c("weekly", "daily", "h4", "h1")

#' Timeframes available for an asset class, read from the config
#'
#' An asset class with hourly source data can show every timeframe.
#' One with only daily data shows weekly and daily.
#' @noRd
timeframes_for <- function(asset_class, config) {
  freqs <- config$asset_classes[[asset_class]]$frequencies
  if ("hourly" %in% freqs) {
    all_timeframes
  } else {
    c("weekly", "daily")
  }
}

#' JavaScript that writes the one-line readout for the hovered candle
#'
#' Order is open, high, low, close, then the change in brackets (close
#' compared with open), coloured green for up and red for down.
#'
#' @param decimals Number of decimals for the prices.
#' @param show_time TRUE to show date and time (H4, H1), FALSE for date only.
#' @noRd
candle_tooltip_js <- function(decimals, show_time) {
  cols <- chart_colors
  js <- r"---(
function(params) {
  var p = null;
  for (var i = 0; i < params.length; i++) {
    if (params[i].seriesType === 'candlestick') { p = params[i]; break; }
  }
  if (!p || !p.data) { return ''; }
  var d = p.data;
  if (d.length === 5) { d = d.slice(1); }
  var o = d[0], c = d[1], l = d[2], h = d[3];
  var t = String(p.axisValueLabel).substring(0, __LEN__);
  var chg = (o !== 0) ? (c - o) / o * 100 : 0;
  var col = c > o ? '__UP__' : (c < o ? '__DOWN__' : '__TEXT__');
  var sign = chg > 0 ? '+' : '';
  var f = function(x) { return Number(x).toFixed(__DEC__); };
  var gap = '&nbsp;&nbsp;&nbsp;';
  return '<span style="color:__DATE__">' + t + '</span>' + gap +
    '<span style="color:__OHLC__">O:' + f(o) + gap + 'H:' + f(h) + gap +
    'L:' + f(l) + gap + 'C:' + f(c) + '</span>' + gap +
    '<span style="color:' + col + '">(' + sign + chg.toFixed(2) + '%)</span>';
}
)---"
  js <- gsub("__UP__", cols$up, js, fixed = TRUE)
  js <- gsub("__DOWN__", cols$down, js, fixed = TRUE)
  js <- gsub("__TEXT__", cols$text, js, fixed = TRUE)
  js <- gsub("__DATE__", cols$date, js, fixed = TRUE)
  js <- gsub("__OHLC__", cols$ohlc, js, fixed = TRUE)
  js <- gsub("__DEC__", as.character(decimals), js, fixed = TRUE)
  js <- gsub("__LEN__", if (show_time) "16" else "10", js, fixed = TRUE)
  htmlwidgets::JS(js)
}

#' Chart colours, kept in one place
#' @noRd
chart_colors <- list(
  up = "#2ecc71", # soft green: price went up
  down = "#e74c3c", # soft red: price went down
  text = "#e6e6e6", # light text for the dark page
  date = "#7fb3ff", # soft sky blue: the date in the readout
  ohlc = "#ffd166" # warm amber: open, high, low, close in the readout
)

#' Chart layout numbers (pixels), kept in one place
#' @noRd
chart_layout <- list(
  title_top = 6, # title distance from the top
  readout_x = 75, # readout starts here: left edge of the price numbers
  readout_y = 38, # readout distance from the top
  grid_left = 130, # room kept for the price numbers and the "Price" name
  grid_top = 80 # where the candles start
)
