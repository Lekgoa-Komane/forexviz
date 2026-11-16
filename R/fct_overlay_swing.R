#' Build the swing point marker series
#'
#' Returns four scatter series: bright and broken swing highs, bright and
#' broken swing lows. Broken means price already closed beyond the swing
#' (invalidated_at is on or before the last candle). The far-future
#' placeholder date therefore counts as "not broken".
#'
#' Prices are written as text in each point's name, so the labels need no
#' JavaScript and the series can be sent to the chart as plain data.
#'
#' @param registry data.table from forexengine (table "registry"), or NULL.
#' @param last_time POSIXct, time of the last candle on the chart.
#' @param decimals Number of decimals for the price labels.
#' @param show_high,show_low FALSE empties the high or the low series.
#' @return A named list of four echarts series.
#' @noRd
build_swing_series <- function(
  registry,
  last_time,
  decimals,
  show_high = TRUE,
  show_low = TRUE
) {
  swing_types <- c("swing_high", "swing_low")
  has_rows <- !is.null(registry) && nrow(registry) > 0L
  sw <- if (has_rows) registry[registry$type %in% swing_types, ] else NULL
  has_rows <- !is.null(sw) && nrow(sw) > 0L

  broken <- if (has_rows) {
    !is.na(sw$invalidated_at) & sw$invalidated_at <= last_time
  } else {
    logical(0)
  }
  times <- if (has_rows) {
    format(sw$formed_at, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  } else {
    character(0)
  }

  points <- function(type, is_broken, show) {
    if (!show || !has_rows) {
      return(list())
    }
    keep <- sw$type == type & broken == is_broken
    prices <- sw$critical_value[keep]
    labels <- formatC(prices, format = "f", digits = decimals)
    unname(Map(
      function(t, p, l) list(name = l, value = list(t, p)),
      times[keep],
      prices,
      labels
    ))
  }

  list(
    swing_high = swing_series(
      "swing_high",
      points("swing_high", FALSE, show_high),
      TRUE,
      FALSE
    ),
    swing_high_broken = swing_series(
      "swing_high_broken",
      points("swing_high", TRUE, show_high),
      TRUE,
      TRUE
    ),
    swing_low = swing_series(
      "swing_low",
      points("swing_low", FALSE, show_low),
      FALSE,
      FALSE
    ),
    swing_low_broken = swing_series(
      "swing_low_broken",
      points("swing_low", TRUE, show_low),
      FALSE,
      TRUE
    )
  )
}

#' One swing series: look and feel for highs or lows, bright or dimmed
#' @noRd
swing_series <- function(name, data, is_high, dim) {
  cols <- chart_colors
  lay <- chart_layout
  opacity <- if (dim) lay$swing_dim else 1

  list(
    name = name,
    type = "scatter",
    symbol = "triangle",
    symbolRotate = if (is_high) 180 else 0, # high points down
    symbolSize = lay$swing_size,
    symbolOffset = c(0, if (is_high) -lay$swing_gap else lay$swing_gap),
    z = 10,
    animation = FALSE,
    itemStyle = list(
      color = if (is_high) cols$swing_high else cols$swing_low,
      opacity = opacity
    ),
    label = list(
      show = TRUE,
      position = if (is_high) "top" else "bottom",
      formatter = "{b}",
      color = if (dim) {
        sprintf("rgba(255,255,255,%s)", opacity)
      } else {
        cols$swing_label
      },
      fontSize = 12,
      fontWeight = "bold"
    ),
    labelLayout = list(hideOverlap = TRUE),
    data = data
  )
}

#' Browser script: show the price labels only when few candles are visible
#'
#' Runs once after the chart is drawn and again at every zoom. Triangles
#' always stay; only the labels switch on and off.
#'
#' @param limit Largest number of visible candles that still shows labels.
#' @return A character string with a JavaScript function(el, x).
#' @noRd
label_toggle_js <- function(limit = chart_layout$label_max_candles) {
  js <- r"---(
function(el, x) {
  var chart = echarts.getInstanceByDom(el);
  if (!chart) { return; }
  var limit = __LIMIT__;
  var last = null;
  function update() {
    var opt = chart.getOption();
    if (!opt.dataZoom || !opt.xAxis || !opt.xAxis[0].data) { return; }
    var dz = opt.dataZoom[0];
    var visible = (dz.end - dz.start) / 100 * opt.xAxis[0].data.length;
    var show = visible <= limit;
    if (show === last) { return; }
    last = show;
    var changes = [];
    (opt.series || []).forEach(function(s) {
      if (s.type === 'scatter') {
        changes.push({ name: s.name, label: { show: show } });
      } else if (s.type === 'line' && s.markLine) {
        changes.push({ name: s.name, markLine: { label: { show: show } } });
      }
    });
    if (changes.length) { chart.setOption({ series: changes }); }
  }
    el.forexvizUpdateLabels = function(force) {
    if (force) { last = null; }
    update();
  };
  chart.on('datazoom', update);
  update();
}
)---"
  gsub("__LIMIT__", format(limit, scientific = FALSE), js, fixed = TRUE)
}
