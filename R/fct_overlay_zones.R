#' Build the shaded zone series for one kind of zone (FVG or OB)
#'
#' Returns two series, bull and bear. Each is an invisible line series that
#' only carries its rectangles (shaded areas must hang on a series).
#'
#' A zone is active while price has not touched its far edge, that is while
#' mit_zone_dist_at is empty or later than the last candle (the far-future
#' placeholder counts as later). An active zone runs from the first candle at
#' or after formed_at to the last candle on the chart.
#'
#' @param registry data.table from forexengine (table "registry"), or NULL.
#' @param candle_times POSIXct vector of the candle times on the chart, in
#'   increasing order.
#' @param kind "fvg" or "ob".
#' @param show_bull,show_bear FALSE empties the bull or the bear series.
#' @return A named list of two echarts series.
#' @noRd
build_zone_series <- function(
  registry,
  candle_times,
  kind,
  show_bull = TRUE,
  show_bear = TRUE
) {
  stopifnot(kind %in% c("fvg", "ob"))
  n <- length(candle_times)
  have <- !is.null(registry) && nrow(registry) > 0L && n > 0L
  fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  last_time <- if (n > 0L) candle_times[n] else NULL
  last_label <- if (n > 0L) fmt(last_time) else NA_character_

  rectangles <- function(type, show) {
    if (!show || !have) {
      return(list())
    }
    rows <- registry$type == type
    z <- registry[rows, ]
    if (nrow(z) == 0L) {
      return(list())
    }

    is_active <- is.na(z$mit_zone_dist_at) | z$mit_zone_dist_at > last_time
    z <- z[is_active, ]
    if (nrow(z) == 0L) {
      return(list())
    }

    # first candle at or after formed_at
    start_idx <- findInterval(
      as.numeric(z$formed_at),
      as.numeric(candle_times),
      left.open = TRUE
    ) +
      1L
    lower <- pmin(z$type_zone_bot_value, z$type_zone_top_value)
    upper <- pmax(z$type_zone_bot_value, z$type_zone_top_value)
    keep_row <- start_idx <= n & !is.na(lower) & !is.na(upper)
    if (!any(keep_row)) {
      return(list())
    }

    starts <- fmt(candle_times[start_idx[keep_row]])
    unname(Map(
      function(x1, lo, hi) {
        list(
          list(xAxis = x1, yAxis = lo),
          list(xAxis = last_label, yAxis = hi)
        )
      },
      starts,
      lower[keep_row],
      upper[keep_row]
    ))
  }

  bull <- paste0("bull_", kind)
  bear <- paste0("bear_", kind)
  out <- list(
    zone_series(
      paste0("zones_", bull),
      rectangles(bull, show_bull),
      chart_colors[[paste0("zone_", bull)]]
    ),
    zone_series(
      paste0("zones_", bear),
      rectangles(bear, show_bear),
      chart_colors[[paste0("zone_", bear)]]
    )
  )
  names(out) <- vapply(out, function(s) s$name, character(1))
  out
}

#' One invisible carrier series with its shaded rectangles
#' @noRd
zone_series <- function(name, rectangles, colour) {
  list(
    name = name,
    type = "line",
    data = list(),
    symbol = "none",
    silent = TRUE,
    animation = FALSE,
    lineStyle = list(opacity = 0),
    markArea = list(
      silent = TRUE,
      animation = FALSE,
      itemStyle = list(color = colour),
      data = rectangles
    )
  )
}
