#' Build the Reversal Watch marker series for one source timeframe
#'
#' Returns six scatter series: swing arrow, aggressive close circle and CHoCH
#' diamond, each bull and bear. Weekly markers are hollow, daily markers are
#' filled. A swing_type of "low" is bullish, "high" is bearish. A marker is
#' skipped when its time is not on the chart or its price is empty.
#'
#' @param rw data.table from forexengine (table "reversal_watch") for the
#'   source timeframe, or NULL.
#' @param candle_times POSIXct vector of the candle times on the chart (H1).
#' @param source_tf "weekly" or "daily": which reversal watch table `rw` is.
#' @param show_zone,show_naked Which setup types to draw (zone_swing,
#'   naked_swing).
#' @return A named list of six echarts series.
#' @noRd
build_reversal_series <- function(
  rw,
  candle_times,
  source_tf,
  show_zone = TRUE,
  show_naked = TRUE
) {
  stopifnot(source_tf %in% c("weekly", "daily"))
  needed <- c(
    "swing_time",
    "swing_type",
    "swing_price",
    "setup_type",
    "aggressive_close_time",
    "aggressive_close_price",
    "h1_choch_time",
    "h1_choch_price"
  )
  have <- !is.null(rw) &&
    nrow(rw) > 0L &&
    length(candle_times) > 0L &&
    all(needed %in% names(rw))

  setups <- c(if (show_zone) "zone_swing", if (show_naked) "naked_swing")
  rows <- NULL
  if (have && length(setups) > 0L) {
    is_wanted <- rw$setup_type %in% setups
    rows <- rw[is_wanted, ]
  }
  has_rows <- !is.null(rows) && nrow(rows) > 0L

  candle_num <- as.numeric(candle_times)
  fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  bullish <- if (has_rows) rows$swing_type == "low" else logical(0)

  points <- function(time_col, price_col, is_bull) {
    if (!has_rows) {
      return(list())
    }
    when <- rows[[time_col]]
    price <- rows[[price_col]]
    keep <- !is.na(bullish) &
      bullish == is_bull &
      !is.na(when) &
      !is.na(price) &
      as.numeric(when) %in% candle_num
    if (!any(keep)) {
      return(list())
    }
    unname(Map(function(t, p) list(t, p), fmt(when[keep]), price[keep]))
  }

  lay <- chart_layout
  kinds <- list(
    list(
      kind = "swing",
      symbol = "arrow",
      size = lay$rw_swing_size,
      z = 15,
      time = "swing_time",
      price = "swing_price"
    ),
    list(
      kind = "aggr",
      symbol = "circle",
      size = lay$rw_aggr_size,
      z = 14,
      time = "aggressive_close_time",
      price = "aggressive_close_price"
    ),
    list(
      kind = "choch",
      symbol = "diamond",
      size = lay$rw_choch_size,
      z = 14,
      time = "h1_choch_time",
      price = "h1_choch_price"
    )
  )

  out <- list()
  for (k in kinds) {
    for (is_bull in c(TRUE, FALSE)) {
      out[[length(out) + 1L]] <- reversal_series(
        name = paste0(
          "rw_",
          source_tf,
          "_",
          k$kind,
          if (is_bull) "_bull" else "_bear"
        ),
        data = points(k$time, k$price, is_bull),
        symbol = k$symbol,
        size = k$size,
        z = k$z,
        rotate = if (k$kind == "swing" && !is_bull) 180 else 0,
        colour = if (is_bull) chart_colors$rw_bull else chart_colors$rw_bear,
        hollow = source_tf == "weekly"
      )
    }
  }
  names(out) <- vapply(out, function(s) s$name, character(1))
  out
}

#' One Reversal Watch marker series
#' @noRd
reversal_series <- function(
  name,
  data,
  symbol,
  size,
  z,
  rotate,
  colour,
  hollow
) {
  style <- if (hollow) {
    list(color = "rgba(0,0,0,0)", borderColor = colour, borderWidth = 2)
  } else {
    list(color = colour, borderColor = colour, borderWidth = 1)
  }
  list(
    name = name,
    type = "scatter",
    symbol = symbol,
    symbolRotate = rotate,
    symbolSize = size,
    z = z,
    silent = TRUE,
    animation = FALSE,
    itemStyle = style,
    data = data
  )
}
