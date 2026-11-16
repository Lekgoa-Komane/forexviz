#' Build the BOS and CHoCH line series
#'
#' Returns four series: bull BOS, bear BOS, bull CHoCH, bear CHoCH. Each is an
#' invisible line series that only carries its lines (lines must hang on a
#' series). A line runs from the candle of the broken swing to the candle where
#' the engine flags the BOS or CHoCH, at the price of the swing.
#'
#' A bull event breaks a swing high, a bear event a swing low. The swing is the
#' latest one of that type with the same price formed at or before the event.
#'
#' @param bos_choch data.table from forexengine (table "bos_choch").
#' @param registry data.table from forexengine (table "registry").
#' @param candle_times POSIXct vector of the candle times on the chart.
#' @param timeframe "weekly", "daily", "h4" or "h1" (the column prefix).
#' @param show_bos,show_choch FALSE empties the BOS or the CHoCH series.
#' @return A named list of four echarts series.
#' @noRd
build_line_series <- function(
  bos_choch,
  registry,
  candle_times,
  timeframe,
  show_bos = TRUE,
  show_choch = TRUE
) {
  col <- function(name) paste0(timeframe, "_", name)
  needed <- col(c(
    "bos_type",
    "choch_type",
    "choch_level",
    "prev_sh",
    "prev_sl"
  ))
  n <- length(candle_times)
  have <- !is.null(bos_choch) &&
    !is.null(registry) &&
    nrow(bos_choch) > 0L &&
    nrow(registry) > 0L &&
    n > 0L &&
    all(needed %in% names(bos_choch))

  fmt <- function(x) format(x, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  candle_num <- as.numeric(candle_times)

  swings <- NULL
  if (have) {
    is_swing <- registry$type %in% c("swing_high", "swing_low")
    swings <- registry[is_swing, ]
  }

  lines_for <- function(kind, direction, show) {
    if (!show || !have || nrow(swings) == 0L) {
      return(list())
    }
    types <- bos_choch[[col(paste0(kind, "_type"))]]
    rows <- which(!is.na(types) & types == paste0(direction, "_", kind))
    if (length(rows) == 0L) {
      return(list())
    }
    ev <- bos_choch[rows, ]

    level <- if (kind == "bos") {
      ev[[col(if (direction == "bull") "prev_sh" else "prev_sl")]]
    } else {
      ev[[col("choch_level")]]
    }

    swing_type <- if (direction == "bull") "swing_high" else "swing_low"
    of_type <- swings$type == swing_type
    pool_key <- sprintf("%.10f", swings$critical_value[of_type])
    pool_time <- as.numeric(swings$formed_at[of_type])
    ev_time <- as.numeric(ev$date_time)
    key <- sprintf("%.10f", level)

    # the latest swing of this type, with this price, formed at or before
    start_time <- vapply(
      seq_along(level),
      function(i) {
        if (is.na(level[i])) {
          return(NA_real_)
        }
        hit <- which(pool_key == key[i] & pool_time <= ev_time[i])
        if (length(hit) == 0L) NA_real_ else max(pool_time[hit])
      },
      numeric(1)
    )

    start_idx <- findInterval(start_time, candle_num, left.open = TRUE) + 1L
    end_idx <- match(ev_time, candle_num)
    keep <- !is.na(start_time) & !is.na(end_idx) & start_idx <= end_idx
    if (!any(keep)) {
      return(list())
    }

    unname(Map(
      function(s, e, y) {
        list(
          list(xAxis = fmt(candle_times[s]), yAxis = y),
          list(xAxis = fmt(candle_times[e]), yAxis = y)
        )
      },
      start_idx[keep],
      end_idx[keep],
      level[keep]
    ))
  }

  out <- list(
    line_series(
      "lines_bos_bull",
      lines_for("bos", "bull", show_bos),
      chart_colors$line_bull,
      dotted = FALSE,
      label = "BOS"
    ),
    line_series(
      "lines_bos_bear",
      lines_for("bos", "bear", show_bos),
      chart_colors$line_bear,
      dotted = FALSE,
      label = "BOS"
    ),
    line_series(
      "lines_choch_bull",
      lines_for("choch", "bull", show_choch),
      chart_colors$line_bull,
      dotted = TRUE,
      label = "CHoCH"
    ),
    line_series(
      "lines_choch_bear",
      lines_for("choch", "bear", show_choch),
      chart_colors$line_bear,
      dotted = TRUE,
      label = "CHoCH"
    )
  )
  names(out) <- vapply(out, function(s) s$name, character(1))
  out
}

#' One invisible carrier series with its lines
#' @noRd
line_series <- function(name, lines, colour, dotted, label) {
  list(
    name = name,
    type = "line",
    data = list(),
    symbol = "none",
    silent = TRUE,
    animation = FALSE,
    lineStyle = list(opacity = 0),
    markLine = list(
      silent = TRUE,
      animation = FALSE,
      lineStyle = list(
        type = if (dotted) "dotted" else "dashed",
        color = colour,
        width = chart_layout$line_width
      ),
      label = list(
        show = TRUE,
        formatter = label,
        position = "middle",
        offset = c(0, -10),
        color = chart_colors$line_label,
        fontSize = chart_layout$line_label_size,
        fontWeight = "bold"
      ),
      data = lines
    )
  )
}
