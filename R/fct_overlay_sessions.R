#' Build the kill zone band series (London and NY)
#'
#' Returns two bar series, one thin bar per flagged candle, on the hidden
#' second price axis (0 to 1), so a run of candles shows as one full-height
#' band.
#'
#' London = kill_zone is TRUE, the candle is in the London session and NOT in
#' the NY session (the London open kill zone). NY = kill_zone is TRUE and the
#' candle is in the NY session. (kill_zone is one combined flag, and London
#' hours overlap the NY kill zone.)
#'
#' @param bos_choch data.table from forexengine (table "bos_choch") with the
#'   columns date_time, london, new_york and kill_zone.
#' @param candle_times POSIXct vector of the candle times on the chart.
#' @param show_london,show_ny FALSE empties the London or the NY series.
#' @return A named list of two echarts series.
#' @noRd
build_session_series <- function(
  bos_choch,
  candle_times,
  show_london = TRUE,
  show_ny = TRUE
) {
  needed <- c("date_time", "london", "new_york", "kill_zone")
  have <- !is.null(bos_choch) &&
    nrow(bos_choch) > 0L &&
    length(candle_times) > 0L &&
    all(needed %in% names(bos_choch))

  london_flag <- ny_flag <- logical(0)
  if (have) {
    in_kill_zone <- as.logical(bos_choch$kill_zone) %in% TRUE
    london_flag <- (bos_choch$london %in% "London") &
      is.na(bos_choch$new_york) &
      in_kill_zone
    ny_flag <- (bos_choch$new_york %in% "NY") & in_kill_zone
  }

  # one [candle position, 1] per flagged candle that is on the chart
  bars_for <- function(flag, show) {
    if (!show || !have) {
      return(list())
    }
    position <- match(
      as.numeric(bos_choch$date_time[flag]),
      as.numeric(candle_times)
    ) -
      1L
    position <- position[!is.na(position)]
    unname(lapply(position, function(i) c(i, 1L)))
  }

  out <- list(
    session_series(
      "kz_london",
      bars_for(london_flag, show_london),
      chart_colors$kz_london
    ),
    session_series("kz_ny", bars_for(ny_flag, show_ny), chart_colors$kz_ny)
  )
  names(out) <- vapply(out, function(s) s$name, character(1))
  out
}

#' One kill zone bar series on the hidden 0-to-1 axis
#' @noRd
session_series <- function(name, data, colour) {
  list(
    name = name,
    type = "bar",
    data = data,
    yAxisIndex = 1, # the hidden axis, 0 to 1
    barWidth = "100%", # bars touch, so a run of candles is one band
    barGap = "-100%", # London and NY bars sit on top of each other
    barCategoryGap = "0%",
    large = TRUE,
    silent = TRUE,
    animation = FALSE,
    z = 1, # behind the candles
    itemStyle = list(color = colour, borderWidth = 0)
  )
}
