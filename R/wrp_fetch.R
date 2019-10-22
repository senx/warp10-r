#' Fetch
#'
#' The FETCH function interacts with the Warp 10 Storage Engine to retrieve data according to given criteria.
#'
#' @inheritParams documentation
#' @param end Most recent timestamp to consider when fetching datapoints. Could be a Posixct Date.
#' @param stop Either a number, a date or a duration, see details.
#'
#' @details
#' The stop parameter can either be a number or a date.
#' If a number, the depth to consider when fetching datapoints,
#' if the value is positive then it is interpreted as a duration,
#' if it is negative then as the maximum number of datapoints to fetch.
#' If a date, the stop timestamp to consider when fetching data, in ISO-8601 format.
#' If a duration, the stop timestamp will be computed first before the request is adressed to Warp 10.
#'
#' @export
#'
wrp_fetch <- function(wrp_con, token = get_token(), class = "~.*", labels = NULL, end = "NOW", stop = -1) {
  labels <- labels_to_string(labels)
  if (lubridate::is.POSIXct(end)) {
    if (is.numeric(stop)) stop("`stop` must be a date or a duration if `end` is a date")
    else if (is.character(stop)) stop <- end - lubridate::duration(stop)
    end <- format_iso8601(end)
  } else if (is.character(end)) {
    if (!is.numeric(stop)) {
      end  <- lubridate::now()
      stop <- end - lubridate::duration(stop)
      end  <- format_iso8601(end)
    }
  }
  if (!is.numeric(stop)) stop <- format_iso8601(stop)
  script <- glue::glue("[ '{token}' '{class}' {{ {labels} }} {end} {stop} ] FETCH")
  wrp_con$set_script(script)
  wrp_con$add_stack("lgts")
  return(wrp_con)
}
