#' Fetch
#'
#' The FETCH function interacts with the Warp 10 Storage Engine to retrieve data according to given criteria.
#'
#' @inheritParams documentation
#' @param end Newest timestamp to consider when fetching datapoints.
#' @param timespan Depth to consider when fetching the datapoints.
#' If the value is positive then it is interpreted as a duration in time units,
#' if it is negative then as the maximum number of datapoints to fetch.
#' If negative, incompatible with `count`.
#' @param count Maximum number of datapoints to fetch for each GTS. Incompatible with negative 'timespan'.
#'
#' @seealso [wrp_find()]
#'
#' @references <https://www.warp10.io/doc/FETCH>
#'
#' @keywords gts
#'
#' @export
#'
wrp_fetch <- function(wrp_con, class = "~.*", labels = NULL, end = "NOW", count = 1, timespan = NULL) {
  assert_token(wrp_con$get_token())
  labels   <- labels_to_string(labels)
  end      <- parse_timestamp(end)
  timespan <- if (!is.null(timespan)) paste("'timespan'", parse_time_unit(timespan)) else ""
  count    <- if (!is.null(count)) paste("'count'", count) else ""
  script   <- glue::glue("{{ 'token' $token 'class' '{class}' 'labels' {{ {labels} }} 'end' {end} {timespan} {count} }} FETCH") # nolint
  add_stack(wrp_con, script, "lgts")
  return(wrp_con)
}
