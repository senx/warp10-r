#' Fetch
#'
#' The FETCH function interacts with the Warp 10 Storage Engine to retrieve data according to given criteria.
#'
#' @inheritParams documentation
#' @param end Newest timestamp to consider when fetching datapoints.
#' @param start Oldest timestamp to consider when fetching datapoints.
#' @param timespan Depth to consider when fetching the datapoints.
#' If the value is positive then it is interpreted as a duration in time units,
#' if it is negative then as the maximum number of datapoints to fetch.
#' If negative, incompatible with `count`.
#' @param count Maximum number of datapoints to fetch for each GTS. Incompatible with negative 'timespan'.
#' @param selector A Geo Time Series™ selector with the syntax class{labels} where class is an exact match
#' or a regular expression starting with ~ and labels a comma separated list of labels selector
#' of the form name=exact or name~regexp.
#' Names and values must be percent URL encoded if needed.
#' @param selectors A list of GTS selectors, each with a syntax identical to that of 'selector'.
#'
#' @section Concerned Geo Time Series:
#'
#' FETCH selects Geo Time Series according to:
#'
#' + The selectors parameter, which is a list of selector.
#' + If selectors is not found, FETCH uses the selector parameter which is a single selector.
#' + If selector is not found, FETCH uses both class and labels parameters.
#'
#' @section Time window:
#'
#' FETCH begins from the newest value and stop when the oldest value is collected.
#' Thus, end must be defined in your request and defines the newest included value in your time window.
#' If end is anterior to your oldest value, the result will be empty (no Geo Time Series).
#' The span of the time window ending at end is then defined according to:
#'
#' + The timespan parameter.
#' + If timespan is not defined, FETCH collects a maximum of count point.
#' + If count is not defined, FETCH determines timespan with start.
#' If start is more recent than end, end and start are permuted internally.
#' Be careful, this means end is included but start is excluded from the time window.
#'
#' @seealso [wrp_find()]
#'
#' @references <https://www.warp10.io/doc/FETCH>
#'
#' @keywords gts
#'
#' @export
#'
wrp_fetch <- function(wrp_con, class = "~.*", labels = NULL, end = "ws:NOW", start = NULL, count = NULL,
                      timespan = NULL, selector = NULL, selectors = NULL) {
  assert_token(wrp_con$get_token())
  if (is.null(start) && is.null(timespan) && is.null(count)) count <- 1
  params <- purrr::compact(list(
    token     = "ws:$token",
    class     = class,
    labels    = labels %||% "ws:{}",
    start     = start,
    end       = end,
    count     = count,
    timespan  = timespan,
    selector  = selector,
    selectors = selectors
  ))
  script <- paste(sanitize(params), "FETCH")
  add_stack(wrp_con, script, "lgts")
  return(wrp_con)
}
