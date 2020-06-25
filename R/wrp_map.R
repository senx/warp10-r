#' Map
#'
#' The MAP framework applies a function on values of a Geo Time Series™ that
#' fall into a sliding window.
#'
#' The MAP framework is designed to use an existing MAPPER or a custom
#' MACROMAPPER.
#' It could also accept a macro: the current window is passed as a GTS to the
#' macro.
#'
#' @inheritParams documentation
#' @param mapper Mapper function to apply
#' @param pre Width of the sliding window before the current tick.
#' This parameter is interpreted as a number of ticks if its value is positive,
#' and as a number of time units if its value is negative. A value of 0 means
#' the sliding window does not cover the past. Use the special values
#' max.tick.sliding.window or max.time.sliding.window to expand the window the
#' farthest into the past. Defaults to 0. When selecting a number of ticks, the
#' value of this parameter is limited to 2**32-1 even if a greater value was
#' provided.
#' @param post Width of the sliding window after the current tick. Values have
#' the same semantics as for the pre parameter. A value of 0 means the sliding
#' window does not cover the future. Use the special values
#' max.tick.sliding.window or max.time.sliding.window to expand the window the
#' farthest into the future. Defaults to 0.
#' @param occurences Limit the number of computations to that number.
#' If the value is 0, compute a value for each tick of the input
#' Geo Time Series™. If it is non-negative, compute that many values starting
#' from the oldest to the most recent tick. In the other case, do that many
#' computations in the reverse order. This is useful when you are interested
#' in a single result, like the max or sum of all values. Defaults to 0. When
#' selecting a number of ticks, the value of this parameter is limited to
#' 2**32-1 even if a greater value was provided.
#' @param step The step size in number of ticks. The mapper begins at the
#' oldest tick and goes to the tick step after the current one and so on.
#' Defaults to 1.
#' @param override_tick If set to true, the tick value returned by the mapper
#' is used to update the current tick, else it is ignored and the original tick
#' is kept. Defaults to false.
#'
#' @keywords mapper,framework
#'
#' @references <https://www.warp10.io/doc/MAP>
#'
#' @export
#'
wrp_map <- function(wrp_con, mapper, pre = 0, post = 0, occurences = 0, step = 1, override_tick = FALSE) {
  script <- paste(
    sanitize(list("ws:SWAP", glue::glue("ws:mapper.{mapper}"), pre, post, occurences, step, override_tick)),
    "MAP"
  )
  return_object <- list(
    gts  = "lgts",
    lgts = "lgts"
  )
  add_stack(wrp_con, script, return_object)
}
