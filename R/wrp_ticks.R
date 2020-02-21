#' Ticks
#'
#' Pushes onto the stack the sorted list of distinct ticks of a Geo Time Series™.
#' If a list of Geo Time Series™ is given,
#' all the distinct ticks are returned in a single sorted list as if all Geo Time Series™ were merged.
#'
#' This function is not to be confused with [wrp_tick_list()] as this function returns distinct ticks and
#' acts as it merges Geo Time Series™.
#'
#' @inheritParams documentation
#'
#' @examples
#' df <- data.frame(tick = 1:10, value = rnorm(10))
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_ticks() %>%
#'   wrp_exec()
#' @seealso [wrp_tick_list()]
#'
#' @references <https://www.warp10.io/doc/TICKS>
#'
#' @keywords gts
#'
#' @export
#'
wrp_ticks <- function(wrp_con) {
  add_stack(wrp_con, "TICKS", list(gts = "list", lgts = "list"))
}
