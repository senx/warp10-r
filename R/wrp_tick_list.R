#' Tick list
#'
#' Pushes onto the stack the list of ticks of a Geo Time Series™ or encoder.
#' The ticks appear in the order in which they are found.
#'
#' If a list of Geo Time Series™ is given,
#' the result is a list of list of ticks as if the TICKLIST function was applied to each
#' Geo Time Series™ separately.
#'
#' @inheritParams documentation
#'
#' @examples
#'
#' df <- data.frame(tick = 1:10, value = rnorm(10))
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_tick_list() %>%
#'   wrp_exec()
#' @seealso [wrp_ticks()]
#'
#' @export
#'
wrp_tick_list <- function(wrp_con) {
  return_object <- list(gts = "list", lgts = "list", encoder = "list", lencoder = "list")
  add_stack(wrp_con, "TICKLIST", return_object = return_object)
}
