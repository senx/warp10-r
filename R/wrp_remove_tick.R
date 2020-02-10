#' Remove tick
#'
#' The REMOVETICK function transforms a Geo Time Series™ by removing all occurrences of a tick or
#' ticks.
#'
#' @inheritParams documentation
#' @param tick Tick (timestamp) to remove.
#'   If NULL, takes element on the stack.
#'
#' @examples
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test") %>%
#'   wrp_add_value(0, 0) %>%
#'   wrp_add_value(1, 1) %>%
#'   wrp_add_value(2, 2) %>%
#'   wrp_remove_tick(1) %>%
#'   wrp_exec()
#' @export
#'
wrp_remove_tick <- function(wrp_con, tick = NULL) {
  if (!is.null(tick)) {
    tick <- parse_timestamp(tick)
  } else {
    add_stack(wrp_con, script = NULL, list("list" = NULL, "numeric" = NULL, map = NULL))
  }

  add_stack(wrp_con, script = paste(tick, "REMOVETICK"), return_object = list(gts = "gts"))
}
