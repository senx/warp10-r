#' First tick
#'
#' The FIRSTTICK function pushes on the stack the timestamp of the first tick of
#' the Geo Time Series™ on top of the stack.
#'
#' When applied to a list of GTS, FIRSTTICK will return the lowest first tick found across all
#' Geo Time Series™.
#'
#' @inheritParams documentation
#'
#' @seealso [wrp_at_tick()], [wrp_last_tick()]
#'
#' @export
#'
#' @references <https://www.warp10.io/doc/FIRSTTICK>
#'
#' @examples
#'
#' df <- data.frame(tick = c(seq(100, 500, by = 100), 100, 200), value = c(10:6, 10, 9))
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_first_tick() %>%
#'   wrp_exec()
wrp_first_tick <- function(wrp_con) {
  add_stack(wrp_con, "FIRSTTICK", list(gts = "long", lgts = "long"))
}
