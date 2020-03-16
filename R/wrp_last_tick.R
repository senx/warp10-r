#' Last tick
#'
#' The LASTTICK function pushes on the stack the newest tick of the Geo Time Series™ on top of the stack or,
#' if the GTS is bucketized, its last bucket.
#'
#' When applied to a list of GTS, LASTTICK will return the greatest last tick found across all Geo Time Series™.
#'
#' If the GTS does not have values, Long.MIN_VALUE is pushed.
#'
#'
#' @inheritParams documentation
#'
#' @seealso [wrp_at_tick()], [wrp_first_tick()]
#'
#' @export
#'
#' @references <https://www.warp10.io/doc/LASTTICK>
#'
#' @examples
#'
#' df <- data.frame(tick = c(seq(100, 500, by = 100), 100, 200), value = c(10:6, 10, 9))
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_last_tick() %>%
#'   wrp_exec()
wrp_last_tick <- function(wrp_con) {
  add_stack(wrp_con, "LASTTICK", list(gts = "long", lgts = "long"))
}
