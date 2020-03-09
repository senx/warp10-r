#' Time shift
#'
#' Shifts the ticks of Geo Time Series™ instances by a given amount.
#'
#' @inheritParams documentation
#' @param offset Offset to apply to the ticks
#'
#' @export
#'
#' @keywords gts
#'
#' @references <https://www.warp10.io/doc/TIMESHIFT>
#'
#' @examples
#'
#' df <- data.frame(tick = 1:100, value = 1:100)
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_time_shift(10) %>%
#'   wrp_exec()
wrp_time_shift <- function(wrp_con, offset) {
  offset <- sanitize(offset)
  add_stack(wrp_con, paste(offset, "TIMESHIFT"), list(gts = "gts", lgts = "lgts"))
}
