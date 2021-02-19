#' At index
#'
#' The ATINDEX function consumes a Geo Time Series™ or a list thereof from the stack,
#' looks at its index-th point and put on the stack a list with the timestamp, longitude,
#' latitude, elevation and value for the index-th point of the GTS.
#'
#' Since 2.1, the index can be negative. In this case the effective index is index + size of the GTS.
#'
#' @inheritParams documentation
#' @param index Index of the tick
#'
#' @seealso [wrp_at_tick()]]
#'
#' @export
#'
#' @references <https://www.warp10.io/doc/ATINDEX>
#'
#' @examples
#'
#' df <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test") %>%
#'   wrp_relabel("label0" = "42", "label1" = "foo") %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_at_index(4) %>%
#'   wrp_exec()
wrp_at_index <- function(wrp_con, index) {
  add_stack(wrp_con, paste(index, "ATINDEX"), list(gts = "data", lgts = "ldata"))
}
