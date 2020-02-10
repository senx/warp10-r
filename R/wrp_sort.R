#' Sort
#'
#' Sorts a Geo Time Series™ or each GTS of a list of GTS in ascending tick order.
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(tick = 10:1, value = runif(10))
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_sort() %>%
#'   wrp_exec()
wrp_sort <- function(wrp_con) {
  add_stack(wrp_con, "SORT", return_object = list(gts = "gts", lgts = "lgts"))
}
