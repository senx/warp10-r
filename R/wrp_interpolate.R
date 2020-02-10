#' Interpolate
#'
#' The INTERPOLATE function fills gaps in a bucketized Geo Time Series™ instance with by interpolating linearly.
#'
#' This function has no effect on non bucketized GTS instances.
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(tick = seq(100, 500, by = 100), value = 10:6)
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_bucketize("mean", 500, 50, 0) %>%
#'   wrp_interpolate() %>%
#'   wrp_sort() %>%
#'   wrp_exec()
wrp_interpolate <- function(wrp_con) {
  add_stack(wrp_con, "INTERPOLATE", return_object = list(gts = "gts", lgts = "lgts"))
}
