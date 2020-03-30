#' Linear Regression
#'
#' Compute the simple linear regression parameters alpha (y-intercept) and beta (line slope)
#' for the given numerical Geo Time Series™.
#'
#' @inheritParams documentation
#'
#' @references <https://www.warp10.io/doc/LR>
#'
#' @keywords gts statistics
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(tick = 1:500, value = NA)
#' df[["value"]][1] <- rnorm(1, sd = 3)
#' for (j in seq_len(nrow(df))[-1]) {
#'   df[["value"]][j] <-  df[["value"]][j - 1] - rnorm(1, sd = 3) + 0.5
#' }
#'
#' lr <- wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("random") %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_lr() %>%
#'   wrp_exec()
#'
#' ggplot(df, aes(tick, value)) +
#'   geom_line() +
#'   geom_abline(slope = lr[[1]], intercept = lr[[2]])
wrp_lr <- function(wrp_con) {
  add_stack(wrp_con, "LR", list(gts = list("double", "double")))
}
