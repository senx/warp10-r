#' Unbucketize
#'
#' Transforms a bucketized Geo Time Series™ into a non bucketized one.
#' Note that this function transforms the original GTS, it does not clone it.
#'
#' @inheritParams documentation
#'
#' @keywords gts
#'
#' @references <https://www.warp10.io/doc/UNBUCKETIZE>
#'
#' @seealso [wrp_bucketize()]
#'
#' @export
#'
#' @examples
#' df <- data.frame(tick = 1:100, value = TRUE)
#' wrp_connect() %>%
#' wrp_new_gts() %>%
#' wrp_add_value_df(df) %>%
#' wrp_bucketize("last", span = 10) %>%
#' wrp_get(0) %>%
#' wrp_unbucketize() %>%
#' wrp_exec()
#'
wrp_unbucketize <- function(wrp_con) {
  return_object <- list(
    gts  = "gts",
    lgts = "lgts"
  )
  add_stack(wrp_con, "UNBUCKETIZE", return_object)
}
