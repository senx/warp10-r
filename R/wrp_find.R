#' Find
#'
#' The FIND function finds Geo Time Series™ labels and attributes of GTS, not their values.
#' It is the fastest way to read attributes of GTS.
#' FIND ask only directory component of Warp 10, while FETCH first ask directory, then ask store to read GTS values.
#'
#' @inheritParams documentation
#'
#' @return A data.frame if the request is exectued, a character string containing the Warpscript otherwise.
#'
#' @keywords gts
#'
#' @references <https://www.warp10.io/doc/FIND>
#'
#' @seealso [wrp_find_stats()], [wrp_find_sets()]
#'
#' @export
#'
wrp_find <- function(wrp_con, class = "~.*", labels = NULL) {
  assert_token(wrp_con$get_token())
  params <- sanitize(list("ws:$token", class, labels %||% "ws:{}"))
  add_stack(wrp_con, paste(params, "FIND"), "lgts")
}
