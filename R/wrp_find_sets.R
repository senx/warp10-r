#' Find Sets
#'
#' The FINDSETS function returns sets of values for the classes,
#' labels and attributes of the Geo Time Series™ which matched the selection criteria.
#'
#' Those results can be used for example to update a UI with lists of possible values for
#' labels when exploring a corpus of GTS.
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @seealso [wrp_find()], [wrp_find_stats()]
#'
#' @references <https://www.warp10.io/doc/FINDSETS>
#'
#' @keywords gts
#'
wrp_find_sets <- function(wrp_con, class = "~.*", labels = NULL) {
  assert_token(wrp_con$get_token())
  labels <- labels_to_string(labels)
  script <- glue::glue(
    "[ $token '{class}' {{ {labels} }} ] FINDSETS"
  )
  add_stack(wrp_con, script, c("list", "list", "list"))
}
