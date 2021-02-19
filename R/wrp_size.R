#' Size
#'
#' Returns the size of the input parameter.
#'
#' The SIZE function computes the size of a LIST, MAP, GTS or ENCODER (number of values), STRING,
#' byte array of GEOSHAPE (number of cells).
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#' df <- data.frame(tick = seq(100, 1000, by = 100), value = 10:1)
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test name") %>%
#'   wrp_relabel("label0" = "42", "label1" = "foo") %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_clone() %>%
#'   wrp_size() %>%
#'   set_script("{ 'label0' '42' 'label1' 'foo' }", add = "map") %>%
#'   wrp_dup() %>%
#'   wrp_size() %>%
#'   set_script("[ 'label0' '42' 'label1' 'foo' ]", add = "list") %>%
#'   wrp_dup() %>%
#'   set_script("one %25") %>%
#'   wrp_dup() %>%
#'   wrp_size() %>%
#'   wrp_exec()
#' @references <https://www.warp10.io/doc/SIZE>
#'
#' @keywords strings lists maps gts geo
#'
#' @seealso [wrp_new_gts()]
#'
wrp_size <- function(wrp_con) {
  return_object <- list(
    list     = "long",
    map      = "long",
    gts      = "long",
    string   = "long",
    bytes    = "long",
    geoshape = "long",
    lgts     = "long"
  )
  add_stack(wrp_con, script = "SIZE", return_object = return_object)
}
