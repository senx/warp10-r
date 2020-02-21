#' Flatten
#'
#' The function FLATTEN inspects the top of the stack.
#'
#' If it is a LIST of values, it inspects each value and
#' replaces each value which was a LIST with its content.
#' FLATTEN proceed recursively until all LISTs have been flattened.
#'
#' If it is not a LIST, the function exits and let the stack in the same state.
#'
#' @inheritParams documentation
#'
#' @keywords lists
#'
#' @export
#'
#' @references <https://www.warp10.io/doc/FLATTEN>
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("[ 'a' 'b' 'c' ]", add = "list") %>%
#'   set_script("[ 'd' 'e' [ 'f' 'g' ] ]", add = "list") %>%
#'   set_script("2 ->LIST", consume = c("list", "list"), add = "list") %>%
#'   wrp_flatten() %>%
#'   wrp_exec()
wrp_flatten <- function(wrp_con) {
  add_stack(wrp_con, "FLATTEN", list(list = "list"))
}
