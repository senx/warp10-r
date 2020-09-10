#' Reverse
#'
#' Reverses the order of the elements of the list or the string.
#'
#' The REVERSE function inverts the order of the elements of the list or the
#' string on the top of the stack. Beware, REVERSE do not create a new object.
#' Use CLONEREVERSE if you want to keep input.
#'
#' Applied on a byte array, REVERSE reverses the endianness.
#'
#' @keywords lists, strings, binary
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   set_script(c("eins", "zwei", "drei")) %>%
#'   wrp_reverse() %>%
#'   set_script("swap tac") %>%
#'   wrp_reverse() %>%
#'   wrp_exec()
#'
wrp_reverse <- function(wrp_con) {
  return_object <- list(
    list  = "list",
    string = "string"
  )
  add_stack(wrp_con, "REVERSE", return_object)
}
