#' Dup
#'
#' The DUP function duplicates the element on the top of the stack, it doesn’t copy the content.
#' If you apply DUP to a GTS you will have two references to the same GTS object on the stack.
#'
#' If you want to make a deep copy of a GTS, i.e. if you want copy the content
#' and not only the reference, you can use the CLONE function.
#'
#' @inheritParams documentation
#'
#' @examples
#'
#' wrp_connect() %>%
#'   set_script(123, add = "numeric") %>%
#'   wrp_dup() %>%
#'   wrp_exec()
#' @export
#'
wrp_dup <- function(wrp_con) {
  script        <- "DUP"
  return_object <- list(any = c("any", "any"))
  add_stack(wrp_con, script, return_object)
}
