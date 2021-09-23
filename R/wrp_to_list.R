#' To List
#'
#' The `wrp_to_list` function create a list from N elements on the stack.
#' N and the N next elements on the stack are consumed.
#'
#' If N is greater than current stack depth, the function raises an error.
#'
#' @inheritParams documentation
#' @param n The number of elements to take on the top of the stack to build the list.
#'
#' @keywords lists, conversion
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("el1") %>%
#'   set_script(TRUE) %>%
#'   set_script("el4") %>%
#'   set_script(4) %>%
#'   wrp_to_list(4) %>%
#'   wrp_exec()
wrp_to_list <- function(wrp_con, n) {
  return_object <- list(
    any = "list"
  )
  stack       <- get_stack(wrp_con)
  len_stack   <- length(stack)
  if (n > len_stack) stop(glue::glue("must have at least {n} elements on the stack, {N} presents."))
  consume <- stack[len_stack:(len_stack - n)]
  if (all(unlist(consume) == "gts")) {
    add <- "lgts"
  } else {
    add <- "list"
  }
  wrp_con$set_script(glue::glue("{n} ->LIST"))
  wrp_con$add_stack(add, consume = consume)
  return(wrp_con)
}
