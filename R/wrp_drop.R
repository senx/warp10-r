#' Drop
#'
#' The DROP function removes the element at the top of the stack.
#'
#' @inheritParams documentation
#'
#' @examples
#'
#' wrp_connect() %>%
#'   set_script("foo") %>%
#'   set_script("bar") %>%
#'   wrp_drop() %>%
#'   wrp_exec()
#' @export
#'
wrp_drop <- function(wrp_con) {
  script  <- glue::glue("DROP")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]

  set_script(wrp_con, script = script, consume = consume, add = list())

  return(wrp_con)
}
