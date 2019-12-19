#' Store
#'
#' The STORE function stores a value in a symbol.
#'
#' @inheritParams documentation
#' @param symbol Name of the symbol to modify.
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("42", add = "long") %>%
#'   wrp_store("foo") %>%
#'   wrp_exec()
#' @export
#'
wrp_store <- function(wrp_con, symbol) {
  script  <- glue::glue("{sanitize(symbol)} STORE")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]
  set_script(wrp_con, script = script, consume = consume, add = list())

  return(wrp_con)
}
