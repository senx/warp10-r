#' Get
#'
#' Retreive a value in a MAP or a list.
#'
#' The GET function consumes on the top of the stack a list, a byte array, or a map, and the index (or the key),
#' then put the result on the stack.
#'
#' If key is not found, it returns NULL on the stack.
#'
#' If index is out of bound, GET raises an error.
#'
#' Since 2.1, the index can be negative. In this case the effective index is index + size of the LIST or BYTE ARRAY.
#'
#' Since 2.1, GET can operate recursively on nested lists. In this case, the index is a list.
#'
#' @inheritParams documentation
#' @param index Depending on map construction, could be a number, a string, a list, another map, a GTS, a boolean.
#'
#' @examples
#' con <- wrp_connect()
#'
#' con %>%
#'   set_script("{ 'foo' 42 'bar' true }") %>%
#'   wrp_get("foo") %>%
#'   wrp_exec()
#'
#' con %>%
#'   set_script("[ 3 12 15 ]") %>%
#'   wrp_get(0) %>%
#'   wrp_exec()
#'
#' con %>%
#'   set_script("{ 'foo' 42 'bar' true }") %>%
#'   wrp_get(33) %>%
#'   wrp_exec()
#' @export
#'
wrp_get <- function(wrp_con, index) {
  if (typeof(index) == "character") index <- sanitize(index)
  script  <- glue::glue("{index} GET")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]
  return  <- switch(
    consume,
    map  = "map",
    list = "value",
    lgts = "gts"
  )

  set_script(wrp_con, script = script, consume = consume, add = return)

  return(wrp_con)
}
