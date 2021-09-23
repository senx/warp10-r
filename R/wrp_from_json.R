#' JSON->
#'
#' The JSON-> function parses a string as JSON from the top of the stack and pushes the result onto the stack.
#'
#' @inherit documentation
#'
#' @keywords conversion
#'
#' @examples
#' wrp_connect() %>%
#'   set_script('[{"menu": {"id": "file","value": "File"}}]') %>%
#'   wrp_from_json() %>%
#'   wrp_get(0) %>%
#'   set_script('{"menu": {"id": "file","value": "File"}}') %>%
#'   wrp_from_json() %>%
#'   wrp_exec()
#' @export
#'
wrp_from_json <- function(wrp_con) {
  return_object <- list(
    string = "list"
  )
  add_stack(wrp_con, "JSON->", return_object)
}