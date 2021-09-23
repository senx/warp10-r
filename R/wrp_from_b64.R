#' B64->
#'
#' Decodes the base64 STRING content on top of the stack.
#'
#' @inherit documentation
#'
#' @keywords strings conversion
#'
#' @seealso `wrp_from_bytes`
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("aGVsbG8gd29ybGQ=") %>%
#'   wrp_from_b64() %>%
#'   wrp_from_bytes() %>%
#'   wrp_exec()
#' @export
#'
wrp_from_b64 <- function(wrp_con) {
  return_object <- list(
    string = "bytes"
  )
  add_stack(wrp_con, "B64->", return_object)
}