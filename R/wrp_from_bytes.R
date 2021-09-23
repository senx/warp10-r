#' BYTES->
#'
#' Converts a bytes array into a string with given a charset and put the string on top of the stack.
#'
#' Charset could be any supported by your java platform. Example: UTF-8 US-ASCII ISO-8859-1 Cp852...
#'
#' Default to UTF-8.
#'
#' @param charset Charset to use.
#'
#' @inherit documentation
#'
#' @keywords strings conversion
#'
#' @seealso `wrp_from_b64`
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("aGVsbG8gd29ybGQ=") %>%
#'   wrp_from_b64() %>%
#'   wrp_from_bytes() %>%
#'   wrp_exec()
#' @export
#'
wrp_from_bytes <- function(wrp_con, charset = "UTF-8") {
  return_object <- list(
    bytes = "string"
  )
  script <- paste(sanitize(charset), "BYTES->")
  add_stack(wrp_con, script, return_object)
}