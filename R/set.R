#' Set Script
#'
#' Add command to a Warp 10 script.
#'
#' @inheritParams documentation
#' @param script A script to be set as a string.
#' @param consume What object(s) the warpscript is consuming from the stack.
#' @param add What object the warpscript is returning in the stack. If NULL,
#'   consuming object is automatically determined by the object.
#'
#' @export
#' @examples
#' wrp_connect() %>%
#'   set_script(list(a = 1)) %>%
#'   set_script("toto")
#'
set_script <- function(wrp_con, script = "", consume = NULL, add = "auto") {
  # For compatibility reasons
  if (!is.null(add) && length(add) == 1 && add == "auto") {
    cl <- class(script)
    if (length(script) == 1 & cl != "list") {
      if (cl == "character") {
        add <- "string"
      } else if (cl == "numeric" | cl == "integer") {
        add <- "numeric"
      }
    }
    else {
      script <- as.list(script)
      if (is.null(names(script)) || any(names(script) == "")) {
        add <- "list"
      } else {
        add <- "map"
      }
    }
    script <- sanitize(script)
  }
  wrp_con$set_script(script)
  wrp_con$add_stack(add, consume)
  wrp_con
}
