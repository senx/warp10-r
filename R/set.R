#' Set Script
#'
#' Add command to a Warp 10 script.
#'
#' @inheritParams documentation
#' @param script A script to be set as a string.
#' @param consume What object(s) the warpscript is consuming from the stack.
#' @param add What object the warpscript is returning in the stack.
#'
#' @export
#'
set_script <- function(wrp_con, script = "", consume = list(), add = "string") {
  if (!is.null(add)) {
    if (length(add) == 1) {
      if (add == "string") {
        script <- sanitize(script)
      }
    }
  }
  wrp_con$set_script(script)
  wrp_con$add_stack(add, consume)
  wrp_con
}
