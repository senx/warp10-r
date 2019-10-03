#' Set Script
#'
#' Add command to a Warp 10 script.
#'
#' @inheritParams documentation
#'
#' @export
#'
set_script <- function(wrp_con, script = "") {
  wrp_con$set_script(script)
}
