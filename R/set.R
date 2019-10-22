#' Set Script
#'
#' Add command to a Warp 10 script.
#'
#' @inheritParams documentation
#' @param script A script to be set as a string.
#'
#' @export
#'
set_script <- function(wrp_con, script = "") {
  wrp_con$set_script(script)
}
