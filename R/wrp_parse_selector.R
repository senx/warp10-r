#' Parse Selector
#'
#' The PARSESELECTOR function parses a GTS Selector (string) and pushes the class selector and
#' labels selector on the stack.
#'
#' Combined with [wrp_to_selector()], it can be used to build a selector from a subset of GTS.
#'
#' @inheritParams documentation
#' @param selector String selector.
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   wrp_parse_selector('io.senx.tutorial.sensors.temperature{sensorId=01,sensortype~numeric.*}') %>%
#'   wrp_exec()
#'
wrp_parse_selector <- function(wrp_con, selector) {
  script <- glue::glue("{sanitize(selector)} PARSESELECTOR")
  set_script(wrp_con, script = script, consume = NULL, add = list("map", "map"))
}
