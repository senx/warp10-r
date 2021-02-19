#' To selector
#'
#' The TOSELECTOR function takes a Geo Time Series™ or Geo Time Series™ LIST from the top of the stack and,
#' for each encountered GTS, replace it with a selector which would select it.
#'
#' This selector can be used as input of PARSESELECTOR for a FETCH.
#'
#' @inheritParams documentation
#'
#' @examples
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test name") %>%
#'   wrp_relabel("label0" = 42, "label1" = "foo") %>%
#'   wrp_add_value(100, value = 10) %>%
#'   wrp_add_value(200, value = 9) %>%
#'   wrp_add_value(300, value = 8) %>%
#'   wrp_store("gts1") %>%
#'   set_script("$gts1", add = "gts") %>%
#'   wrp_clone() %>%
#'   wrp_relabel("label0" = "33") %>%
#'   wrp_store("gts2") %>%
#'   wrp_drop() %>%
#'   set_script("[ $gts1 $gts2 ]", add = "lgts") %>%
#'   wrp_to_selector()
#' @export
#'
wrp_to_selector <- function(wrp_con) {
  return_object  <- list(
    gts         = "selector",
    lgts        = "list",
    encoder     = "selector",
    lencoder    = "list"
  )

  add_stack(wrp_con, script = "TOSELECTOR", return_object = return_object)
}
