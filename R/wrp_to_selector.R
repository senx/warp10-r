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
#'   wrp_relabel(c("label0", 42, "label1", "foo")) %>%
#'   wrp_add_value(100, value = 10) %>%
#'   wrp_add_value(200, value = 9) %>%
#'   wrp_add_value(300, value = 8) %>%
#'   wrp_store("gts1") %>%
#'   set_script("$gts1", add = "gts") %>%
#'   wrp_clone() %>%
#'   wrp_relabel(c("label0", "33")) %>%
#'   wrp_store("gts2") %>%
#'   wrp_drop() %>%
#'   set_script("[ $gts1 $gts2 ]", add = "gtslist")
#' wrp_to_selector()
#' @export
#'
wrp_to_selector <- function(wrp_con) {
  script  <- glue::glue("TOSELECTOR")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]
  return  <- switch(
    consume,
    gts         = "selector",
    gtslist     = "selectorlist",
    encoder     = "selector",
    encoderlist = "selectorlist"
  )

  set_script(wrp_con, script = script, consume = consume, add = return)

  return(wrp_con)
}
