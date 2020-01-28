#' Unwrap
#'
#' Unwraps packed Geo Time Series™ instances.
#'
#' @inheritParams documentation
#'
#' @examples
#' wrp_connect() %>%
#'   set_script("60V.5k.L.0N.5k..KV.N5GyA1.........0nNL7O4W..rXE6gwV....Lm.3G..") %>%
#'   wrp_unwrap() %>%
#'   wrp_exec()
#' @export
#'
wrp_unwrap <- function(wrp_con) {
  script     <- glue::glue("UNWRAP")
  stack      <- get_stack(wrp_con)
  last_stack <- stack[[length(stack)]]
  return     <- switch(
    last_stack,
    string = "gts",
    bytes  = "gts",
    list   = "lgts",
    stop(glue::glue("`UNWRAP` cannot consume {last_stack}"))
  )

  set_script(wrp_con, script = script, consume = last_stack, add = return)
  return(wrp_con)
}
