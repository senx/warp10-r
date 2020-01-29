#' Sort
#'
#' Sorts a Geo Time Series™ or each GTS of a list of GTS in ascending tick order.
#'
#' @export
#'
#' @examples
#'
#' df <- data.frame(tick = 10:1, value = runif(10))
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_sort() %>%
#'   wrp_exec()
wrp_sort <- function(wrp_con) {
  script  <- glue::glue("SORT")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]
  return  <- switch(
    consume,
    gts = "gts",
    lgts = "lgts",
    stop(glue::glue("`wrp_sort` can't consume {consume}."))
  )

  set_script(wrp_con, script = script, consume = consume, add = return)

  return(wrp_con)
}
