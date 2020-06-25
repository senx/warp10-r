#' Dedup
#'
#' Removes duplicate ticks.
#'
#' The DEDUP function consumes a Geo Time Series or a LIST of Geo Time Series from the top of the stack
#' and pushes back the Geo Time Series™ with deduplicated ticks.
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_dedup <- function(wrp_con) {
  return_object <- list(
    gts = "gts",
    lgts = "lgts"
  )
  add_stack(wrp_con, "DEDUP", return_object)
}
