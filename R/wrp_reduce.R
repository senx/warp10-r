#' Reduce
#'
#' The REDUCE framework groups Geo Time Series™ by equivalence classes based on
#' label values and applies a function on each equivalence class of
#' Geo Time Series™, considering the values of each tick and producing one GTS
#' per equivalence class.
#'
#' As the selected reducer function is applied tick by tick, it is usually wise
#' to BUCKETIZE the GTS first with a common value of lastbucket.
#'
#' The labels whose values are common to all the GTS in an equivalence class
#' will be retained.
#'
#' Since 2.1 you can make REDUCE override the GTSs ticks. This usage in mainly
#' to be used in conjuction with MACROREDUCER.
#'
#' @inheritParams documentation
#' @param labels List of label names to consider for creating equivalence
#' classes. If the list is empty all Geo Time Series™ will end up in the same
#' equivalence class. If the labels parameter NULL, all labels will be
#' considered.
#' @param reducer Reducer function to apply.
#' @param override_tick Boolean, if true allows the reducer to modify the ticks
#' of the GTSs. If not set, the reducer cannot modify the ticks.
#'
#' @keywords reducer, framework
#'
#' @references <https://www.warp10.io/doc/REDUCE>
#'
#' @export
#'
wrp_reduce <- function(wrp_con, reducer, labels = NULL, override_ticks = FALSE) {
  script <- paste(
    sanitize(list("ws:SWAP", as.list(labels), glue::glue("ws:reducer.{reducer}"), override_ticks)),
    "REDUCE"
  )
  return_object <- list(lgts = "lgts")
  add_stack(wrp_con, script, return_object)
}
