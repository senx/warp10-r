#' Last Activity
#'
#' Extracts the timestamp of last activity recorded in the Geo Time Series™ metadata.
#'
#' The timestamp is expressed as platform time units elapsed since the Unix Epoch.
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_last_activity <- function(wrp_con) {
  add_stack(wrp_con, "LASTACTIVITY", list("gts" = "lastactivity"))
}
