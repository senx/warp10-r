#' Meta
#'
#' Stores the attributes of a list of Geo Time Series™ in the Warp 10 backend.
#'
#' This function expects on top of the stack a write token which will be used
#' to authenticate with the Warp 10 backend and a Geo Time Series™ or a list
#' thereof.
#'
#' Every Geo Time Series™ to which META is applied must have a non empty name
#' and attributes (possibly empty).
#'
#' @inheritParams documentation
#' @param write_token GTS list with new or modified attributes
#'
#' @export
#'
#' @keywords gts
#'
#' @seealso [wrp_set_attributes()], [wrp_update()]
#'
wrp_meta <- function(wrp_con, write_token = Sys.getenv("ML_WARP10_WRITE_TOKEN")) {
  script        <- paste(sanitize(write_token), "META")
  return_object <- list(lgts = NULL)
  add_stack(wrp_con, script = script, return_object = return_object)
}
