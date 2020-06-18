#' Delete
#'
#' The DELETE function is used to delete a set of GTS from a Warp 10 platform.
#' A delete endpoint must be set on the Warp 10 configuration.
#'
#' Delete expects 5 parameters on top of the stack: the write token of the
#' Warp 10 application, a string corresponding to a gts selector, a start and
#' a end timestamp and finally the number of GTS expected to be deleted.
#'
#' For safety reasons DELETE will first perform a dryrun call to the /delete
#' endpoint to retrieve the number of GTS which would be deleted by the call.
#' If this number is above the expected number provided by the user the actual
#' delete will not be performed and instead an error will be raised.
#'
#' Delete will push as a result the number of GTS really deleted.
#'
#' If both end_timestamp and start_timestamp are NULL,
#' Warp 10 will perform a delete all. FETCH won't find anything after a delete
#' all.
#'
#' @inheritParams documentation
#' @param write_token A writtable token
#' @param selector GTS selector
#' @param start,end Timestamp or NULL
#' @param count Number of GTS expected to be deleted
#'
#' @keywords gts
#'
#' @export
#'
wrp_delete <- function(wrp_con, selector, write_token = Sys.getenv("ML_WARP10_WRITE_TOKEN"),
                       start = NULL, end = NULL, count = 0) {
  script <- paste(
    sanitize(write_token),
    sanitize(selector),
    sanitize(start),
    sanitize(end),
    count,
    "DELETE"
  )
  add_stack(wrp_con, script = script, return_object = "numeric")
}
