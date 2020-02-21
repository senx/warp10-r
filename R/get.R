#' Get Endpoint and Token
#'
#' Get endpoint and token from R options
#'
#' @export
#'
get_endpoint <- function() {
  Sys.getenv("ML_WARP10_URL")
}

#' @rdname get_endpoint
#'
#' @export
#'
get_token <- function() {
  Sys.getenv("ML_WARP10_READ_TOKEN")
}

#' Get scripts
#'
#' Returns a scripts to be send to endpoint.
#'
#' @inheritParams documentation
#'
#' @export
#'
get_script <- function(wrp_con) {
  cat(wrp_con$get_script())
}

#' Get stack
#'
#' Returns objects of the stack.
#'
#' @inheritParams documentation
#'
#' @export
#'
get_stack <- function(wrp_con) {
  wrp_con$get_stack()
}
