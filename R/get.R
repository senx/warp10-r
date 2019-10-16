#' Get Endpoint and Token
#'
#' Get endpoint and token from R options
#'
#' @export
#'
get_endpoint <- function() {
  getOption("warp10")[["endpoint"]]
}

#' @rdname get_endpoint
#'
#' @export
#'
get_token <- function() {
  getOption("warp10")[["token"]]
}

#' @inheritParams documentation
#'
#' @export
#'
get_script <- function(wrp_con) {
  wrp_con$get_script()
}

#' @inheritParams documentation
#'
#' @export
#'
get_stack <- function(wrp_con) {
  wrp_con$get_stack()
}
