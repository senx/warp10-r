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

#' @inheritParams wrp_con
#'
#' @export
#'
get_script <- function(wrp_con) {
  wrp_con$get_script()
}
