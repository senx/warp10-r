#' Get Endpoint and Token
#'
#' Get endpoint and token from R options
#'
#' @param endpoint endpoint
#'
#' @export
#'
get_endpoint <- function() {
  system_endpoint <- Sys.getenv("WRPENDPOINT")
  if (system_endpoint != "") return(system_endpoint)
  getOption("warp10")[["endpoint"]]
}

#' @rdname get_endpoint
#'
#' @export
#'
get_token <- function(endpoint = get_endpoint()) {
  system_token <- Sys.getenv("WRPTOKEN")
  if (system_token != "") return(system_token)
  endpoint_option <- get_endpoint()
  if (!is.null(endpoint_option) && endpoint == endpoint_option) return(getOption("warp10")[["token"]])
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
  wrp_con$get_script()
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
