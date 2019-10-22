#' Assert Endpoint
#'
#' Check that endpoint is available.
#'
#' @family assert
#'
#' @param endpoint A string defining the endpoint of the Warp 10 database.
#'
assert_endpoint <- function(endpoint) {
  if (is.null(endpoint)) {
    stop(
      paste(
        "Endpoint is required to fetch data from Warp 10 database.",
        "Endpoint can be set as an R option with `options(\"warp10\" = c(endpoint = \"...\"))`.",
        "Or it can be provided directly."
      )
    )
  }
}

#' Assert Token
#'
#' Check that token is available.
#'
#' @family assert
#'
#' @param token A string defining the token used to fetch or update data.
#'
assert_token <- function(token) {
  if (is.null(token)) {
    stop(
      paste(
        "Token is required to fetch data from endpoint.",
        "Token can be set as an R option with `options(\"warp10\" = c(token = \"...\"))`.",
      )
    )
  }
}
