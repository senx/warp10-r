#' Helper function
#'
#' Helper function to assert that values are correctly sets.
#'
#' @param endpoint
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

#' @rdname assert_endpoint
#'
#' @param token
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
