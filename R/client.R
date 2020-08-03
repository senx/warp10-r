#' Post Warpscript Code
#'
#' Post warpscript code to a Warp 10 instance and retrieve response as character vector, json,
#' named list or data.table.
#' If outputType is "data.table", only the top of the stack is converted and it must be a list of GTS.
#' If this list is not a singleton, these GTS must have at most one value per timestamp.
#'
#' @param warpscript code or file name ending with .mc2
#' @param endpoint egress endpoint.
#' @return json
#' @export
#' @importFrom httr POST content headers config
#' @importFrom jsonlite minify

post_warpscript <- function(warpscript, endpoint = get_endpoint()) {

  endpoint <- paste0(endpoint, "/exec")
  if (substr(warpscript, nchar(warpscript) - 3, nchar(warpscript)) == ".mc2") {
    warpscript <- readLines(warpscript, warn = FALSE)
  }

  request <- POST(endpoint, body = warpscript, config = config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))

  # retrieve body
  body <- content(request, "text", encoding = "UTF-8")

  # check status and return error or parsed JSON
  if (request$status != 200) {

    # parse error message
    h <- headers(request) # nolint unused variable
    msg <- glue::glue(
      "Status: {request$status}",
      "ERRPR line #{h[['X-Warp10-Error-Line']]}: {h[['X-Warp10-Error-Message']]}"
    )
    stop(msg, call. = FALSE)
  } else {

    body <- gsub("(,*)NaN(,*)", "\\1null\\2", body)

    return(minify(body))
  }
}
