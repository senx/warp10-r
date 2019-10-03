#' Execute Warp Script
#'
#' Execute the Warp Script by sending it to the server.
#'
#' @inheritParams documentation
#'
#' @export
#'
wrp_exec <- function(wrp_con) {
  wrp_script <- get_script(wrp_con)
  endpoint   <- wrp_con$get_endpoint()

  res <- postWarpscript(warpscript = wrp_script, endpoint = endpoint)
  if (is.null(res)) {
    return(cat(wrp_script))
  }
  res <- jsonlite::fromJSON(res)
  if (is.character(res)) {
    return(res)
  }
  if (!is.data.frame(res)) {
    res <- res[[1]]
  }
  tibble::as_tibble(res)
}
