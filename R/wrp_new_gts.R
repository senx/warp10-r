#' New GTS
#'
#' The NEWGTS function pushes onto the stack an empty Geo Time Series instance.
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   con <- wrp_connect()
#'   con %>%
#'     clear_script() %>%
#'     wrp_new_gts()
#' }
#'
wrp_new_gts <- function(wrp_con) {
  wrp_con$set_script("NEWGTS")
}
