#' Update
#'
#' Pushes Geo Time Series™ data to the Warp 10 instance.
#'
#' The UPDATE function allows you to push data directly from your WarpScript code
#' without having to retrieve the data and use the /update endpoint.
#'
#' The GTS or Encoder instances to push MUST have a non empty name and in the
#' case of Geo Time Series™ MUST have been renamed (to avoid pushing data
#' by mistake which could override existing data you just retrieved).
#'
#' @inheritParams documentation
#' @param token A valid token used to write GTS
#'
#' @export
#'
#' @seealso [wrp_meta()]
#'
wrp_update <- function(wrp_con, token = Sys.getenv("ML_WARP10_WRITE_TOKEN")) {
  script        <- glue::glue("{sanitize(token)} UPDATE")
  return_object <- list(
    gts      = NULL,
    lgts     = NULL,
    encoder  = NULL,
    lencoder = NULL
  )
  add_stack(wrp_con, script = script, return_object = return_object)
}
