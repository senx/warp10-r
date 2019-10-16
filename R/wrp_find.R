#' Find
#'
#' The FIND function finds Geo Time Series™ labels and attributes of GTS, not their values.
#' It is the fastest way to read attributes of GTS.
#' FIND ask only directory component of Warp 10, while FETCH first ask directory, then ask store to read GTS values.
#'
#' @inheritParams documentation
#'
#' @return A data.frame if the request is exectued, a character string containing the Warpscript otherwise.
#'
#' @export
#'
wrp_find <- function(wrp_con, token = get_token(), class = "~.*", labels = NULL) {
  assert_token(token)
  labels <- labels_to_string(labels)
  script <- glue::glue(
    "[ '{token}' '{class}' {{ {labels} }} ] FIND"
  )
  wrp_con$set_script(script)
  wrp_con$add_stack("lgts")
  return(wrp_con)
}
