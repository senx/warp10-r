#' Set attributes
#'
#' Modifies the attributes of a Geo Time Series™, an encoder or a list thereof.
#' The SETATTRIBUTES function expects a parameter MAP whose entries are attributes to set or alter.
#'
#' If the parameter MAP has an entry with a NULL key,
#' the SETATTRIBUTES function will use the rest of the MAP as the actual attributes to set for the GTS.
#' If no NULL key exist, then the other entries of the MAP will alter the existing attributes.
#'
#' An entry with an empty STRING value will have the effect of removing the
#' attribute from the attributes of the GTSs or encoders.
#'
#' @inheritParams  documentation
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_clone() %>%
#'   wrp_set_attributes(list(foo = "bar", bar = "foo")) %>%
#'   wrp_clone() %>%
#'   wrp_set_attributes(list("null" = NULL, star = 'treck')) %>%
#'   wrp_clone() %>%
#'   wrp_set_attributes(list('next' = 'generation', heckle = 'jeckle')) %>%
#'   wrp_clone() %>%
#'   wrp_set_attributes(list('next' = '')) %>%
#'   wrp_clone() %>%
#'   wrp_set_attributes(list(heckle = 'Peter')) %>%
#'   wrp_exec()
#'
#' @keywords gts encoder
#'
#' @seealso [wrp_relabel()], [wrp_rename()], [wrp_meta()], [wrp_attributes()], [wrp_labels()]
#'
wrp_set_attributes <- function(wrp_con, attributes) {
  script        <- glue::glue("{sanitize(attributes)} SETATTRIBUTES")
  return_object <- list(
    gts      = "gts",
    lgts     = "lgts",
    encoder  = "encoder",
    lencoder = "lencoder"
  )
  add_stack(wrp_con, script = script, return_object = return_object)
}
