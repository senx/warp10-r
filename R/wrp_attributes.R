#' Attributes
#'
#' Retrieves the attributes of a Geo Time Series™.
#'
#' The ATTRIBUTES function takes a GTS on top of the stack and push back a MAP
#' including all its attributes.
#'
#' An attribute corresponds to a tag for a specific series. The attribute system
#' allows the user to add some information that can change in a series.
#'
#' @inheritParams documentation
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_rename("test") %>%
#'   wrp_set_attributes(attr1 = "42", attr2 = "foo") %>%
#'   wrp_attributes() %>%
#'   wrp_exec()
#'
#' @keywords gts
#'
#' @seealso [wrp_set_attributes()], [wrp_labels()], [wrp_name()]
#'
wrp_attributes <- function(wrp_con) {
  return_object <- list(
    gts     = "map",
    encoder = "map"
  )
  add_stack(wrp_con, "ATTRIBUTES", return_object)
}
