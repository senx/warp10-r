#' Relabel
#'
#' Modifies the labels of a Geo Time Series™.
#' The RELABEL function expects a parameter MAP whose entries are labels to set or alter.
#'
#' If the parameter MAP has an entry with a NULL key, the RELABEL function will use the rest of the MAP
#' as the actual labels to set for the GTS.
#' If no NULL key exist, then the other entries of the MAP will alter the existing labels.
#'
#' An entry with an empty STRING value or a NULL value will have the effect of removing the label from the GTS' labels.
#'
#' @inheritParams documentation
#' @param ... Named parameters of label names to values.
#'
#' @examples
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_clone() %>%
#'   wrp_relabel(foo = "bar", bar = "foo") %>%
#'   wrp_clone() %>%
#'   wrp_relabel("NULL" = NULL, "star" = "treck") %>%
#'   wrp_clone() %>%
#'   wrp_relabel("next" = "generation", heckle = "jeckle") %>%
#'   wrp_clone() %>%
#'   wrp_relabel(heckle =NULL) %>%
#'   wrp_clone() %>%
#'   wrp_relabel("next" = "") %>%
#'   wrp_exec()
#' @export
#'
#' @seealso [wrp_set_attributes()], [wrp_rename()]
#'
wrp_relabel <- function(wrp_con, ...) {
  labels <- rlang::list2(...)
  script <- paste(sanitize(as.list(labels)), "RELABEL")
  return_object  <- list(
    gts      = "gts",
    lgts     = "lgts",
    encoder  = "encoder",
    lencoder = "lencoder"
  )

  add_stack(wrp_con, script, return_object)
}
