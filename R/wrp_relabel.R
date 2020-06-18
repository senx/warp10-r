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
#' @param labels Parameter map of label names to values.
#'
#' @examples
#'
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_clone() %>%
#'   wrp_relabel(c("foo", "bar", "bar", "foo")) %>%
#'   wrp_clone() %>%
#'   wrp_relabel(list(NULL, NULL, "star", "treck")) %>%
#'   wrp_clone() %>%
#'   wrp_relabel(c("next", "generation", "heckle", "jeckle")) %>%
#'   wrp_clone() %>%
#'   wrp_relabel(list("heckle", NULL)) %>%
#'   wrp_clone() %>%
#'   wrp_relabel(c("next", "")) %>%
#'   wrp_exec()
#' @export
#'
#' @seealso [wrp_set_attributes()], [wrp_rename()]
#'
wrp_relabel <- function(wrp_con, labels) {
  labels  <- paste(purrr::map(labels, sanitize), collapse = " ")
  script  <- glue::glue("{{ {labels} }} RELABEL")
  stack   <- get_stack(wrp_con)
  consume <- stack[[length(stack)]]
  return  <- switch(
    consume,
    gts      = "gts",
    lgts     = "lgts",
    encoder  = "encoder",
    lencoder = "lencoder"
  )

  set_script(wrp_con, script = script, consume = consume, add = return)

  return(wrp_con)
}
