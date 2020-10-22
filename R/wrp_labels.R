#' Labels
#'
#' The LABELS function consumes a GTS from the stack, extracts its labels,
#' puts them in a map and pushes the map onto the stack.
#'
#' @inheritParams documentation
#'
#' @keywords gts
#'
#' @export
#'
#' @seealso [wrp_attributes()], [wrp_set_attributes()], [wrp_values()]
#'
#' @examples
#' wrp_connect() %>%
#'     wrp_new_gts() %>%
#'     wrp_relabel(list(label0 = "42", label1 = "foo")) %>%
#'     wrp_labels() %>%
#'     wrp_exec()
wrp_labels <- function(wrp_con) {
    return_object <- list(
        gts      = "map",
        encoder  = "map"
    )
    add_stack(wrp_con, "LABELS", return_object)
}
