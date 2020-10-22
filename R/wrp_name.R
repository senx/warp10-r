#' Name
#'
#' The NAME function consumes a Geo Time Series™ from the stack,
#' extracts its class name, and pushes it onto the stack.
#'
#' @inheritParams documentation
#'
#' @keywords gts
#'
#' @export
#'
#' @seealso [wrp_attributes()], [wrp_labels()], [wrp_rename()], [wrp_set_attributes()], [wrp_values()]
#'
#' @examples
#' wrp_connect() %>%
#'     wrp_new_gts() %>%
#'     wrp_rename("GTS1") %>%
#'     wrp_name() %>%
#'     wrp_exec()
wrp_name <- function(wrp_con) {
    return_object <- list(
        gts      = "map",
        encoder  = "map"
    )
    add_stack(wrp_con, "NAME", return_object)
}
