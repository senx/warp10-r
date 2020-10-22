#' Swap
#'
#' Exchanges the positions of the top two elements of the stack.
#'
#' @inheritParams documentation
#'
#' @keywords stack
#'
#' @export
#'
#' @examples
#' wrp_connect() %>%
#'     set_script("Thrid level") %>%
#'     set_script("Second level") %>%
#'     set_script("Top") %>%
#'     wrp_swap() %>%
#'     wrp_exec()
wrp_swap <- function(wrp_con) {
    return_object <- list(
        any = "any"
    )
    add_stack(wrp_con, "SWAP", return_object)
}
