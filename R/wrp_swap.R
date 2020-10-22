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
    stack       <- get_stack(wrp_con)
    n           <- length(stack)
    if (n < 2) stop("must have at least two elements on the stack.")
    last_object <- stack[[n]]
    last_last_object <- stack[[n - 1]]
    wrp_con$set_script("SWAP")
    wrp_con$add_stack(c(last_object, last_last_object), c(last_object, last_last_object))
    return(wrp_con)
}
