#' Values
#'
#' Gets the values of Geo Time Series™ or encoders.
#'
#' The VALUES function consumes a Geo Time Series™, an encoder or a list thereof
#' from the stack, and it replaces each instance of Geo Time Series™ or encoder
#' by a list of its values.
#'
#' @inheritParams documentation
#'
#' @keywords gts
#'
#' @export
#' 
#' @seealso [wrp_labels()]
#'
#' @examples
#' df <- data.frame(tick = c(100, 200, 300, 400, 500, 700, 800, 900, 1000, 1100), value = 10:1)
#' wrp_connect() %>%
#'   wrp_new_gts() %>%
#'   wrp_add_value_df(df) %>%
#'   wrp_values() %>%
#'   wrp_exec()
#'
wrp_values <- function(wrp_con) {
  return_object <- list(
    gts      = "list",
    lgts     = "list",
    encoder  = "list",
    lencoder = "list"
  )
  add_stack(wrp_con, "VALUES", return_object)
}
